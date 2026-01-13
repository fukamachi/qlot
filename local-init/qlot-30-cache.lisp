;;; qlot-30-cache.lisp - Release-level cache for ql:quickload
;;;
;;; This file is self-contained and sets up release-level caching
;;; so that ql:quickload can use the global cache after qlot install.
;;;

(defpackage #:qlot/local-init/cache
  (:use #:cl))
(in-package #:qlot/local-init/cache)

;;;
;;; Configuration
;;;
;;; Cache settings are computed from environment variables and XDG defaults,
;;; matching the logic in qlot/cache.lisp's initialize-cache function.
;;;

(defvar *cache-enabled* nil
  "When T, release-level caching is enabled.")

(defvar *cache-directory* nil
  "Directory where cached releases are stored.")

(defun initialize-cache ()
  "Initialize cache settings from environment variables and XDG defaults."
  (setf *cache-enabled*
        (not (uiop:getenvp "QLOT_NO_CACHE")))
  (setf *cache-directory*
        (uiop:ensure-directory-pathname
         (or (uiop:getenvp "QLOT_CACHE_DIRECTORY")
             (merge-pathnames "qlot/" (uiop:xdg-cache-home))))))

;;;
;;; Utility macro
;;;

(defmacro with-package-functions (package-designator functions &body body)
  (let ((args (gensym "ARGS")))
    `(flet (,@(loop for fn in functions
                    collect `(,fn (&rest ,args)
                                  (apply
                                   ,(if (and (listp fn) (eq (car fn) 'setf))
                                        `(eval `(function (setf ,(intern ,(string (cadr fn)) ',package-designator))))
                                        `(symbol-function (intern ,(string fn) ',package-designator)))
                                   ,args))))
       ,@body)))

;;;
;;; Utility functions (from cache.lisp)
;;;

(defun reduce-cache-path (base components)
  (reduce (lambda (pathname component)
            (merge-pathnames (make-pathname :directory (list :relative component))
                             pathname))
          components
          :initial-value (uiop:ensure-directory-pathname base)))

(defun split-path (path)
  (when (and path (string/= "" path))
    (remove-if (lambda (segment) (string= segment ""))
               (uiop:split-string path :separator "/"))))

(defun strip-trailing-slash (path)
  "Remove trailing slash from a path string."
  (let ((str (if (pathnamep path)
                 (namestring path)
                 path)))
    (if (and (< 1 (length str))
             (char= (char str (1- (length str))) #\/))
        (subseq str 0 (1- (length str)))
        str)))

(defun symlink-p (path)
  "Return T if PATH is a symbolic link."
  (let ((clean-path (strip-trailing-slash path)))
    #+sbcl
    (handler-case
        (let ((stat (sb-posix:lstat clean-path)))
          (= (logand (sb-posix:stat-mode stat) #o170000)
             #o120000))
      (error () nil))
    #+ccl
    (handler-case
        (eq (ccl::%file-kind clean-path nil) :symbolic-link)
      (error () nil))
    #+ecl
    (handler-case
        (eq (ext:file-kind clean-path :follow-symlinks nil) :link)
      (error () nil))
    #+abcl
    (handler-case
        (java:jstatic "isSymbolicLink"
                      "java.nio.file.Files"
                      (java:jstatic "get"
                                    "java.nio.file.Paths"
                                    clean-path
                                    (java:jnew-array "java.lang.String" 0)))
      (error () nil))
    #-(or sbcl ccl ecl abcl)
    ;; Fallback: use shell command (works on Unix-like systems)
    (handler-case
        (zerop (nth-value 2
                 (uiop:run-program (list "test" "-L" clean-path)
                                   :ignore-error-status t)))
      (error () nil))))

(defun remove-path (path)
  (cond
    ;; Check for symlink first (before directory check, which follows symlinks)
    ((symlink-p path)
     (delete-file (parse-namestring (string-right-trim "/" (namestring path)))))
    ((uiop:directory-exists-p path)
     (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore))
    ((probe-file path)
     (uiop:delete-file-if-exists path))
    (t nil)))

(defun copy-directory (dir destination &key exclude)
  "Recursively copy directory."
  (let ((files
          (remove-if (or exclude (constantly nil)) (uiop:directory-files dir))))
    (when files
      (ensure-directories-exist destination)
      (dolist (file files)
        (uiop:copy-file file
                        (merge-pathnames (file-namestring file) destination)))))
  (dolist (subdir (uiop:subdirectories dir))
    (copy-directory subdir
                    (merge-pathnames (enough-namestring subdir dir) destination)
                    :exclude exclude)))

(defun copy-directory-tree (from to)
  (ensure-directories-exist to)
  (copy-directory from to))

(defun create-symlink (target link)
  "Create a symbolic link at LINK pointing to TARGET."
  (remove-path link)
  (handler-case
      (let ((target-str (strip-trailing-slash target))
            (link-str (strip-trailing-slash link)))
        #+sbcl
        (sb-posix:symlink target-str link-str)
        #-sbcl
        (uiop:run-program (list "ln" "-s" target-str link-str)
                          :ignore-error-status t))
    (error ()
      ;; Fallback to copy when symlink creation fails
      (copy-directory-tree target link))))

(defun move-directory (from to)
  (handler-case
      (progn
        (ensure-directories-exist to)
        (rename-file from to)
        to)
    (file-error ()
      (copy-directory-tree from to)
      (uiop:delete-directory-tree from :validate t)
      to)))

(defun make-directory-read-only (path)
  #+sbcl
  (progn
    (dolist (file (uiop:directory-files path))
      (sb-posix:chmod (namestring file) #o444))
    (dolist (subdir (uiop:subdirectories path))
      (make-directory-read-only subdir))
    (sb-posix:chmod (namestring path) #o755))
  #-sbcl
  (uiop:run-program (list "chmod" "-R" "a-w,a+r" (uiop:native-namestring path))
                    :ignore-error-status t))

;;;
;;; File locking
;;;

(defun cache-lock-file ()
  (merge-pathnames "cache.lock" *cache-directory*))

(defun acquire-lock (stream mode)
  #+sbcl
  (let ((fd (sb-sys:fd-stream-fd stream)))
    (ecase mode
      (:shared
       (sb-posix:fcntl fd sb-posix:f-setlkw
                       (make-instance 'sb-posix:flock
                                      :type sb-posix:f-rdlck
                                      :whence sb-posix:seek-set
                                      :start 0
                                      :len 0)))
      (:exclusive
       (sb-posix:fcntl fd sb-posix:f-setlkw
                       (make-instance 'sb-posix:flock
                                      :type sb-posix:f-wrlck
                                      :whence sb-posix:seek-set
                                      :start 0
                                      :len 0)))))
  #+ccl
  (let ((fd (ccl::stream-device stream :input)))
    (ecase mode
      (:shared (ccl::%flock fd 1))
      (:exclusive (ccl::%flock fd 2))))
  #+ecl
  (let ((fd (ext:file-stream-fd stream)))
    (ecase mode
      (:shared (ext:flock fd :shared :wait t))
      (:exclusive (ext:flock fd :exclusive :wait t))))
  #-(or sbcl ccl ecl)
  (declare (ignore stream mode)))

(defun release-lock (stream)
  #+sbcl
  (let ((fd (sb-sys:fd-stream-fd stream)))
    (sb-posix:fcntl fd sb-posix:f-setlk
                    (make-instance 'sb-posix:flock
                                   :type sb-posix:f-unlck
                                   :whence sb-posix:seek-set
                                   :start 0
                                   :len 0)))
  #+ccl
  (let ((fd (ccl::stream-device stream :input)))
    (ccl::%flock fd 8))
  #+ecl
  (let ((fd (ext:file-stream-fd stream)))
    (ext:flock fd :unlock))
  #-(or sbcl ccl ecl)
  (declare (ignore stream)))

(defmacro with-cache-lock ((mode) &body body)
  (let ((lock-stream (gensym "LOCK-STREAM")))
    `(let* ((lock-file (cache-lock-file))
            (,lock-stream (progn
                            (ensure-directories-exist lock-file)
                            (open lock-file
                                  :direction :io
                                  :if-does-not-exist :create
                                  :if-exists :overwrite))))
       (unwind-protect
            (progn
              (acquire-lock ,lock-stream ,mode)
              ,@body)
         (release-lock ,lock-stream)
         (close ,lock-stream)))))

;;;
;;; Release-level cache functions
;;;

(defun releases-directory ()
  "Return the directory where release caches are stored."
  (merge-pathnames "releases/" *cache-directory*))

(defun canonicalize-dist-url (url)
  "Convert dist URL to filesystem-safe identifier.
e.g., https://dist.quicklisp.org/dist/quicklisp.txt -> dist.quicklisp.org/quicklisp"
  (when (and url (stringp url) (< 0 (length url)))
    (let ((result url))
      ;; Remove scheme
      (when (uiop:string-prefix-p "https://" result)
        (setf result (subseq result (length "https://"))))
      (when (uiop:string-prefix-p "http://" result)
        (setf result (subseq result (length "http://"))))
      ;; Remove trailing .txt
      (when (uiop:string-suffix-p result ".txt")
        (setf result (subseq result 0 (- (length result) 4))))
      ;; Remove /dist/ prefix if present
      (setf result (uiop:split-string result :separator "/"))
      (setf result (remove-if (lambda (s) (string= s "dist")) result))
      (format nil "~{~A~^/~}" result))))

(defun release-cache-key (dist-canonical-url release-name archive-md5)
  "Generate cache key components for a release."
  (append (split-path dist-canonical-url)
          (list release-name archive-md5)))

(defun release-cache-path (dist-canonical-url release-name archive-md5)
  "Return the path to a cached release directory."
  (reduce-cache-path (releases-directory)
                     (release-cache-key dist-canonical-url release-name archive-md5)))

(defun release-cache-exists-p (dist-canonical-url release-name archive-md5)
  "Check if a release is cached and valid."
  (when *cache-enabled*
    (let ((path (release-cache-path dist-canonical-url release-name archive-md5)))
      (and path
           (uiop:directory-exists-p path)
           ;; Must have at least one subdirectory (the prefix dir)
           (uiop:subdirectories path)))))

(defun restore-release-from-cache (dist-canonical-url release-name archive-md5 target-dir prefix)
  "Create symlink at TARGET-DIR/PREFIX pointing to cached release.
Returns T on success."
  (with-cache-lock (:shared)
    (let* ((cache-path (release-cache-path dist-canonical-url release-name archive-md5))
           (cached-prefix-dir (merge-pathnames (make-pathname :directory (list :relative prefix))
                                               cache-path))
           (link-path (merge-pathnames (make-pathname :directory (list :relative prefix))
                                       target-dir)))
      (ensure-directories-exist target-dir)
      (create-symlink cached-prefix-dir link-path)
      t)))

(defun save-release-to-cache (dist-canonical-url release-name archive-md5 source-dir prefix)
  "Move installed release to cache and create symlink back."
  (with-cache-lock (:exclusive)
    (let* ((cache-path (release-cache-path dist-canonical-url release-name archive-md5))
           (cached-prefix-dir (merge-pathnames (make-pathname :directory (list :relative prefix))
                                               cache-path))
           (source-prefix-dir (merge-pathnames (make-pathname :directory (list :relative prefix))
                                               source-dir)))
      ;; Check if already cached (maybe by concurrent process)
      (unless (release-cache-exists-p dist-canonical-url release-name archive-md5)
        (ensure-directories-exist cache-path)
        (move-directory source-prefix-dir cached-prefix-dir)
        (make-directory-read-only cached-prefix-dir))
      ;; Create symlink back (whether we just cached or it already existed)
      (remove-path source-prefix-dir)
      (create-symlink cached-prefix-dir source-prefix-dir))))

;;;
;;; Install markers (for Quicklisp to recognize as installed)
;;;

(defun create-release-install-markers (release)
  "Create Quicklisp install markers for a release restored from cache.
This makes Quicklisp recognize the release as installed."
  (with-package-functions #:ql-dist (dist base-directory
                                     system-files install-metadata-file
                                     provided-systems find-system-in-dist)
    (let ((dist (dist release))
          (tracking (install-metadata-file release)))
      ;; Write release tracking file
      (ensure-directories-exist tracking)
      (with-open-file (stream tracking
                              :direction :output
                              :if-exists :supersede)
        (write-line (uiop:symbol-call '#:ql-setup '#:qenough (base-directory release)) stream))
      ;; Write system tracking files
      (let ((provided (provided-systems release)))
        (dolist (file (system-files release))
          (let ((system (find-system-in-dist (pathname-name file) dist)))
            (when (member system provided)
              (let ((system-tracking (install-metadata-file system))
                    (system-file (merge-pathnames file (base-directory release))))
                (ensure-directories-exist system-tracking)
                (with-open-file (stream system-tracking
                                        :direction :output
                                        :if-exists :supersede)
                  (write-line (uiop:symbol-call '#:ql-setup '#:qenough system-file)
                              stream))))))))))

;;;
;;; Release cache hook implementation
;;;

(defun release-cache-install-impl (release default-fn)
  "Handle caching logic for release installation via hook."
  (with-package-functions #:ql-dist (archive-url dist canonical-distinfo-url name
                                     archive-md5 prefix relative-to)
    (let ((archive-url (archive-url release)))
      ;; Skip caching for qlot:// sources (already handled by dist-level cache)
      (when (uiop:string-prefix-p "qlot://" archive-url)
        (return-from release-cache-install-impl (funcall default-fn release)))

      (let* ((dist (dist release))
             (dist-url (canonicalize-dist-url (canonical-distinfo-url dist)))
             (release-name (name release))
             (archive-md5 (archive-md5 release))
             (prefix (prefix release))
             (software-dir (relative-to dist
                                        (make-pathname :directory '(:relative "software")))))
        ;; Check cache first
        (if (release-cache-exists-p dist-url release-name archive-md5)
            ;; Cache hit - try to restore, fall back to normal install on failure
            (handler-case
                (progn
                  (restore-release-from-cache dist-url release-name archive-md5 software-dir prefix)
                  (create-release-install-markers release)
                  release)
              (error ()
                ;; Cache restoration failed, fall back to normal install
                (funcall default-fn release)))
            ;; Cache miss - install normally then try to cache (ignore cache errors)
            (progn
              (funcall default-fn release)
              (ignore-errors
                (save-release-to-cache dist-url release-name archive-md5 software-dir prefix))
              release))))))

;;;
;;; Hook setup
;;;

(defun qlot-install-mode-p ()
  "Check if we're running inside qlot install (where :around method handles caching)."
  (and (find-package '#:qlot/install)
       (let ((sym (find-symbol (string '#:*release-cache-active*) '#:qlot/install)))
         (and sym (boundp sym) (symbol-value sym)))))

(defun release-cache-hook (release default-fn)
  "Hook for ql-dist:*install-release-hook* to enable caching."
  (cond
    ;; During qlot install, the :around method handles caching - just call default
    ((qlot-install-mode-p)
     (funcall default-fn release))
    ;; Normal ql:quickload - use cache
    (*cache-enabled*
     (release-cache-install-impl release default-fn))
    ;; Cache disabled
    (t
     (funcall default-fn release))))

(defun setup-release-cache-hook ()
  (when (and *cache-enabled* (find-package '#:ql-dist))
    (setf ql-dist:*install-release-hook* #'release-cache-hook)))

;;;
;;; Initialize on load
;;;

(initialize-cache)
(setup-release-cache-hook)
