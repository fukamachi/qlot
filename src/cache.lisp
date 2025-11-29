(defpackage #:qlot/cache
  (:use #:cl)
  (:import-from #:qlot/source/base
                #:source
                #:source-project-name
                #:source-version
                #:source-dist-name)
  (:import-from #:qlot/source/ql
                #:source-ql
                #:source-ql-upstream)
  (:import-from #:qlot/source/ultralisp
                #:source-ultralisp)
  (:import-from #:qlot/source/dist
                #:source-dist
                #:source-distribution)
  (:import-from #:qlot/source/git
                #:source-git
                #:source-git-remote-url
                #:source-git-ref)
  (:import-from #:qlot/source/github
                #:source-github
                #:source-github-repos
                #:source-github-ref)
  (:import-from #:qlot/utils/file
                #:copy-directory)
  (:export #:*cache-directory*
           #:*cache-enabled*
           #:cache-key
           #:cache-metadata-path
           #:cache-sources-path
           #:cache-exists-p
           #:restore-from-cache
           #:save-to-cache
           #:normalize-git-url
           #:split-path
           #:url-has-credentials-p
           #:source-has-credentials-p
           #:make-directory-read-only
           #:staging-path
           #:validate-dist-installation
           #:clear-cache
           #:with-cache-lock
           #:cache-lock-file))
(in-package #:qlot/cache)

(defvar *cache-directory*
  (uiop:ensure-directory-pathname
   (merge-pathnames "qlot/" (uiop:xdg-cache-home))))

(defvar *cache-enabled* t)

(defparameter *cache-format-version* 1)

(defun cache-version-file ()
  (merge-pathnames "VERSION" *cache-directory*))

(defun metadata-directory ()
  (merge-pathnames "metadata/" *cache-directory*))

(defun sources-directory ()
  (merge-pathnames "sources/" *cache-directory*))

(defun initialize-cache ()
  (let ((env-dir (uiop:getenv "QLOT_CACHE_DIRECTORY")))
    (when env-dir
      (setf *cache-directory* (uiop:ensure-directory-pathname env-dir))))
  (when (uiop:getenv "QLOT_NO_CACHE")
    (setf *cache-enabled* nil))
  (when *cache-enabled*
    (ensure-cache-writable)
    (check-cache-version)
    (cleanup-orphaned-staging)))

(defun ensure-cache-writable ()
  (handler-case
      (let ((test-file (merge-pathnames "write-test" *cache-directory*)))
        (uiop:ensure-directory-pathname *cache-directory*)
        (ensure-directories-exist test-file)
        (with-open-file (stream test-file
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
          (write-line "ok" stream))
        (uiop:delete-file-if-exists test-file)
        t)
    (error (condition)
      (warn "Cache directory ~A is not writable: ~A. Disabling cache."
            *cache-directory* condition)
      (setf *cache-enabled* nil)
      nil)))

(defun check-cache-version ()
  (let ((version-file (cache-version-file)))
    (cond
      ((not (uiop:file-exists-p version-file))
       (ensure-directories-exist version-file)
       (with-open-file (stream version-file
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
         (write *cache-format-version* :stream stream)))
      (t
       (let ((stored (with-open-file (stream version-file)
                       (read stream nil nil))))
         (unless (eql stored *cache-format-version*)
           (warn "Cache version mismatch (~A vs ~A). Clearing cache."
                 stored *cache-format-version*)
           (clear-cache)
           (ensure-directories-exist version-file)
           (with-open-file (stream version-file
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write *cache-format-version* :stream stream))))))))

(defun clear-cache ()
  (when (uiop:directory-exists-p *cache-directory*)
    (uiop:delete-directory-tree *cache-directory* :validate t)))

(defun cleanup-orphaned-staging ()
  (flet ((staging-directories (base)
           (when (uiop:directory-exists-p base)
             (remove-if-not (lambda (dir)
                              (uiop:string-suffix-p (namestring dir) ".staging/"))
                            (uiop:subdirectories base)))))
    (dolist (dir (append (staging-directories (metadata-directory))
                         (staging-directories (sources-directory))))
      (ignore-errors
        (uiop:delete-directory-tree dir :validate t)))))

(defgeneric cache-key (source)
  (:documentation "Return list components representing cache path for SOURCE."))

(defmethod cache-key ((source source-ql))
  (list "ql" "quicklisp"
        (format nil "~A-~A"
                (source-project-name source)
                (source-version source))))

(defmethod cache-key ((source source-ultralisp))
  (list "ql" "ultralisp"
        (format nil "~A-~A"
                (source-project-name source)
                (source-version source))))

(defmethod cache-key ((source source-dist))
  (list "dist"
        (source-dist-name source)
        (source-version source)))

(defmethod cache-key ((source source-git))
  (let ((normalized (normalize-git-url (source-git-remote-url source))))
    (when normalized
      (append (list "git")
              (split-path normalized)
              (when (source-git-ref source)
                (list (source-git-ref source)))))))

(defmethod cache-key ((source source-github))
  (append (list "github")
          (split-path (source-github-repos source))
          (when (source-github-ref source)
            (list (source-github-ref source)))))

(defmethod cache-key ((source source-ql-upstream))
  (let ((url (source-git-remote-url source)))
    (when url
      (let ((normalized (normalize-git-url url)))
        (append (list "ql-upstream")
                (split-path normalized)
                (when (source-git-ref source)
                  (list (source-git-ref source))))))))

(defun reduce-cache-path (base components)
  (reduce (lambda (pathname component)
            (merge-pathnames (make-pathname :directory (list :relative component))
                             pathname))
          components
          :initial-value (uiop:ensure-directory-pathname base)))

(defun cache-metadata-path (source)
  (let ((key (cache-key source)))
    (when key
      (reduce-cache-path (metadata-directory) key))))

(defun cache-sources-path (source)
  (let ((key (cache-key source)))
    (when key
      (reduce-cache-path (sources-directory) key))))

(defun normalize-git-url (url)
  (when (and (stringp url)
             (plusp (length url)))
    (let ((result (string-right-trim "/" url)))
      (when (uiop:string-suffix-p result ".git")
        (setf result (subseq result 0 (- (length result) 4))))
      (cond
        ((uiop:string-prefix-p "git@" result)
         (let* ((trimmed (subseq result 4))
                (colon (position #\: trimmed)))
           (when colon
             (format nil "~A/~A"
                     (string-downcase (subseq trimmed 0 colon))
                     (subseq trimmed (1+ colon))))))
        ((uiop:string-prefix-p "ssh://git@" result)
         (let* ((trimmed (subseq result (length "ssh://git@")))
                (slash (position #\/ trimmed)))
           (when slash
             (format nil "~A~A"
                     (string-downcase (subseq trimmed 0 slash))
                     (subseq trimmed slash)))))
        ((or (uiop:string-prefix-p "https://" result)
             (uiop:string-prefix-p "http://" result))
         (let* ((scheme-length (if (uiop:string-prefix-p "https://" result)
                                   (length "https://")
                                   (length "http://")))
                (trimmed (subseq result scheme-length))
                (slash (position #\/ trimmed)))
           (when slash
             (format nil "~A~A"
                     (string-downcase (subseq trimmed 0 slash))
                     (subseq trimmed slash)))))
        (t result)))))

(defun split-path (path)
  (when (and path (string/= "" path))
    (remove-if (lambda (segment) (string= segment ""))
               (uiop:split-string path :separator "/"))))

(defun staging-path (pathname)
  (let* ((namestring (uiop:native-namestring (uiop:ensure-directory-pathname pathname)))
         (trimmed (string-right-trim "/" namestring)))
    (uiop:ensure-directory-pathname
     (format nil "~A.staging/" trimmed))))

(defun url-has-credentials-p (url)
  (when (stringp url)
    (let ((at (position #\@ url))
          (scheme (search "://" url)))
      (and at scheme
           (< scheme at)
           (let* ((auth (subseq url (+ scheme 3) at))
                  (colon (position #\: auth)))
             colon)))))

(defun source-has-credentials-p (source)
  (cond
    ((typep source 'source-git)
     (url-has-credentials-p (source-git-remote-url source)))
    ((typep source 'source-ql-upstream)
     (url-has-credentials-p (source-git-remote-url source)))
    ((typep source 'source-github)
     (url-has-credentials-p (source-github-repos source)))
    ((typep source 'source-dist)
     (url-has-credentials-p (source-distribution source)))
    (t nil)))

(defun cache-exists-p (source)
  (when (and *cache-enabled*
             (not (source-has-credentials-p source)))
    (let ((metadata (cache-metadata-path source))
          (sources (cache-sources-path source)))
      (and metadata sources
           (uiop:directory-exists-p metadata)
           (uiop:directory-exists-p sources)
           (uiop:file-exists-p (merge-pathnames "distinfo.txt" metadata))
           (uiop:file-exists-p (merge-pathnames "systems.txt" metadata))
           (uiop:file-exists-p (merge-pathnames "releases.txt" metadata))))))

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

(defun copy-directory-tree (from to)
  (ensure-directories-exist to)
  (copy-directory from to))

(defun strip-trailing-slash (path)
  "Remove trailing slash from a path string."
  (let ((str (if (pathnamep path)
                 (namestring path)
                 path)))
    (if (and (< 1 (length str))
             (char= (char str (1- (length str))) #\/))
        (subseq str 0 (1- (length str)))
        str)))

(defun create-symlink (target link)
  "Create a symbolic link at LINK pointing to TARGET.
TARGET and LINK can be pathnames or strings. Directory pathnames
with trailing slashes are handled correctly."
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
      ;; Fallback to copy when symlink creation fails (e.g., restricted FS)
      (copy-directory-tree target link))))

(defun remove-path (path)
  (cond
    ((uiop:directory-exists-p path)
     (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore))
    ((probe-file path)
     (uiop:delete-file-if-exists path))
    (t nil)))

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

(defun restore-from-cache (source dist-path)
  "Restore SOURCE into DIST-PATH from cache. Returns T on success, NIL on fallback."
  (handler-case
      (with-cache-lock (:shared)
        (let ((metadata-cache (cache-metadata-path source))
              (sources-cache (cache-sources-path source)))
          (ensure-directories-exist dist-path)
          ;; copy metadata files
          (dolist (file '("distinfo.txt" "systems.txt" "releases.txt"))
            (let ((src (merge-pathnames file metadata-cache))
                  (dst (merge-pathnames file dist-path)))
              (uiop:delete-file-if-exists dst)
              (uiop:copy-file src dst)))
          ;; link software dirs
          (let ((software-dir (merge-pathnames "software/" dist-path)))
            (ensure-directories-exist software-dir)
            (dolist (project-dir (uiop:subdirectories sources-cache))
              (let* ((dir-name (car (last (pathname-directory project-dir))))
                     (link-path (merge-pathnames
                                 (make-pathname :directory (list :relative dir-name))
                                 software-dir)))
                (create-symlink project-dir link-path))))
          (create-install-markers dist-path)
          t))
    ((or file-error type-error) (e)
      (warn "Cache for ~A is unusable, will reinstall: ~A"
            (source-project-name source) e)
      (ignore-errors
        (uiop:delete-directory-tree (cache-metadata-path source) :validate t)
        (uiop:delete-directory-tree (cache-sources-path source) :validate t))
      nil)))

(defun save-to-cache (source dist-path)
  "Attempt to save SOURCE from DIST-PATH into cache; non-fatal on failure."
  (when (source-has-credentials-p source)
    (warn "Skipping cache for ~A: credentials present in URL" (source-project-name source))
    (return-from save-to-cache))
  (handler-case
      (save-to-cache-impl source dist-path)
    (storage-condition (e)
      (warn "Failed to cache ~A: storage error ~A" (source-project-name source) e))
    (file-error (e)
      (warn "Failed to cache ~A: ~A" (source-project-name source) e))))

(defun save-to-cache-impl (source dist-path)
  (let ((metadata-cache (cache-metadata-path source))
        (sources-cache (cache-sources-path source)))
    (with-cache-lock (:exclusive)
      (when (cache-exists-p source)
        ;; ensure local software points at cache if someone else wrote it
        (let ((software-dir (merge-pathnames "software/" dist-path)))
          (dolist (project-dir (uiop:subdirectories software-dir))
            (let* ((dir-name (car (last (pathname-directory project-dir))))
                   (cache-target (merge-pathnames
                                  (make-pathname :directory (list :relative dir-name))
                                  sources-cache)))
              (remove-path project-dir)
              (create-symlink cache-target project-dir))))
        (return-from save-to-cache-impl))
      (let ((metadata-staging (staging-path metadata-cache))
            (sources-staging (staging-path sources-cache)))
        (ignore-errors (uiop:delete-directory-tree metadata-staging :validate t))
        (ignore-errors (uiop:delete-directory-tree sources-staging :validate t))
        (unwind-protect
             (progn
               ;; metadata
               (ensure-directories-exist metadata-staging)
               (dolist (file '("distinfo.txt" "systems.txt" "releases.txt"))
                 (let ((src (merge-pathnames file dist-path))
                       (dst (merge-pathnames file metadata-staging)))
                   (uiop:delete-file-if-exists dst)
                   (uiop:copy-file src dst)))
               ;; sources
               (let* ((software-dir (merge-pathnames "software/" dist-path))
                      (project-dirs (uiop:subdirectories software-dir)))
                 (ensure-directories-exist sources-staging)
                 (dolist (project-dir project-dirs)
                   (let* ((dir-name (car (last (pathname-directory project-dir))))
                          (staging-target (merge-pathnames
                                           (make-pathname :directory (list :relative dir-name))
                                           sources-staging)))
                     (move-directory project-dir staging-target))))
               ;; atomically publish
               (rename-file metadata-staging metadata-cache)
               (rename-file sources-staging sources-cache)
               (make-directory-read-only sources-cache)
               ;; recreate symlinks in project pointing to cache
               (let ((software-dir (merge-pathnames "software/" dist-path)))
                 (ensure-directories-exist software-dir)
                 (dolist (project-dir (uiop:subdirectories sources-cache))
                   (let* ((dir-name (car (last (pathname-directory project-dir))))
                          (link-path (merge-pathnames
                                      (make-pathname :directory (list :relative dir-name))
                                      software-dir)))
                     (create-symlink project-dir link-path))))))
          (ignore-errors (uiop:delete-directory-tree metadata-staging :validate t))
          (ignore-errors (uiop:delete-directory-tree sources-staging :validate t))))))

(defun parse-releases-txt (dist-path)
  "Parse releases.txt and return an alist mapping prefix to release-name."
  (let ((releases-file (merge-pathnames "releases.txt" dist-path))
        (result nil))
    (when (uiop:file-exists-p releases-file)
      (with-open-file (stream releases-file)
        (loop for line = (read-line stream nil nil)
              while line
              do (unless (or (zerop (length line))
                             (char= (char line 0) #\#))
                   ;; Format: project url size file-md5 content-sha1 prefix [system-file1..]
                   (let* ((parts (uiop:split-string line :separator " "))
                          (release-name (first parts))
                          (prefix (sixth parts)))
                     (when (and release-name prefix)
                       (push (cons prefix release-name) result)))))))
    result))

(defun create-install-markers (dist-path)
  "Create Quicklisp install marker files for all projects in dist-path.
Paths in marker files are stored relative to qlhome (parent of dists/)."
  (let* ((installed-dir (merge-pathnames "installed/" dist-path))
         (releases-dir (merge-pathnames "releases/" installed-dir))
         (systems-dir (merge-pathnames "systems/" installed-dir))
         (software-dir (merge-pathnames "software/" dist-path))
         ;; qlhome is the parent of dists/, which is two levels up from dist-path
         ;; dist-path: .qlot/dists/<distname>/ -> qlhome: .qlot/
         (qlhome (uiop:pathname-parent-directory-pathname
                  (uiop:pathname-parent-directory-pathname dist-path)))
         ;; Get prefix -> release-name mapping
         (prefix-to-name (parse-releases-txt dist-path)))
    (ensure-directories-exist releases-dir)
    (ensure-directories-exist systems-dir)
    (dolist (project-path (uiop:subdirectories software-dir))
      (let* ((prefix (car (last (pathname-directory project-path))))
             ;; Use the release name from releases.txt, fall back to prefix if not found
             (release-name (or (cdr (assoc prefix prefix-to-name :test #'string=))
                               prefix)))
        (with-open-file (s (merge-pathnames (format nil "~A.txt" release-name) releases-dir)
                           :direction :output :if-exists :supersede)
          (write-line (enough-namestring project-path qlhome) s))
        (dolist (asd-file (uiop:directory-files project-path "*.asd"))
          (let ((system-name (pathname-name asd-file)))
            (with-open-file (s (merge-pathnames (format nil "~A.txt" system-name) systems-dir)
                               :direction :output :if-exists :supersede)
              (write-line (enough-namestring asd-file qlhome) s))))))))

(defun list-directory-entries (directory)
  "List all entries (files and subdirectories) in DIRECTORY without resolving symlinks."
  (append (uiop:directory-files directory)
          (uiop:subdirectories directory)))

(defun validate-dist-installation (dist-path)
  "Validate that all entries in the dist's software directory are accessible.
Returns T if the installation is valid, NIL otherwise."
  (let ((software (merge-pathnames "software/" dist-path)))
    (if (uiop:directory-exists-p software)
        (let ((entries (list-directory-entries software)))
          (and entries
               (loop for entry in entries
                     always (probe-file entry))))
        t)))

(initialize-cache)
