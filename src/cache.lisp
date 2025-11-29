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
           (probe-file sources)))))

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

(defun create-symlink (target link)
  #+sbcl
  (sb-posix:symlink (namestring target) (namestring link))
  #-sbcl
  (uiop:run-program (list "ln" "-s"
                          (uiop:native-namestring target)
                          (uiop:native-namestring link))
                    :ignore-error-status t))

(defun remove-path (path)
  (cond
    ((uiop:directory-exists-p path)
     (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore))
    ((probe-file path)
     (uiop:delete-file-if-exists path))
    (t nil)))

(defun make-directory-read-only (path)
  #+sbcl
  (sb-posix:chmod (namestring path) #o555)
  #-sbcl
  (uiop:run-program (list "chmod" "-R" "a-w,a+r" (uiop:native-namestring path))
                    :ignore-error-status t))

(defun restore-from-cache (source dist-path)
  (declare (ignore source dist-path))
  nil)

(defun save-to-cache (source dist-path)
  (declare (ignore source dist-path))
  nil)

(defun save-to-cache-impl (source dist-path)
  (declare (ignore source dist-path))
  nil)

(defun create-install-markers (dist-path)
  (declare (ignore dist-path))
  nil)

(defun cache-lock-file ()
  (merge-pathnames "cache.lock" *cache-directory*))

(defstruct cache-lock
  stream)

(defun acquire-lock (&key (mode :exclusive))
  (declare (ignore mode))
  (let ((lock-file (cache-lock-file)))
    (ensure-directories-exist lock-file)
    (make-cache-lock :stream (open lock-file
                                   :direction :io
                                   :if-does-not-exist :create
                                   :if-exists :append))))

(defun release-lock (lock)
  (when (and lock (cache-lock-stream lock))
    (close (cache-lock-stream lock))))

(defmacro with-cache-lock ((&optional (mode :exclusive)) &body body)
  `(let ((lock (acquire-lock :mode ,mode)))
     (unwind-protect (progn ,@body)
       (release-lock lock))))

(defun validate-dist-installation (dist-path)
  (let ((software (merge-pathnames "software/" dist-path)))
    (if (uiop:directory-exists-p software)
        (let ((entries (ql-dist::directory-entries software)))
          (and entries
               (loop for entry in entries
                     always (probe-file entry))))
        t)))

(initialize-cache)
