(defpackage #:qlot/install/quicklisp
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/utils/file
                #:copy-directory)
  (:export #:install-quicklisp
           #:install-local-init-files))
(in-package #:qlot/install/quicklisp)

(defun version-match-p (path)
  (let ((version-file (merge-pathnames #P"qlot-version.txt" path)))
    (and (uiop:file-exists-p version-file)
         (equal (asdf:component-version (asdf:find-system :qlot))
                (string-trim '(#\Newline #\Return #\Space)
                             (uiop:read-file-string version-file))))))

(defun copy-local-init-files (path)
  (let ((local-init-dir (merge-pathnames #P"local-init/" path)))
    (ensure-directories-exist local-init-dir)
    (dolist (file (uiop:directory-files (asdf:system-relative-pathname :qlot #P"local-init/")))
      (uiop:copy-file file (merge-pathnames (file-namestring file)
                                            local-init-dir)))))

(defun install-local-init-files (path)
  (let ((local-init-dir (merge-pathnames #P"local-init/" path)))
    (unless (and (uiop:directory-exists-p local-init-dir)
                 (version-match-p path))
      (uiop:delete-directory-tree local-init-dir
                                  :if-does-not-exist :ignore
                                  :validate t)
      (copy-local-init-files path)
      (write-version-txt path))))

(defun write-version-txt (path)
  (let ((file (merge-pathnames #P"qlot-version.txt" path)))
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write-string
       (asdf:component-version (asdf:find-system :qlot))
       out))))

(defun install-quicklisp-from-subdir (path)
  (ensure-directories-exist path)
  (let ((quicklisp-client (asdf:system-relative-pathname :qlot #P"quicklisp-client/")))
    (copy-directory quicklisp-client path
                    :exclude (lambda (file)
                               (not (or (equal (pathname-type file) "lisp")
                                        (equal (pathname-type file) "asd")
                                        (equal (file-namestring file) "version.txt"))))))
  (mapc #'ensure-directories-exist
        (mapcar (lambda (dirname)
                  (merge-pathnames dirname path))
                (list "local-projects/"
                      "dists/"
                      "tmp/")))
  (write-version-txt path)
  t)

(defun install-quicklisp (path)
  (message "Installing Quicklisp to ~A..." path)
  (assert (uiop:file-exists-p (asdf:system-relative-pathname :qlot #P"quicklisp-client/setup.lisp")))
  (install-quicklisp-from-subdir path)
  (copy-local-init-files path)
  t)
