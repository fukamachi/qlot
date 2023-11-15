(defpackage #:qlot/install/quicklisp
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/utils/file
                #:copy-directory)
  (:export #:install-quicklisp
           #:copy-local-init-file))
(in-package #:qlot/install/quicklisp)

(defun copy-local-init-files (path)
  (let ((local-init-dir (merge-pathnames #P"local-init/" path)))
    (ensure-directories-exist local-init-dir)
    (dolist (file (uiop:directory-files (asdf:system-relative-pathname :qlot #P"local-init/")))
      (uiop:copy-file file (merge-pathnames (file-namestring file)
                                            local-init-dir)))))

(defun install-quicklisp-from-subdir (path)
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
  t)

(defun install-quicklisp (path)
  (message "Installing Quicklisp to ~A..." path)
  (assert (uiop:file-exists-p (asdf:system-relative-pathname :qlot #P"quicklisp-client/setup.lisp")))
  (install-quicklisp-from-subdir path)
  (copy-local-init-files path)
  t)
