(defpackage #:qlot/install/quicklisp
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/config
                #:dump-qlot-config)
  (:import-from #:qlot/utils/file
                #:copy-directory)
  (:export #:install-quicklisp
           #:install-local-init-files
           #:install-qlot-config-file))
(in-package #:qlot/install/quicklisp)

(defun copy-local-init-files (path)
  (let ((local-init-dir (merge-pathnames #P"local-init/" path)))
    (ensure-directories-exist local-init-dir)
    (dolist (file (uiop:directory-files (asdf:system-relative-pathname :qlot #P"local-init/")))
      (uiop:copy-file file (merge-pathnames (file-namestring file)
                                            local-init-dir)))))

(defun install-local-init-files (path)
  (let ((local-init-dir (merge-pathnames #P"local-init/" path)))
    (when (uiop:directory-exists-p local-init-dir)
      (mapc #'delete-file
            (uiop:directory-files local-init-dir "qlot-*.lisp")))
    (copy-local-init-files path)))

(defun install-qlot-config-file (path)
  (with-open-file (out (merge-pathnames "qlot.conf" path)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (dump-qlot-config out)))

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
  t)

(defun install-quicklisp (path)
  (message "Installing Quicklisp to ~A..." path)
  (assert (uiop:file-exists-p (asdf:system-relative-pathname :qlot #P"quicklisp-client/setup.lisp")))
  (install-quicklisp-from-subdir path)
  (copy-local-init-files path)
  (install-qlot-config-file path)
  t)
