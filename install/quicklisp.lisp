(defpackage #:qlot/install/quicklisp
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
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
                                        (equal (file-namestring file) "version.txt"))))))
  (mapc #'ensure-directories-exist
        (mapcar (lambda (dirname)
                  (merge-pathnames dirname path))
                (list "local-projects/"
                      "dists/"
                      "tmp/"))))

(defun install-quicklisp-with-installer (path)
  (let ((quicklisp-file (asdf:system-relative-pathname :qlot #P"quicklisp/quicklisp-installer.lisp")))
    (run-lisp (list
               `(let ((*standard-output* (make-broadcast-stream)))
                  (load ,quicklisp-file))
               "(setf quicklisp-quickstart:*after-initial-setup-message* \"\")"
               (format nil "(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (quicklisp-quickstart:install :path #P\"~A\"~@[ :proxy \"~A\"~]))"
                       path
                       *proxy*)))))

(defun install-quicklisp (path)
  (message "Installing Quicklisp to ~A..." path)
  (if (uiop:file-exists-p (asdf:system-relative-pathname :qlot #P"quicklisp-client/setup.lisp"))
      (install-quicklisp-from-subdir path)
      (install-quicklisp-with-installer path))
  (copy-local-init-files path)
  t)
