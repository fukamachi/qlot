(defpackage #:qlot/install/quicklisp
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:export #:install-quicklisp
           #:copy-local-init-file))
(in-package #:qlot/install/quicklisp)

(defun copy-local-init-files (path)
  (let ((local-init-dir (merge-pathnames #P"local-init/" path)))
    (ensure-directories-exist local-init-dir)
    (dolist (file (uiop:directory-files (asdf:system-relative-pathname :qlot #P"local-init/")))
      (uiop:copy-file file (merge-pathnames (file-namestring file)
                                            local-init-dir)))))

(defun install-quicklisp (path)
  (message "Installing Quicklisp to ~A ..." path)
  (let ((quicklisp-file (asdf:system-relative-pathname :qlot #P"quicklisp/quicklisp-installer.lisp")))
    (run-lisp (list
               `(let ((*standard-output* (make-broadcast-stream)))
                  (load ,quicklisp-file))
               "(setf quicklisp-quickstart:*after-initial-setup-message* \"\")"
               (format nil "(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (quicklisp-quickstart:install :path #P\"~A\"~@[ :proxy \"~A\"~]))"
                       path
                       *proxy*))
              :without-quicklisp t)
    (copy-local-init-files path)
    t))
