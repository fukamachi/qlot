(defpackage #:qlot/install/quicklisp
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:export #:install-quicklisp))
(in-package #:qlot/install/quicklisp)

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
    t))
