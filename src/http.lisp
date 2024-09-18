(defpackage #:qlot/http
  (:use #:cl)
  (:shadow #:get)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/logger
                #:*debug*)
  (:import-from #:dexador)
  #-(or mswindows win32)
  (:import-from #:cl+ssl)
  (:export #:fetch
           #:get))
(in-package #:qlot/http)

(defun call-with-retry (fn)
  (let ((retry-request (dex:retry-request 2 :interval 3))
        (retry-connect (dex:retry-request 1)))
    (handler-bind ((dex:http-request-failed
                     (lambda (e)
                       (when (<= 500 (dex:response-status e))
                         (funcall retry-request e))))
                   #-(or mswindows win32)
                   ((or usocket:host-down-error
                        usocket:host-unreachable-error)
                     retry-request)
                   (end-of-file retry-connect)
                   #+sbcl
                   ((or sb-bsd-sockets:interrupted-error
                        sb-bsd-sockets:operation-timeout-error)
                     retry-connect)
                   #-(or mswindows win32)
                   ((or usocket:connection-reset-error
                        usocket:timeout-error
                        cl+ssl::ssl-error)
                     retry-connect))
      (funcall fn))))

(defmacro with-retry (() &body body)
  `(call-with-retry (lambda () ,@body)))

(defun fetch (url file &key basic-auth)
  (with-retry ()
    (apply #'dex:fetch url file
           :if-exists :supersede
           :keep-alive nil
           :proxy *proxy*
           :verbose *debug*
           (and basic-auth
                (list :basic-auth basic-auth)))))

(defun get (url &rest args &key want-stream basic-auth force-binary)
  (declare (ignore want-stream basic-auth force-binary))
  (with-retry ()
    (apply #'dex:get url
           :keep-alive nil
           :proxy *proxy*
           :verbose *debug*
           :connect-timeout nil
           :read-timeout 30
           args)))
