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

(defmacro with-retry (() &body body)
  `(let ((retry-request (dex:retry-request 2 :interval 3)))
     (handler-bind ((dex:http-request-failed retry-request)
                    #-(or mswindows win32)
                    (cl+ssl::ssl-error retry-request))
       ,@body)))

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
           args)))
