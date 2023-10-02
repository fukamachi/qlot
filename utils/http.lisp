(defpackage #:qlot/utils/http
  (:use #:cl)
  (:shadow #:get)
  (:nicknames #:qdex)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:dexador)
  #-windows
  (:import-from #:cl+ssl)
  (:export #:fetch
           #:get))
(in-package #:qlot/utils/http)

(defmacro with-retry (() &body body)
  `(let ((retry-request (dex:retry-request 2 :interval 3)))
     (handler-bind ((dex:http-request-failed retry-request)
                    #-windows
                    (cl+ssl::ssl-error retry-request))
       ,@body)))

(defun fetch (url file &key basic-auth)
  (with-retry ()
    (apply #'dex:fetch url file
           :if-exists :supersede
           :keep-alive nil
           :proxy *proxy*
           (and basic-auth
                (list :basic-auth basic-auth)))))

(defun get (url &rest args)
  (with-retry ()
    (apply #'dex:get url
           :keep-alive nil
           :proxy *proxy*
           args)))
