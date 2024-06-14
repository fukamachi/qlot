(defpackage #:qlot/http
  (:use #:cl)
  (:shadow #:get)
  #-qlot-no-dexador
  (:import-from #:qlot/proxy
                #:*proxy*)
  #-qlot-no-dexador
  (:import-from #:dexador)
  #-(or qlot-no-dexador windows)
  (:import-from #:cl+ssl)
  (:export #:fetch
           #:get))
(in-package #:qlot/http)

(declaim (ftype (function (string (or string pathname) &key (:basic-auth *))) fetch))
(declaim (ftype (function (string &key (:want-stream boolean) (:basic-auth *) (:force-binary boolean)) (values (or string stream) integer hash-table)) get))

#-qlot-no-dexador
(defmacro with-retry (() &body body)
  `(let ((retry-request (dex:retry-request 2 :interval 3)))
     (handler-bind ((dex:http-request-failed retry-request)
                    #-windows
                    (cl+ssl::ssl-error retry-request))
       ,@body)))

#-qlot-no-dexador
(defun fetch (url file &key basic-auth)
  (with-retry ()
    (apply #'dex:fetch url file
           :if-exists :supersede
           :keep-alive nil
           :proxy *proxy*
           (and basic-auth
                (list :basic-auth basic-auth)))
    (values)))

#-qlot-no-dexador
(defun get (url &rest args &key want-stream basic-auth force-binary)
  (declare (ignore want-stream basic-auth force-binary))
  (with-retry ()
    (apply #'dex:get url
           :keep-alive nil
           :proxy *proxy*
           args)))
