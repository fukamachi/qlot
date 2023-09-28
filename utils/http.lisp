(defpackage #:qlot/utils/http
  (:use #:cl)
  (:shadow #:get)
  (:nicknames #:qdex)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:dexador)
  (:export #:fetch
           #:get))
(in-package #:qlot/utils/http)

(defun fetch (url file &key basic-auth)
  (apply #'dex:fetch url file
         :if-exists :supersede
         :keep-alive nil
         :proxy *proxy*
         (and basic-auth
              (list :basic-auth basic-auth))))

(defun get (url &key want-stream basic-auth)
  (apply #'dex:get url
         :keep-alive nil
         :want-stream want-stream
         :proxy *proxy*
         (and basic-auth
              (list :basic-auth basic-auth))))
