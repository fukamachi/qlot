(in-package #:cl-user)
(defpackage #:qlot/http
  (:use #:cl)
  (:import-from #:qlot/util
                #:with-package-functions)
  (:import-from #:qlot/proxy
                #:get-proxy)
  (:import-from #:uiop)
  (:export #:http-get))
(in-package #:qlot/http)

(defun http-get (url)
  (progv (list (intern #.(string :*proxy-url*) :ql-http))
      (list (get-proxy))
    (with-package-functions :ql-http (http-fetch)
      (uiop:with-temporary-file (:pathname file)
        (http-fetch url file)
        (uiop:read-file-string file)))))
