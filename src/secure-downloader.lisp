(defpackage #:qlot/secure-downloader
  (:use #:cl)
  (:import-from #:qlot/http)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/logger
                #:progress)
  (:import-from #:qlot/utils
                #:https-of)
  (:export #:with-secure-installer))
(in-package #:qlot/secure-downloader)

(defun https-fetch (url file &rest args)
  (declare (ignore args))
  (let ((url (https-of url)))
    (progress "Downloading ~S." url)
    (qlot/http:fetch url file)
    (progress "Downloaded ~S." url)))

(defmacro with-secure-installer (() &body body)
  `(progv (list (intern #.(string '#:*fetch-scheme-functions*) '#:ql-http)
                (intern #.(string '#:*proxy-url*) '#:ql-http))
       (list
        (append `(("http" . https-fetch)
                  ("https" . https-fetch))
                (symbol-value (intern #.(string '#:*fetch-scheme-functions*) '#:ql-http)))
        *proxy*)
     ,@body))
