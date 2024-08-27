(defpackage #:qlot/secure-downloader
  (:use #:cl)
  (:import-from #:qlot/http)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/logger
                #:whisper)
  (:import-from #:qlot/utils
                #:https-of)
  (:export #:with-secure-installer))
(in-package #:qlot/secure-downloader)

(defun https-fetch (url file &rest args &key quiet &allow-other-keys)
  (declare (ignore args))
  (let ((url (https-of url)))
    (unless quiet
      (whisper "Downloading ~S." url))
    (qlot/http:fetch url file)
    (unless quiet
      (whisper "Downloaded ~S." url))
    (values (make-instance (intern #.(string '#:header) '#:ql-http) :status 200)
            (probe-file file))))

(defmacro with-secure-installer (() &body body)
  `(progv (list (intern #.(string '#:*fetch-scheme-functions*) '#:ql-http)
                (intern #.(string '#:*proxy-url*) '#:ql-http))
       (list
        (append `(("http" . https-fetch)
                  ("https" . https-fetch))
                (symbol-value (intern #.(string '#:*fetch-scheme-functions*) '#:ql-http)))
        *proxy*)
     ,@body))
