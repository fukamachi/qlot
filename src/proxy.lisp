(in-package :cl-user)
(defpackage qlot.proxy
  (:use :cl)
  (:import-from :uiop
                :getenvp)
  (:export :get-proxy))
(in-package :qlot.proxy)

(defvar *proxy*
  (let ((proxy (or (getenvp "http_proxy")
                   (getenvp "HTTP_PROXY"))))
    (and (not (string= "" proxy)) proxy)))

(defun get-proxy ()
  *proxy*)

#+quicklisp
(progn
  ;; dummy for suppress style warning
  (defun orig-http-fetch ())
  (setf (symbol-function 'orig-http-fetch) (fdefinition
                                             (find-symbol (string :http-fetch) :ql-http)))
  ;; do not use proxy if connect localhost
  (setf (fdefinition (find-symbol (string :http-fetch) :ql-http))
        (lambda (url &rest rest)
          (let ((ql:*proxy-url*
                  (if (ppcre:scan "^http://127\\.0\\.0\\.1" url)
                    nil
                    ql:*proxy-url*)))
            (apply #'orig-http-fetch url rest)))))
