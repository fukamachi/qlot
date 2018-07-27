(defpackage #:qlot/proxy
  (:use #:cl)
  (:import-from #:uiop
                #:getenvp)
  (:export #:get-proxy))
(in-package #:qlot/proxy)

(defvar *proxy*
  (let ((proxy (or (getenvp "http_proxy")
                   (getenvp "HTTP_PROXY"))))
    (and (not (string= "" proxy)) proxy)))

(defun get-proxy ()
  *proxy*)

#+quicklisp
(progn
  ;; dummy for suppress style warning
  (defun orig-http-fetch (&rest args)
    (declare (ignore args)))
  (setf (symbol-function 'orig-http-fetch) (fdefinition
                                             (find-symbol (string :http-fetch) :ql-http)))
  ;; do not use proxy if connect localhost
  (setf (fdefinition (find-symbol (string :http-fetch) :ql-http))
        (lambda (url &rest rest)
          (let ((ql:*proxy-url*
                  (if (eql (search "http://127.0.0.1" url) 0)
                      nil
                      ql:*proxy-url*)))
            (apply #'orig-http-fetch url rest)))))
