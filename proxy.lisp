(defpackage #:qlot/proxy
  (:use #:cl)
  (:export #:*proxy*))
(in-package #:qlot/proxy)

(defvar *proxy*
  (or (uiop:getenvp "http_proxy")
      (uiop:getenvp "HTTP_PROXY")))

#+quicklisp
(progn
  ;; dummy for suppress style warning
  (defun orig-http-fetch (&rest args)
    (declare (ignore args)))
  (setf (symbol-function 'orig-http-fetch) (fdefinition
                                             (find-symbol (string '#:http-fetch) '#:ql-http)))
  ;; do not use proxy if connect localhost
  (setf (fdefinition (find-symbol (string '#:http-fetch) '#:ql-http))
        (lambda (url &rest rest)
          (let ((ql:*proxy-url*
                  (if (eql (search "qlot://localhost/" url) 0)
                      nil
                      ql:*proxy-url*)))
            (apply #'orig-http-fetch url rest)))))
