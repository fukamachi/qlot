(defpackage #:qlot/proxy
  (:use #:cl)
  (:export #:*proxy*))
(in-package #:qlot/proxy)

(defvar *proxy*
  (or (uiop:getenvp "http_proxy")
      (uiop:getenvp "HTTP_PROXY")))

#+quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Previously, this wrapper was defined in the next
  ;; eval-when block as a lambda form.
  ;; 
  ;; But this caused issue when loading cached
  ;; files:
  ;; https://github.com/fukamachi/qlot/issues/104
  ;; 
  ;; For some reason, a separate function definition
  ;; works as expected.
  (defun qlot-http-fetch (url &rest rest)
    (let ((ql:*proxy-url*
            (if (eql (search #1="qlot://localhost/" url
                             :end2 (length #1#))
                     0)
                nil
                ql:*proxy-url*)))
      (apply #'orig-http-fetch url rest))))

#+quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'orig-http-fetch)
    ;; dummy for suppress style warning
    (defun orig-http-fetch (&rest args)
      (declare (ignore args)))
    (setf (symbol-function 'orig-http-fetch)
          (fdefinition
           (find-symbol (string '#:http-fetch) '#:ql-http)))
    ;; do not use proxy if connect localhost
    (setf (fdefinition (find-symbol (string '#:http-fetch) '#:ql-http))
          #'qlot-http-fetch)))
