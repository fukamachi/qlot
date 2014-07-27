(in-package :cl-user)
(defpackage qlot.http
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms
                :once-only)
  (:export :http-request
           :safety-http-request
           :http-request-failed))
(in-package :qlot.http)

(define-condition http-request-failed (simple-error)
  ((status :initarg :status :type integer)
   (url :initarg :url :type string))
  (:report
   (lambda (condition stream)
     (format stream "HTTP Request to ~A has failed (Code=~D)."
             (slot-value condition 'url)
             (slot-value condition 'status)))))

(defun http-request (url &rest args)
  (apply #'drakma:http-request
         url
         (append args
                 (list :user-agent (format nil "qlot/~A"
                                           (asdf::component-version (asdf:find-system :qlot)))))))

(defmacro safety-http-request (url &rest args)
  (with-gensyms (stream status)
    (once-only (url)
      `(multiple-value-bind (,stream ,status)
           (http-request ,url ,@args)
         (unless (= ,status 200)
           (error 'http-request-failed
                  :url ,url
                  :status ,status))

         ,stream))))
