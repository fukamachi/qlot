(defpackage #:qlot/errors
  (:use #:cl)
  (:export #:qlot-error
           #:no-source-type))
(in-package #:qlot/errors)

(define-condition qlot-error (error) ())

(define-condition no-source-type (qlot-error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Unknown source type: ~A"
                     (slot-value condition 'name)))))
