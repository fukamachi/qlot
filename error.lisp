(defpackage #:qlot/error
  (:use #:cl)
  (:export #:qlot-error
           #:qlot-qlfile-error))
(in-package #:qlot/error)

(define-condition qlot-error (simple-error) ())

(define-condition qlot-qlfile-error (qlot-error) ())
