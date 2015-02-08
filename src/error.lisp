(in-package :cl-user)
(defpackage qlot.error
  (:use :cl)
  (:export :qlot-error
           :qlot-qlfile-error
           :qlot-sources-incompatible))
(in-package :qlot.error)

(define-condition qlot-error (simple-error) ())

(define-condition qlot-qlfile-error (qlot-error) ())

(define-condition qlot-sources-incompatible (qlot-error) ())
