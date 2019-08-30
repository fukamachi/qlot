(defpackage #:qlot/errors
  (:use #:cl)
  (:export #:qlot-error
           #:no-source-type
           #:qlfile-parse-failed))
(in-package #:qlot/errors)

(define-condition qlot-error (error) ())

(define-condition no-source-type (qlot-error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Unknown source type: ~A"
                     (slot-value condition 'name)))))

(define-condition qlfile-parse-failed (qlot-error)
  ((file :initarg :file)
   (lineno :initarg :lineno)
   (error :initarg :error))
  (:report (lambda (condition stream)
             (with-slots (file lineno error)
                 (format stream "Error raised while parsing '~A' at line ~A:~%  ~A"
                         file lineno error)))))
