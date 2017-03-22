(in-package :cl-user)
(defpackage shelly.error
  (:use :cl))
(in-package :shelly.error)

(cl-annot:enable-annot-syntax)

@export
(define-condition shelly-error (simple-error) ())

@export
(define-condition shelly-read-error (shelly-error)
  ((expression :initarg :expression)))

@export
(define-condition shelly-command-not-found-error (shelly-error)
  ((command :initarg :command))
  (:report
   (lambda (condition stream)
     (format stream "Command \"~A\" not found"
             (slot-value condition 'command)))))
