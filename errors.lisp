(defpackage #:qlot/errors
  (:use #:cl)
  (:export #:qlot-error
           #:qlot-simple-error
           #:no-source-type
           #:qlfile-parse-failed
           #:ros-command-error
           #:command-not-found))
(in-package #:qlot/errors)

(define-condition qlot-error (error) ())

(define-condition qlot-simple-error (qlot-error simple-error) ())

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
             (with-slots (file lineno error) condition
               (format stream "Error raised while parsing '~A' at line ~A:~%  ~A"
                       file lineno error)))))

(define-condition ros-command-error (qlot-error)
  ((message :initarg :message))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'message) stream))))

(defun ros-command-error (format-control &rest format-arguments)
  (error 'ros-command-error
         :message (apply #'format nil format-control format-arguments)))

(define-condition command-not-found (ros-command-error)
  ((command :initarg :command))
  (:report (lambda (condition stream)
             (format stream "Command not found: ~A" (slot-value condition 'command)))))
