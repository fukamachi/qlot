(defpackage #:qlot/errors
  (:use #:cl)
  (:export #:qlot-error
           #:qlot-simple-error
           #:unknown-source
           #:invalid-definition
           #:duplicate-project
           #:qlfile-parse-failed
           #:ros-command-error
           #:command-not-found
           #:qlot-warning
           #:qlot-simple-warning
           #:ros-command-warn))
(in-package #:qlot/errors)

(define-condition qlot-error (error) ())

(define-condition qlot-simple-error (qlot-error simple-error) ())

(define-condition qlot-syntax-error (qlot-error) ())

(define-condition unknown-source (qlot-syntax-error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Unknown source: ~A"
                     (slot-value condition 'name)))))

(define-condition invalid-definition (qlot-syntax-error)
  ((source :initarg :source)
   (usage :initarg :usage)
   (reason :initarg :reason
           :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid definition of '~(~A~)'.~@[~%Reason: ~A~]~%[usage] ~A"
                     (slot-value condition 'source)
                     (slot-value condition 'reason)
                     (slot-value condition 'usage)))))

(define-condition duplicate-project (qlot-syntax-error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Duplicate project: ~A"
                     (slot-value condition 'name)))))

(define-condition qlfile-parse-failed (qlot-error)
  ((file :initarg :file)
   (lineno :initarg :lineno)
   (line :initarg :line)
   (error :initarg :error))
  (:report (lambda (condition stream)
             (with-slots (file lineno line error) condition
               (format stream "Error raised while parsing '~A' at line ~A:~2%  ~A~2%~A"
                       file lineno line error)))))

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

(define-condition qlot-warning (warning) ())

(define-condition qlot-simple-warning (qlot-warning simple-warning) ())

(defun ros-command-warn (format-control &rest format-arguments)
  (restart-case
      (error 'qlot-simple-warning
             :format-control format-control
             :format-arguments format-arguments)
    (continue ())))
