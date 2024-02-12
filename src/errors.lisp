(defpackage #:qlot/errors
  (:use #:cl)
  (:export #:qlot-error
           #:qlot-simple-error
           #:unknown-source
           #:invalid-definition
           #:duplicate-project
           #:qlfile-parse-failed
           #:missing-projects
           #:unnecessary-projects
           #:outdated-projects
           #:qlfile-not-found
           #:qlfile-lock-not-found
           #:qlot-directory-not-found
           #:qlot-directory-invalid
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

(define-condition missing-projects (qlot-error)
  ((projects :initarg :projects))
  (:report (lambda (condition stream)
             (with-slots (projects) condition
               (format stream "The following libraries are missing:~%~{ * ~A~^~%~}"
                       projects)))))

(define-condition unnecessary-projects (qlot-error)
  ((projects :initarg :projects))
  (:report (lambda (condition stream)
             (with-slots (projects) condition
               (format stream "The following libraries need to be removed:~%~{ * ~A~^~%~}"
                       projects)))))

(define-condition outdated-projects (qlot-error)
  ((projects :initarg :projects))
  (:report (lambda (condition stream)
             (with-slots (projects) condition
               (format stream "New updates found for the following libraries:~%~{ * ~A~^~%~}"
                       projects)))))

(define-condition file-not-found (qlot-error)
  ((path :initarg :path))
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "File not found: ~A" path)))))

(define-condition qlfile-not-found (file-not-found)
  ()
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "qlfile not found: ~A" path)))))

(define-condition qlfile-lock-not-found (file-not-found)
  ()
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "qlfile.lock not found: ~A" path)))))

(define-condition qlot-directory-not-found (file-not-found)
  ()
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "Directory '~A' does not exist." path)))))

(define-condition qlot-directory-invalid (file-not-found)
  ()
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "Directory '~A' is not valid." path)))))

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
