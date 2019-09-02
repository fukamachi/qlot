(defpackage #:qlot/logger
  (:use #:cl)
  (:export #:*logger-message-stream*
           #:*logger-debug-stream*
           #:message
           #:debug-log))
(in-package #:qlot/logger)

(defvar *logger-message-stream*
  (make-synonym-stream '*standard-output*))

(defvar *logger-debug-stream* nil)

(defun message (format-control &rest format-arguments)
  (format *logger-message-stream*
          "~&~A~%"
          (apply #'format nil format-control format-arguments)))

(defun debug-log (format-control &rest format-arguments)
  (when *logger-debug-stream*
    (format *logger-debug-stream*
            "~&~A~%"
            (apply #'format nil format-control format-arguments))))
