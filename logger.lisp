(defpackage #:qlot/logger
  (:use #:cl)
  (:export #:*logger-message-stream*
           #:*logger-debug-stream*
           #:*debug*
           #:message
           #:debug-log))
(in-package #:qlot/logger)

(defvar *logger-message-stream*
  (make-synonym-stream '*standard-output*))

(defvar *logger-debug-stream*
  (make-synonym-stream '*logger-message-stream*))

(defvar *debug* nil)

(defun message (format-control &rest format-arguments)
  (format *logger-message-stream*
          "~&~A~%"
          (apply #'format nil format-control format-arguments)))

(defun debug-log (format-control &rest format-arguments)
  (when *debug*
    (format *logger-debug-stream*
            "~&~A~%"
            (apply #'format nil format-control format-arguments))))
