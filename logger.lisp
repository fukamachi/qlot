(defpackage #:qlot/logger
  (:use #:cl)
  (:import-from #:qlot/color
                #:color-text)
  (:export #:*logger-message-stream*
           #:*logger-debug-stream*
           #:*debug*
           #:progress
           #:clear-progress
           #:message
           #:debug-log))
(in-package #:qlot/logger)

(defvar *logger-message-stream*
  (make-synonym-stream '*standard-output*))

(defvar *logger-debug-stream*
  (make-synonym-stream '*logger-message-stream*))

(defvar *debug* nil)

(defvar *previous-progress* "")

(defun fill-spaces-to-clear-progress (text)
  (write-string
   (make-string (max 0
                     (- (length *previous-progress*)
                        (length text)))
                :initial-element #\Space)
   *logger-message-stream*)
  (setf *previous-progress* text)
  text)

(defun progress (format-control &rest format-arguments)
  (let ((out *logger-message-stream*)
        (text (apply #'format nil format-control format-arguments)))
    (write-string (color-text :gray text) out)
    (fill-spaces-to-clear-progress text)
    (write-char #\Return out)
    (force-output out)))

(defun clear-progress ()
  (when (= 0 (length *previous-progress*))
    (return-from clear-progress))
  (write-char #\Return *logger-message-stream*)
  (write-string
   (make-string (length *previous-progress*) :initial-element #\Space)
   *logger-message-stream*)
  (write-char #\Return *logger-message-stream*)
  (force-output *logger-message-stream*)
  (setf *previous-progress* "")
  (values))

(defun message (format-control &rest format-arguments)
  (let ((out *logger-message-stream*)
        (text (apply #'format nil format-control format-arguments)))
    (write-string text out)
    (fill-spaces-to-clear-progress text)
    (write-char #\Newline out))
  (values))

(defun debug-log (format-control &rest format-arguments)
  (when *debug*
    (clear-progress)
    (format *logger-debug-stream*
            "[debug] ~A~%"
            (apply #'format nil format-control format-arguments)))
  (values))
