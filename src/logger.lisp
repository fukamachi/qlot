(defpackage #:qlot/logger
  (:use #:cl)
  (:import-from #:qlot/color
                #:color-text)
  (:export #:*logger-message-stream*
           #:*logger-debug-stream*
           #:*debug*
           #:*enable-whisper*
           #:whisper
           #:clear-whisper
           #:message
           #:debug-log))
(in-package #:qlot/logger)

(defvar *logger-message-stream*
  (make-synonym-stream '*standard-output*))

(defvar *logger-debug-stream*
  (make-synonym-stream '*standard-output*))

(defvar *debug* nil)
(defvar *enable-whisper* t)

(defvar *previous-progress* "")

(defparameter *padding* "- ")

(defun terminal-p ()
  (not (uiop:getenvp "QLOT_NO_TERMINAL")))

(defun whisper (format-control &rest format-arguments)
  (when *enable-whisper*
    (let* ((out *logger-message-stream*)
           (text (apply #'format nil format-control format-arguments))
           (text (concatenate 'string *padding* text)))
      (when (terminal-p)
        (format out "~C[2K" #\Esc))
      (write-char #\Return out)
      (write-string (color-text :gray text) out)
      (force-output out)
      (setf *previous-progress* text))))

(defun clear-whisper ()
  (when (= 0 (length *previous-progress*))
    (return-from clear-whisper))
  (when (terminal-p)
    (format *logger-message-stream* "~C[2K" #\Esc))
  (write-char #\Return *logger-message-stream*)
  (force-output *logger-message-stream*)
  (setf *previous-progress* "")
  (values))

(defun message (format-control &rest format-arguments)
  (let ((out *logger-message-stream*)
        (text (apply #'format nil format-control format-arguments)))
    (clear-whisper)
    (write-string text out)
    (write-char #\Newline out))
  (values))

(defun debug-log (format-control &rest format-arguments)
  (when *debug*
    (clear-whisper)
    (format *logger-debug-stream*
            "[debug] ~A~%"
            (apply #'format nil format-control format-arguments)))
  (values))
