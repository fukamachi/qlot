(defpackage #:qlot/logger
  (:use #:cl)
  (:import-from #:qlot/color
                #:color-text)
  (:export #:*logger-message-stream*
           #:*logger-debug-stream*
           #:*debug*
           #:*enable-whisper*
           #:*terminal*
           #:whisper
           #:clear-whisper
           #:message
           #:warn-message
           #:debug-log))
(in-package #:qlot/logger)

(defvar *logger-message-stream*
  (make-synonym-stream '*standard-output*))

(defvar *logger-debug-stream*
  (make-synonym-stream '*standard-output*))

(defvar *debug* nil)
(defvar *enable-whisper* t)

(defvar *previous-progress* "")

(defvar *terminal* nil)

(defun whisper (format-control &rest format-arguments)
  (when *enable-whisper*
    (let ((out *logger-message-stream*)
          (text (apply #'format nil format-control format-arguments)))
      (if *terminal*
          (progn
            (format out "~C[2K" #\Esc)
            (write-char #\Return out))
          (fresh-line out))
      (write-string (color-text :gray text) out)
      (force-output out)
      (setf *previous-progress* text))))

(defun clear-whisper ()
  (when (= 0 (length *previous-progress*))
    (return-from clear-whisper))
  (when *terminal*
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

(defun warn-message (format-control &rest format-arguments)
  (let ((out *logger-message-stream*)
        (text (apply #'format nil format-control format-arguments)))
    (clear-whisper)
    (write-string (color-text :yellow text) out)
    (write-char #\Newline out))
  (values))

(defun debug-log (format-control &rest format-arguments)
  (when *debug*
    (clear-whisper)
    (format *logger-debug-stream*
            "[debug] ~A~%"
            (apply #'format nil format-control format-arguments)))
  (values))
