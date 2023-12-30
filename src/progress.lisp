(defpackage #:qlot/progress
  (:use #:cl)
  (:import-from #:bordeaux-threads)
  (:export #:make-progress
           #:make-line
           #:print-progress
           #:refresh-progress-line
           #:add-line))
(in-package #:qlot/progress)

(defgeneric print-progress (object stream))

(defstruct (progress-line
             (:constructor make-line (header &optional body)))
  (header "" :type string)
  (body "" :type string))

(defmethod print-progress ((object progress-line) stream)
  (format stream "~A~A~%"
          (progress-line-header object)
          (progress-line-body object)))

(defstruct (progress-manager (:constructor %make-progress-manager))
  (lines nil :type list)
  (printed-p nil :type boolean)
  (stream nil :type (or stream null))
  (lock (bt2:make-lock :name "progress-manager-lock")
        :type bt2:lock))

(defun make-progress (line-or-lines)
  (let ((lines (if (listp line-or-lines)
                   line-or-lines
                   (list line-or-lines))))
    (assert (every (lambda (line) (typep line 'progress-line)) lines))
    (%make-progress-manager :lines lines)))

(defmacro with-manager-lock ((manager) &body body)
  `(bt2:with-lock-held ((progress-manager-lock ,manager))
     ,@body))

(defmethod print-progress ((manager progress-manager) stream)
  (assert (not (progress-manager-printed-p manager)))
  (with-manager-lock (manager)
    (loop for line in (progress-manager-lines manager)
          do (print-progress line stream))
    (setf (progress-manager-stream manager) stream
          (progress-manager-printed-p manager) t))
  (values))

(defvar *progress-output* nil)

(defun move-up (n)
  (format *progress-output* "~C[~DA" #\Esc n))

(defun clear-line ()
  (format *progress-output* "~C[2K" #\Esc))

(defmacro with-excursion ((stream) &body body)
  `(let ((*progress-output* ,stream))
     (format *progress-output* "~C[s" #\Esc)
     (prog1 (progn ,@body)
       (format *progress-output* "~C[u" #\Esc)
       (force-output *progress-output*))))

(defun refresh-progress-line (manager line)
  (check-type manager progress-manager)
  (check-type line progress-line)
  (assert (progress-manager-printed-p manager))
  (assert (find line (progress-manager-lines manager) :test 'eq))
  (with-manager-lock (manager)
    (let* ((lines (progress-manager-lines manager))
           (pos (position line lines :test 'eq))
           (len (length lines)))
      (with-excursion ((progress-manager-stream manager))
        (move-up (- len pos))
        (clear-line)
        (print-progress line (progress-manager-stream manager)))))
  (values))

(defun add-line (manager header &key (initial-body ""))
  (check-type manager progress-manager)
  (let ((new-line (make-line header initial-body)))
    (with-manager-lock (manager)
      (when (progress-manager-printed-p manager)
        (print-progress new-line (progress-manager-stream manager)))
      (setf (cdr (last (progress-manager-lines manager)))
            (list new-line)))
    new-line))
