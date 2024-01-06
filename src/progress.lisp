(defpackage #:qlot/progress
  (:use #:cl)
  (:import-from #:qlot/logger
                #:*terminal*)
  (:import-from #:qlot/color
                #:color-text)
  (:import-from #:bordeaux-threads)
  (:import-from #:lparallel
                #:pmapc
                #:*kernel*
                #:make-kernel)
  (:import-from #:lparallel.queue
                #:make-queue
                #:push-queue
                #:pop-queue)
  (:export #:make-progress
           #:make-line
           #:print-progress
           #:refresh-progress-line
           #:add-line
           #:progress
           #:run-in-parallel))
(in-package #:qlot/progress)

(defgeneric print-progress (object stream))

(defstruct (progress-line
             (:constructor make-line (header &optional body)))
  (type :in-progress :type (or null (member :in-progress :done :aborted)))
  (header "" :type string)
  (body "" :type string))

(defun progress-symbol (type)
  (ecase type
    (:in-progress
     (color-text :yellow
                 (string (code-char 9679))))
    (:done
     (color-text :green
                 (string (code-char 10003))))
    (:aborted
     (color-text :red
                 (string (code-char 10799))))
    ('nil nil)))

(defmethod print-progress ((object progress-line) stream)
  (format stream "~@[~A ~]~A  ~A~%"
          (progress-symbol (progress-line-type object))
          (progress-line-header object)
          (color-text :gray (progress-line-body object))))

(defstruct (progress-manager (:constructor %make-progress-manager))
  (lines nil :type list)
  (stream nil :type (or stream null))
  (lock (bt2:make-lock :name "progress-manager-lock")
        :type bt2:lock))

(defun make-progress (line-or-lines &key (stream *standard-output*))
  (let ((lines (if (listp line-or-lines)
                   line-or-lines
                   (list line-or-lines))))
    (assert (every (lambda (line) (typep line 'progress-line)) lines))
    (%make-progress-manager :lines lines
                            :stream stream)))

(defmacro with-manager-lock ((manager) &body body)
  `(bt2:with-lock-held ((progress-manager-lock ,manager))
     ,@body))

(defmethod print-progress ((manager progress-manager) stream)
  (with-manager-lock (manager)
    (loop for line in (progress-manager-lines manager)
          do (print-progress line stream)))
  (values))

(defvar *progress-output* nil)

(defun move-up (n)
  (when *terminal*
    (format *progress-output* "~C[~DA" #\Esc n)))

(defun clear-line ()
  (when *terminal*
    (format *progress-output* "~C[2K" #\Esc)))

(defmacro with-excursion ((stream) &body body)
  `(let ((*progress-output* ,stream))
     (when *terminal*
       (format *progress-output* "~C[s" #\Esc))
     (prog1 (progn ,@body)
       (when *terminal*
         (format *progress-output* "~C[u" #\Esc))
       (force-output *progress-output*))))

(defun refresh-progress-line (manager line)
  (check-type manager progress-manager)
  (check-type line progress-line)
  (assert (find line (progress-manager-lines manager) :test 'eq))
  (with-manager-lock (manager)
    (let* ((lines (progress-manager-lines manager))
           (pos (position line lines :test 'eq))
           (len (length lines)))
      (with-excursion ((progress-manager-stream manager))
        (when pos
          (move-up (- len pos))
          (clear-line))
        (print-progress line (progress-manager-stream manager)))))
  (values))

(defun add-line (manager header &key (initial-body ""))
  (check-type manager progress-manager)
  (let ((new-line (make-line header initial-body)))
    (with-manager-lock (manager)
      (print-progress new-line (progress-manager-stream manager))
      (if (progress-manager-lines manager)
          (setf (cdr (last (progress-manager-lines manager)))
                (list new-line))
          (setf (progress-manager-lines manager)
                (list new-line))))
    new-line))

(defvar *mailbox*)
(defvar *progress-line*)

(defun progress (type &optional control &rest args)
  (when (boundp '*progress-line*)
    (unless (keywordp type)
      (push control args)
      (psetf control type
             type nil))
    (when type
      (setf (progress-line-type *progress-line*) type))
    (let ((text (apply #'format nil control args)))
      (setf (progress-line-body *progress-line*) text)
      (push-queue *progress-line* *mailbox*)
      text)))

(defun run-in-parallel (worker-fn jobs &key (concurrency 1) job-header-fn)
  (let* ((manager (make-progress nil))
         (*mailbox* (make-queue))
         (bt2:*default-special-bindings* (cons `(*mailbox* . ,*mailbox*)
                                               bt2:*default-special-bindings*))
         (*kernel* (make-kernel concurrency
                                :bindings bt2:*default-special-bindings*))
         (progress-thread
           (bt2:make-thread
            (lambda ()
              (loop for line = (pop-queue *mailbox*)
                    do (refresh-progress-line manager line)))
            :name "qlot progress manager")))
    (unwind-protect
        (pmapc
         (lambda (job)
           (let ((*progress-line*
                   (add-line manager
                             (funcall (or job-header-fn #'princ-to-string) job))))
             (funcall worker-fn job)))
         jobs)
      (bt2:destroy-thread progress-thread))))
