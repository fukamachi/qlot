(defpackage #:qlot/shell
  (:use #:cl)
  (:export #:safety-shell-command
           #:shell-command-error))
(in-package #:qlot/shell)

(define-condition shell-command-error (simple-error)
  ((command :type cons
            :initarg :command)
   (code :type integer
         :initarg :code)
   (stderr :type string
           :initarg :stderr))
  (:report
   (lambda (condition stream)
     (format stream "Error while executing a shell command: ~{~A~^ ~} (Code=~D)~2%  ~A"
             (slot-value condition 'command)
             (slot-value condition 'code)
             (slot-value condition 'stderr)))))

(defun safety-shell-command (program args)
  (setf args (mapcar #'princ-to-string args))
  (with-output-to-string (stdout)
    (with-output-to-string (stderr)
      (multiple-value-bind (output error code)
          (uiop:run-program (cons program args)
                            :input :interactive
                            :output (make-broadcast-stream *standard-output*
                                                           stdout)
                            :error-output stderr
                            :ignore-error-status t)
        (declare (ignore output error))
        (unless (zerop code)
          (error 'shell-command-error
                 :command (cons program args)
                 :code code
                 :stderr (get-output-stream-string stderr)))))))
