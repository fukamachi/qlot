(in-package :cl-user)
(defpackage qlot.shell
  (:use :cl)
  (:import-from :external-program
                :run)
  (:export :safety-shell-command
           :shell-command-error))
(in-package :qlot.shell)

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
  (with-output-to-string (stdout)
    (let ((stderr (make-string-output-stream)))
      (multiple-value-bind (status code)
          (external-program:run program args
                                :output (make-broadcast-stream *standard-output*
                                                               stdout)
                                :error (make-broadcast-stream *error-output*
                                                              stderr))
        (declare (ignore status))
        (unless (zerop code)
          (error 'shell-command-error
                 :command (cons program args)
                 :code code
                 :stderr (get-output-stream-string stderr)))))))
