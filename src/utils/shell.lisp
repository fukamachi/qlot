(defpackage #:qlot/utils/shell
  (:use #:cl)
  (:import-from #:qlot/logger
                #:*debug*
                #:debug-log)
  (:export #:safety-shell-command
           #:shell-command-error
           #:shell-command-error-output))
(in-package #:qlot/utils/shell)

(define-condition shell-command-error (simple-error)
  ((command :type cons
            :initarg :command)
   (code :type integer
         :initarg :code)
   (stderr :type string
           :initarg :stderr
           :reader shell-command-error-output))
  (:report
   (lambda (condition stream)
     (format stream "Error while executing a shell command: ~{~S~^ ~} (Code=~D)~2%  ~A"
             (slot-value condition 'command)
             (slot-value condition 'code)
             (slot-value condition 'stderr)))))

(defun safety-shell-command (program args &key (output :string))
  (setf args (mapcar #'princ-to-string args))
  (debug-log "Running shell command: ~A~{ ~S~}" program args)
  (uiop:with-temporary-file (:pathname stderr-file
                             :stream stderr
                             :direction :io)
    (handler-case
        (uiop:run-program (cons program args)
                          :input :interactive
                          :output output
                          :error-output (if *debug*
                                            :interactive
                                            stderr-file))
      (uiop/run-program:subprocess-error (e)
        (error 'shell-command-error
               :command (cons program args)
               :code (uiop/run-program:subprocess-error-code e)
               :stderr (uiop:slurp-stream-string stderr))))))
