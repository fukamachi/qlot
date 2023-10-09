(defpackage #:qlot/utils/shell
  (:use #:cl)
  (:import-from #:qlot/logger
                #:*debug*
                #:debug-log)
  (:export #:safety-shell-command
           #:shell-command-error
           #:shell-command-error-output
           #:run-lisp
           #:launch-lisp
           #:*qlot-source-directory*))
(in-package #:qlot/utils/shell)

(defparameter *qlot-source-directory*
  (asdf:system-source-directory :qlot))

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
  (uiop:with-temporary-file (:pathname stderr
                             :direction :output)
    (handler-case
        (uiop:run-program (cons program args)
                          :input :interactive
                          :output output
                          :error-output (if *debug*
                                            :interactive
                                            stderr))
      (uiop/run-program:subprocess-error (e)
        (error 'shell-command-error
               :command (cons program args)
               :code (uiop/run-program:subprocess-error-code e)
               :stderr (uiop:read-file-string stderr))))))

(defun safety-background-command (program args &key input output)
  (setf args (mapcar #'princ-to-string args))
  (debug-log "Running a background command: ~A~{ ~S~}" program args)
  (uiop:launch-program (cons program args)
                       :input input
                       :output output
                       :error-output :stream))

(defvar *current-lisp-path*
  (or #+ccl (car ccl:*command-line-argument-list*)
      #+sbcl (car sb-ext:*posix-argv*)
      #+allegro (car (system:command-line-arguments))
      #+clisp "clisp"
      #+cmu (car ext:*command-line-strings*)
      #+ecl (car (si:command-args))
      #+abcl "java"))

(defvar *eval-option*
  (or
    #+ros.init "-e"
    #+ccl "--eval"
    #+sbcl "--eval"
    #+allegro "-e"
    #+clisp "-x"
    #+cmu "-eval"
    #+ecl "-eval"
    #+abcl "--eval"))

(defun str (form)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (let ((*package* (find-package :cl-user)))
    (if (stringp form)
        form
        (let ((*print-case* :downcase) (*print-pretty* nil))
          (prin1-to-string form)))))

(defun -e (form)
  (list *eval-option* (str form)))

(defun default-args ()
  (append
    (-e "(require 'asdf)")
    (-e
      `(setf *debugger-hook*
             (lambda (cl-user::c cl-user::parent)
               (declare (ignore cl-user::parent))
               (format *error-output* "~&Error: ~A~%" cl-user::c)
               ,@(and *debug*
                      '((uiop:print-backtrace :condition cl-user::c)))
               (uiop:quit -1))))))

(defun build-command-args (forms &key systems source-registry without-quicklisp)
  (let ((qlhome (if without-quicklisp
                    nil
                    (symbol-value (intern (string '#:*quicklisp-home*) '#:ql)))))
    (append
      (default-args)

      (when source-registry
        (-e `(push ,source-registry asdf:*central-registry*)))

      (-e '(setf asdf::*default-source-registries*
                 (quote (asdf::environment-source-registry
                          asdf::system-source-registry
                          asdf::system-source-registry-directory))))

      (when qlhome
        (-e `(load ,(merge-pathnames #P"setup.lisp" qlhome))))

      (loop for system in systems
            append (-e
                     (if qlhome
                         `(uiop:symbol-call :ql :quickload ,system :silent t)
                         `(let ((*standard-output* (make-broadcast-stream))
                                (*trace-output* (make-broadcast-stream)))
                            (asdf:load-system ,system)))))

      (loop for form in forms
            append (-e
                     (if (pathnamep form)
                         `(load ,form)
                         form))))))

#+ros.init
(defun precommand-options ()
  '("+Q"))

#-ros.init
(defun precommand-options ()
  #+abcl `("-jar" ,(uiop:native-namestring
                    (first (pathname-device ext:*lisp-home*))))

  #+ccl '("--no-init" "--quiet" "--batch")
  #+sbcl '("--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
  #+allegro '("--qq")
  #+clisp '("-norc" "--quiet" "--silent" "-on-error" "exit")
  #+cmu '("-noinit")
  #+ecl '("-norc")
  #+abcl '("--noinform" "--noinit"))

#+ros.init
(defun postcommand-options () nil)

#-ros.init
(defun postcommand-options ()
  (-e
   (quote
    #+ccl (ccl:quit)
    #+sbcl (sb-ext:exit)
    #+allegro (excl:exit :quiet t)
    #+clisp (ext:quit)
    #+cmucl (unix:unix-exit)
    #+ecl (ext:quit)
    #+abcl (ext:quit)
    #-(or ccl sbcl allegro clisp cmucl ecl abcl) (cl-user::quit))))

(defun command-options (forms args)
  (append
   (precommand-options)
   (apply #'build-command-args forms args)
   (postcommand-options)))

(defun launch-lisp (forms &rest args &key systems source-registry without-quicklisp)
  (declare (ignore systems source-registry without-quicklisp))
  (safety-background-command
   #-ros.init *current-lisp-path*
   #+ros.init (or (ros:opt "wargv0")
                  (ros:opt "argv0"))
   (command-options forms args)
   :input :stream
   :output :stream))

(defun run-lisp (forms &rest args &key systems source-registry without-quicklisp (output :interactive))
  (declare (ignore systems source-registry without-quicklisp))
  (remf args :output)
  (safety-shell-command #-ros.init *current-lisp-path*
                        #+ros.init (or (ros:opt "wargv0")
                                       (ros:opt "argv0"))
                        (command-options forms args)
                        :output output))
