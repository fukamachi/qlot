(defpackage #:qlot/utils/shell
  (:use #:cl)
  (:export #:safety-shell-command
           #:shell-command-error
           #:run-lisp))
(in-package #:qlot/utils/shell)

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

(defvar *current-lisp-path*
  (or #+ccl (car ccl:*command-line-argument-list*)
      #+sbcl (car sb-ext:*posix-argv*)
      #+allegro (car (system:command-line-arguments))
      #+clisp "clisp"
      #+cmu (car ext:*command-line-strings*)
      #+ecl (car (si:command-args))))

(defvar *eval-option*
  (or
    #+ccl "--eval"
    #+sbcl "--eval"
    #+allegro "-e"
    #+clisp "-x"
    #+cmu "-eval"
    #+ecl "-eval"))

(defun str (form)
  (let ((*package* (find-package :cl-user)))
    (if (stringp form)
        form
        (prin1-to-string form))))

(defun run-lisp (forms &key systems source-registry without-quicklisp)
  (safety-shell-command *current-lisp-path*
                        (append
                          #+ccl '("--no-init" "--quiet" "--batch")
                          #+sbcl '("--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
                          #+allegro '("--qq")
                          #+clisp '("-norc" "--quiet" "--silent" "-on-error" "exit")
                          #+cmu '("-noinit")
                          #+ecl '("-norc")

                          (list *eval-option* "(require 'asdf)")

                          (when source-registry
                            (list *eval-option*
                                  (str `(push ,source-registry asdf:*central-registry*))))

                          (list *eval-option*
                                (str '(setf asdf::*default-source-registries*
                                            (quote (asdf::environment-source-registry
                                                     asdf::system-source-registry
                                                     asdf::system-source-registry-directory)))))

                          ;; XXX: Don't load qlot/distify with the local Quicklisp
                          #+quicklisp
                          (unless without-quicklisp
                            (when ql:*quicklisp-home*
                              `(,*eval-option*
                                 ,(str `(load ,(merge-pathnames #P"setup.lisp" ql:*quicklisp-home*))))))

                          (loop for system in systems
                                append (list *eval-option*
                                             #+quicklisp
                                             (str (if (or without-quicklisp
                                                          (null ql:*quicklisp-home*))
                                                      `(asdf:load-system ,system)
                                                      `(ql:quickload ,system)))
                                             #-quicklisp
                                             (str `(asdf:load-system ,system))))

                          (loop for form in forms
                                append (list *eval-option*
                                             (str (if (pathnamep form)
                                                      `(load ,form)
                                                      form))))

                          `(,*eval-option*
                             ,(prin1-to-string
                                (quote
                                  #+ccl (ccl:quit)
                                  #+sbcl (sb-ext:exit)
                                  #+allegro (excl:exit :quiet t)
                                  #+clisp (ext:quit)
                                  #+cmucl (unix:unix-exit)
                                  #+ecl (ext:quit)
                                  #-(or ccl sbcl allegro clisp cmucl ecl) (cl-user::quit)))))))
