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

#+ros.init
(defun run-roswell (forms &key systems source-registry without-quicklisp)
  (let ((ros (or (ros:opt "wargv0")
                 (ros:opt "argv0"))))
    (with-output-to-string (s)
      (uiop:run-program (append (list ros)
                                (when without-quicklisp
                                  (list "+Q"))
                                (when source-registry
                                  (list "-S" (princ-to-string source-registry)))
                                (loop for system in systems
                                      append (list "-s" (princ-to-string system)))
                                (loop for form in forms
                                      append (list "-e"
                                                   (str (if (pathnamep form)
                                                            `(load ,form)
                                                            form)))))
                        :output s
                        :error-output *error-output*))))

(defun run-lisp (forms &key systems source-registry without-quicklisp)
  #+ros.init
  (run-roswell forms :systems systems :source-registry source-registry :without-quicklisp without-quicklisp)
  #-ros.init
  (safety-shell-command *current-lisp-path*
                        (append
                          #+ccl '("--no-init" "--quiet" "--batch")
                          #+sbcl '("--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
                          #+allegro '("--qq")
                          #+clisp '("-norc" "--quiet" "--silent" "-on-error" "exit")
                          #+cmu '("-noinit")
                          #+ecl '("-norc")

                          #+quicklisp
                          (unless without-quicklisp
                            (when ql:*quicklisp-home*
                              `(,*eval-option*
                                 ,(str `(load ,(merge-pathnames #P"setup.lisp" ql:*quicklisp-home*))))))

                          (when source-registry
                            (list *eval-option*
                                  (str `(push ,source-registry asdf:*central-registry*))))

                          (loop for system in systems
                                append (list *eval-option*
                                             (str `(ql:quickload ,system))))

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
