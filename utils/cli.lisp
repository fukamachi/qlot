(defpackage #:qlot/utils/cli
  (:use #:cl)
  (:export #:exec
           #:which
           #:command-line-arguments))
(in-package #:qlot/utils/cli)

(defun command-line-arguments ()
  #+allegro (system:command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+ecl (si:command-args)
  #+cmu ext:*command-line-words*
  #+ccl ccl:*command-line-argument-list*
  #+lispworks system:*line-arguments-list*)

#+(and unix sbcl) ;; from swank
(progn
  (sb-alien:define-alien-routine ("execvp" %execvp) sb-alien:int
    (program sb-alien:c-string)
    (argv (* sb-alien:c-string)))
  (defun execvp (program args)
    "Replace current executable with another one."
    (let ((a-args (sb-alien:make-alien sb-alien:c-string
                                       (+ 1 (length args)))))
      (unwind-protect
           (progn
             (loop for index from 0 by 1
                   and item in (append args '(nil))
                   do (setf (sb-alien:deref a-args index)
                            item))
             (when (minusp
                    (%execvp program a-args))
               (let ((errno (sb-impl::get-errno)))
                 (case errno
                   (2 (error "No such file or directory: ~S" program))
                   (otherwise
                    (error "execvp(3) failed. (Code=~D)" errno))))))
        (sb-alien:free-alien a-args)))))

(defun exec (args)
  (assert args)
  #+(and unix sbcl)
  (execvp (first args) args)
  #-(and unix sbcl)
  (uiop:run-program args
                    :output t
                    :error-output t))

(defun which (cmd)
  (handler-case
      (string-right-trim '(#\Newline)
                         (with-output-to-string (s)
                           (uiop:run-program `("which" ,cmd)
                                             :output s)))
    (uiop/run-program:subprocess-error ()
      nil)))
