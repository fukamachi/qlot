(in-package :cl-user)
(defpackage shelly.impl
  (:use :cl))
(in-package :shelly.impl)

(cl-annot:enable-annot-syntax)

@export
(defvar *current-lisp-name*
    (or
     #+ccl "ccl"
     #+sbcl "sbcl"
     #+allegro "alisp"
     #+clisp "clisp"
     #+cmu "cmucl"
     #+ecl "ecl"
     #+abcl "abcl"))

@export
(defvar *current-lisp-path*
    (or
     #+ccl (car ccl:*command-line-argument-list*)
     #+sbcl (car sb-ext:*posix-argv*)
     #+allegro (car (system:command-line-arguments))
     #+clisp "clisp"
     #+cmu (car ext:*command-line-strings*)
     #+ecl (car (si:command-args))))

@export
(defvar *eval-option*
    (or
     #+ccl "--eval"
     #+sbcl "--eval"
     #+allegro "-e"
     #+clisp "-x"
     #+cmu "-eval"
     #+ecl "-eval"))

@export
(defun save-core-image (filepath)
  (declare (ignorable filepath))
  #+allegro (progn (excl:dumplisp :name filepath) (excl:exit 1 :quiet t))
  #+ccl (ccl:save-application filepath)
  #+sbcl (sb-ext:save-lisp-and-die filepath)
  #+clisp (progn (ext:saveinitmem filepath) (ext:quit))
  #+cmu (ext:save-lisp filepath :load-init-file nil)
  #-(or allegro ccl sbcl clisp cmu)
  (error "Dumping core image isn't supported on this implementation."))

@export
(defun condition-undefined-function-name (condition)
  (declare (ignorable condition))
  (or
   #+sbcl (slot-value condition 'sb-kernel::name)
   #+ecl (slot-value condition 'si::name)
   #+cmu (getf (conditions::condition-actual-initargs condition) :name)
   #+allegro (slot-value condition 'excl::name)
   #+ccl (slot-value condition 'ccl::name)
   #+clisp (slot-value condition 'system::$name)))
