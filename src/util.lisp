(in-package :cl-user)
(defpackage qlot.util
  (:use :cl)
  (:export :with-quicklisp-home
           :with-package-functions))
(in-package :qlot.util)

(defmacro with-quicklisp-home (qlhome &body body)
  `(flet ((main () ,@body))
     (eval `(let ((,(intern #.(string :*quicklisp-home*) :ql) ,,qlhome)) (funcall ,#'main)))))

(defmacro with-package-functions (package-designator functions &body body)
  (let ((args (gensym "ARGS")))
    `(flet (,@(loop for fn in functions
                    collect `(,fn (&rest ,args)
                                  (apply
                                   ,(if (and (listp fn) (eq (car fn) 'setf))
                                        `(function (setf ,(intern (string (cadr fn)) package-designator)))
                                        `(function ,(intern (string fn) package-designator)))
                                   ,args))))
       ,@body)))
