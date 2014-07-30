(in-package :cl-user)
(defpackage qlot.util
  (:use :cl)
  (:export :with-quicklisp-home
           :with-package-functions
           :pathname-in-directory-p
           :find-qlfile))
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
                                        `(eval `(function (setf ,(intern ,(string (cadr fn)) ,package-designator))))
                                        `(symbol-function (intern ,(string fn) ,package-designator)))
                                   ,args))))
       ,@body)))

(defun pathname-in-directory-p (path directory)
  (loop for dir1 in (pathname-directory directory)
        for dir2 in (pathname-directory path)
        unless (string= dir1 dir2)
          do (return nil)
        finally
           (return t)))

(defun find-qlfile (directory &key (errorp t) use-snapshot)
  (check-type directory pathname)
  (unless (probe-file directory)
    (error "~S does not exist." directory))
  (let ((qlfile (merge-pathnames (if use-snapshot
                                     "qlfile.snapshot"
                                     "qlfile")
                                 directory)))
    (unless (probe-file qlfile)
      (when errorp
        (error "'~A' is not found at '~A'." qlfile directory))
      (setf qlfile nil))

    qlfile))
