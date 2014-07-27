(in-package :cl-user)
(defpackage qlot.tmp
  (:use :cl)
  (:export :*tmp-directory*
           :tmp-path))
(in-package :qlot.tmp)

(defparameter *tmp-directory* #P"/tmp/qlot/")

(defun tmp-path (&rest pathnames)
  (reduce #'merge-pathnames
          (reverse pathnames)
          :initial-value *tmp-directory*
          :from-end t))
