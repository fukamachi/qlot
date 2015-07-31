(in-package :cl-user)
(defpackage qlot.tmp
  (:use :cl)
  (:export :*tmp-directory*
           :tmp-path))
(in-package :qlot.tmp)

(defvar *tmp-directory*
  (merge-pathnames #P"qlot/" (uiop:temporary-directory)))

(defun tmp-path (&rest pathnames)
  (reduce #'merge-pathnames
          (reverse pathnames)
          :initial-value *tmp-directory*
          :from-end t))
