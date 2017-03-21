(defpackage #:qlot/tmp
  (:use #:cl)
  (:import-from #:qlot/util
                #:generate-random-string)
  (:export #:*tmp-directory*
           #:tmp-path))
(in-package #:qlot/tmp)

(defvar *tmp-directory*
  (merge-pathnames (format nil "qlot-~A/" (generate-random-string))
                   (uiop:temporary-directory)))

(defun tmp-path (&rest pathnames)
  (reduce #'merge-pathnames
          (reverse pathnames)
          :initial-value *tmp-directory*
          :from-end t))
