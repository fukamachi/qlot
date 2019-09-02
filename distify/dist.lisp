(defpackage #:qlot/distify/dist
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-dist
                #:source-distribution
                #:source-project-name)
  (:import-from #:dexador)
  (:export #:distify-dist))
(in-package #:qlot/distify/dist)

(defun distify-dist (source destination &key distinfo-only)
  (declare (ignore distinfo-only))
  (check-type source source-dist)
  (let ((destination (truename destination)))
    (dex:fetch (source-distribution source)
               (make-pathname :name (source-project-name source)
                              :type "txt"
                              :defaults destination)
               :if-exists :supersede)
    destination))
