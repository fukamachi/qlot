(defpackage #:qlot/distify/dist
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-dist
                #:source-distinfo-url
                #:source-project-name)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:dexador)
  (:export #:distify-dist))
(in-package #:qlot/distify/dist)

(defun distify-dist (source destination &key distinfo-only)
  (declare (ignore distinfo-only))
  (check-type source source-dist)
  (let ((destination (truename destination)))
    (dex:fetch (source-distinfo-url source)
               (make-pathname :name (source-project-name source)
                              :type "txt"
                              :defaults destination)
               :if-exists :supersede
               :proxy *proxy*)
    destination))
