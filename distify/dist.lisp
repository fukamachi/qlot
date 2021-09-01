(defpackage #:qlot/distify/dist
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-dist
                #:source-distribution
                #:source-distinfo-url
                #:source-project-name)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/distify
                #:get-distinfo-url)
  (:import-from #:dexador)
  (:export #:distify-dist))
(in-package #:qlot/distify/dist)

(defun distify-dist (source destination &key distinfo-only)
  (declare (ignore distinfo-only))
  (check-type source source-dist)
  (unless (source-distinfo-url source)
    (setf (source-distinfo-url source)
          (get-distinfo-url (source-distribution source)
                            (slot-value source 'qlot/source/dist::%version))))
  (let ((destination (truename destination)))
    (dex:fetch (source-distinfo-url source)
               (make-pathname :name (source-project-name source)
                              :type "txt"
                              :defaults destination)
               :if-exists :supersede
               :proxy *proxy*)
    destination))
