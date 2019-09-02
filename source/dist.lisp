(defpackage #:qlot/source/dist
  (:nicknames #:qlot.source.dist)
  (:use #:cl
        #:qlot/source/base)
  (:export #:source-dist
           #:source-distribution
           #:source-distinfo
           #:source-distinfo-url))
(in-package #:qlot/source/dist)

(defclass source-dist (source)
  ((%version :initarg :%version)
   (distribution :initarg :distribution
                 :reader source-distribution)
   (%distinfo :accessor source-distinfo)))

(defmethod source-distinfo-url ((source source-dist))
  (source-distribution source))

(defmethod make-source ((source (eql :dist)) &rest initargs)
  (destructuring-bind (project-name distribution &optional (version :latest)) initargs
    (make-instance 'source-dist
                   :project-name project-name
                   :distribution distribution
                   :%version version)))

(defmethod print-object ((source source-dist) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "~A ~A ~A"
            (source-project-name source)
            (source-distribution source)
            (if (slot-boundp source 'qlot/source/base::version)
                (source-version source)
                (slot-value source '%version)))))

(defmethod source= ((source1 source-dist) (source2 source-dist))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (source-distribution source1)
                (source-distribution source2))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))
