(defpackage #:qlot.source.dist
  (:nicknames #:qlot/source/dist)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/utils/ql
                #:make-versioned-distinfo-url)
  (:export #:source-dist
           #:source-dist-project
           #:source-distribution
           #:source-distinfo-url))
(in-package #:qlot/source/dist)

(defclass source-dist-project (source)
  ((%version :initarg :%version)
   (distribution :initarg :distribution)))

(defclass source-dist (source-dist-project)
  ())

(defmethod source-distribution ((source source-dist-project))
  (cond
    ((slot-boundp source 'qlot/source/base::version)
     (make-versioned-distinfo-url (slot-value source 'distribution)
                                  (subseq (source-version source) (length (source-version-prefix source)))))
    ((eq (slot-value source '%version) :latest)
     (slot-value source 'distribution))
    (t
     (make-versioned-distinfo-url (slot-value source 'distribution)
                                  (slot-value source '%version)))))

(defmethod source-distinfo-url ((source source-dist-project))
  (source-distribution source))

(defmethod make-source ((source (eql :dist)) &rest initargs)
  (destructuring-bind (project-name distribution &optional (version :latest)) initargs
    (make-instance 'source-dist
                   :project-name project-name
                   :distribution distribution
                   :%version version)))

(defmethod defrost-source :after ((source source-dist-project))
  (setf (slot-value source '%version)
        (subseq (source-version source)
                (length (source-version-prefix source)))))

(defmethod print-object ((source source-dist-project) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "~A ~A ~A"
            (source-project-name source)
            (source-distribution source)
            (if (slot-boundp source 'qlot/source/base::version)
                (source-version source)
                (slot-value source '%version)))))

(defmethod source= ((source1 source-dist-project) (source2 source-dist-project))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (slot-value source1 'distribution)
                (slot-value source2 'distribution))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))

(defmethod source-version-prefix ((source source-dist))
  "")
