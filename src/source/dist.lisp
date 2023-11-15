(defpackage #:qlot/source/dist
  (:nicknames #:qlot.source.dist)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/utils/ql
                #:make-versioned-distinfo-url)
  (:import-from #:qlot/utils
                #:https-of)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:export #:source-dist
           #:source-dist-project
           #:source-distribution
           #:source-distinfo-url))
(in-package #:qlot/source/dist)

(defclass source-dist-project (source)
  ((%version :initarg :%version)
   (distinfo :initarg :distinfo
             :initform nil
             :accessor source-distinfo-url)))

(defclass source-dist (source-dist-project)
  ((distribution :initarg :distribution
                 :accessor source-distribution)))

(defmethod source-distribution ((source source-dist-project))
  (error "Must be implemented in subclasses"))

(defmethod make-source ((source (eql :dist)) &rest initargs)
  (handler-case
      (destructuring-bind (project-name distribution &optional (version :latest))
          initargs
        (make-instance 'source-dist
                       :project-name project-name
                       :distribution (https-of distribution)
                       :%version version))
    (error ()
      (error 'invalid-definition
             :source :dist
             :usage "dist <project name> <distribution URL> [<version>]"))))

(defmethod defrost-source :after ((source source-dist-project))
  (when (slot-boundp source 'qlot/source/base::version)
    (setf (slot-value source '%version)
          (subseq (source-version source)
                  (length (source-version-prefix source)))))
  (setf (source-distinfo-url source)
        (https-of (source-distinfo-url source))))

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
       (or (string= (source-distribution source1)
                    (source-distribution source2))
           ;; Backward-compatibility
           (string= (https-of (source-distribution source1))
                    (https-of (source-distribution source2))))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))

(defmethod source-version-prefix ((source source-dist))
  "")
