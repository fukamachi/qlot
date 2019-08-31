(defpackage #:qlot/source/ql
  (:nicknames #:qlot.source.ql)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/utils/ql
                #:quicklisp-distinfo-url)
  (:export #:source-ql
           #:source-ql-all
           #:source-distribution))
(in-package #:qlot/source/ql)

(defclass source-ql (source)
  ((%version :initarg :%version)
   (distribution :initarg :distribution
                 :reader source-distribution)
   (%distinfo :accessor source-distinfo)))

(defclass source-ql-all (source)
  ((%version :initarg :%version)
   (distribution :initarg :distribution
                 :reader source-distribution
                 :initform (quicklisp-distinfo-url))
   (%distinfo :accessor source-distinfo)))

(defmethod source-distinfo-url ((source source-ql-all))
  (source-distribution source))

(defmethod make-source ((source (eql :ql)) &rest args
                        &key distribution
                          &allow-other-keys)
  (remf args :distribution)

  (destructuring-bind (project-name version) args
    (check-type project-name (or string (eql :all)))
    (check-type version (or string (eql :latest)))

    (let ((distribution (or distribution
                            (if (eq version :latest)
                                (quicklisp-distinfo-url)
                                (quicklisp-distinfo-url version)))))
      (if (eq project-name :all)
          (make-instance 'source-ql-all
                         :project-name "quicklisp"
                         :distribution distribution
                         :%version version)
          (make-instance 'source-ql
                         :project-name project-name
                         :distribution distribution
                         :%version version)))))

(defmethod print-object ((source source-ql-all) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "quicklisp ~A"
            (if (slot-boundp source 'version)
                (source-version source)
                (slot-value source '%version)))))

(defmethod print-object ((source source-ql) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "~A ~A"
            (source-project-name source)
            (if (slot-boundp source 'version)
                (source-version source)
                (slot-value source '%version)))))

(defmethod source= ((source1 source-ql-all) (source2 source-ql-all))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))

(defmethod source= ((source1 source-ql) (source2 source-ql))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))
