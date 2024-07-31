(defpackage #:qlot/source/dist
  (:nicknames #:qlot.source.dist)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/http)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file
                #:make-versioned-distinfo-url)
  (:import-from #:qlot/utils
                #:https-of
                #:starts-with)
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

(defmethod usage-of-source ((source (eql :dist)))
  "dist <distribution URL> [<version>]")

(defmethod make-source ((source (eql :dist)) &rest initargs)
  (let (project-name)
    ;; project-name isn't necessary anymore
    (unless (or (starts-with "http://" (first initargs))
                (starts-with "https://" (first initargs)))
      (setf project-name (pop initargs)))
    (destructuring-bind (distribution &optional (version :latest))
        initargs
      (apply #'make-instance 'source-dist
             :distribution (https-of distribution)
             :%version version
             (and project-name
                  (list :project-name project-name))))))

(defmethod prepare-source ((source source-dist))
  (unless (source-project-name source)
    (let ((project-name
            (uiop:with-temporary-file (:pathname tmp-file)
              (qlot/http:fetch (source-distribution source) tmp-file)
              (cdr (assoc "name" (parse-distinfo-file tmp-file) :test 'equal)))))
      (setf (source-project-name source) project-name))))

(defmethod defrost-source :after ((source source-dist-project))
  (when (slot-boundp source 'qlot/source/base::version)
    (setf (slot-value source '%version)
          (subseq (source-version source)
                  (length (source-version-prefix source)))))
  (setf (source-distinfo-url source)
        (https-of (source-distinfo-url source))))

(defmethod print-object ((source source-dist-project) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "~A ~A"
            (source-distribution source)
            (if (slot-boundp source 'qlot/source/base::version)
                (source-version source)
                (slot-value source '%version)))))

(defmethod source-identifier ((source source-dist))
  (https-of (source-distribution source)))

(defmethod source= ((source1 source-dist-project) (source2 source-dist-project))
  (and (or (string= (source-distribution source1)
                    (source-distribution source2))
           ;; Backward-compatibility
           (string= (https-of (source-distribution source1))
                    (https-of (source-distribution source2))))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))

(defmethod source-version-prefix ((source source-dist))
  "")
