(defpackage #:qlot/source/dist
  (:nicknames #:qlot.source.dist)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/utils/ql
                #:make-versioned-distinfo-url)
  (:import-from #:qlot/utils
                #:https-of
                #:starts-with)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
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
      (progn
        ;; project-name isn't necessary anymore
        (when (or (starts-with "http://" (first initargs))
                  (starts-with "https://" (first initargs)))
          (push nil initargs))
        (destructuring-bind (project-name distribution &optional (version :latest))
            initargs
          (make-instance 'source-dist
                         :project-name project-name
                         :distribution (https-of distribution)
                         :%version version)))
    (error ()
      (error 'invalid-definition
             :source :dist
             :usage "dist <distribution URL> [<version>]"))))

(defmethod prepare-source ((source source-dist))
  (unless (source-project-name source)
    (let ((project-name
            (run-lisp `((uiop:with-temporary-file (:pathname cl-user::tmp-file)
                          (uiop:symbol-call :qlot/http :fetch ,(source-distribution source) cl-user::tmp-file)
                          (write-string
                           (cdr (assoc "name" (uiop:symbol-call :qlot/utils/ql :parse-distinfo-file cl-user::tmp-file) :test 'equal)))))
                      :systems '("qlot/http" "qlot/utils/ql")
                      :source-registry (asdf:system-source-directory :qlot)
                      :output :string)))
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
    (format stream "~A ~A ~A"
            (source-project-name source)
            (source-distribution source)
            (if (slot-boundp source 'qlot/source/base::version)
                (source-version source)
                (slot-value source '%version)))))

(defmethod source-identifier ((source source-dist))
  (source-distribution source))

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
