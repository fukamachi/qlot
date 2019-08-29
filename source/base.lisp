(defpackage #:qlot/source/base
  (:use #:cl)
  (:import-from #:qlot/errors
                #:no-source-type)
  (:export #:source
           #:source-project-name
           #:source-version
           #:source-initargs
           #:source-defrost-args
           #:make-source
           #:source-frozen-slots
           #:freeze-source
           #:source-dist-name
           #:source=))
(in-package #:qlot/source/base)

(defclass source ()
  ((project-name :initarg :project-name
                 :reader source-project-name)
   (version :initarg :version
            :initform nil
            :accessor source-version)

   ;; Keep these variables for dumping to qlfile.lock.
   (initargs :reader source-initargs)
   (defrost-args :initform '()
                 :accessor source-defrost-args))
  (:documentation "A representation of each lines of qlfile"))

(defmethod initialize-instance :after ((source source) &rest initargs)
  (setf (slot-value source 'initargs) initargs))

(defgeneric make-source (source &rest args)
  (:documentation "Receives a keyword, denoting a source type and returns an instance of such source.")
  (:method (source &rest args)
    (declare (ignore args))
    (error 'no-source-type :name source)))

(defgeneric source-frozen-slots (source)
  (:method ((source source))
    '()))

(defgeneric freeze-source (source)
  (:method ((source source))
    (with-slots (project-name version initargs) source
      `(,project-name .
        (:class ,(type-of source)
         :initargs ,initargs
         :version ,version
         ,@(source-frozen-slots source))))))

(defmethod print-object ((source source) stream)
  (format stream "#<~S ~A ~A>"
          (type-of source)
          (source-project-name source)
          (source-version source)))

(defgeneric source-dist-name (source)
  (:method ((source source))
    (source-project-name source)))

(defgeneric source= (source1 source2)
  (:method (source1 source2)
    nil)
  (:method ((source1 source) (source2 source))
    (string= (source-project-name source1)
             (source-project-name source2))))
