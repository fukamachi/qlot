(defpackage #:qlot/source/local
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:export #:source-local
           #:source-local-path))
(in-package #:qlot/source/local)

(defclass source-local (source)
  ((path :initarg :path
         :accessor source-local-path))
  (:default-initargs
   :version "none"))

(defmethod make-source ((source (eql :local)) &rest initargs)
  (handler-case
      (destructuring-bind (project-name path) initargs
        (check-type project-name string)
        (check-type path (or string pathname))
        (make-instance 'source-local
                       :project-name project-name
                       :path (uiop:ensure-directory-pathname path)))
    (error ()
      (error 'invalid-definition
             :source :local
             :usage "local <project name> <directory path>"))))
