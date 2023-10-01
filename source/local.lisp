(defpackage #:qlot/source/local
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/source/base
                #:initargs)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:import-from #:qlot/utils
                #:starts-with)
  (:export #:source-local
           #:source-local-path
           #:source-local-registry-directive))
(in-package #:qlot/source/local)

(defclass source-local (source)
  ((path :initarg :path
         :reader source-local-path))
  (:default-initargs
   :version "none"))

(defun convert-local-path (path)
  (etypecase path
    (string
     (if (starts-with "~/" path)
         `(:home ,(uiop:ensure-directory-pathname (subseq path 2)))
         (uiop:ensure-directory-pathname path)))
    (pathname path)))

(defun source-local-registry-directive (source)
  (check-type source source-local)
  (convert-local-path (source-local-path source)))

(defmethod make-source ((source (eql :local)) &rest initargs)
  (handler-case
      (destructuring-bind (project-name path) initargs
        (check-type project-name string)
        (check-type path (or string pathname))
        (make-instance 'source-local
                       :project-name project-name
                       :path path))
    (error ()
      (error 'invalid-definition
             :source :local
             :usage "local <project name> <directory path>"))))

(defmethod initialize-instance :after ((source source-local) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value source 'initargs)
        (loop for (k v) on (slot-value source 'initargs) by #'cddr
              unless (eq k :version)
              append (list k v))))

(defmethod freeze-source ((source source-local))
  (let ((source-params (call-next-method)))
    (cons (first source-params)
          ;; Delete unnecessary 'version'
          (loop for (k v) on (rest source-params) by #'cddr
                unless (eq k :version)
                append (list k v)))))
