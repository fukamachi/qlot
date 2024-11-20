(defpackage #:qlot/source/asdf
  (:use #:cl
        #:qlot/source/base)
  (:export #:source-asdf
           #:source-asdf-remote-url))
(in-package #:qlot/source/asdf)

(defclass source-asdf (source)
  ((remote-url :initform "https://gitlab.common-lisp.net/asdf/asdf.git"
               :accessor source-asdf-remote-url))
  (:default-initargs
   :project-name "asdf"))

(defmethod usage-of-source ((source (eql :asdf)))
  "asdf <version>")

(defmethod make-source ((source (eql :asdf)) &rest initargs)
  (destructuring-bind (version &rest args) initargs
    (check-type version string)
    (apply #'make-instance 'source-asdf
           :version version
           args)))

(defmethod source= ((source1 source-asdf) (source2 source-asdf))
  (and (string= (source-asdf-remote-url source1)
                (source-asdf-remote-url source2))
       (string= (source-version source1)
                (source-version source2))))
