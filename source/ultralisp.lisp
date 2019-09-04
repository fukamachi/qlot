(defpackage #:qlot.source.ultralisp
  (:nicknames #:qlot/source/ultralisp)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/source/ql
                #:source-ql)
  (:export #:source-ultralisp))
(in-package #:qlot/source/ultralisp)

(defclass source-ultralisp (source-ql)
  ()
  (:default-initargs
    :distribution "http://dist.ultralisp.org/ultralisp.txt"))

(defmethod make-source ((source (eql :ultralisp)) &rest initargs)
  (destructuring-bind (project-name version)
      initargs
    (check-type project-name string)
    (check-type version (or string (eql :latest)))

    (make-instance 'source-ultralisp
                   :project-name project-name
                   :%version version)))
