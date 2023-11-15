(defpackage #:qlot/source/ultralisp
  (:nicknames #:qlot.source.ultralisp)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/source/ql
                #:source-ql)
  (:import-from #:qlot/source/dist
                #:source-distribution)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:export #:source-ultralisp))
(in-package #:qlot/source/ultralisp)

(defclass source-ultralisp (source-ql)
  ())

(defmethod source-distribution ((source source-ultralisp))
  "http://dist.ultralisp.org/ultralisp.txt")

(defmethod make-source ((source (eql :ultralisp)) &rest initargs)
  (handler-case
      (destructuring-bind (project-name &optional (version :latest))
          initargs
        (check-type project-name string)
        (check-type version (or string (eql :latest)))

        (make-instance 'source-ultralisp
                       :project-name project-name
                       :%version version))
    (error ()
      (error 'invalid-definition
             :source :ultralisp
             :usage "ultralisp <project name> [<version>]"))))
