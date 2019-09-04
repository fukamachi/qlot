(defpackage #:qlot.source.ql
  (:nicknames #:qlot/source/ql)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/source/dist
                #:source-dist
                #:source-dist-base)
  (:import-from #:qlot/utils/ql
                #:quicklisp-distinfo-url)
  (:export #:source-ql
           #:source-ql-all))
(in-package #:qlot/source/ql)

(defclass source-ql (source-dist-base)
  ()
  (:default-initargs
    :distribution (quicklisp-distinfo-url)))

(defclass source-ql-all (source-dist)
  ()
  (:default-initargs
    :project-name "quicklisp"
    :distribution (quicklisp-distinfo-url)))

(defmethod make-source ((source (eql :ql)) &rest initargs)

  (destructuring-bind (project-name version &key distribution) initargs
    (check-type project-name (or string (eql :all)))
    (check-type version (or string (eql :latest)))

    (let ((distribution (or distribution
                            (quicklisp-distinfo-url))))
      (if (eq project-name :all)
          (make-instance 'source-ql-all
                         :project-name "quicklisp"
                         :distribution distribution
                         :%version version)
          (make-instance 'source-ql
                         :project-name project-name
                         :distribution distribution
                         :%version version)))))

(defmethod source-distinfo-url ((source source-ql))
  (format nil "qlot://localhost/~A.txt" (source-project-name source)))
