(defpackage #:qlot/source/ql
  (:nicknames #:qlot.source.ql)
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
  ())

(defclass source-ql-all (source-dist)
  ()
  (:default-initargs
    :distribution (quicklisp-distinfo-url)))

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

(defmethod source-distinfo-url ((source source-ql))
  (format nil "qlot://localhost/~A.txt" (source-project-name source)))
