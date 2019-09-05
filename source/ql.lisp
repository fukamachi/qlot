(defpackage #:qlot.source.ql
  (:nicknames #:qlot/source/ql)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/source/dist
                #:source-dist
                #:source-dist-project)
  (:import-from #:qlot/utils/ql
                #:quicklisp-distinfo-url)
  (:export #:source-ql
           #:source-ql-all))
(in-package #:qlot/source/ql)

(defclass source-ql (source-dist-project)
  ()
  (:default-initargs
    :distribution (quicklisp-distinfo-url)))

(defclass source-ql-all (source-dist)
  ()
  (:default-initargs
    :distribution (quicklisp-distinfo-url)))

;; For backward-compatibility
;; source-ql-dist will be replaced by source-dist
(defmethod initialize-instance :around ((source source-ql-all) &rest initargs)
  (let* ((source (apply #'call-next-method source initargs))
         (source (apply #'change-class source 'source-dist initargs)))
    (setf (slot-value source 'qlot/source/base::initargs) initargs)
    source))

(defmethod make-source ((source (eql :ql)) &rest initargs)
  (destructuring-bind (project-name version &key distribution) initargs
    (check-type project-name (or string (eql :all)))
    (check-type version (or string (eql :latest)))

    (let ((distribution (or distribution
                            (quicklisp-distinfo-url))))
      (if (eq project-name :all)
          (make-instance 'source-dist
                         :project-name "quicklisp"
                         :distribution distribution
                         :%version version)
          (make-instance 'source-ql
                         :project-name project-name
                         :distribution distribution
                         :%version version)))))
