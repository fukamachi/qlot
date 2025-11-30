(defpackage #:qlot/source/ql-dist
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/source/dist
                #:source-dist
                #:source-dist-project
                #:source-distribution
                #:source-distinfo-url)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:export #:source-ql-dist
           #:source-ql-dist-dist-name
           #:resolve-ql-dist-sources))
(in-package #:qlot/source/ql-dist)

(defclass source-ql-dist (source-dist-project)
  ((dist-name :initarg :dist-name
              :accessor source-ql-dist-dist-name
              :documentation "Name of the distribution to fetch from (e.g., \"shirakumo\")")
   (distribution :initarg :distribution
                 :initform nil
                 :accessor source-distribution
                 :documentation "Resolved distribution URL")))

(defmethod make-source ((source (eql :ql-dist)) &rest args)
  (destructuring-bind (dist-name project-name &optional (version :latest)) args
    (check-type dist-name string)
    (check-type project-name string)
    (check-type version (or string (eql :latest)))
    (make-instance 'source-ql-dist
                   :dist-name dist-name
                   :project-name project-name
                   :%version version)))

(defmethod usage-of-source ((source (eql :ql-dist)))
  "ql-dist <dist-name> <project-name> [<version>]")

;; source= must compare dist-name, not distribution URL (URL may not be set yet)
(defmethod source= ((source1 source-ql-dist) (source2 source-ql-dist))
  (and (string= (source-ql-dist-dist-name source1)
                (source-ql-dist-dist-name source2))
       (string= (source-project-name source1)
                (source-project-name source2))
       (equal (slot-value source1 'qlot/source/dist::%version)
              (slot-value source2 'qlot/source/dist::%version))))

;; Freeze dist-name and distribution for qlfile.lock
(defmethod source-frozen-slots ((source source-ql-dist))
  (append (call-next-method)
          `(:dist-name ,(source-ql-dist-dist-name source)
            :distinfo ,(source-distinfo-url source))
          (when (source-distribution source)
            `(:distribution ,(source-distribution source)))))

(defun resolve-ql-dist-sources (sources)
  "Resolve distribution URLs for ql-dist sources from dist sources in the same qlfile."
  (let ((dist-map (make-hash-table :test 'equal)))
    ;; Build map from dist sources
    (dolist (source sources)
      (when (typep source 'source-dist)
        ;; Ensure dist has its name (may require prepare-source)
        (unless (source-project-name source)
          (prepare-source source))
        (setf (gethash (source-project-name source) dist-map)
              (source-distribution source))))
    ;; Resolve ql-dist sources
    (dolist (source sources)
      (when (and (typep source 'source-ql-dist)
                 (null (source-distribution source)))
        (let ((url (gethash (source-ql-dist-dist-name source) dist-map)))
          (unless url
            (error 'qlot-simple-error
                   :format-control "Unknown dist '~A' referenced by ql-dist. Add a dist declaration first:~%  dist ~A <url>"
                   :format-arguments (list (source-ql-dist-dist-name source)
                                           (source-ql-dist-dist-name source))))
          (setf (source-distribution source) url)))))
  sources)
