(in-package :cl-user)
(defpackage qlot.source.http
  (:use :cl
        :qlot.source)
  (:import-from :qlot.tmp
                :tmp-path)
  (:import-from :qlot.archive
                :extract-tarball)
  (:import-from :qlot.http
                :download-file)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :digest-file)
  (:import-from :alexandria
                :copy-stream)
  (:export :source-http
           :source-http-url))
(in-package :qlot.source.http)

(defclass source-http (source-has-directory)
  ((url :initarg :url
        :accessor source-http-url)))

(defmethod make-source ((source (eql 'source-http)) &rest args)
  (destructuring-bind (project-name url) args
    (make-instance 'source-http
                   :project-name project-name
                   :url url)))

(defmethod print-object ((source source-http) stream)
  (format stream "#<~S ~A ~A>"
          (type-of source)
          (source-project-name source)
          (source-http-url source)))

(defmethod prepare ((source source-http))
  (setf (source-archive source)
        (pathname
         (format nil "~A.tar.gz" (source-project-name source))))
  (download-file (source-http-url source)
                 (source-archive source))
  (setf (source-directory source)
        (extract-tarball (source-archive source)
                         (tmp-path #P"source-http/repos/")))
  (setf (source-version source)
        (ironclad:byte-array-to-hex-string
         (ironclad:digest-file :md5 (source-archive source)))))
