(in-package :cl-user)
(defpackage qlot.source.http
  (:use :cl
        :qlot.source)
  (:import-from :qlot.tmp
                :tmp-path)
  (:import-from :qlot.archive
                :extract-tarball)
  (:import-from :qlot.http
                :safety-http-request)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :digest-file)
  (:import-from :alexandria
                :copy-stream))
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

(defmethod initialize ((source source-http))
  (setf (source-archive source)
        (pathname
         (format nil "~A.tar.gz" (source-project-name source))))
  (download-archive source)
  (setf (source-directory source)
        (extract-tarball (source-archive source)
                         (tmp-path #P"source-http/repos/")))
  (setf (source-version source)
        (ironclad:byte-array-to-hex-string
         (ironclad:digest-file :md5 (source-archive source)))))

(defun download-archive (source)
  (check-type source source-http)
  (let ((stream (safety-http-request (source-http-url source) :want-stream t)))
    (with-open-file (out (source-archive source)
                         :direction :output :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (alexandria:copy-stream stream out
                              :element-type '(unsigned-byte 8)))))
