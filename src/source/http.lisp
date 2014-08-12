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
  (:import-from :function-cache
                :defcached)
  (:import-from :alexandria
                :copy-stream)
  (:export :source-http
           :source-http-url))
(in-package :qlot.source.http)

(defclass source-http (source-has-directory)
  ((url :initarg :url
        :accessor source-http-url)
   (%version :initarg :%version
             :initform nil
             :accessor source-http-%version)))

(defmethod make-source ((source (eql 'source-http)) &rest args)
  (destructuring-bind (project-name url &optional version) args
    (make-instance 'source-http
                   :project-name project-name
                   :url url
                   :%version version)))

(defmethod freeze-source-slots ((source source-http))
  `(:url ,(source-http-url source)
    :archive-md5 ,(source-http-archive-md5 source)))

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
  (let ((file-md5 (source-http-archive-md5 source)))
    (when (and (source-http-%version source)
               (not (string= (source-http-%version source) file-md5)))
      (cerror "Ignore and continue."
              "File MD5 of ~S is different from ~S.~%The content seems to have changed."
              (source-http-url source)
              file-md5))
    (setf (source-version source)
          (format nil "http-~A" file-md5))))

(defcached source-http-archive-md5 (source)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :md5 (source-archive source))))
