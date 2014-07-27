(in-package :cl-user)
(defpackage qlot.source.http
  (:use :cl
        :qlot.source)
  (:import-from :qlot.tmp
                :tmp-path)
  (:import-from :qlot.archive
                :walk-tarball-file)
  (:import-from :alexandria
                :copy-stream))
(in-package :qlot.source.http)

(defclass source-http (source)
  ((url :initarg :url
        :accessor source-http-url)))

(defmethod print-object ((source source-http) stream)
  (format stream "#<~S ~A>"
          (type-of source)
          (source-http-url source)))

(defmethod initialize ((source source-http))
  (ensure-directories-exist (tmp-path #P"http/archive/"))
  (download-archive source)
  (setf (source-version source)
        (ironclad:byte-array-to-hex-string
         (ironclad:digest-file :md5 (archive-path source ".tar.gz")))))

(defun archive-path (source &optional (type ".tar.gz"))
  (merge-pathnames (format nil "~A-~A~A"
                           (source-project-name source)
                           (source-version source)
                           type)
                   (tmp-path #P"http/archive/")))

(defun download-archive (source)
  (check-type source source-http)
  (let ((stream (http-request (source-http-url source) :want-stream t)))
    (ensure-directories-exist (tmp-path #P"http/archive/"))
    (with-open-file (out (archive-path source ".tar.gz")
                         :direction :output :if-exists :supersede)
      (alexandria:copy-stream stream out))))

(defmethod systems.txt ((source source-http))
  ; TODO
  )

(defmethod releases.txt ((source source-http))
  ; TODO
  )

(defmethod archive ((source source-http))
  (archive-path source ".tar.gz"))

(defmethod url-path-for ((source source-http) (for (eql 'archive)))
  )
