(defpackage #:qlot.source.http
  (:nicknames #:qlot/source/http)
  (:use #:cl
        #:qlot/source)
  (:import-from #:qlot/tmp
                #:tmp-path)
  (:import-from #:qlot/archive
                #:extract-tarball)
  (:import-from #:qlot/proxy
                #:get-proxy)
  (:import-from #:qlot/util
                #:with-package-functions)
  (:import-from #:dexador)
  (:export #:source-http
           #:source-http-url))
(in-package #:qlot/source/http)

(defclass source-http (source-has-directory)
  ((url :initarg :url
        :accessor source-http-url)
   (archive-md5 :initarg :archive-md5
                :initform nil
                :accessor source-http-archive-md5)))

(defmethod make-source ((source (eql 'source-http)) &rest args)
  (destructuring-bind (project-name url &optional archive-md5) args
    (make-instance 'source-http
                   :project-name project-name
                   :url url
                   :archive-md5 archive-md5)))

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
  (dex:fetch (source-http-url source)
             (source-archive source)
             :if-exists :supersede
             :proxy (get-proxy))
  (setf (source-directory source)
        (extract-tarball (source-archive source)
                         (tmp-path #P"source-http/repos/")))
  (let ((file-md5 (archive-md5 source)))
    (when (and (source-http-archive-md5 source)
               (not (string= (source-http-archive-md5 source) file-md5)))
      (cerror "Ignore and continue."
              "File MD5 of ~S is different from ~S.~%The content seems to have changed."
              (source-http-url source)
              file-md5))
    (setf (source-http-archive-md5 source) file-md5)
    (setf (source-version source)
          (format nil "http-~A" file-md5))))

(defmethod source-equal ((source1 source-http) (source2 source-http))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (source-http-url source1)
                (source-http-url source2))
       (equal (source-http-archive-md5 source1)
              (source-http-archive-md5 source2))))

(defun archive-md5 (source)
  #+quicklisp (ql:quickload :ironclad :silent t)
  #-quicklisp (asdf:load-system :ironclad)
  (with-package-functions :ironclad (byte-array-to-hex-string digest-file)
    (byte-array-to-hex-string
     (digest-file :md5 (source-archive source)))))
