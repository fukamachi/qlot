(defpackage #:qlot/source/http
  (:nicknames #:qlot.source.http)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:export #:source-http
           #:source-http-url
           #:source-http-archive-md5))
(in-package #:qlot/source/http)

(defclass source-http (source)
  ((url :initarg :url
        :accessor source-http-url)
   (archive-md5 :initarg :archive-md5
                :initform nil
                :accessor source-http-archive-md5)))

(defmethod make-source ((source (eql :http)) &rest initargs)
  (handler-case
      (destructuring-bind (project-name url &optional archive-md5) initargs
        (check-type project-name string)
        (check-type url string)
        (make-instance 'source-http
                       :project-name project-name
                       :url url
                       :archive-md5 archive-md5))
    (error ()
      (error 'invalid-definition
             :source :http
             :usage "http <project name> <tarball URL> [<archive MD5>]"))))

(defmethod defrost-source :after ((source source-http))
  (when (slot-boundp source 'qlot/source/base::version)
    (setf (source-http-archive-md5 source)
          (subseq (source-version source)
                  (length (source-version-prefix source))))))

(defmethod print-object ((source source-http) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "~A ~A"
            (source-project-name source)
            (source-http-url source))))

(defmethod source= ((source1 source-http) (source2 source-http))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (source-http-url source1)
                (source-http-url source2))
       (equal (source-http-archive-md5 source1)
              (source-http-archive-md5 source2))))
