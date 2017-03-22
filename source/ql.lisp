(defpackage #:qlot/source/ql
  (:nicknames #:qlot.source.ql)
  (:use #:cl
        #:qlot/source)
  (:import-from #:qlot/util
                #:find-qlfile
                #:with-package-functions)
  (:import-from #:qlot/proxy
                #:get-proxy)
  (:import-from #:dexador)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:source-ql
           #:source-ql-all))
(in-package #:qlot/source/ql)

(defclass source-ql (source)
  ((%version :initarg :%version)))

(defclass source-ql-all (source)
  ((%version :initarg :%version)))

(defmethod make-source ((source (eql 'source-ql)) &rest args)
  (destructuring-bind (project-name version) args
    (if (eq project-name :all)
        (make-instance 'source-ql-all
                       :project-name "quicklisp"
                       :%version version)
        (make-instance 'source-ql
                       :project-name project-name
                       :%version version))))

(defmethod print-object ((source source-ql-all) stream)
  (with-slots (project-name %version version) source
    (format stream "#<~S ~A ~A~:[~;~:*(~A)~]>"
            (type-of source)
            (if (stringp project-name)
                project-name
                (prin1-to-string project-name))
            (if (stringp %version)
                %version
                (prin1-to-string %version))
            (and (slot-boundp source 'version)
                 (source-version source)))))

(defmethod print-object ((source source-ql) stream)
  (with-slots (project-name %version) source
    (format stream "#<~S ~A ~A>"
            (type-of source)
            (if (stringp project-name)
                project-name
                (prin1-to-string project-name))
            (if (stringp %version)
                %version
                (prin1-to-string %version)))))

(defmethod prepare ((source source-ql-all))
  (unless (slot-boundp source 'version)
    (setf (source-version source)
          (if (eq (slot-value source '%version) :latest)
              (ql-latest-version)
              (slot-value source '%version)))))

(defmethod prepare ((source source-ql))
  (setf (source-version source)
        (format nil "ql-~A" (source-ql-version source))))

(defmethod source-equal ((source1 source-ql-all) (source2 source-ql-all))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))

(defmethod source-equal ((source1 source-ql) (source2 source-ql))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))

(defcached ql-latest-version ()
  (let ((stream (dex:get "http://beta.quicklisp.org/dist/quicklisp.txt"
                         :want-stream t
                         :proxy (get-proxy))))
    (or
     (loop for line = (read-line stream nil nil)
           while line
           when (starts-with-subseq "version: " line)
             do (return (subseq line 9)))
     (error "Failed to get the latest version of Quicklisp."))))

(defun retrieve-quicklisp-releases (version)
  (dex:get (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/releases.txt"
                   version)
           :want-stream t
           :proxy (get-proxy)))

(defun retrieve-quicklisp-systems (version)
  (dex:get (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/systems.txt"
                   version)
           :want-stream t
           :proxy (get-proxy)))

(defun source-ql-releases (source)
  (with-slots (project-name) source
    (let* ((version (source-ql-version source))
           (body (retrieve-quicklisp-releases version)))
      (loop with project-name/sp = (concatenate 'string project-name " ")
            for line = (read-line body nil nil)
            while line
            when (starts-with-subseq project-name/sp line)
              do (return (ppcre:split "\\s+" line))
            finally
               (error "~S doesn't exist in quicklisp ~A."
                      project-name
                      version)))))

(defun source-ql-systems (source)
  (with-slots (project-name) source
    (let* ((version (source-ql-version source))
           (body (retrieve-quicklisp-systems version)))
      (loop with project-name/sp = (concatenate 'string project-name " ")
            for line = (read-line body nil nil)
            while line
            when (starts-with-subseq project-name/sp line)
              collect (ppcre:split "\\s+" line)))))

(defgeneric source-ql-version (source)
  (:method ((source source-ql))
    (with-slots (%version) source
      (if (eq %version :latest)
          (ql-latest-version)
          %version)))
  (:method ((source source-ql-all))
    (with-slots (version) source
      (if (eq version :latest)
          (ql-latest-version)
          version))))

(defmethod distinfo.txt ((source source-ql))
  (format nil "~{~(~A~): ~A~%~}"
          (list :name                      (source-project-name source)
                :version                   (source-version source)
                :system-index-url          (url-for source 'systems.txt)
                :release-index-url         (url-for source 'releases.txt)
                :archive-base-url          "http://beta.quicklisp.org/"
                :canonical-distinfo-url    (url-for source 'distinfo.txt)
                :distinfo-subscription-url (url-for source 'project.txt))))

(defmethod systems.txt ((source source-ql))
  (format nil "# project system-file system-name [dependency1..dependencyN]~%~{~{~A~^ ~}~%~}"
          (source-ql-systems source)))

(defmethod releases.txt ((source source-ql))
  (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%~{~A~^ ~}~%"
          (source-ql-releases source)))

(defmethod url-path-for ((source source-ql-all) (for (eql 'project.txt)))
  (prepare source)
  (with-slots (version) source
    (if (eq version :latest)
        "http://beta.quicklisp.org/dist/quicklisp.txt"
        (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt" version))))
