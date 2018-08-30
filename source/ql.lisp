(defpackage #:qlot.source.ql
  (:nicknames #:qlot/source/ql)
  (:use #:cl
        #:qlot/source)
  (:import-from #:qlot/util
                #:find-qlfile
                #:with-package-functions)
  (:import-from #:qlot/http
                #:http-get)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:source-ql
           #:source-ql-all))
(in-package #:qlot/source/ql)


(defparameter *default-distribution*
  "http://beta.quicklisp.org/dist/quicklisp.txt")


(defclass source-ql (source)
  ((%version :initarg :%version)
   (distribution :initarg :distribution
                 :reader source-distribution)))

(defclass source-ql-all (source)
  ((%version :initarg :%version)
   (distribution :initarg :distribution
                 :reader source-distribution)))


(defun set-default-distribution (instance)
  (when (not (slot-boundp instance 'distribution))
    (setf (slot-value instance 'distribution)
          *default-distribution*)))

(defmethod initialize-instance :after ((instance source-ql)
                                       &rest initargs)
  (declare (ignorable initargs))
  (set-default-distribution instance))


(defmethod initialize-instance :after ((instance source-ql-all)
                                       &rest initargs)
  (declare (ignorable initargs))
  (set-default-distribution instance))


(defun get-distribution-url-pattern (distribution)
  (check-type distribution string)
  ;; TODO: add autodiscovery
  (if (string-equal distribution
                    *default-distribution*)
      "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt"
      "http://dist.ultralisp.org/ultralisp/~A/releases.txt"))


(defun get-versioned-distribution-url (distribution version)
  (check-type distribution string)
  (check-type version string)
  
  (format nil
          (get-distribution-url-pattern distribution)
          version))


(defmethod make-source ((source (eql :ql)) &rest args
                        &key (distribution *default-distribution*)
                          &allow-other-keys)
  (remf args :distribution)
  
  (destructuring-bind (project-name version) args
    (let ((url (if (eql version :latest)
                   distribution
                   (get-versioned-distribution-url distribution version))))
      (if (eq project-name :all)
          (make-instance 'source-ql-all
                         :project-name (if (string= distribution *default-distribution*)
                                           "quicklisp"
                                           "ultralisp")
                         :distribution url
                         :%version version)
          (make-instance 'source-ql
                         :project-name project-name
                         :distribution url
                         :%version version)))))

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
              (ql-latest-version source)
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

(defun ql-latest-version (source)
  (check-type source (or source-ql source-ql-all))
  (let ((quicklisp.txt (http-get (source-distribution source))))
    (or
     (loop for line in (split-sequence #\Newline quicklisp.txt)
           when (starts-with-subseq "version: " line)
             do (return (subseq line 9)))
     (error "Failed to get the latest version of Quicklisp."))))


(defun retrieve-quicklisp-metadata-item (source item-name)
  (check-type source (or source-ql
                         source-ql-all))
  (check-type item-name string)
  
  (let* ((url (source-distribution source))
         (dist-metadata (http-get url))
         (search-for (format nil "~A: " item-name))
         (value (loop for line in (split-sequence #\Newline dist-metadata)
                      when (starts-with-subseq search-for line)
                        do (return (subseq line (length search-for))))))
    (unless value
      (error "Failed to get the latest version of Quicklisp."))
    value))

(defun retrieve-quicklisp-releases (source)
  (let* ((release-index-url (retrieve-quicklisp-metadata-item source "release-index-url")))
    (http-get release-index-url)))

(defun retrieve-quicklisp-systems (source)
  (let* ((system-index-url (retrieve-quicklisp-metadata-item source "system-index-url")))
    (http-get system-index-url)))

(defun source-ql-releases (source)
  (with-slots (project-name) source
    (let* ((version (source-ql-version source))
           (releases.txt (retrieve-quicklisp-releases source)))
      (loop with project-name/sp = (concatenate 'string project-name " ")
            for line in (split-sequence #\Newline releases.txt)
            when (starts-with-subseq project-name/sp line)
              do (return (split-sequence #\Space line :remove-empty-subseqs t))
            finally
               (error "~S doesn't exist in quicklisp ~A."
                      project-name
                      version)))))

(defun source-ql-systems (source)
  (with-slots (project-name) source
    (let ((systems.txt (retrieve-quicklisp-systems source)))
      (loop with project-name/sp = (concatenate 'string project-name " ")
            for line in (split-sequence #\Newline systems.txt)
            when (starts-with-subseq project-name/sp line)
              collect (split-sequence #\Space line :remove-empty-subseqs t)))))

(defgeneric source-ql-version (source)
  (:method ((source source-ql))
    (with-slots (%version) source
      (if (eq %version :latest)
          (ql-latest-version source)
          %version)))
  (:method ((source source-ql-all))
    (with-slots (version) source
      (if (eq version :latest)
          (ql-latest-version source)
          version))))

(defmethod distinfo.txt ((source source-ql))
  (break)
  (format nil "~{~(~A~): ~A~%~}"
          (list :name                      (source-project-name source)
                :version                   (source-version source)
                :system-index-url          (url-for source 'systems.txt)
                :release-index-url         (url-for source 'releases.txt)
                ;; TODO: replace this with auto-discovery
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
  (source-distribution source))
