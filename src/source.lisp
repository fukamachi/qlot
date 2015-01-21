(in-package :cl-user)
(defpackage qlot.source
  (:use :cl)
  (:import-from :qlot.tmp
                :tmp-path)
  (:import-from :qlot.util
                :find-qlfile
                :with-package-functions)
  (:import-from :fad
                :list-directory
                :directory-pathname-p
                :pathname-absolute-p)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :digest-file
                :digest-sequence)
  (:import-from :alexandria
                :copy-stream
                :when-let)
  (:export :*dist-base-url*
           :source
           :make-source
           :freeze-source
           :freeze-source-slots
           :defrost-source
           :source-direct-dependencies
           :find-source-class
           :prepare
           :project-name
           :version
           :source-project-name
           :source-version
           :source-dist-name
           :source-prepared
           :source-defrost-args
           :source-equal
           :project.txt
           :distinfo.txt
           :releases.txt
           :systems.txt
           :archive
           :url-path-for

           :source-has-directory
           :source-directory
           :source-archive))
(in-package :qlot.source)

(defvar *dist-base-url* nil)

(defun find-source-class (class-name)
  (intern (format nil "~A-~:@(~A~)" #.(string :source) class-name)
          (format nil "~A.~:@(~A~)" #.(string :qlot.source) class-name)))

(defclass source ()
  ((project-name :initarg :project-name
                 :reader source-project-name)
   (version :initarg :version
            :accessor source-version)
   (initargs :reader source-initargs)
   (defrost-args :initform '()
                 :accessor source-defrost-args)
   (prepared :initform nil
             :accessor source-prepared)))

(defmethod initialize-instance :after ((source source) &rest initargs)
  (setf (slot-value source 'initargs) initargs))

(defgeneric make-source (source &rest args))

(defgeneric freeze-source-slots (source)
  (:method ((source source))
    '()))

(defgeneric freeze-source (source)
  (:method ((source source))
    (with-slots (project-name version initargs) source
      `(,project-name .
        (:class ,(type-of source)
         :initargs ,initargs
         :version ,version
         ,@(freeze-source-slots source))))))

(defgeneric defrost-source (source)
  (:method ((source source))
    (let ((class-pkg (symbol-package (type-of source))))
      (loop for (k v) on (source-defrost-args source) by #'cddr
            for slot-name = (intern (string k) class-pkg)
            when (slot-exists-p source slot-name)
              do (setf (slot-value source slot-name) v)))))

(defgeneric source-direct-dependencies (source)
  (:method ((source source))
    '()))

(defmethod print-object ((source source) stream)
  (format stream "#<~S ~A ~A>"
          (type-of source)
          (source-project-name source)
          (source-version source)))

(defgeneric source-dist-name (source)
  (:method ((source source))
    (source-project-name source)))

(defgeneric prepare (source)
  (:method ((source source))))

(defmethod prepare :around (source)
  (if (source-prepared source)
      t
      (call-next-method)))

(defmethod prepare :after (source)
  (setf (source-prepared source) t))

(defgeneric source-equal (source1 source2)
  (:method ((source1 t) (source2 t))
    nil)
  (:method ((source1 source) (source2 source))
    (string= (source-project-name source1)
             (source-project-name source2))))

(defmethod source-equal :around ((source1 source) (source2 source))
  (if (eq (type-of source1) (type-of source2))
      (call-next-method)
      nil))


;;
;; Pages

(defgeneric project.txt (source)
  (:method ((source source))
    (format nil "name: ~A
version: ~A
distinfo-subscription-url: ~A~A
release-index-url: ~A~A
system-index-url: ~A~A
"
            (source-dist-name source)
            (source-version source)
            *dist-base-url* (url-path-for source 'project.txt)
            *dist-base-url* (url-path-for source 'releases.txt)
            *dist-base-url* (url-path-for source 'systems.txt))))

(defgeneric distinfo.txt (source)
  (:method ((source source))
    (format nil "name: ~A
version: ~A
system-index-url: ~A~A
release-index-url: ~A~A
archive-base-url: ~A/
canonical-distinfo-url: ~A~A
distinfo-subscription-url: ~A~A
"
            (source-dist-name source)
            (source-version source)
            *dist-base-url* (url-path-for source 'systems.txt)
            *dist-base-url* (url-path-for source 'releases.txt)
            *dist-base-url*
            *dist-base-url* (url-path-for source 'distinfo.txt)
            *dist-base-url* (url-path-for source 'project.txt))))

(defgeneric releases.txt (source))

(defgeneric systems.txt (source))

(defgeneric archive (source))

(defgeneric url-path-for (source for)
  (:method (source (for (eql 'project.txt)))
    (format nil "/~A.txt" (source-project-name source)))
  (:method (source (for (eql 'distinfo.txt)))
    (format nil "/~A/~A/distinfo.txt"
            (source-project-name source)
            (source-version source)))
  (:method (source (for (eql 'systems.txt)))
    (format nil "/~A/~A/systems.txt"
            (source-project-name source)
            (source-version source)))
  (:method (source (for (eql 'releases.txt)))
    (format nil "/~A/~A/releases.txt"
            (source-project-name source)
            (source-version source)))
  (:method (source (for (eql 'archive)))
    nil))


;;
;; source-has-directory

(defclass source-has-directory (source)
  ((directory :initarg :directory
              :reader source-directory)
   (archive :initarg :archive
            :reader source-archive)))

(defmethod prepare :before ((source source-has-directory))
  (ensure-directories-exist (tmp-path (pathname (format nil "~(~A~)/repos/" (type-of source)))))
  (ensure-directories-exist (tmp-path (pathname (format nil "~(~A~)/archive/" (type-of source))))))

(defmethod source-direct-dependencies ((source source-has-directory))
  (when-let (qlfile (find-qlfile (source-directory source) :errorp nil))
    (with-package-functions :qlot.parser (parse-qlfile)
      (parse-qlfile qlfile))))

(defgeneric (setf source-directory) (value source)
  (:method (value (source source-has-directory))
    (setf (slot-value source 'directory)
          (if (fad:pathname-absolute-p value)
              value
              (tmp-path (pathname (format nil "~(~A~)/repos/" (type-of source)))
                        value)))))

(defgeneric (setf source-archive) (value source)
  (:method (value (source source-has-directory))
    (setf (slot-value source 'archive)
          (if (fad:pathname-absolute-p value)
              value
              (tmp-path (pathname (format nil "~(~A~)/archive/" (type-of source)))
                        value)))))

(defun source-system-files (source)
  (check-type source source-has-directory)
  (labels ((asd-file-p (path)
             (and (equal (pathname-type path) "asd")
                  ;; KLUDGE: Ignore skeleton.asd of CL-Project
                  (not (search "skeleton" (pathname-name path)))))
           (collect-asd-files (path)
             (cond
               ((and (fad:directory-pathname-p path)
                     (not (find (car (last (pathname-directory path)))
                                asdf::*default-source-registry-exclusions*
                                :test #'string=)))
                (collect-asd-files-in-directory path))
               ((asd-file-p path) (list path) )
               (T (list))))
           (collect-asd-files-in-directory (dir)
             (mapcan #'collect-asd-files (fad:list-directory dir))))
    (collect-asd-files-in-directory (source-directory source))))

(defparameter *dependencies* nil)

(defun make-hook (old-hook)
  (labels ((dep-list-name (dep)
             (ecase (first dep)
               ((:version :require) (second dep))
               (:feature (third dep))))
           (normalize (dep)
             (cond
               ((and (consp dep)
                     (keywordp (car dep)))
                (dep-list-name dep))
               ((or (symbolp dep)
                    (stringp dep))
                (string-downcase dep))
               (error "Can't normalize dependency: ~S" dep))))
    (lambda (fun form env)
      (when (and (consp form)
                 (eq (car form) 'asdf:defsystem))
        (let ((defsystem-depends-on (getf (cddr form) :defsystem-depends-on))
              (depends-on (getf (cddr form) :depends-on))
              (weakly-depends-on (getf (cddr form) :weakly-depends-on)))
          #+quicklisp
          (when defsystem-depends-on
            (ql:quickload defsystem-depends-on))
          (setf (gethash (string-downcase (cadr form)) *dependencies*)
                (remove-duplicates
                 (mapcar #'normalize
                         (append defsystem-depends-on depends-on weakly-depends-on))
                 :test #'equalp))))
      (funcall old-hook fun form env))))

(defun system-file-systems (name)
  (handler-bind ((style-warning #'muffle-warning))
    (let* ((*macroexpand-hook* (make-hook *macroexpand-hook*))
           (system (asdf:find-system name))
           (target (asdf:system-source-file system))
           (result '()))
      (asdf:map-systems
       (lambda (system)
         (when (equalp target (asdf:system-source-file system))
           (push system result))))
      (nreverse result))))

(defmethod systems.txt ((source source-has-directory))
  (with-output-to-string (s)
    (format s "# project system-file system-name [dependency1..dependencyN]~%")
    (let ((asdf:*central-registry* (cons (source-directory source)
                                         asdf:*central-registry*))
          (*dependencies* (make-hash-table :test 'equal)))
      (dolist (system-file (source-system-files source))
        (dolist (system (system-file-systems (pathname-name system-file)))
          (format s "~A ~A ~A ~{~A~^ ~}~%"
                  (source-project-name source)
                  (pathname-name system-file)
                  (asdf:component-name system)
                  (gethash (asdf:component-name system) *dependencies*))
          (asdf:clear-system system))))))

(defmethod releases.txt ((source source-has-directory))
  (let ((tarball-file (source-archive source))
        (prefix (car (last (pathname-directory (source-directory source))))))
    (multiple-value-bind (size file-md5 content-sha1)
        (with-open-file (in tarball-file :element-type '(unsigned-byte 8))
          (values (file-length in)
                  (ironclad:byte-array-to-hex-string
                   (ironclad:digest-file :md5 tarball-file))
                  (ironclad:byte-array-to-hex-string
                   (ironclad:digest-sequence :sha1
                                             (let ((out (make-array (file-length in) :element-type '(unsigned-byte 8))))
                                               (read-sequence out in)
                                               out)))))
      (with-slots (project-name) source
        (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]
~A ~A~A ~A ~A ~A ~A~{ ~A~}
"
                project-name
                *dist-base-url* (url-path-for source 'archive)
                size
                file-md5
                content-sha1
                prefix
                (mapcar #'file-namestring
                        (source-system-files source)))))))

(defmethod archive ((source source-has-directory))
  (source-archive source))

(defmethod url-path-for ((source source-has-directory) (for (eql 'archive)))
  (format nil "/archive/~A/~A/~A"
          (source-project-name source)
          (source-version source)
          (file-namestring (archive source))))
