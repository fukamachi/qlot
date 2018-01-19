(defpackage #:qlot/source
  (:use #:cl)
  (:import-from #:qlot/tmp
                #:tmp-path)
  (:import-from #:qlot/util
                #:find-qlfile
                #:with-package-functions
                #:sbcl-contrib-p)
  (:import-from #:uiop
                #:directory-files
                #:subdirectories
                #:directory-pathname-p
                #:absolute-pathname-p)
  (:export #:*dist-base-url*
           #:source
           #:make-source
           #:freeze-source
           #:freeze-source-slots
           #:defrost-source
           #:source-direct-dependencies
           #:find-source-class
           #:prepare
           #:update-available-p
           #:project-name
           #:version
           #:source-project-name
           #:source-version
           #:source-dist-name
           #:source-prepared
           #:source-defrost-args
           #:source-equal
           #:project.txt
           #:distinfo.txt
           #:releases.txt
           #:systems.txt
           #:archive
           #:url-path-for
           #:url-for

           #:source-has-directory
           #:source-directory
           #:source-archive))
(in-package #:qlot/source)

(defvar *dist-base-url* nil)

(defun find-source-class (class-name)
  (let* ((package-name (format nil "~A/~:@(~A~)"
                               :qlot/source class-name))
         (system-name (string-downcase package-name))
         (package (or (find-package package-name)
                      (progn
                        #+quicklisp (ql:quickload system-name :silent t)
                        #-quicklisp (asdf:load-system system-name)
                        (find-package package-name)))))
    (when package
      (intern (format nil "~A-~:@(~A~)" :source class-name)
              package))))

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

(defgeneric update-available-p (source current-version)
  (:method (source current-version)
    (unless (slot-boundp source 'qlot/source::version)
      (prepare source))

    (not (string= current-version (source-version source)))))


;;
;; Pages

(defgeneric project.txt (source)
  (:method ((source source))
    (format nil "~{~(~A~): ~A~%~}"
            (list :name                      (source-dist-name source)
                  :version                   (source-version source)
                  :distinfo-subscription-url (url-for source 'project.txt)
                  :release-index-url         (url-for source 'releases.txt)
                  :system-index-url          (url-for source 'systems.txt)))))

(defgeneric distinfo.txt (source)
  (:method ((source source))
    (format nil "~{~(~A~): ~A~%~}"
            (list :name                      (source-dist-name source)
                  :version                   (source-version source)
                  :system-index-url          (url-for source 'systems.txt)
                  :release-index-url         (url-for source 'releases.txt)
                  :archive-base-url          *dist-base-url*
                  :canonical-distinfo-url    (url-for source 'distinfo.txt)
                  :distinfo-subscription-url (url-for source 'project.txt)))))

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

(defun url-for (source for)
  (concatenate 'string *dist-base-url* (url-path-for source for)))


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
  (let ((qlfile (find-qlfile (source-directory source) :errorp nil)))
    (when qlfile
      (with-package-functions :qlot.parser (parse-qlfile)
        (parse-qlfile qlfile)))))

(defgeneric (setf source-directory) (value source)
  (:method (value (source source-has-directory))
    (setf (slot-value source 'directory)
          (if (uiop:absolute-pathname-p value)
              value
              (tmp-path (pathname (format nil "~(~A~)/repos/" (type-of source)))
                        value)))))

(defgeneric (setf source-archive) (value source)
  (:method (value (source source-has-directory))
    (setf (slot-value source 'archive)
          (if (uiop:absolute-pathname-p value)
              value
              (tmp-path (pathname (format nil "~(~A~)/archive/" (type-of source)))
                        value)))))

(defun source-system-files (source)
  (check-type source source-has-directory)
  (labels ((asd-file-p (path)
             (and (equal (pathname-type path) "asd")
                  ;; KLUDGE: Ignore skeleton.asd of CL-Project
                  (not (search "skeleton" (pathname-name path)))))
           (collect-asd-files-in-directory (dir)
             (unless (find (car (last (pathname-directory dir)))
                           asdf::*default-source-registry-exclusions*
                           :test #'string=)
               (nconc
                (remove-if-not #'asd-file-p
                               (uiop:directory-files dir))
                (mapcan #'collect-asd-files-in-directory (uiop:subdirectories dir))))))
    (collect-asd-files-in-directory (source-directory source))))

(defparameter *dependencies* nil)

(defun make-hook (old-hook)
  (labels ((dep-list-name (dep)
             (ecase (first dep)
               ((:version :require) (second dep))
               (:feature (third dep))))
           (normalize (dep)
             (real-system-name
              (cond
                ((and (consp dep)
                      (keywordp (car dep)))
                 (dep-list-name dep))
                ((or (symbolp dep)
                     (stringp dep))
                 (string-downcase dep))
                (t (error "Can't normalize dependency: ~S" dep)))))
           (real-system-name (name)
             (subseq name 0 (position #\/ name))))
    (lambda (fun form env)
      (when (and (consp form)
                 (eq (first form) 'asdf:defsystem))
        (destructuring-bind (system-name &rest system-form) (cdr form)
          (let ((defsystem-depends-on (getf system-form :defsystem-depends-on))
                (depends-on (getf system-form :depends-on))
                (weakly-depends-on (getf system-form :weakly-depends-on))
                (system-name (string-downcase system-name)))
            #+quicklisp
            (when defsystem-depends-on
              (ql:quickload defsystem-depends-on :silent t))
            (setf (gethash system-name *dependencies*)
                  (sort
                   (remove system-name
                           (remove-if #'sbcl-contrib-p
                                      (remove-duplicates
                                       (mapcar #'normalize
                                               (append defsystem-depends-on depends-on weakly-depends-on))
                                       :test #'equalp))
                           :test #'string=)
                   #'string<)))))
      (funcall old-hook fun form env))))

(defun system-file-systems (system-file)
  (handler-bind ((style-warning #'muffle-warning))
    (let ((*macroexpand-hook* (make-hook *macroexpand-hook*))
          (*package* (find-package :asdf-user))
          (result '()))
      (load system-file)
      (asdf:map-systems
       (lambda (system)
         (when (equalp system-file (asdf:system-source-file system))
           (push system result))))
      (nreverse result))))

(defmethod systems.txt ((source source-has-directory))
  (with-output-to-string (s)
    (format s "# project system-file system-name [dependency1..dependencyN]~%")
    (let ((asdf:*central-registry* (cons (source-directory source)
                                         asdf:*central-registry*))
          (*dependencies* (make-hash-table :test 'equal)))
      (dolist (system-file (source-system-files source))
        (dolist (system (system-file-systems system-file))
          (format s "~A ~A ~A~{ ~A~}~%"
                  (source-project-name source)
                  (pathname-name system-file)
                  (asdf:component-name system)
                  (gethash (asdf:component-name system) *dependencies*))
          (asdf:clear-system system))))))

(defun normalize-pathname (path)
  "Return the pathname PATH resolved to a normalized pathname in the filesystem.
Does not resolve symlinks, but PATH must actually exist in the filesystem."
  (when (wild-pathname-p path)
    (error "Wild pathnames cannot be normalized."))
  (first (uiop:directory* path)))

(defmethod releases.txt ((source source-has-directory))
  #+quicklisp (ql:quickload :ironclad :silent t)
  #-quicklisp (asdf:load-system :ironclad)
  (let* ((tarball-file (source-archive source))
         (source-dir (normalize-pathname (source-directory source)))
         (prefix (car (last (pathname-directory source-dir)))))
    (multiple-value-bind (size file-md5 content-sha1)
        (with-open-file (in tarball-file :element-type '(unsigned-byte 8))
          (with-package-functions :ironclad (byte-array-to-hex-string digest-file digest-sequence)
            (values (file-length in)
                    (byte-array-to-hex-string
                     (digest-file :md5 tarball-file))
                    (byte-array-to-hex-string
                     (digest-sequence :sha1
                                      (let ((out (make-array (file-length in) :element-type '(unsigned-byte 8))))
                                        (read-sequence out in)
                                        out))))))
      (with-slots (project-name) source
        (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%~A ~A~A ~A ~A ~A ~A~{ ~A~}~%"
                project-name
                *dist-base-url* (url-path-for source 'archive)
                size
                file-md5
                content-sha1
                prefix
                (mapcar (lambda (file)
                          (subseq (namestring file) (length (namestring source-dir))))
                        (source-system-files source)))))))

(defmethod archive ((source source-has-directory))
  (source-archive source))

(defmethod url-path-for ((source source-has-directory) (for (eql 'archive)))
  (format nil "/archive/~A/~A/~A"
          (source-project-name source)
          (source-version source)
          (file-namestring (archive source))))
