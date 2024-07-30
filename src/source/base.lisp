(defpackage #:qlot/source/base
  (:use #:cl)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/errors
                #:unknown-source
                #:invalid-project-name)
  (:export #:source
           #:source-project-name
           #:source-version
           #:source-initargs
           #:source-defrost-args
           #:make-source
           #:prepare-source
           #:source-frozen-slots
           #:freeze-source
           #:defrost-source
           #:source-dist-name
           #:source-identifier
           #:source=
           #:write-distinfo
           #:source-install-url
           #:source-version-prefix))
(in-package #:qlot/source/base)

(defclass source ()
  ((project-name :initarg :project-name
                 :initform nil
                 :accessor source-project-name)
   (version :initarg :version
            :accessor source-version)

   ;; Keep these variables for dumping to qlfile.lock.
   (initargs :reader source-initargs)
   (defrost-args :initform '()
                 :accessor source-defrost-args))
  (:documentation "A representation of each lines of qlfile"))

(defmethod initialize-instance ((source source) &rest initargs)
  (prog1 (call-next-method)
    (remf initargs :project-name)
    (setf (slot-value source 'initargs) initargs)))

(defmethod initialize-instance :after ((source source) &rest initargs)
  (declare (ignore initargs))
  (let ((project-name (source-project-name source)))
    (check-type project-name (or string null))
    (when project-name
      (let ((forbidden-chars
              (loop for char in
                       #-(or mswindows win32)
                       '(#\/)
                       #+(or mswindows win32)
                       '(#\< #\> #\: #\" #\\ #\/ #\| #\? #\*)
                    when (find char project-name :test #'char=)
                    collect char)))
        (when forbidden-chars
          (error 'invalid-project-name
                 :name project-name
                 :reason (format nil "Project names must not contain ~{'~A'~#[~;, and ~:;, ~]~}"
                                 forbidden-chars)))))))

(defgeneric make-source (source &rest args)
  (:documentation "Receives a keyword, denoting a source type and returns an instance of such source.")
  (:method (source &rest args)
    (declare (ignore args))
    (error 'unknown-source :name source)))

(defgeneric prepare-source (source)
  (:method (source)
    (declare (ignore source))))

(defgeneric source-frozen-slots (source)
  (:method ((source source))
    '()))

(defgeneric freeze-source (source)
  (:method ((source source))
    (with-slots (project-name version initargs) source
      `(,project-name .
        (:class ,(type-of source)
         :initargs ,initargs
         :version ,version
         ,@(source-frozen-slots source))))))

(defgeneric defrost-source (source)
  (:method ((source source))
    (let ((class-pkg (symbol-package (type-of source))))
      (loop for (k v) on (source-defrost-args source) by #'cddr
            for slot-name = (case k
                              (:version 'version)
                              (:project-name 'project-name)
                              (otherwise (intern (string k) class-pkg)))
            when (slot-exists-p source slot-name)
            do (setf (slot-value source slot-name) v)))
    source)
  (:method :after ((source source))
    (prepare-source source)))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "~A ~A"
            (source-identifier source)
            (if (slot-boundp source 'version)
                (source-version source)
                "<no version>"))))

(defgeneric source-dist-name (source)
  (:method ((source source))
    (source-project-name source)))

(defgeneric source-identifier (source)
  (:method ((source source))
    (source-dist-name source)))

(defgeneric source= (source1 source2)
  (:method (source1 source2)
    nil)
  (:method ((source1 source) (source2 source))
    (and (eq (class-of source1) (class-of source2))
         (string= (source-project-name source1)
                  (source-project-name source2)))))

(defun write-distinfo (source &optional (stream *standard-output*))
  (format stream "~{~(~A~): ~A~%~}"
          (list :name (source-dist-name source)
                :version (source-version source)
                :distinfo-subscription-url (format nil "qlot://localhost/~A.txt"
                                                   (source-project-name source))
                :canonical-distinfo-url (format nil "qlot://localhost/~A.txt"
                                                (source-project-name source))
                :release-index-url (format nil "qlot://localhost/~A/~A/releases.txt"
                                           (source-project-name source)
                                           (source-version source))
                :system-index-url (format nil "qlot://localhost/~A/~A/systems.txt"
                                          (source-project-name source)
                                          (source-version source)))))

(defgeneric source-install-url (source)
  (:method ((source source))
    (format nil "qlot://localhost/~A.txt" (source-project-name source))))

(defgeneric source-version-prefix (source)
  (:method ((source source))
    (concatenate 'string
                 (let ((class-name (string-downcase (type-of source))))
                   (if (eql 0 (search "source-" class-name))
                       (subseq class-name #.(length "source-"))
                       class-name))
                 "-")))
