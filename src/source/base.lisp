(defpackage #:qlot/source/base
  (:use #:cl)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/errors
                #:unknown-source
                #:invalid-project-name
                #:qlot-syntax-error
                #:invalid-definition)
  (:export #:source
           #:source-project-name
           #:source-version
           #:source-published-at
           #:source-initargs
           #:source-defrost-args
           #:usage-of-source
           #:make-source
           #:prepare-source
           #:source-frozen-slots
           #:freeze-source
           #:defrost-source
           #:source-dist-name
           #:source-identifier
           #:source-name-for-report
           #:source=
           #:source-install-url
           #:source-version-prefix))
(in-package #:qlot/source/base)

(defclass source ()
  ((project-name :initarg :project-name
                 :initform nil
                 :accessor source-project-name)
   (version :initarg :version
            :accessor source-version)
   (published-at :initarg :published-at
                 :initform nil
                 :accessor source-published-at)

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

(defgeneric usage-of-source (source)
  (:method (source) nil))

(defgeneric make-source (source &rest args)
  (:documentation "Receives a keyword, denoting a source type and returns an instance of such source.")
  (:method (source &rest args)
    (declare (ignore args))
    (error 'unknown-source :name source))
  (:method :around (source &rest args)
    (declare (ignore args))
    (handler-bind ((error
                     (lambda (e)
                       (unless (typep e 'qlot-syntax-error)
                         (error 'invalid-definition
                                :source source
                                :reason e
                                :usage (usage-of-source source))))))
      (call-next-method))))

(defgeneric prepare-source (source)
  (:method (source)
    (declare (ignore source))))

(defgeneric source-frozen-slots (source)
  (:method ((source source))
    nil))

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

(defun source-identifier-name-for-report (identifier)
  (cond
    ((and (stringp identifier)
          (or (uiop:string-prefix-p "https://" identifier)
              (uiop:string-prefix-p "http://" identifier)))
     (let* ((without-query (subseq identifier 0 (or (position #\? identifier)
                                                     (length identifier))))
            (trimmed (string-right-trim "/" without-query))
            (slash (position #\/ trimmed :from-end t))
            (name (if slash
                      (subseq trimmed (1+ slash))
                      trimmed))
            (dot (position #\. name :from-end t)))
       (if dot
           (subseq name 0 dot)
           name)))
    (t
     identifier)))

(defun source-name-for-report (source)
  (or (source-project-name source)
      (source-dist-name source)
      (source-identifier-name-for-report (source-identifier source))))

(defgeneric source= (source1 source2)
  (:method (source1 source2)
    nil)
  (:method ((source1 source) (source2 source))
    (and (eq (class-of source1) (class-of source2))
         (string= (source-project-name source1)
                  (source-project-name source2)))))

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
