(defpackage #:qlot/utils/asdf
  (:use #:cl)
  (:import-from #:qlot/utils
                #:starts-with
                #:with-package-functions)
  (:import-from #:qlot/logger
                #:*debug*
                #:warn-message)
  (:export #:with-autoload-on-missing
           #:directory-system-files
           #:system-class-name
           #:system-pathname
           #:with-directory
           #:directory-lisp-files
           #:lisp-file-system-name
           #:lisp-file-dependencies
           #:with-source-registry))
(in-package #:qlot/utils/asdf)

(defparameter *exclude-directories*
  (append (list "quicklisp" ".qlot" "bundle-libs")
          asdf::*default-source-registry-exclusions*))

(defmacro with-muffle-streams (&body body)
  (let ((main (gensym "MAIN")))
    `(flet ((,main () ,@body))
       (if *debug*
           (,main)
           (let ((*standard-output* (make-broadcast-stream))
                 (*error-output* (make-broadcast-stream))
                 (*trace-output* (make-broadcast-stream)))
             (,main))))))

(defun sbcl-contrib-p (name)
  (let ((name (princ-to-string name)))
    (and (<= 3 (length name))
         (string-equal name "sb-" :end1 3))))

(defmacro with-autoload-on-missing (&body body)
  (let ((retrying (gensym))
        (e (gensym)))
    `(let ((,retrying (make-hash-table :test 'equal)))
       (#+asdf3.3 asdf/session:with-asdf-session #+asdf3.3 (:override t)
        #-asdf3.3 progn
         (handler-bind ((asdf:missing-component
                          (lambda (,e)
                            (unless (gethash (asdf::missing-requires ,e) ,retrying)
                              (setf (gethash (asdf::missing-requires ,e) ,retrying) t)
                              (when (find :quicklisp *features*)
                                (uiop:symbol-call '#:ql-dist '#:ensure-installed
                                                  (uiop:symbol-call '#:ql-dist '#:find-system
                                                                    (asdf::missing-requires ,e)))
                                (invoke-restart (find-restart 'asdf:retry ,e)))))))
           ,@body)))))

(defun bundle-directory-p (dir)
  (and (uiop:directory-exists-p dir)
       (uiop:file-exists-p (merge-pathnames "system-index.txt" dir))
       (uiop:file-exists-p (merge-pathnames "setup.lisp" dir))
       t))

(defun directory-system-files (directory &key ignore-directories)
  (labels ((asd-file-p (path)
             (and (equal (pathname-type path) "asd")
                  ;; KLUDGE: Ignore skeleton.asd of CL-Project
                  (not (search "skeleton" (pathname-name path)))))
           (collect-asd-files-in-directory (dir)
             (let ((dir-name (car (last (pathname-directory dir)))))
               (unless (or (find dir
                                 ignore-directories
                                 :test 'uiop:pathname-equal)
                           (find dir-name
                                 *exclude-directories*
                                 :test #'string=)
                           (and (stringp dir-name)
                                (char= (aref dir-name 0) #\.))
                           (bundle-directory-p dir))
                 (nconc
                   (mapcar #'truename
                           (remove-if-not #'asd-file-p
                                          (uiop:directory-files dir)))
                   (mapcan #'collect-asd-files-in-directory (uiop:subdirectories dir)))))))
    (sort
      (collect-asd-files-in-directory directory)
      #'string<
      :key #'uiop:native-namestring)))

(defvar *registry*)
(defvar *package-system*)
(defvar *load-asd-file*)
(defvar *system-class-name*)
(defvar *system-pathname*)

(defmacro with-traversal-context (&body body)
  `(let ((*registry* (make-hash-table :test 'equal))
         (*package-system* (make-hash-table :test 'equal))
         (*system-class-name* (make-hash-table :test 'equal))
         (*system-pathname* (make-hash-table :test 'equal)))
     ,@body))

(defun read-asd-form (form)
  (labels ((dep-list-name (dep)
             (ecase (first dep)
               ((:version :require) (second dep))
               (:feature (third dep))))
           (normalize (dep)
             (cond
               ((and (consp dep)
                     (keywordp (car dep)))
                (let ((name (dep-list-name dep)))
                  (etypecase name
                    ((or symbol string) (string-downcase name))
                    (list (ecase (first name)
                            (:require (normalize (second name))))))))
               ((or (symbolp dep)
                    (stringp dep))
                (string-downcase dep))
               (t (error "Can't normalize dependency: ~S" dep)))))
    ;; See only toplevel forms
    (cond
      ((or (not (consp form))
           (not (symbolp (first form)))) nil)
      ((eq (first form) 'asdf:defsystem)
       (destructuring-bind (system-name &rest system-form) (cdr form)
         (let ((defsystem-depends-on (getf system-form :defsystem-depends-on))
               (depends-on (getf system-form :depends-on))
               (weakly-depends-on (getf system-form :weakly-depends-on))
               (system-name (asdf::coerce-name system-name)))
           (push (cons system-name
                       (sort
                         (remove system-name
                                 (remove-if #'sbcl-contrib-p
                                            (remove-duplicates
                                              (mapcar #'normalize
                                                      (append defsystem-depends-on depends-on weakly-depends-on))
                                              :test #'equalp))
                                 :test #'string=)
                         #'string<))
                 (gethash *load-asd-file* *registry*))
           (setf (gethash system-name *system-class-name*)
                 (getf system-form :class))
           (setf (gethash system-name *system-pathname*)
                 (getf system-form :pathname)))))
      ((eq (first form) 'asdf:register-system-packages)
       (destructuring-bind (system-name package-names)
           (rest form)
         (when (consp package-names)
           (dolist (pkg
                    ;; unquote
                    (if (member (first package-names) '(quote list))
                        (second package-names)
                        package-names))
             (let ((pkg (typecase pkg
                          (symbol (symbol-name pkg))
                          (string pkg))))
               (when pkg
                 (setf (gethash pkg *package-system*)
                       system-name))))))))))

(defun read-asd-file (file &key eval-form)
  (uiop:with-input-file (in file)
    (let ((*load-pathname* file)
          (*load-truename* file)
          (*load-asd-file* file)
          (tried-so-far (make-hash-table :test 'equalp)))
      (uiop:with-safe-io-syntax (:package :asdf-user)
        (let ((*read-eval* t))
          (loop with eof = '#:eof
                for form = (read-preserving-whitespace in nil eof)
                until (eq form eof)
                do (read-asd-form form)
                   (when eval-form
                     (tagbody retry
                       (handler-case
                           (with-muffle-streams
                               (eval form))
                         (asdf:missing-dependency-of-version (c)
                           (error c))
                         (asdf:missing-dependency (c)
                           (with-package-functions #:ql-dist (ensure-installed find-system)
                             (let ((missing (asdf::missing-requires c)))
                               (if (gethash missing tried-so-far)
                                   (error "Dependency looping -- already tried to load ~A" missing)
                                   (setf (gethash missing tried-so-far) missing))
                               (let ((system (find-system missing)))
                                 (unless system
                                   (error c))
                                 (with-muffle-streams
                                   (ensure-installed system)
                                   (asdf:load-system missing :verbose nil)))))
                           (go retry)))))))))))

(defun system-class-name (system-name)
  (gethash system-name *system-class-name*))

(defun system-pathname (system-name)
  (gethash system-name *system-pathname*))

(defmacro with-directory ((system-file system-name dependencies &key eval-form ignore-directories)
                          directory &body body)
  (let ((value (gensym "VALUE"))
        (dir-system-files (gensym "DIR-SYSTEM-FILES")))
    `(with-traversal-context
       (let ((,dir-system-files (directory-system-files ,directory
                                                        :ignore-directories ,ignore-directories)))
         (dolist (,system-file ,dir-system-files)
           (handler-case
               (handler-bind ((style-warning #'muffle-warning))
                 (read-asd-file ,system-file
                                :eval-form ,eval-form))
             (error (c)
               (warn-message "Failed to calculate dependencies for '~A' due to an error while loading it."
                             (file-namestring ,system-file))
               (when *debug*
                 (warn-message "    ~A~%" c)))))
         (dolist (,system-file ,dir-system-files)
           (declare (ignorable ,system-file))
           (let ((,value (or (gethash ,system-file *registry*)
                             (list (list (pathname-name ,system-file))))))
             (dolist (,value ,value)
               (destructuring-bind (,system-name &rest ,dependencies) ,value
                 (declare (ignorable ,system-name ,dependencies))
                 ,@body))))))))

(defun directory-lisp-files (directory &key ignore-directories)
  (append (uiop:directory-files directory "*.lisp")
          (loop for subdir in (uiop:subdirectories directory)
                for dir-name = (car (last (pathname-directory subdir)))
                unless (or (find subdir
                                 ignore-directories
                                 :test 'uiop:pathname-equal)
                           (find dir-name
                                 *exclude-directories*
                                 :test 'string=)
                           (char= (aref dir-name 0) #\.)
                           (bundle-directory-p subdir))
                append (directory-lisp-files subdir
                                             :ignore-directories ignore-directories))))

(defun lisp-file-system-name (file root primary-system-name)
  (block nil
    (handler-bind ((error
                     (lambda (e)
                       (declare (ignorable e))
                       ;;(uiop:print-condition-backtrace e)
                       (return nil))))
      (flet ((primary-system-prefix-p (package-name)
               (check-type package-name string)
               (starts-with (format nil "~A/" primary-system-name) package-name)))
        (let ((defpackage-form (with-muffle-streams
                                 (asdf/package-inferred-system::file-defpackage-form file))))
          (when defpackage-form
            (let* ((package-names (mapcar #'string-downcase
                                          (cons (second defpackage-form)
                                                (cdr
                                                 (assoc :nicknames (cddr defpackage-form)
                                                        :test #'string=)))))
                   (system-name (find-if
                                 (lambda (name)
                                   (and (primary-system-prefix-p name)
                                        (uiop:pathname-equal
                                         (merge-pathnames
                                          (format nil "~A~@[.~A~]"
                                                  (subseq name (1+ (length primary-system-name)))
                                                  (pathname-type file))
                                          root)
                                         file)))
                                 package-names)))
              (when system-name
                (string-downcase system-name)))))))))

(defun lisp-file-dependencies (file &key test)
  (flet ((ensure-car (object)
           (if (listp object)
               (car object)
               object)))
    (let* ((defpackage-form
               ;; Prevent errors when loading a file without defpackage form
               (ignore-errors
                 (asdf/package-inferred-system::file-defpackage-form file)))
           (defpackage-form (remove-if (lambda (key)
                                         (find key '(:local-nicknames :lock :implement)
                                               :test 'equal))
                                       defpackage-form
                                       :key #'ensure-car)))
      (and defpackage-form
           (typep (second defpackage-form) '(or symbol string))
           (let ((package-name (string-downcase (second defpackage-form))))
             (and (or (null test)
                      (funcall test package-name))
                  (values
                   (mapcar (lambda (name)
                             (let ((name-str (typecase name
                                               (symbol (and name
                                                            (symbol-name name)))
                                               (string name))))
                               (string-downcase
                                (or (and name-str
                                         (gethash name-str *package-system*))
                                    name))))
                           (remove-if (lambda (dep-name)
                                        (or (sbcl-contrib-p dep-name)
                                            (member (string-downcase dep-name)
                                                    '("cl" "common-lisp")
                                                    :test 'string=)))
                                      (asdf/package-inferred-system::package-dependencies defpackage-form)))
                   package-name)))))))

(defun call-with-source-registry (source-registry fn)
  (let ((outer-source-registry asdf:*source-registry-parameter*))
    (unwind-protect
         (progn
           (asdf:initialize-source-registry source-registry)
           (funcall fn))
      (asdf:initialize-source-registry outer-source-registry))))

(defmacro with-source-registry ((source-registry) &body body)
  `(call-with-source-registry ,source-registry (lambda () ,@body)))
