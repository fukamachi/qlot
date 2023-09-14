(defpackage #:qlot/utils/asdf
  (:use #:cl)
  (:export #:with-autoload-on-missing
           #:directory-system-files
           #:with-directory
           #:directory-lisp-files
           #:lisp-file-system-name
           #:lisp-file-dependencies))
(in-package #:qlot/utils/asdf)

(defparameter *exclude-directories*
  (append (list "quicklisp" ".qlot" "bundle-libs")
          asdf::*default-source-registry-exclusions*))

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

(defun directory-system-files (directory)
  (labels ((asd-file-p (path)
             (and (equal (pathname-type path) "asd")
                  ;; KLUDGE: Ignore skeleton.asd of CL-Project
                  (not (search "skeleton" (pathname-name path)))))
           (collect-asd-files-in-directory (dir)
             (let ((dir-name (car (last (pathname-directory dir)))))
               (unless (or (find dir-name
                                 *exclude-directories*
                                 :test #'string=)
                           (and (stringp dir-name)
                                (char= (aref dir-name 0) #\.)))
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
(defvar *load-asd-file*)

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
    (cond
      ((not (consp form)) nil)
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
                 (gethash *load-asd-file* *registry*)))))
      ((macro-function (first form))
       (read-asd-form (macroexpand-1 form))))))

(defun read-asd-file (file)
  (uiop:with-input-file (in file)
    (let ((*load-pathname* file))
      (uiop:with-safe-io-syntax (:package :asdf-user)
        (let ((*read-eval* t))
          (loop with eof = '#:eof
                for form = (read-preserving-whitespace in nil eof)
                until (eq form eof)
                do (read-asd-form form)))))))

(defmacro with-directory ((system-file system-name dependencies) directory &body body)
  (let ((value (gensym "VALUE"))
        (dir-system-files (gensym "DIR-SYSTEM-FILES")))
    `(let ((*registry* (make-hash-table :test 'equal))
           (,dir-system-files (directory-system-files ,directory)))
       (dolist (,system-file ,dir-system-files)
         (handler-bind ((style-warning #'muffle-warning))
           (let ((*load-asd-file* ,system-file))
             (read-asd-file ,system-file))))
       (dolist (,system-file ,dir-system-files)
         (declare (ignorable ,system-file))
         (let ((,value (gethash ,system-file *registry*)))
           (dolist (,value ,value)
             (destructuring-bind (,system-name &rest ,dependencies) ,value
               (declare (ignorable ,system-name ,dependencies))
               ,@body)))))))

(defun directory-lisp-files (directory)
  (append (uiop:directory-files directory "*.lisp")
          (loop for subdir in (uiop:subdirectories directory)
                for dir-name = (car (last (pathname-directory subdir)))
                unless (or (find dir-name
                                 *exclude-directories*
                                 :test 'string=)
                           (char= (aref dir-name 0) #\.))
                append (directory-lisp-files subdir))))

(defun starts-with (prefix value)
  (check-type prefix string)
  (check-type value string)
  (and (<= (length prefix) (length value))
       (string= prefix value :end2 (length prefix))))

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
        (let ((defpackage-form (asdf/package-inferred-system::file-defpackage-form file)))
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

(defun lisp-file-dependencies (file)
  (flet ((ensure-car (object)
           (if (listp object)
               (car object)
               object)))
    (let* ((defpackage-form (asdf/package-inferred-system::file-defpackage-form file))
           (defpackage-form (remove :local-nicknames
                                    defpackage-form
                                    :key #'ensure-car
                                    :test 'equal)))
      (and defpackage-form
           (remove-if (lambda (dep-name)
                        (or (sbcl-contrib-p dep-name)
                            (member (princ-to-string dep-name) '("cl" "common-lisp")
                                    :test 'string-equal)))
                      (asdf/package-inferred-system::package-dependencies defpackage-form))))))
