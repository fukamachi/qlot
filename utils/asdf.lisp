(defpackage #:qlot/utils/asdf
  (:use #:cl)
  (:export #:with-autoload-on-missing
           #:directory-system-files
           #:with-directory
           #:directory-lisp-files
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
        #-asdf3.3 tagbody #-asdf3.3 retry
         (handler-bind ((asdf:missing-component
                          (lambda (,e)
                            (unless (gethash (asdf::missing-requires ,e) ,retrying)
                              (setf (gethash (asdf::missing-requires ,e) ,retrying) t)
                              (when (find :quicklisp *features*)
                                (uiop:symbol-call '#:ql-dist '#:ensure-installed
                                                  (uiop:symbol-call '#:ql-dist '#:find-system
                                                                    (asdf::missing-requires ,e)))
                                #+asdf3.3
                                (invoke-restart (find-restart 'asdf:clear-configuration-and-retry ,e))
                                #-asdf3.3
                                (go retry))))))
           ,@body)))))

(defun directory-system-files (directory)
  (labels ((asd-file-p (path)
             (and (equal (pathname-type path) "asd")
                  ;; KLUDGE: Ignore skeleton.asd of CL-Project
                  (not (search "skeleton" (pathname-name path)))))
           (collect-asd-files-in-directory (dir)
             (unless (find (car (last (pathname-directory dir)))
                           *exclude-directories*
                           :test #'string=)
               (nconc
                 (mapcar #'truename
                         (remove-if-not #'asd-file-p
                                        (uiop:directory-files dir)))
                 (mapcan #'collect-asd-files-in-directory (uiop:subdirectories dir))))))
    (collect-asd-files-in-directory directory)))

(defvar *registry*)
(defvar *load-asd-file*)

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
                  (let ((name (dep-list-name dep)))
                    (etypecase name
                      ((or symbol string) (string-downcase name))
                      (list (ecase (first name)
                              (:require (normalize (second name))))))))
                 ((or (symbolp dep)
                      (stringp dep))
                  (string-downcase dep))
                 (t (error "Can't normalize dependency: ~S" dep)))))
           (real-system-name (name)
             (subseq name 0 (position #\/ name))))
    (lambda (fun form env)
      (when (and (consp form)
                 (eq (first form) 'asdf:defsystem)
                 (equalp *load-asd-file* *load-pathname*))
        (destructuring-bind (system-name &rest system-form) (cdr form)
          (let ((defsystem-depends-on (getf system-form :defsystem-depends-on))
                (depends-on (getf system-form :depends-on))
                (weakly-depends-on (getf system-form :weakly-depends-on))
                (system-name (asdf::coerce-name system-name)))
            #+quicklisp
            (when defsystem-depends-on
              (let ((map (make-hash-table :test 'equal)))
                (labels ((get-deps (name)
                           (when (gethash name map)
                             (return-from get-deps nil))
                           (setf (gethash name map) t)
                           (let ((system (ql-dist:find-system name)))
                             (when system
                               (cons system
                                     (loop for system-name in (ql-dist:required-systems system)
                                           append (get-deps system-name)))))))
                  (mapc #'ql-dist:ensure-installed
                        (mapcan #'get-deps
                                (copy-seq defsystem-depends-on))))))
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
      (funcall old-hook fun form env))))

(defmacro with-directory ((system-file system-name dependencies) directory &body body)
  (let ((value (gensym "VALUE")))
    `(let ((*registry* (make-hash-table :test 'equal)))
       (dolist (,system-file (directory-system-files ,directory))
         (handler-bind ((style-warning #'muffle-warning))
           (let ((*macroexpand-hook* (make-hook *macroexpand-hook*))
                 (*package* (find-package :asdf-user))
                 (*load-asd-file* ,system-file))
             (with-autoload-on-missing
               (load ,system-file)))))
       (maphash (lambda (,system-file ,value)
                  (declare (ignorable ,system-file))
                  (dolist (,value ,value)
                    (destructuring-bind (,system-name &rest ,dependencies) ,value
                      (declare (ignorable ,system-name ,dependencies))
                      ,@body)))
                *registry*))))

(defun directory-lisp-files (directory)
  (append (uiop:directory-files directory "*.lisp")
          (loop for subdir in (uiop:subdirectories directory)
                when (not (member (car (last (pathname-directory subdir)))
                                  *exclude-directories*
                                  :test 'equal))
                append (directory-lisp-files subdir))))

(defun lisp-file-dependencies (file)
  (when (ignore-errors (asdf/package-inferred-system::file-defpackage-form file))
    (asdf/package-inferred-system::package-inferred-system-file-dependencies file)))
