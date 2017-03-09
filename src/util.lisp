(in-package :cl-user)
(defpackage qlot.util
  (:use :cl)
  (:export :with-quicklisp-home
           :with-package-functions
           :pathname-in-directory-p
           :find-qlfile
           :call-in-local-quicklisp
           :with-local-quicklisp
           :ensure-installed-in-local-quicklisp
           :all-required-systems
           :generate-random-string
           :with-in-directory))
(in-package :qlot.util)

(defmacro with-quicklisp-home (qlhome &body body)
  `(flet ((main () ,@body))
     (eval `(let ((,(intern #.(string :*quicklisp-home*) :ql) ,,qlhome)) (funcall ,#'main)))))

(defmacro with-package-functions (package-designator functions &body body)
  (let ((args (gensym "ARGS")))
    `(flet (,@(loop for fn in functions
                    collect `(,fn (&rest ,args)
                                  (apply
                                   ,(if (and (listp fn) (eq (car fn) 'setf))
                                        `(eval `(function (setf ,(intern ,(string (cadr fn)) ,package-designator))))
                                        `(symbol-function (intern ,(string fn) ,package-designator)))
                                   ,args))))
       ,@body)))

(defun pathname-in-directory-p (path directory)
  (let ((directory (pathname-directory directory))
        (path (pathname-directory path)))
    (loop for dir1 = (pop directory)
          for dir2 = (pop path)
          if (null dir1)
            do (return t)
          else if (null dir2)
            do (return nil)
          else if (string/= dir1 dir2)
            do (return nil)
          finally
             (return t))))

(defun find-qlfile (directory &key (errorp t) use-lock)
  (check-type directory pathname)
  (unless #+clisp (ext:probe-directory directory)
          #-clisp (probe-file directory)
    (error "~S does not exist." directory))
  (let ((qlfile (merge-pathnames (if use-lock
                                     "qlfile.lock"
                                     "qlfile")
                                 directory)))
    (unless (probe-file qlfile)
      (when errorp
        (error "'~A' is not found at '~A'." qlfile directory))
      (setf qlfile nil))

    qlfile))

(defun merge-hash-tables (from-table to-table)
  "Add all entries from FROM-TABLE to TO-TABLE, overwriting existing entries
with the same key."
  (flet ((add-to-original (value key)
           (setf (gethash value to-table) key)))
    (maphash #'add-to-original from-table)))

(defun call-in-local-quicklisp (fn system qlhome)
  (unless #+clisp (ext:probe-directory qlhome)
          #-clisp (probe-file qlhome)
    (error "Directory ~S does not exist." qlhome))

  (let* (#+quicklisp
         (ql:*quicklisp-home* qlhome)
         #+quicklisp
         (ql:*local-project-directories* (list (merge-pathnames #P"local-projects/" qlhome)))
         (asdf:*central-registry* (list (asdf:system-source-directory system)))
         (asdf::*source-registry* (make-hash-table :test 'equal))
         (asdf::*default-source-registries*
          '(asdf::environment-source-registry
            asdf::system-source-registry
            asdf::system-source-registry-directory))
         (original-defined-systems asdf::*defined-systems*)
         (asdf::*defined-systems* (make-hash-table :test 'equal)))

    ;; Set systems already loaded to prevent reloading the same library in the local Quicklisp.
    (maphash (lambda (name system)
               (let ((system-path (asdf:system-source-directory (cdr system))))
                 (when (or (null system-path)
                           (pathname-in-directory-p system-path qlhome)
                           (typep (cdr system) 'asdf:require-system))
                   (setf (gethash name asdf::*defined-systems*) system))))
             original-defined-systems)

    #-quicklisp
    (load (merge-pathnames #P"quicklisp/setup.lisp" qlhome))
    #+quicklisp
    (push (merge-pathnames #P"quicklisp/" qlhome) asdf:*central-registry*)

    (asdf:initialize-source-registry)

    (multiple-value-prog1 (funcall fn)
      ;; Make all systems that were actually loaded from the local quicklisp
      ;; visible through ASDF outside of the local environment.
      (merge-hash-tables asdf::*defined-systems* original-defined-systems))))

(defmacro with-local-quicklisp (system &body body)
  (let ((qlot-dir (gensym "QLOT-DIR"))
        (system-dir (gensym "SYSTEM-DIR"))
        (register-directory (gensym "REGISTER-DIRECTORY")))
    `(let ((,qlot-dir (asdf:system-source-directory :qlot))
           (,system-dir (asdf:system-source-directory ,system)))
       (flet ((,register-directory (directory)
                (map nil
                     (lambda (asd)
                       (setf (gethash (pathname-name asd) asdf::*source-registry*) asd))
                     (asdf::directory-asd-files directory))))
         (call-in-local-quicklisp
          (lambda ()
            (,register-directory ,system-dir)
            (,register-directory ,qlot-dir)
            ,@body)
          ,system
          (asdf:system-relative-pathname ,system #P"quicklisp/"))))))

(defun sbcl-contrib-p (name)
  (let ((name (princ-to-string name)))
    (and (<= 3 (length name))
         (string-equal name "sb-" :end1 3))))

(defun all-required-systems (systems)
  (let ((systems (if (listp systems) systems (list systems))))
    (with-package-functions :ql (required-systems find-system)
      (labels ((main (system-name)
                 (unless (or (string-equal system-name "asdf")
                             (sbcl-contrib-p system-name))
                   (let* ((system (find-system system-name))
                          (req (and system (required-systems system)))
                          (req (remove :asdf req :test #'string-equal)))
                     (if req
                         (append req (mapcan #'main req))
                         ())))))
        (delete-duplicates (mapcan #'main systems) :test #'string-equal)))))

(defun ensure-installed-in-local-quicklisp (system qlhome)
  (let ((dependencies
          (remove-if (lambda (component-name)
                       (or (null component-name)
                           (equal component-name "asdf")
                           (sbcl-contrib-p component-name)))
                     (mapcar (lambda (dependency-def)
                               (if (consp dependency-def)
                                   (and (eq (first dependency-def) :version)
                                        (second dependency-def))
                                   dependency-def))
                             (asdf::component-sideway-dependencies system)))))
    (with-package-functions :ql-dist (find-system required-systems name ensure-installed)
      (call-in-local-quicklisp
       (lambda ()
         (labels ((system-dependencies (system-name)
                    (let ((system (find-system (string-downcase system-name))))
                      (when system
                        (cons system
                              (mapcan #'system-dependencies (copy-list (required-systems system))))))))
           (map nil #'ensure-installed
                (delete-duplicates (mapcan #'system-dependencies dependencies)
                                   :key #'name
                                   :test #'string=))))
       system
       qlhome))))

(defun generate-random-string ()
  (format nil "~36R" (random (expt 36 #-gcl 8 #+gcl 5))))

(defmacro with-in-directory (dir &body body)
  (let ((cwd (gensym "CWD")))
    `(let ((,cwd (uiop:getcwd)))
       (uiop:chdir ,dir)
       (unwind-protect
            (progn ,@body)
         (uiop:chdir ,cwd)))))
