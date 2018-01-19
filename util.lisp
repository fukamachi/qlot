(defpackage #:qlot/util
  (:use #:cl)
  (:export #:*system-quicklisp-home*
           #:with-quicklisp-home
           #:with-package-functions
           #:pathname-in-directory-p
           #:find-qlfile
           #:call-in-local-quicklisp
           #:with-local-quicklisp
           #:ensure-installed-in-local-quicklisp
           #:all-required-systems
           #:generate-random-string
           #:with-in-directory
           #:project-systems
           #:sbcl-contrib-p))
(in-package #:qlot/util)

(defvar *system-quicklisp-home*
  #+quicklisp ql:*quicklisp-home*)

(defmacro with-quicklisp-home (qlhome &body body)
  `(progv (list (intern #.(string :*quicklisp-home*) :ql))
       (list ,qlhome)
     ,@body))

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

(defun call-in-local-quicklisp (fn qlhome &key systems (central-registry '()))
  (unless #+clisp (ext:probe-directory qlhome)
          #-clisp (probe-file qlhome)
    (error "Directory ~S does not exist." qlhome))

  (unless (probe-file (merge-pathnames #P"setup.lisp" qlhome))
    (if (probe-file (merge-pathnames #P"quicklisp/setup.lisp" qlhome))
        ;; The given `qlhome' is the project root.
        (setf qlhome (merge-pathnames #P"quicklisp/" qlhome))
        (error "~S is not a quicklisp directory." qlhome)))

  (let* (#+quicklisp
         (ql:*quicklisp-home* qlhome)
         #+quicklisp
         (ql:*local-project-directories* (list (merge-pathnames #P"local-projects/" qlhome)))
         (asdf:*central-registry* central-registry)
         (asdf::*source-registry* (make-hash-table :test 'equal))
         (asdf::*default-source-registries*
          '(asdf::environment-source-registry
            asdf::system-source-registry
            asdf::system-source-registry-directory))
         (original-defined-systems #+asdf3.3 asdf::*registered-systems*
                                   #-asdf3.3 asdf::*defined-systems*)
         (#+asdf3.3 asdf::*registered-systems*
          #-asdf3.3 asdf::*defined-systems* (make-hash-table :test 'equal)))

    ;; Set systems already loaded to prevent reloading the same library in the local Quicklisp.
    (maphash (lambda (name system)
               (let* ((system-object #+asdf3.3 system #-asdf3.3 (cdr system))
                      (system-path (asdf:system-source-directory system-object)))
                 (when (or (null system-path)
                           (pathname-in-directory-p system-path qlhome)
                           (typep system-object 'asdf:require-system))
                   (setf (gethash name #+asdf3.3 asdf::*registered-systems*
                                       #-asdf3.3 asdf::*defined-systems*) system))))
             original-defined-systems)

    #-quicklisp
    (load (merge-pathnames #P"quicklisp/setup.lisp" qlhome))
    #+quicklisp
    (push (merge-pathnames #P"quicklisp/" qlhome) asdf:*central-registry*)

    (asdf:initialize-source-registry)

    (dolist (system systems)
      (setf (gethash (pathname-name system) asdf::*source-registry*) system))

    (multiple-value-prog1 (funcall fn)
      ;; Make all systems that were actually loaded from the local quicklisp
      ;; visible through ASDF outside of the local environment.
      (merge-hash-tables #+asdf3.3 asdf::*registered-systems*
                         #-asdf3.3 asdf::*defined-systems* original-defined-systems))))

(defmacro with-local-quicklisp ((qlhome &key systems central-registry) &body body)
  (let ((g-qlhome (gensym "QLHOME"))
        (g-systems (gensym "SYSTEMS"))
        (g-system-file (gensym "SYSTEM-FILE")))
    `(let* ((,g-qlhome ,qlhome)
            (,g-systems ,systems)
            (,g-system-file (and (keywordp ,g-qlhome)
                                 (asdf:system-source-file ,g-qlhome))))
       (when ,g-system-file
         (push ,g-system-file ,g-systems)
         (setf ,g-qlhome
               (make-pathname :name nil
                              :type nil
                              :defaults ,g-system-file)))
       (call-in-local-quicklisp
        (lambda () ,@body)
        ,g-qlhome
        :systems ,g-systems
        :central-registry (append ,central-registry
                                  (list (asdf:system-source-directory :qlot)))))))

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

(defun generate-random-string ()
  (format nil "~36R" (random (expt 36 #-gcl 8 #+gcl 5))))

(defmacro with-in-directory (dir &body body)
  (let ((cwd (gensym "CWD")))
    `(let ((,cwd (uiop:getcwd)))
       (uiop:chdir ,dir)
       (unwind-protect
            (progn ,@body)
         (uiop:chdir ,cwd)))))

(defun project-systems (project-dir)
  (let ((qlhome (merge-pathnames #P"quicklisp/" project-dir))
        systems)
    (asdf::collect-sub*directories-asd-files
     project-dir
     :collect (lambda (asd)
                (unless (or (pathname-in-directory-p asd qlhome)
                            ;; KLUDGE: Ignore skeleton.asd of CL-Project
                            (search "skeleton" (pathname-name asd)))
                  (push asd systems)))
     :exclude (append (list "bundle-libs" "quicklisp")
                      asdf::*default-source-registry-exclusions*))
    systems))
