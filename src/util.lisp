(in-package :cl-user)
(defpackage qlot.util
  (:use :cl)
  (:import-from :qlot.asdf
                :system-quicklisp-home)
  (:export :with-quicklisp-home
           :with-package-functions
           :pathname-in-directory-p
           :find-qlfile
           :call-in-local-quicklisp
           :with-local-quicklisp
           :ensure-installed-in-local-quicklisp))
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
  (loop for dir1 in (pathname-directory directory)
        for dir2 in (pathname-directory path)
        unless (string= dir1 dir2)
          do (return nil)
        finally
           (return t)))

(defun find-qlfile (directory &key (errorp t) use-lock)
  (check-type directory pathname)
  (unless (probe-file directory)
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

(defun call-in-local-quicklisp (fn system qlhome)
  (unless (probe-file qlhome)
    (error "Directory ~S does not exist." qlhome))

  (let (#+quicklisp
        (ql:*quicklisp-home* qlhome)
        (asdf::*source-registry* (make-hash-table :test 'equal))
        (asdf::*default-source-registries*
          '(asdf::environment-source-registry
            asdf::system-source-registry
            asdf::system-source-registry-directory))
        (asdf:*central-registry* (list (asdf:system-source-directory system))))
    #-quicklisp
    (load (merge-pathnames #P"setup.lisp" qlhome))
    #+quicklisp
    (push (merge-pathnames #P"quicklisp/" qlhome) asdf:*central-registry*)

    (asdf:initialize-source-registry)

    (funcall fn)))

(defmacro with-local-quicklisp (system &body body)
  `(call-in-local-quicklisp
    (lambda () ,@body)
    ,system
    (system-quicklisp-home (asdf:find-system ,system))))

(defun register-system-with-local-quicklisp (system qlhome &key (if-does-not-exist :error))
  (let ((global-source-registry asdf::*source-registry*))
    (labels ((component-already-loaded-p (component)
               (let ((asdf::*source-registry* global-source-registry))
                 (and (find (string-downcase component)
                            (asdf:registered-systems) :test #'string=)
                      (asdf::component-loaded-p component))))
             (register-system (system-name &key (if-does-not-exist if-does-not-exist))
               (with-package-functions :ql-dist (ensure-installed find-system name dependency-tree)
                 (let ((system (find-system system-name)))
                   (unless system
                     (ecase if-does-not-exist
                       (:error (error "~S is not found in ~S." system-name qlhome))
                       (:ignore (return-from register-system))))
                   (when (component-already-loaded-p system-name)
                     (if (pathname-in-directory-p
                          (let ((asdf::*source-registry* global-source-registry))
                            (asdf:system-source-directory system-name))
                          qlhome)
                         (return-from register-system)
                         (warn "Other version of ~S is already loaded." system-name)))

                   (asdf:clear-system system-name)
                   (ensure-installed system)
                   (asdf:find-system system-name)

                   (loop for dep in (asdf::component-sideway-dependencies (asdf:find-system system-name))
                         do (register-system (string-downcase dep)))))))
      (call-in-local-quicklisp
       (lambda ()
         (register-system (asdf:component-name system) :if-does-not-exist :ignore)
         (loop for dep in (asdf::component-sideway-dependencies system)
               do (register-system (string-downcase dep))))
       system
       qlhome))))

(defun load-system-with-local-quicklisp (system qlhome &key quickload-args (if-does-not-exist :error))
  (register-system-with-local-quicklisp system qlhome :if-does-not-exist if-does-not-exist)
  (call-in-local-quicklisp
   (lambda ()
     (with-package-functions :ql (quickload)
       (apply #'quickload (asdf:component-name system) quickload-args)))
   system
   qlhome))

(defun ensure-installed-in-local-quicklisp (system qlhome)
  (with-package-functions :ql-dist (find-system required-systems name ensure-installed)
    (call-in-local-quicklisp
     (lambda ()
       (labels ((system-dependencies (system-name)
                  (let ((system (find-system (string-downcase system-name))))
                    (when system
                      (cons system
                            (mapcan #'system-dependencies (copy-list (required-systems system))))))))
         (map nil #'ensure-installed
              (delete-duplicates (mapcan #'system-dependencies
                                         (copy-list (asdf::component-sideway-dependencies system)))
                                 :key #'name
                                 :test #'string=))))
     system
     qlhome)))
