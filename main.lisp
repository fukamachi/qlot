(defpackage #:qlot
  (:nicknames #:qlot/main)
  (:use #:cl)
  (:import-from #:qlot/install
                #:install-project
                #:update-project
                #:init-project
                #:*qlot-directory*
                #:*default-qlfile*)
  (:import-from #:qlot/bundle
                #:bundle-project)
  (:import-from #:qlot/logger
                #:*debug*
                #:*logger-message-stream*
                #:*logger-debug-stream*)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils
                #:pathname-in-directory-p
                #:merge-hash-tables)
  (:export #:init
           #:install
           #:update
           #:with-local-quicklisp
           #:quickload
           #:bundle
           #:*proxy*
           #:*debug*
           #:*logger-message-stream*
           #:*logger-debug-stream*))
(in-package #:qlot)

(defun init (object)
  (init-project object))

(defun install (object)
  (install-project object))

(defun update (object)
  (update-project object))

(defun call-in-local-quicklisp (fn qlhome &key (central-registry '()))
  (unless (uiop:directory-exists-p qlhome)
    (error "Directory ~S does not exist." qlhome))

  (unless (uiop:file-exists-p (merge-pathnames #P"setup.lisp" qlhome))
    (error "~S is not a quicklisp directory." qlhome))

  (unless (find :ql *features*)
    (load (merge-pathnames #P"setup.lisp" qlhome)))

  (progv (list (intern (string '#:*quicklisp-home*) '#:ql)
               (intern (string '#:*local-project-directories*) '#:ql))
      (list qlhome
            (list (merge-pathnames #P"local-projects/" qlhome)))

    (let* ((asdf:*central-registry* central-registry)
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

      (asdf:initialize-source-registry)

      (multiple-value-prog1 (funcall fn)
        ;; Make all systems that were actually loaded from the local quicklisp
        ;; visible through ASDF outside of the local environment.
        (merge-hash-tables #+asdf3.3 asdf::*registered-systems*
                           #-asdf3.3 asdf::*defined-systems* original-defined-systems)))))

(defun object-to-qlhome (object)
  (etypecase object
    ((or keyword string asdf:system)
     (asdf:system-relative-pathname object *qlot-directory*))
    (pathname
      (merge-pathnames *qlot-directory* (uiop:pathname-directory-pathname object)))))

(defmacro with-local-quicklisp ((object &key central-registry) &body body)
  (let ((g-object (gensym "OBJECT"))
        (g-qlhome (gensym "QLHOME"))
        (asd-specified-p (gensym "ASD-SPECIFIED-P")))
    `(let* ((,g-object ,object)
            (,g-qlhome (object-to-qlhome ,g-object))
            (,asd-specified-p (and (pathnamep ,g-object)
                                   (uiop:file-pathname-p ,g-object)
                                   (equal (pathname-type ,g-object) "asd"))))
       (call-in-local-quicklisp
         (lambda ()
           (when ,asd-specified-p
             (asdf:load-asd ,g-object))
           ,@body)
         ,g-qlhome
         :central-registry (append ,central-registry
                                   (list (asdf:system-source-directory :qlot)))))))

(defun quickload (systems &rest args &key verbose prompt explain &allow-other-keys)
  (declare (ignore verbose prompt explain))
  (let ((systems (if (consp systems)
                     systems
                     (list systems))))
    (dolist (system systems systems)
      (with-local-quicklisp (system)
        (apply #'uiop:symbol-call '#:ql '#:quickload system args)))))

(defun bundle (object)
  (bundle-project object))
