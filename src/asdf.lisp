(in-package :cl-user)
(defpackage qlot.asdf
  (:nicknames :qlot-asdf)
  (:use :cl)
  (:import-from :qlot.util
                :with-package-functions)
  (:export :qlot-system
           :system-quicklisp-home
           :quickload))
(in-package :qlot.asdf)

(defclass qlot-system (asdf:system)
  ((quicklisp-home :initarg :quicklisp-home
                   :initform #P"quicklisp/")
   (qlhome-initialized :initform nil)))

(defgeneric system-quicklisp-home (system)
  (:method ((system asdf:system))
    (asdf:system-relative-pathname system #P"quicklisp/"))
  (:method ((system qlot-system))
    (asdf:system-relative-pathname system (slot-value system 'quicklisp-home))))

(defun pathname-in-directory (path directory)
  (loop for dir1 in (pathname-directory directory)
        for dir2 in (pathname-directory path)
        unless (string= dir1 dir2)
          do (return nil)
        finally
           (return t)))

(defun load-system-with-local-quicklisp (system qlhome)
  (unless (probe-file qlhome)
    (error "Directory ~S does not exist." qlhome))

  (let (#+quicklisp
        (ql:*quicklisp-home* qlhome)
        (global-source-registry asdf::*source-registry*)
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

    (labels ((component-already-loaded-p (component)
               (let ((asdf::*source-registry* global-source-registry))
                 (and (find (string-downcase component)
                            (asdf:registered-systems) :test #'string=)
                      (asdf:component-loaded-p component))))
             (load-system (system-name)
               (with-package-functions :ql-dist (ensure-installed find-system name dependency-tree)
                 (let ((system (find-system system-name)))
                   (unless system
                     (error "~S is not found in ~S." system-name qlhome))
                   (when (component-already-loaded-p system-name)
                     (if (pathname-in-directory
                          (let ((asdf::*source-registry* global-source-registry))
                            (asdf:system-source-directory system-name))
                          qlhome)
                         (return-from load-system)
                         (warn "Other version of ~S is already loaded." system-name)))

                   (asdf:clear-system system-name)
                   (ensure-installed system)
                   (asdf:find-system system-name)

                   (loop for dep in (asdf:component-sideway-dependencies (asdf:find-system system-name))
                         do (load-system (string-downcase dep)))))))
      (loop for dep in (asdf:component-sideway-dependencies system)
            do (load-system (string-downcase dep))))

    (with-package-functions :ql (quickload)
      (quickload (asdf:component-name system)))))

(defun quickload (systems)
  (unless (consp systems)
    (setf systems (list systems)))
  (loop for system-name in systems
        for system = (asdf:find-system (string-downcase system-name))
        do (load-system-with-local-quicklisp
            system
            (system-quicklisp-home system)))
  systems)

(import '(qlot-system) :asdf)
