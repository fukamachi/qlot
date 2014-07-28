(in-package :cl-user)
(defpackage qlot
  (:use :cl)
  (:import-from :qlot.asdf
                :system-quicklisp-home)
  (:import-from :qlot.util
                :with-package-functions
                :pathname-in-directory-p)
  (:export :install
           :install-quicklisp
           :quickload))
(in-package :qlot)

(defun install (&rest args)
  #+quicklisp (ql:quickload :qlot-install)
  #-quicklisp (asdf:load-system :qlot-install)
  (with-package-functions :qlot.install (install-project)
    (if (evenp (length args))
        (apply #'install-project *default-pathname-defaults* args)
        (apply #'install-project args))))

(defun install-quicklisp (&optional (path nil path-specified-p))
  (declare (ignore path))
  #+quicklisp (ql:quickload :qlot-install)
  #-quicklisp (asdf:load-system :qlot-install)
  (with-package-functions :qlot.install (install-quicklisp)
    (apply #'install-quicklisp (if path-specified-p
                                   (list path)
                                   nil))))

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
                     (if (pathname-in-directory-p
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
