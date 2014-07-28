(in-package :cl-user)
(defpackage qlot.asdf
  (:nicknames :qlot-asdf)
  (:use :cl)
  (:import-from :qlot.util
                :with-package-functions)
  (:export :qlot-system
           :system-quicklisp-home))
(in-package :qlot.asdf)

(defclass qlot-system (asdf:system)
  ((quicklisp-home :initarg :quicklisp-home
                   :initform #P"quicklisp/"
                   :accessor system-quicklisp-home)
   (qlhome-initialized :initform nil)))

(defmethod asdf:component-depends-on :before (op (system qlot-system))
  (unless (slot-value system 'qlhome-initialized)
    (let ((qlhome (asdf:system-relative-pathname system (slot-value system 'quicklisp-home))))
      (unless (probe-file qlhome)
        (error "Directory \"quicklisp/\" does not exist in ~A.
Try (qlot:install :~A) to install Quicklisp in the project root."
               (asdf:system-source-directory system)
               (asdf:component-name system)))

      #+quicklisp
      (setf ql:*quicklisp-home* qlhome)

      (load (merge-pathnames #P"setup.lisp" qlhome))

      (setf asdf::*default-source-registries*
            '(asdf::environment-source-registry
              asdf::system-source-registry
              asdf::system-source-registry-directory))
      (asdf:initialize-source-registry)

      (map nil
           (lambda (dep)
             (asdf:clear-system dep)
             (with-package-functions :ql-dist (ensure-installed find-release)
               (let ((release (find-release (string-downcase dep))))
                 (when release
                   (ensure-installed release)))))
           (asdf:component-sideway-dependencies system)))
    (setf (slot-value system 'qlhome-initialized) t)))

(import '(qlot-system) :asdf)
