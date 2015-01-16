(in-package :cl-user)
(defpackage qlot
  (:use :cl)
  (:import-from :qlot.asdf
                :system-quicklisp-home)
  (:import-from :qlot.util
                :with-package-functions
                :pathname-in-directory-p
                :with-local-quicklisp)
  (:export :install
           :update
           :install-quicklisp
           :quickload
           :with-local-quicklisp))
(in-package :qlot)

(defun install (&rest args)
  "Install Quicklisp and libraries that declared in qlfile project-locally.
qlfile.lock will be used with precedence if it exists."
  (unless (find-package :qlot.install)
    #+quicklisp (ql:quickload :qlot-install :silent t)
    #-quicklisp (asdf:load-system :qlot-install))
  (with-package-functions :qlot.install (install-project)
    (if (evenp (length args))
        (apply #'install-project *default-pathname-defaults* args)
        (apply #'install-project args))))

(defun update (&rest args)
  "Update the project-local 'quicklisp/' directory using qlfile."
  (unless (find-package :qlot.install)
    #+quicklisp (ql:quickload :qlot-install :silent t)
    #-quicklisp (asdf:load-system :qlot-install))
  (with-package-functions :qlot.install (update-project)
    (if (evenp (length args))
        (apply #'update-project *default-pathname-defaults* args)
        (apply #'update-project args))))

(defun install-quicklisp (&optional (path nil path-specified-p))
  "Install Quicklisp in the given PATH.
If PATH isn't specified, this installs it to './quicklisp/'."
  (unless (find-package :qlot.install)
    #+quicklisp (ql:quickload :qlot-install :silent t)
    #-quicklisp (asdf:load-system :qlot-install))
  (with-package-functions :qlot.install (install-quicklisp)
    (apply #'install-quicklisp (if path-specified-p
                                   (list path)
                                   nil))))

(defun quickload (systems &rest args &key verbose prompt explain &allow-other-keys)
  "Load SYSTEMS in the each project-local `quicklisp/`."
  (declare (ignore verbose prompt explain))
  (unless (consp systems)
    (setf systems (list systems)))
  (with-package-functions :ql (quickload)
    (loop for system-name in systems
          do (with-local-quicklisp system-name
               (apply #'quickload system-name args))))
  systems)
