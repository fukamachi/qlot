(defpackage #:qlot/local-init/qlot-setup
  (:use #:cl))
(in-package #:qlot/local-init/qlot-setup)

(defun setup-source-registry ()
  #+ros.init (setf roswell:*local-project-directories* nil)
  (let* ((source-registry (ql-setup:qmerge "source-registry.conf"))
         (local-source-registry-form
           (and (uiop:file-exists-p source-registry)
                (uiop:read-file-form source-registry)))
         (project-root
           (uiop:pathname-parent-directory-pathname ql:*quicklisp-home*)))
    (asdf:initialize-source-registry
     (if local-source-registry-form
         (append local-source-registry-form
                 `((:tree ,project-root)))
         `(:source-registry :ignore-inherited-configuration
           (:tree ,project-root)
           (:also-exclude ".qlot"))))))

(setup-source-registry)
