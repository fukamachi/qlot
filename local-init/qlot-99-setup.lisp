(defpackage #:qlot/local-init/setup
  (:use #:cl))
(in-package #:qlot/local-init/setup)

(defun setup-source-registry ()
  #+ros.init (setf roswell:*local-project-directories* nil)
  (let* ((source-registry (ql-setup:qmerge "source-registry.conf"))
         (local-source-registry-form
           (and (uiop:file-exists-p source-registry)
                (uiop:read-file-form source-registry)))
         (project-root
           (uiop:pathname-parent-directory-pathname ql:*quicklisp-home*))
         (config-file (ql-setup:qmerge "config.lisp"))
         (qlot-home
           (and (uiop:file-exists-p config-file)
                (getf (uiop:read-file-form config-file) :qlot-home))))
    (asdf:initialize-source-registry
     (if local-source-registry-form
         (append local-source-registry-form
                 `((:tree ,project-root)))
         `(:source-registry :ignore-inherited-configuration
           (:also-exclude ".qlot")
           (:directory ,qlot-home)
           (:tree ,project-root))))))

(setup-source-registry)
