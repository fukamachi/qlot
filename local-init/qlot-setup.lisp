(defpackage #:qlot/local-init/qlot-setup
  (:use #:cl))
(in-package #:qlot/local-init/qlot-setup)

(defun setup-source-registry ()
  #+ros.init (setf roswell:*local-project-directories* nil)
  (let* ((source-registry (ql-setup:qmerge "source-registry.conf"))
         (local-source-registry-form
           (and (uiop:file-exists-p source-registry)
                (uiop:read-file-form source-registry))))
    (asdf:initialize-source-registry
     (if local-source-registry-form
         (append local-source-registry-form
                 `((:tree ,(probe-file (merge-pathnames #P"../" ql:*quicklisp-home*)))))
         `(:source-registry :ignore-inherited-configuration
           (:tree ,(probe-file (merge-pathnames #P"../" ql:*quicklisp-home*)))
           (:also-exclude ".qlot"))))))

(setup-source-registry)
