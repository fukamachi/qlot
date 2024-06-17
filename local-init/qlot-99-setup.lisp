(defpackage #:qlot/local-init/setup
  (:use #:cl))
(in-package #:qlot/local-init/setup)

(defvar *project-root*
  (uiop:pathname-parent-directory-pathname ql:*quicklisp-home*))

(pushnew :qlot.project *features*)

(defun setup-source-registry ()
  #+ros.init (setf roswell:*local-project-directories* nil)
  (let* ((source-registry (ql-setup:qmerge "source-registry.conf"))
         (local-source-registry-form
           (and (uiop:file-exists-p source-registry)
                (uiop:read-file-form source-registry))))
    (asdf:initialize-source-registry
     (or local-source-registry-form
         (let* ((config-file (ql-setup:qmerge "qlot.conf"))
                (qlot-source-directory
                  (and (uiop:file-exists-p config-file)
                       (getf (uiop:read-file-form config-file) :qlot-source-directory))))
           `(:source-registry :ignore-inherited-configuration
             (:also-exclude ".qlot")
             (:also-exclude ".bundle-libs")
             ,@(and qlot-source-directory
                    `((:directory ,qlot-source-directory)))))))))

(defvar *project-system-cache*
  (make-hash-table :test 'equal))

(defstruct system-cache
  system-files
  (created-at (get-universal-time)))

(defun cache-stale-p (dir cache)
  (< (system-cache-created-at cache)
     (ql-impl-util:directory-write-date dir)))

(defun cache-key (dir)
  (uiop:native-namestring (uiop:ensure-absolute-pathname dir)))

(defun find-cache (directory)
  (let* ((key (cache-key directory))
         (cache (gethash key *project-system-cache*)))
    (and cache
         (not (cache-stale-p directory cache))
         (system-cache-system-files cache))))

(defun put-cache (directory system-files)
  (setf (gethash (cache-key directory) *project-system-cache*)
        (make-system-cache :system-files system-files)))

(defun project-system-searcher (system-name)
  (when (and (not (asdf::registered-system system-name))
             (equal (asdf:primary-system-name system-name) system-name))
    (block nil
      (uiop:collect-sub*directories
       *project-root*
       (lambda (dir)
         (let ((dirname (car (last (pathname-directory dir)))))
           (and (stringp dirname)
                (not (equal dirname ""))
                (not (char= (aref dirname 0) #\.))
                (not (find dirname asdf/source-registry:*default-source-registry-exclusions*
                           :test 'equal)))))
       t
       (lambda (dir)
         (let* ((system-files
                  (or (find-cache dir)
                      (let ((asd-files
                              (uiop:directory-files dir "*.asd")))
                        (put-cache dir asd-files)
                        asd-files)))
                (system-file
                  (find system-name system-files
                        :key #'pathname-name
                        :test 'equal)))
           (when system-file
             (return system-file))))))))

(defun add-system-definition-search-function ()
  (setf asdf:*system-definition-search-functions*
        (append asdf:*system-definition-search-functions*
                (list 'project-system-searcher))))

(setup-source-registry)
(add-system-definition-search-function)
