(defpackage #:qlot/utils/project
  (:use #:cl)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:with-autoload-on-missing
                #:directory-lisp-files
                #:lisp-file-dependencies)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/logger
                #:*debug*
                #:message
                #:debug-log)
  (:export #:*qlot-directory*
           #:local-quicklisp-installed-p
           #:local-quicklisp-local-init-installed-p
           #:local-quicklisp-home
           #:project-dependencies))
(in-package #:qlot/utils/project)

(defvar *qlot-directory* #P".qlot/")
(defvar *default-qlfile* #P"qlfile")

(defun local-quicklisp-installed-p (project-root)
  (let ((qlhome (merge-pathnames *qlot-directory* project-root)))
    (when (and (uiop:directory-exists-p qlhome)
               (uiop:file-exists-p (merge-pathnames "setup.lisp" qlhome)))
      qlhome)))

(defun local-quicklisp-local-init-installed-p (project-root)
  (let ((local-init-dir
          (merge-pathnames #P"local-init/" (merge-pathnames *qlot-directory* project-root))))
    (when (uiop:directory-exists-p local-init-dir)
      local-init-dir)))

(defun local-quicklisp-home (project-root)
  (let ((project-root (uiop:ensure-directory-pathname project-root)))
    (uiop:ensure-absolute-pathname
      (merge-pathnames *qlot-directory* project-root)
      *default-pathname-defaults*)))

(defun project-dependencies (project-root)
  (with-package-functions #:ql-dist (find-system name)
    (let ((all-dependencies '())
          (loaded-asd-files '())
          (project-system-names '()))
      (with-directory (system-file system-name dependencies) project-root
        (pushnew system-name project-system-names :test 'equal)
        (unless (find system-file loaded-asd-files :test 'equal)
          (push system-file loaded-asd-files)
          (message "Loading '~A'..." system-file)
          (let ((errout *error-output*))
            (handler-bind ((error
                             (lambda (e)
                               (uiop:print-condition-backtrace e :stream errout))))
              (let ((*standard-output* (make-broadcast-stream))
                    (*trace-output* (make-broadcast-stream))
                    (*error-output* (if *debug*
                                        *error-output*
                                        (make-broadcast-stream))))
                (with-autoload-on-missing
                  (asdf:load-asd system-file))))))
        (when (typep (asdf:find-system system-name) 'asdf:package-inferred-system)
          (let ((pis-dependencies
                  (loop for file in (directory-lisp-files (uiop:pathname-directory-pathname system-file))
                        append (lisp-file-dependencies file))))
            (setf dependencies
                  (delete-duplicates
                    (nconc dependencies pis-dependencies)
                    :test 'equal))))
        (let ((dependencies (remove-if-not #'find-system dependencies)))
          (debug-log "'~A' requires ~S" system-name dependencies)
          (setf all-dependencies
                (nconc all-dependencies
                       (remove-if-not #'find-system dependencies)))))
      (with-package-functions #:ql-dist (required-systems name)
        (let ((already-seen (make-hash-table :test 'equal)))
          (labels ((find-system-with-fallback (system-name)
                     (or (find-system system-name)
                         (find-system (asdf:primary-system-name system-name))))
                   (system-dependencies (system-name)
                     (unless (gethash system-name already-seen)
                       (setf (gethash system-name already-seen) t)
                       (let ((system (find-system-with-fallback system-name)))
                         (when system
                           (cons system
                                 (mapcan #'system-dependencies (copy-seq (required-systems system)))))))))
            (setf all-dependencies
                  (delete-duplicates
                    (loop for dependency in all-dependencies
                          append (system-dependencies dependency))
                    :key #'name
                    :test 'string=)))))

      (remove-if (lambda (dep)
                   (find (name dep) project-system-names :test 'equal))
                 all-dependencies))))
