(defpackage #:qlot/init
  (:use #:cl)
  (:import-from #:qlot/utils/project
                #:*default-qlfile*)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:export #:init-project))
(in-package #:qlot/init)

(defun init-project (object)
  (etypecase object
    ((or symbol string)
     (init-project (asdf:find-system object)))
    (asdf:system
     (init-project
      (asdf:component-pathname object)))
    (pathname
     (unless (uiop:directory-exists-p object)
       (error 'qlot-simple-error
              :format-control "Directory does not exist: ~A"
              :format-arguments (list object)))
     (let ((qlfile (merge-pathnames *default-qlfile* object)))
       ;; Create 'qlfile'
       (unless (uiop:file-exists-p (merge-pathnames *default-qlfile* object))
         (message "Creating ~A" qlfile)
         (with-open-file (out qlfile :if-does-not-exist :create)
           (declare (ignorable out))))
       ;; Add .qlot/ to .gitignore (if .git/ directory exists)
       (let ((git-dir (merge-pathnames #P".git/" object)))
         (when (uiop:directory-exists-p git-dir)
           (let* ((gitignore (merge-pathnames #P".gitignore" object))
                  (ignore-entries
                    (when (uiop:file-exists-p gitignore)
                      (uiop:read-file-lines gitignore))))
             (unless (member ".qlot/" ignore-entries :test 'equal)
               (message "Adding .qlot/ to .gitignore")
               (with-open-file (out (merge-pathnames ".gitignore" object)
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :append)
                 (format out "~&.qlot/~%"))))))
       qlfile))))
