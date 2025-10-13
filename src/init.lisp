(defpackage #:qlot/init
  (:use #:cl)
  (:import-from #:qlot/add
                #:add-project)
  (:import-from #:qlot/utils/project
                #:*default-qlfile*)
  (:import-from #:qlot/utils
                #:starts-with)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:export #:init-project))
(in-package #:qlot/init)

(defun dist-url (dist)
  (check-type dist string)
  (cond
    ((equal dist "ultralisp")
     "https://dist.ultralisp.org/")
    (t
     (unless (or (starts-with "http://" dist)
                 (starts-with "https://" dist))
       (error 'qlot-simple-error
              :format-control "Invalid dist URL: ~A"
              :format-arguments (list dist)))
     dist)))

(defun init-project (object &key dist)
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
     (let* ((qlfile (merge-pathnames *default-qlfile* object))
            (qlfile-exists (uiop:file-exists-p qlfile)))
       ;; Create 'qlfile'
       (unless qlfile-exists
         (message "Creating ~A" qlfile)
         (with-open-file (out qlfile
                              :if-does-not-exist :create
                              :direction :output)))
       (when dist
         (add-project `("dist" ,(dist-url dist)) qlfile))
       ;; Add .qlot/ to .gitignore (if .git/ directory exists)
       (let ((git-dir (merge-pathnames #P".git/" object)))
         (when (uiop:directory-exists-p git-dir)
           (let* ((gitignore (merge-pathnames #P".gitignore" object))
                  (ignore-entries
                    (when (uiop:file-exists-p gitignore)
                      (uiop:read-file-lines gitignore))))
             (unless (member ".qlot/" ignore-entries :test 'equal)
               (message "Adding /.qlot/ to .gitignore")
               (with-open-file (out (merge-pathnames ".gitignore" object)
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :append)
                 (format out "~&/.qlot/~%"))))))
       (values qlfile (not qlfile-exists))))))
