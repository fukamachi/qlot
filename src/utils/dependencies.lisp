(defpackage #:qlot/utils/dependencies
  (:use #:cl)
  (:import-from #:qlot/utils/project
                #:project-dependencies)
  (:import-from #:qlot/utils/shell
                #:run-lisp
                #:*qlot-source-directory*)
  (:import-from #:qlot/logger
                #:*terminal*)
  (:import-from #:qlot/color
                #:*enable-color*)
  (:export #:project-dependencies-in-child-process))
(in-package #:qlot/utils/dependencies)

(defun project-dependencies-in-child-process (project-root quicklisp-home
                                              &key exclude ignore-directories)
  (uiop:with-temporary-file (:pathname tmp)
    (run-lisp `((load ,(merge-pathnames #P"setup.lisp" quicklisp-home))
                (setf *enable-color* ,*enable-color*)
                (setf *terminal* ,*terminal*)
                (with-open-file (cl-user::out ,tmp
                                              :direction :output
                                              :if-exists :supersede)
                  (prin1
                   (delete-duplicates
                    (mapcar
                     (lambda (cl-user::dep)
                       (uiop:symbol-call '#:ql-dist '#:name cl-user::dep))
                     (project-dependencies ,project-root :exclude (list ,@exclude)
                                           :ignore-directories (list ,@ignore-directories)))
                    :test 'equal
                    :from-end t)
                   cl-user::out)))
              :systems '("qlot/utils/project" "qlot/color")
              :source-registry *qlot-source-directory*)
    (uiop:read-file-form tmp)))
