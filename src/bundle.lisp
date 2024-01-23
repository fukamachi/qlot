(defpackage #:qlot/bundle
  (:use #:cl)
  (:import-from #:qlot/utils/project
                #:check-local-quicklisp
                #:find-project-root
                #:local-quicklisp-home)
  (:import-from #:qlot/utils/dependencies
                #:project-dependencies-in-child-process)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:export #:bundle-project))
(in-package #:qlot/bundle)

(defvar *bundle-directory* #P".bundle-libs/")

(defun %bundle-project (directory &key exclude)
  (let ((project-root (find-project-root directory)))

    (check-local-quicklisp project-root)

    (let ((quicklisp-home (local-quicklisp-home project-root)))
      (unless (find-package '#:ql)
        (load (merge-pathnames #P"setup.lisp" quicklisp-home)))

      (with-quicklisp-home quicklisp-home
        (let* ((dependencies (with-package-functions #:ql-dist (find-system)
                               (mapcar #'find-system
                                       (project-dependencies-in-child-process project-root quicklisp-home
                                                                              :exclude exclude))))
               (dep-releases (with-package-functions #:ql-dist (release name)
                               (delete-duplicates
                                (mapcar #'release dependencies)
                                :key #'name
                                :test 'equal)))
               (bundle-directory (merge-pathnames *bundle-directory* project-root)))
          (when dep-releases
            (message "Bundling ~D ~:*dependenc~[ies~;y~:;ies~]..."
                     (length dep-releases))
            (with-package-functions #:ql-dist (name)
              (let ((dependency-names (mapcar #'name dependencies)))
                (with-package-functions #:ql (bundle-systems)
                  (bundle-systems dependency-names
                                  :to bundle-directory))))
            (message "Successfully bundled at '~A'." bundle-directory)))))))

(defun bundle-project (object &key exclude)
  (etypecase object
    ((or symbol string)
     (bundle-project (asdf:find-system object) :exclude exclude))
    (asdf:system
     (%bundle-project (asdf:system-source-directory object) :exclude exclude))
    (pathname
     (%bundle-project object :exclude exclude))))
