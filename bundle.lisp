(defpackage #:qlot/bundle
  (:use #:cl)
  (:import-from #:qlot/utils/project
                #:project-dependencies
                #:local-quicklisp-installed-p
                #:local-quicklisp-home)
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

(defun %bundle-project (project-root)
  (assert (uiop:absolute-pathname-p project-root))

  (unless (local-quicklisp-installed-p project-root)
    (error 'qlot-simple-error
           :format-control "Local Quicklisp is not installed yet."))

  (let ((quicklisp-home (local-quicklisp-home project-root)))
    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" quicklisp-home)))

    (with-quicklisp-home quicklisp-home
      (let* ((dependencies (project-dependencies project-root))
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
          (message "Successfully bundled at '~A'." bundle-directory))))))

(defun bundle-project (object)
  (etypecase object
    ((or symbol string)
     (bundle-project (asdf:find-system object)))
    (asdf:system
      (%bundle-project (asdf:system-source-directory object)))
    (pathname
      (%bundle-project object))))
