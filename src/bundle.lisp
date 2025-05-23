(defpackage #:qlot/bundle
  (:use #:cl)
  (:import-from #:qlot/utils/project
                #:check-local-quicklisp
                #:qlot-directory-not-found
                #:local-quicklisp-home)
  (:import-from #:qlot/utils/dependencies
                #:project-dependencies-in-child-process)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/utils/asdf
                #:with-source-registry)
  (:import-from #:qlot/utils
                #:with-package-functions
                #:ensure-package-loaded)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:export #:bundle-project))
(in-package #:qlot/bundle)

(defvar *default-bundle-directory-name* ".bundle-libs")

(defun %bundle-project (project-root &key exclude output)
  (assert (uiop:absolute-pathname-p project-root))

  (handler-case
      (check-local-quicklisp project-root)
    (qlot-directory-not-found ()
      (ensure-package-loaded :qlot/install)
      (uiop:symbol-call '#:qlot/install '#:install-project
                        project-root
                        :install-deps nil)))

  (let ((quicklisp-home (local-quicklisp-home project-root)))
    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" quicklisp-home)))

    (with-quicklisp-home quicklisp-home
      (let* ((bundle-directory (uiop:ensure-directory-pathname
                                 (merge-pathnames (or output *default-bundle-directory-name*)
                                                  project-root)))
             (dependencies (with-package-functions #:ql-dist (find-system)
                             (mapcar #'find-system
                                     (project-dependencies-in-child-process project-root quicklisp-home
                                                                            :exclude exclude
                                                                            :ignore-directories
                                                                            (list bundle-directory)))))
             (dep-releases (with-package-functions #:ql-dist (release name)
                             (delete-duplicates
                              (mapcar #'release dependencies)
                              :key #'name
                              :test 'equal))))
        (ensure-directories-exist bundle-directory)
        (when dep-releases
          (message "Bundling ~D ~:*dependenc~[ies~;y~:;ies~]..."
                   (length dep-releases))
          (with-package-functions #:ql-dist (name)
            (let ((dependency-names (mapcar #'name dependencies)))
              (with-package-functions #:ql (bundle-systems)
                (with-source-registry (`(:source-registry :ignore-inherited-configuration
                                         (:tree ,project-root)
                                         (:also-exclude ".qlot")
                                         (:also-exclude ,(uiop:native-namestring bundle-directory))))
                  (handler-bind ((error
                                   (lambda (e)
                                     (when (eq (class-name (class-of e))
                                               (uiop:intern* '#:system-not-found '#:ql-bundle))
                                       (when (asdf:find-system (uiop:symbol-call '#:ql-bundle '#:system-not-found-system e) nil)
                                         (invoke-restart (find-restart (uiop:intern* '#:omit '#:ql-bundle) e)))))))
                    (bundle-systems dependency-names
                                    :to bundle-directory
                                    :overwrite t))))))
          ;; Delete bundle-info.sexp.
          (let ((bundle-info.sexp (merge-pathnames #P"bundle-info.sexp" bundle-directory)))
            (when (uiop:file-exists-p bundle-info.sexp)
              (delete-file bundle-info.sexp)))
          ;; Rename bundle.lisp -> setup.lisp.
          (rename-file (merge-pathnames #P"bundle.lisp" bundle-directory)
                       (merge-pathnames #P"setup.lisp" bundle-directory))
          ;; Copy local-projects/asdf.
          (let ((asdf-directory (merge-pathnames #P"local-projects/asdf/" quicklisp-home)))
            (when (uiop:directory-exists-p asdf-directory)
              (uiop:symbol-call '#:ql-bundle '#:copy-directory-tree
                                asdf-directory
                                (merge-pathnames #P"local-projects/asdf/" bundle-directory))))
          (message "Successfully bundled at '~A'." bundle-directory))))))

(defun bundle-project (object &key exclude output)
  (etypecase object
    ((or symbol string)
     (bundle-project (asdf:find-system object)
                     :exclude exclude
                     :output output))
    (asdf:system
     (%bundle-project (asdf:system-source-directory object)
                      :exclude exclude
                      :output output))
    (pathname
     (%bundle-project object
                      :exclude exclude
                      :output output))))
