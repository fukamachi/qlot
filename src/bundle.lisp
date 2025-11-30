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
  (:export #:bundle-project))
(in-package #:qlot/bundle)

(defun release-installed-directory (release)
  "Return the installed software directory for RELEASE."
  (uiop:symbol-call '#:ql-dist '#:relative-to
                    (uiop:symbol-call '#:ql-dist '#:dist release)
                    (make-pathname :directory (list :relative "software"
                                                    (uiop:symbol-call '#:ql-dist '#:prefix release)))))

(defun copy-installed-release (release target)
  "Copy the installed files for RELEASE to TARGET.
Uses truename to follow symlinks, so cached symlinked releases are properly copied."
  (let* ((installed-dir (release-installed-directory release))
         (prefix (uiop:symbol-call '#:ql-dist '#:prefix release))
         (target-dir (merge-pathnames (make-pathname :directory (list :relative "software" prefix))
                                      (uiop:ensure-directory-pathname target))))
    (when (uiop:directory-exists-p installed-dir)
      (uiop:symbol-call '#:ql-bundle '#:copy-directory-tree installed-dir target-dir)
      release)))

(defvar *use-qlot-unpack-release* nil
  "When T, unpack-release copies from installed directories instead of downloading archives.")

(defvar *qlot-unpack-release-method-added* nil
  "T if the :around method for unpack-release has been added.")

(defun release-has-installed-files-p (release)
  "Return T if the release's software directory exists and has files.
This works even when installedp returns NIL (e.g., when symlinks are broken)."
  (let ((dir (release-installed-directory release)))
    (and (uiop:directory-exists-p dir)
         ;; Check that truename works (not a broken symlink)
         (ignore-errors (truename dir)))))

(defun ensure-qlot-unpack-release-method ()
  "Ensure the :around method on ql-bundle::unpack-release is defined.
This is called at runtime when the ql-bundle package exists."
  (unless *qlot-unpack-release-method-added*
    ;; Use EVAL to define the method at runtime when the package exists
    (eval
     '(defmethod ql-bundle::unpack-release :around (release target)
       "When *use-qlot-unpack-release* is T, copy from installed directories
instead of downloading archives. This handles qlot:// URLs."
       (if (and qlot/bundle::*use-qlot-unpack-release*
                (qlot/bundle::release-has-installed-files-p release))
           (qlot/bundle::copy-installed-release release target)
           (call-next-method))))
    (setf *qlot-unpack-release-method-added* t)))

(defmacro with-qlot-unpack-release (&body body)
  "Execute BODY with ql-bundle:unpack-release overridden to copy from
installed locations for already-installed releases. This handles qlot:// URLs that
would otherwise fail because the qlot server isn't running during bundle."
  `(progn
     (ensure-qlot-unpack-release-method)
     (let ((*use-qlot-unpack-release* t))
       ,@body)))

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
                  (with-qlot-unpack-release
                    (handler-bind ((error
                                     (lambda (e)
                                       (when (eq (class-name (class-of e))
                                                 (uiop:intern* '#:system-not-found '#:ql-bundle))
                                         (when (asdf:find-system (uiop:symbol-call '#:ql-bundle '#:system-not-found-system e) nil)
                                           (invoke-restart (find-restart (uiop:intern* '#:omit '#:ql-bundle) e)))))))
                      (bundle-systems dependency-names
                                      :to bundle-directory
                                      :overwrite t)))))))
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
          ;; Keep local-projects/.
          (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-directory))
          (with-open-file (out (merge-pathnames #P"local-projects/.keep" bundle-directory)
                               :if-exists :overwrite
                               :if-does-not-exist :create
                               :direction :output))
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
