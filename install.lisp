(defpackage #:qlot/install
  (:use #:cl)
  (:import-from #:qlot/install/quicklisp
                #:install-quicklisp
                #:copy-local-init-files)
  (:import-from #:qlot/source
                #:source-dist
                #:source-dist-name
                #:source-local
                #:source-local-path
                #:source-local-registry-directive
                #:source-project-name
                #:source-version
                #:source-install-url
                #:freeze-source)
  (:import-from #:qlot/parser
                #:read-qlfile-for-install)
  (:import-from #:qlot/server
                #:with-qlot-server
                #:run-distify-source-process)
  (:import-from #:qlot/logger
                #:message
                #:debug-log
                #:progress)
  (:import-from #:qlot/secure-downloader
                #:with-secure-installer
                #:with-download-logs)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:with-autoload-on-missing
                #:directory-lisp-files
                #:lisp-file-dependencies)
  (:import-from #:qlot/utils/project
                #:*qlot-directory*
                #:project-dependencies
                #:local-quicklisp-installed-p
                #:local-quicklisp-local-init-installed-p
                #:local-quicklisp-home)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory
                #:delete-tmp-directory)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  #+sbcl
  (:import-from #:sb-posix)
  (:export #:install-qlfile
           #:update-qlfile
           #:install-project
           #:update-project
           #:init-project))
(in-package #:qlot/install)

(defvar *default-qlfile* #P"qlfile")

(defun install-dependencies (project-root qlhome)
  (with-quicklisp-home qlhome
    (let ((all-dependencies (project-dependencies project-root)))
      (with-package-functions #:ql-dist (ensure-installed release name)
        (let ((releases (delete-duplicates (mapcar #'release all-dependencies)
                                           :key #'name
                                           :test 'equal
                                           :from-end t)))
          (format t "~&Ensuring ~D ~:*dependenc~[ies~;y~:;ies~] installed.~%" (length releases))
          (mapc #'ensure-installed releases))))))

(defun install-qlfile (qlfile &key quicklisp-home
                                   (install-deps t)
                                   cache-directory)
  (unless (uiop:file-exists-p qlfile)
    (error 'qlot-simple-error
           :format-control "File does not exist: ~A"
           :format-arguments (list qlfile)))

  (let* ((project-root (uiop:pathname-directory-pathname qlfile))
         (quicklisp-home (if quicklisp-home
                             (uiop:ensure-directory-pathname quicklisp-home)
                             (local-quicklisp-home project-root))))

    (cond
      ((not (local-quicklisp-installed-p project-root))
       (ensure-directories-exist quicklisp-home)
       (install-quicklisp quicklisp-home))
      ((not (local-quicklisp-local-init-installed-p project-root))
       (copy-local-init-files quicklisp-home)))

    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" quicklisp-home)))

    (with-secure-installer (:no-logs t)
      (apply-qlfile-to-qlhome qlfile quicklisp-home
                              :cache-directory cache-directory)

      ;; Install project dependencies
      (when install-deps
        (with-download-logs
          (install-dependencies project-root quicklisp-home))))

    (message "Successfully installed.")))

(defun update-qlfile (qlfile &key quicklisp-home
                                  projects
                                  (install-deps t)
                                  cache-directory)
  (unless (uiop:file-exists-p qlfile)
    (error 'qlot-simple-error
           :format-control "Failed to update because the file '~A' does not exist."
           :format-arguments (list qlfile)))

  (let* ((project-root (uiop:pathname-directory-pathname qlfile))
         (quicklisp-home (if quicklisp-home
                             (uiop:ensure-absolute-pathname quicklisp-home)
                             (local-quicklisp-home project-root))))

    (unless (local-quicklisp-installed-p project-root)
      (error 'qlot-simple-error
             :format-control "Local Quicklisp is not installed yet."))

    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" quicklisp-home)))

    (with-secure-installer ()
      (apply-qlfile-to-qlhome qlfile quicklisp-home
                              :ignore-lock t
                              :projects projects
                              :cache-directory cache-directory)

      ;; Install project dependencies
      (when install-deps
        (install-dependencies project-root quicklisp-home)))

    (message "Successfully installed.")))

(defun install-release-roswell-scripts (release)
  (let* ((qlhome (symbol-value (intern (string '#:*quicklisp-home*) '#:ql)))
         (ros-dir (merge-pathnames #P"roswell/" (uiop:symbol-call '#:ql-dist '#:base-directory release)))
         (bin-dir (merge-pathnames #P"bin/" qlhome))
         (scripts (uiop:directory-files ros-dir "*.*")))
    (when scripts
      (ensure-directories-exist bin-dir)
      (dolist (script scripts)
        (let ((to (make-pathname
                   :name (pathname-name script)
                   :type #+unix (if (equalp (pathname-type script) "ros")
                                    nil
                                    (pathname-type script))
                   #-unix (pathname-type script)
                   :defaults bin-dir)))
          (uiop:with-output-file (out to
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
            (format out "#!/bin/sh
CURRENT=$(dirname $0)
cd \"$CURRENT/../..\"
ROOT=$(pwd)
export QUICKLISP_HOME=\"$CURRENT/../\"
exec /bin/sh \"$CURRENT/../~A\" \"$@\"
"
                    (subseq (namestring script)
                            (length (namestring qlhome)))))
          #+sbcl (sb-posix:chmod to #o700)))))
  (values))

(defun install-release (release)
  (uiop:symbol-call '#:ql-dist '#:ensure-installed release)
  (install-release-roswell-scripts release)
  (values))

(defun install-all-releases (source)
  (unless (typep source 'source-dist)
    (with-package-functions #:ql-dist (find-dist provided-releases name)
      (let ((dist (find-dist (source-dist-name source))))
        (progress "Getting the list of releases.")
        (let ((releases (provided-releases dist)))
          (dolist (release releases)
            (progress "Installing a new release ~S." (name release))
            (install-release release)))))))

(defun install-source (source)
  (with-package-functions #:ql-dist (install-dist version)
    (progress "Installing the new dist.")
    (let ((new-dist (let ((*standard-output* (make-broadcast-stream)))
                      (install-dist (source-install-url source)
                                    :prompt nil
                                    :replace nil))))
      (setf (source-version source) (version new-dist))
      (install-all-releases source)
      new-dist)))

(defun update-source (source tmp-dir)
  (with-package-functions #:ql-dist (find-dist update-in-place available-update name version uninstall installed-releases)
    (let ((dist (find-dist (source-dist-name source))))
      (let ((new-dist (available-update dist)))
        (if new-dist
            (progn
              (message "Updating dist ~S version ~S -> ~S."
                       (name dist)
                       (version dist)
                       (version new-dist))
              (map nil #'uninstall (installed-releases dist))
              (run-distify-source-process source tmp-dir
                                          :quicklisp-home (symbol-value (intern (string '#:*quicklisp-home*) '#:ql)))
              (setf dist (find-dist (source-dist-name source))
                    new-dist (available-update dist))
              (let ((*trace-output* (make-broadcast-stream)))
                (update-in-place dist new-dist))
              (setf (source-version source) (version new-dist))
              (install-all-releases source)
              (message "=> Updated dist ~S to version ~S."
                       (source-project-name source)
                       (source-version source)))
            (progn
              (setf (source-version source) (version (find-dist (source-dist-name source))))
              (message "Already have dist ~S version ~S."
                       (source-dist-name source)
                       (source-version source))))
        new-dist))))

(defun dump-source-registry-conf (file sources)
  (uiop:with-output-file (out file :if-exists :supersede)
    (let ((*print-pretty* nil)
          (*print-case* :downcase))
      (format out
              "~&(~{~S~^~% ~})~%"
              `(:source-registry
                :ignore-inherited-configuration
                (:also-exclude ".qlot")
                ,@(loop for source in sources
                        when (typep source 'source-local)
                        collect (progn
                                  (message "Adding ~S located at '~A'."
                                           (source-project-name source)
                                           (source-local-path source))
                                  `(:tree ,(source-local-registry-directive source)))))))))

(defun dump-qlfile-lock (file sources)
  (uiop:with-output-file (out file :if-exists :supersede)
    (let ((*print-pretty* nil)
          (*print-case* :downcase))
      (loop for source in sources
            for (project-name . contents) = (freeze-source source)
            do (format out "~&(~S .~% (~{~S ~S~^~%  ~}))~%" project-name contents)))))

(defun apply-qlfile-to-qlhome (qlfile qlhome &key ignore-lock projects cache-directory)
  (let ((sources (read-qlfile-for-install qlfile
                                          :ignore-lock ignore-lock
                                          :projects projects)))
    (let ((preference (get-universal-time))
          (system-qlhome (and (find :quicklisp *features*)
                              (symbol-value (intern (string '#:*quicklisp-home*) '#:ql))))
          (tmp-dir (or cache-directory (tmp-directory))))
      (ensure-directories-exist tmp-dir)
      (unwind-protect
          (dolist (source (remove-if (lambda (source)
                                       (typep source 'source-local))
                                     sources))
            (with-quicklisp-home qlhome
              (with-package-functions #:ql-dist (find-dist version)
                (let ((dist (find-dist (source-dist-name source))))
                  (cond
                    ((not dist)
                     (message "Installing dist ~S." (source-project-name source))
                     (with-quicklisp-home system-qlhome
                       (with-qlot-server (source qlhome tmp-dir)
                         (debug-log "Using temporary directory '~A'" tmp-dir)
                         (install-source source)))
                     (message "=> Newly installed ~S version ~S."
                              (source-project-name source)
                              (source-version source)))
                    ((and (slot-boundp source 'qlot/source/base::version)
                          (equal (version dist)
                                 (source-version source)))
                     (message "Already have dist ~S version ~S."
                              (source-project-name source)
                              (source-version source)))
                    ((string= (source-dist-name source) "quicklisp")
                     (message "Installing dist ~S." (source-project-name source))
                     (with-package-functions #:ql-dist (uninstall version)
                       (let* ((current-dist (find-dist "quicklisp"))
                              (current-version (version current-dist)))
                         (uninstall (find-dist "quicklisp"))
                         (with-quicklisp-home system-qlhome
                           (with-qlot-server (source qlhome tmp-dir)
                             (debug-log "Using temporary directory '~A'" tmp-dir)
                             (install-source source)))
                         (if (equal current-version (source-version source))
                             (message "=> No update on dist \"quicklisp\" version ~S."
                                      current-version)
                             (message "=> Updated dist \"quicklisp\" version ~S -> ~S."
                                      current-version
                                      (source-version source))))))
                    (t
                     (with-quicklisp-home system-qlhome
                       (with-qlot-server (source qlhome tmp-dir t)
                         (debug-log "Using temporary directory '~A'" tmp-dir)
                         (update-source source tmp-dir))))))))
            (with-quicklisp-home qlhome
              (with-package-functions #:ql-dist (find-dist (setf preference))
                (setf (preference (find-dist (source-dist-name source)))
                      (incf preference)))))
        (unless cache-directory
          (delete-tmp-directory tmp-dir)))
      (with-quicklisp-home qlhome
        (with-package-functions #:ql-dist (uninstall name all-dists)
          (dolist (dist (all-dists))
            (unless (find (name dist) sources :test #'string= :key #'source-dist-name)
              (message "Removing dist ~S." (name dist))
              (uninstall dist))))))

    (dump-source-registry-conf (merge-pathnames #P"source-registry.conf" qlhome)
                               sources)
    (dump-qlfile-lock (make-pathname :name (file-namestring qlfile)
                                     :type "lock"
                                     :defaults qlfile)
                      sources)

    (values)))

(defun ensure-qlfile-pathname (object)
  (cond
    ((uiop:file-exists-p object)
     object)
    ((uiop:directory-exists-p (uiop:ensure-directory-pathname object))
     (merge-pathnames *default-qlfile* (uiop:ensure-directory-pathname object)))
    (t object)))

(defun install-project (object &key (install-deps t) cache-directory)
  (etypecase object
    ((or symbol string)
     (install-project (asdf:find-system object)
                      :install-deps install-deps
                      :cache-directory cache-directory))
    (asdf:system
      (install-qlfile (asdf:system-relative-pathname object *default-qlfile*)
                      :quicklisp-home (asdf:system-relative-pathname
                                       object *qlot-directory*)
                      :install-deps install-deps
                      :cache-directory cache-directory))
    (pathname
      (install-qlfile (ensure-qlfile-pathname object)
                      :install-deps install-deps
                      :cache-directory cache-directory))))

(defun update-project (object &key projects
                                   (install-deps t)
                                   cache-directory)
  (etypecase object
    ((or symbol string)
     (update-project (asdf:find-system object)
                     :projects projects
                     :install-deps install-deps
                     :cache-directory cache-directory))
    (asdf:system
      (update-qlfile (asdf:system-relative-pathname object *default-qlfile*)
                     :quicklisp-home (asdf:system-relative-pathname object *qlot-directory*)
                     :projects projects
                     :install-deps install-deps
                     :cache-directory cache-directory))
    (pathname
      (update-qlfile (ensure-qlfile-pathname object)
                     :projects projects
                     :install-deps install-deps
                     :cache-directory cache-directory))))

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
       ;; Run 'qlot install'
       (let ((qlfile.lock (make-pathname :type "lock"
                                         :defaults qlfile)))
         (unless (uiop:file-exists-p qlfile.lock)
           (install-qlfile qlfile)))
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
                 (format out "~&.qlot/~%"))))))))))
