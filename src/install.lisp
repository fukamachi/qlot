(defpackage #:qlot/install
  (:use #:cl)
  (:import-from #:qlot/install/quicklisp
                #:install-quicklisp
                #:install-local-init-files
                #:install-qlot-config-file)
  (:import-from #:qlot/source
                #:prepare-source
                #:source-dist
                #:source-dist-name
                #:source-local
                #:source-asdf
                #:source-asdf-remote-url
                #:source-project-name
                #:source-version
                #:source-install-url)
  (:import-from #:qlot/parser
                #:read-qlfile-for-install)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/distify
                #:distify)
  (:import-from #:qlot/logger
                #:*debug*
                #:*enable-whisper*
                #:*terminal*
                #:message
                #:without-linewrap)
  (:import-from #:qlot/secure-downloader
                #:with-secure-installer)
  (:import-from #:qlot/progress
                #:run-in-parallel
                #:progress)
  (:import-from #:qlot/utils
                #:with-package-functions
                #:find-duplicated-entry)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/utils/qlot
                #:dump-source-registry-conf
                #:dump-qlfile-lock)
  (:import-from #:qlot/utils/project
                #:*qlot-directory*
                #:*default-qlfile*
                #:ensure-qlfile-pathname
                #:local-quicklisp-installed-p
                #:check-local-quicklisp
                #:local-quicklisp-home)
  (:import-from #:qlot/utils/dependencies
                #:project-dependencies-in-child-process)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory
                #:delete-tmp-directory)
  (:import-from #:qlot/utils/git
                #:git-switch-tag
                #:git-clone)
  (:import-from #:qlot/cache
                #:*cache-enabled*
                #:*cache-directory*
                #:initialize-cache
                #:cache-exists-p
                #:restore-from-cache
                #:save-to-cache
                #:validate-dist-installation
                #:canonicalize-dist-url
                #:release-cache-exists-p
                #:restore-release-from-cache
                #:save-release-to-cache)
  (:import-from #:qlot/color
                #:color-text
                #:*enable-color*)
  (:import-from #:qlot/errors
                #:qlot-simple-error
                #:missing-projects
                #:duplicate-project
                #:qlfile-not-found)
  (:import-from #:bordeaux-threads)
  #+sbcl
  (:import-from #:sb-posix)
  (:export #:install-qlfile
           #:update-qlfile
           #:install-project
           #:update-project
           #:source-dist-path
           #:register-dist-with-quicklisp
           #:invalidate-broken-dist
           #:format-cache-status
           #:with-release-cache))
(in-package #:qlot/install)

;;;
;;; Release-level caching hook
;;;

(defun create-release-install-markers (release)
  "Create Quicklisp install markers for a release restored from cache.
This makes Quicklisp recognize the release as installed."
  (with-package-functions #:ql-dist (dist base-directory
                                     system-files install-metadata-file
                                     provided-systems find-system-in-dist)
    (let ((dist (dist release))
          (tracking (install-metadata-file release)))
      ;; Write release tracking file
      (ensure-directories-exist tracking)
      (with-open-file (stream tracking
                              :direction :output
                              :if-exists :supersede)
        (write-line (uiop:symbol-call '#:ql-setup '#:qenough (base-directory release)) stream))
      ;; Write system tracking files
      (let ((provided (provided-systems release)))
        (dolist (file (system-files release))
          (let ((system (find-system-in-dist (pathname-name file) dist)))
            (when (member system provided)
              (let ((system-tracking (install-metadata-file system))
                    (system-file (merge-pathnames file (base-directory release))))
                (ensure-directories-exist system-tracking)
                (with-open-file (stream system-tracking
                                        :direction :output
                                        :if-exists :supersede)
                  (write-line (uiop:symbol-call '#:ql-setup '#:qenough system-file)
                              stream))))))))))

(defun release-cache-install-around (release next-method-fn)
  "Handle caching logic for release installation via :around method."
  (with-package-functions #:ql-dist (archive-url dist canonical-distinfo-url name
                                     archive-md5 prefix relative-to)
    (let ((archive-url (archive-url release)))
      ;; Skip caching for qlot:// sources (already handled by dist-level cache)
      (when (uiop:string-prefix-p "qlot://" archive-url)
        (return-from release-cache-install-around (funcall next-method-fn)))

      (let* ((dist (dist release))
             (dist-url (canonicalize-dist-url (canonical-distinfo-url dist)))
             (release-name (name release))
             (archive-md5 (archive-md5 release))
             (prefix (prefix release))
             (software-dir (relative-to dist
                                        (make-pathname :directory '(:relative "software")))))
        ;; Check cache first
        (if (release-cache-exists-p dist-url release-name archive-md5)
            (progn
              (restore-release-from-cache dist-url release-name archive-md5 software-dir prefix)
              (create-release-install-markers release)
              release)
            ;; Cache miss - install normally then cache
            (progn
              (funcall next-method-fn)
              (save-release-to-cache dist-url release-name archive-md5 software-dir prefix)
              release))))))

(defvar *release-cache-active* nil
  "When T, the release-level cache :around method performs caching.")

(defvar *release-cache-method-loaded* nil
  "Flag to track if the release cache :around method has been loaded.")

(defun load-release-cache-method ()
  "Define the :around method for release-level caching.
Should be called after Quicklisp is loaded."
  (unless *release-cache-method-loaded*
    (let ((install-sym (uiop:intern* '#:install '#:ql-dist))
          (release-sym (uiop:intern* '#:release '#:ql-dist)))
      (eval
       `(defmethod ,install-sym :around ((release ,release-sym))
          (if (and *release-cache-active* qlot/cache:*cache-enabled*)
              (release-cache-install-around release (lambda () (call-next-method)))
              (call-next-method)))))
    (setf *release-cache-method-loaded* t)))

(defmacro with-release-cache (&body body)
  "Execute BODY with release-level caching enabled."
  `(if *cache-enabled*
       (let ((*release-cache-active* t))
         ,@body)
       (progn ,@body)))

(defun install-dependencies (project-root qlhome)
  (with-quicklisp-home qlhome
    (let ((all-dependencies (with-package-functions #:ql-dist (find-system)
                              (mapcar #'find-system (project-dependencies-in-child-process project-root qlhome)))))
      (with-package-functions #:ql-dist (ensure-installed release name)
        (let ((releases (delete-duplicates
                         (mapcar #'release all-dependencies)
                         :key #'name
                         :test 'equal)))
          (message "Ensuring ~D ~:*dependenc~[ies~;y~:;ies~] installed." (length releases))
          (with-release-cache
            (mapc #'ensure-installed releases)))))))

(defun install-qlfile (qlfile &key quicklisp-home
                                   (install-deps t)
                                   concurrency)
  (unless (uiop:file-exists-p qlfile)
    (restart-case
        (error 'qlfile-not-found :path qlfile)
      (create-qlfile ()
        (with-open-file (out qlfile
                             :if-does-not-exist :create
                             :direction :output)))))

  (let* ((project-root (uiop:pathname-directory-pathname qlfile))
         (quicklisp-home (if quicklisp-home
                             (uiop:ensure-directory-pathname quicklisp-home)
                             (local-quicklisp-home project-root))))

    (cond
      ((not (local-quicklisp-installed-p project-root))
       (install-quicklisp quicklisp-home))
      (t
       (install-local-init-files quicklisp-home)
       (install-qlot-config-file quicklisp-home)))

    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" quicklisp-home)))

    (when *cache-enabled*
      (load-release-cache-method))

    (without-linewrap ()
      (with-secure-installer ()
        (apply-qlfile-to-qlhome qlfile quicklisp-home
                                :concurrency concurrency)

        ;; Install project dependencies
        (when install-deps
          (install-dependencies project-root quicklisp-home))))

    (message "Successfully installed.")))

(defun update-qlfile (qlfile &key quicklisp-home
                                  projects
                                  (install-deps t)
                                  concurrency)
  (unless (uiop:file-exists-p qlfile)
    (error 'qlfile-not-found :path qlfile))

  (let* ((project-root (uiop:pathname-directory-pathname qlfile))
         (quicklisp-home (if quicklisp-home
                             (uiop:ensure-absolute-pathname quicklisp-home)
                             (local-quicklisp-home project-root))))

    (check-local-quicklisp project-root)

    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" quicklisp-home)))

    (when *cache-enabled*
      (load-release-cache-method))

    (without-linewrap ()
      (with-secure-installer ()
        (apply-qlfile-to-qlhome qlfile quicklisp-home
                                :ignore-lock t
                                :projects projects
                                :concurrency concurrency)

        ;; Install project dependencies
        (when install-deps
          (install-dependencies project-root quicklisp-home))))

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
  (with-release-cache
    (uiop:symbol-call '#:ql-dist '#:ensure-installed release))
  (install-release-roswell-scripts release)
  (values))

(defun install-all-releases (source)
  (unless (typep source 'source-dist)
    (with-package-functions #:ql-dist (find-dist provided-releases name)
      (let ((dist (find-dist (source-dist-name source))))
        (progress "Getting the list of releases.")
        (let ((releases (provided-releases dist)))
          (with-release-cache
            (dolist (release releases)
              (progress "Installing a new release ~S." (name release))
              (uiop:symbol-call '#:ql-dist '#:ensure-installed release)
              (install-release-roswell-scripts release))))))))

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
  (with-package-functions #:ql-dist (find-dist update-in-place available-update version uninstall installed-releases)
    (let ((dist (find-dist (source-dist-name source))))
      (let ((new-dist (available-update dist)))
        (if new-dist
            (progn
              (map nil #'uninstall (installed-releases dist))
              (distify source tmp-dir)
              (setf dist (find-dist (source-dist-name source))
                    new-dist (available-update dist))
              (let ((*trace-output* (make-broadcast-stream)))
                (update-in-place dist new-dist))
              (setf (source-version source) (version new-dist))
              (install-all-releases source)
              (progress :done "Updated dist ~S to version ~S."
                        (source-project-name source)
                        (source-version source)))
            (progn
              (setf (source-version source) (version (find-dist (source-dist-name source))))
              (progress :done "No update on dist ~S version ~S"
                        (source-dist-name source)
                        (source-version source))))
        new-dist))))

(defun progress-indicator (current max &key label)
  (concatenate 'string
               (color-text :gray
                           (format nil "[~A/~A]"
                                   (format nil "~v,' d"
                                           (length (princ-to-string max))
                                           current)
                                   max))
               (format nil "~@[ ~A~]" label)))

(defun source-dist-path (source qlhome)
  (merge-pathnames
   (make-pathname :directory (list :relative "dists" (source-dist-name source)))
   qlhome))

(defun format-cache-status (source cache-status install-type elapsed-time)
  (format nil "~A ~S version ~S~A. (~,1Fs)"
          (ecase install-type
            (:new "Installed")
            (:update "Updated"))
          (source-project-name source)
          (source-version source)
          (ecase cache-status
            (:hit " from cache")
            (:miss "")
            (:skip "")
            (:disabled ""))
          elapsed-time))

(defun register-dist-with-quicklisp (source qlhome)
  (let* ((dist-path (source-dist-path source qlhome))
         (enabled (merge-pathnames "enabled.txt" dist-path))
         (preference (merge-pathnames "preference.txt" dist-path)))
    (ensure-directories-exist enabled)
    (with-open-file (out enabled
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (write-line (source-dist-name source) out))
    (with-open-file (out preference
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (write-line "100" out))
    dist-path))

(defun invalidate-broken-dist (dist-path)
  (ignore-errors
    (uiop:delete-directory-tree dist-path :validate t)))

(defun apply-qlfile-to-qlhome (qlfile qlhome &key ignore-lock projects concurrency)
  (check-type concurrency (or null (integer 0)))
  (let* ((qlfile-lock (make-pathname :defaults qlfile
                                     :name (file-namestring qlfile)
                                     :type "lock"))
         (sources-from-lock (and (not ignore-lock)
                                 (uiop:file-exists-p qlfile-lock)))
         (sources (read-qlfile-for-install qlfile
                                           :ignore-lock ignore-lock
                                           :projects projects)))
    (when projects
      (let ((missing (set-difference projects (mapcar #'source-project-name sources)
                                     :test #'string=)))
        (when missing
          (error 'missing-projects :projects missing))))
    (let ((tmp-dir (tmp-directory)))
      (ensure-directories-exist tmp-dir)
      (unless sources-from-lock
        (mapc #'prepare-source sources))
      (let ((dup (find-duplicated-entry sources
                                        :key #'source-project-name
                                        :test 'equal)))
        (when dup
          (error 'duplicate-project :name dup)))
      (unwind-protect
           (let* ((sources-non-local
                    (remove-if (lambda (source)
                                 (typep source '(or source-local source-asdf)))
                               sources))
                  (sources-to-install
                    (with-quicklisp-home qlhome
                      (with-package-functions #:ql-dist (find-dist version)
                        (remove-if (lambda (source)
                                     (let* ((dist (find-dist (source-dist-name source)))
                                            (dist-path (and dist (source-dist-path source qlhome))))
                                       (and dist
                                            (not (member (source-project-name source)
                                                         projects
                                                         :test #'string=))
                                            (slot-boundp source 'qlot/source/base::version)
                                            (equal (version dist)
                                                   (source-version source))
                                            (let ((valid (validate-dist-installation dist-path)))
                                              (unless valid
                                                (invalidate-broken-dist dist-path))
                                              valid))))
                                   sources-non-local))))
                  (bt2:*default-special-bindings* (append `((*enable-color* . ,*enable-color*)
                                                            (*terminal* . ,*terminal*)
                                                            (*debug* . ,*debug*)
                                                            (*enable-whisper* . nil)
                                                            (qlot/cache:*cache-directory* . ,qlot/cache:*cache-directory*)
                                                            (qlot/cache:*cache-enabled* . ,qlot/cache:*cache-enabled*)
                                                            (,(uiop:intern* '#:*fetch-scheme-functions* '#:ql-http) . ',(symbol-value (uiop:intern* '#:*fetch-scheme-functions* '#:ql-http))))
                                                          bt2:*default-special-bindings*))
                  (lock (bt2:make-lock))
                  (install-lock (bt2:make-lock))
                  (current-count 0)
                  (max-count (length sources-to-install)))
             (run-in-parallel
             (lambda (source)
                (with-quicklisp-home qlhome
                  (with-package-functions #:ql-dist (find-dist version)
                    (block install-source-block
                      (let ((dist-path (source-dist-path source qlhome))
                            (start-time (get-internal-real-time))
                            (dist (find-dist (source-dist-name source)))
                            (cache-restore-allowed (and *cache-enabled* (not ignore-lock))))
                        (when (and dist
                                   (not ignore-lock)
                                   (not (slot-boundp source 'qlot/source/base::version)))
                          (setf (source-version source) (version dist)))
                        (labels ((elapsed ()
                                   (/ (- (get-internal-real-time) start-time)
                                      internal-time-units-per-second))
                                 (report (cache-status install-type)
                                   (progress :done "~A"
                                             (format-cache-status source cache-status install-type (elapsed))))
                                 (cache-status ()
                                   (cond
                                     ((not *cache-enabled*) :disabled)
                                     ((progn
                                        (save-to-cache source dist-path)
                                        (cache-exists-p source))
                                      :miss)
                                     (t :skip))))
                          (when (and cache-restore-allowed
                                     (cache-exists-p source)
                                     (restore-from-cache source dist-path))
                            (register-dist-with-quicklisp source qlhome)
                            ;; Install releases (needed when sources-cache is empty due to release-level caching)
                            (install-all-releases source)
                            (report :hit (if dist :update :new))
                            (return-from install-source-block))
                          (cond
                            ((not dist)
                             (progress :in-progress "Installing ~S."
                                       (source-dist-name source))
                             (with-qlot-server (source :destination tmp-dir :silent t)
                               (bt2:with-lock-held (install-lock)
                                 (install-source source)))
                             (report (cache-status) :new))
                            ((and (not ignore-lock)
                                  (slot-boundp source 'qlot/source/base::version)
                                  (equal (version dist)
                                         (source-version source)))
                             (progress :done "Already have dist ~S version ~S."
                                       (source-project-name source)
                                       (source-project-name source)
                                       (source-version source)))
                            ((typep source 'source-dist)
                             (with-package-functions #:ql-dist (uninstall version)
                               (let* ((current-dist (find-dist (source-dist-name source)))
                                      (current-version (version current-dist)))
                                 (uninstall current-dist)
                                 (with-qlot-server (source :destination tmp-dir :silent t)
                                   (bt2:with-lock-held (install-lock)
                                     (install-source source)))
                                 (if (equal current-version (source-version source))
                                     (progress :done "No update on dist \"~A\" version ~S."
                                               (source-dist-name source)
                                               current-version)
                                     (progress :done "Updated dist \"~A\" version ~S -> ~S."
                                               (source-dist-name source)
                                               current-version
                                               (source-version source)))
                                 (report (cache-status) :update))))
                            (t
                             (with-qlot-server (source :destination tmp-dir
                                                       :distinfo-only t
                                                       :silent t)
                               (bt2:with-lock-held (install-lock)
                                 (update-source source tmp-dir)))
                             (report (cache-status) :update)))))))))
              sources-to-install
              :concurrency (or concurrency 4)
              :job-header-fn
              (lambda (source)
                (bt2:with-lock-held (lock)
                  (let ((i (incf current-count)))
                    (progress-indicator i max-count
                                        :label (source-project-name source)))))
              :failed-fn
              (lambda ()
                (progress :aborted "Failed to install")))
             (let ((preference (get-universal-time)))
               (with-quicklisp-home qlhome
                 (with-package-functions #:ql-dist (find-dist name all-dists (setf preference))
                   (dolist (source sources-non-local)
                     (let* ((dist-name (source-dist-name source))
                            (dist (find-dist dist-name)))
                       (unless dist
                         (error 'qlot-simple-error
                                :format-control "Unable to find dist with name ~S. You should use one of these names in the qlfile: ~A"
                                :format-arguments (list dist-name
                                                        (mapcar #'name (all-dists)))))
                       (setf (preference dist)
                             (incf preference))))))))
        (delete-tmp-directory tmp-dir))
      (with-quicklisp-home qlhome
        (with-package-functions #:ql-dist (uninstall name all-dists)
          (dolist (dist (all-dists))
            (unless (find (name dist) sources :test #'string= :key #'source-dist-name)
              (message "Removing dist ~S." (name dist))
              (uninstall dist))))))

    ;; ASDF
    (let ((asdf-source (find-if (lambda (source)
                                  (typep source 'source-asdf))
                                sources))
          (asdf-dir (merge-pathnames #P"local-projects/asdf/" qlhome)))
      (cond
        (asdf-source
         (cond
           ((uiop:directory-exists-p asdf-dir)
            ;; Tag switch
            (git-switch-tag asdf-dir (source-version asdf-source)))
           (t
            (message "Downloading ASDF to '~A'." asdf-dir)
            ;; Clone
            (git-clone (source-asdf-remote-url asdf-source)
                       asdf-dir
                       :checkout-to (source-version asdf-source)
                       :recursive nil))))
        (t
         (when (uiop:directory-exists-p asdf-dir)
           (message "Removing ASDF at '~A'." asdf-dir)
           (uiop:delete-directory-tree asdf-dir :validate t :if-does-not-exist :ignore)))))

    (with-open-file (out (merge-pathnames #P"source-registry.conf" qlhome)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (dump-source-registry-conf out sources))
    (dump-qlfile-lock (make-pathname :name (file-namestring qlfile)
                                     :type "lock"
                                     :defaults qlfile)
                      sources)

    (values)))

(defun install-project (object &key (install-deps t) cache-directory concurrency)
  (when cache-directory
    (setf *cache-directory*
          (uiop:ensure-absolute-pathname
           (uiop:ensure-directory-pathname cache-directory)
           *default-pathname-defaults*))
    (initialize-cache))
  (handler-bind ((qlfile-not-found
                   (lambda (e)
                     (invoke-restart (find-restart 'create-qlfile e)))))
    (etypecase object
      ((or symbol string)
       (install-project (asdf:find-system object)
                        :install-deps install-deps
                        :cache-directory cache-directory
                        :concurrency concurrency))
      (asdf:system
       (install-qlfile (asdf:system-relative-pathname object *default-qlfile*)
                       :quicklisp-home (asdf:system-relative-pathname
                                        object *qlot-directory*)
                       :install-deps install-deps
                       :concurrency concurrency))
      (pathname
       (install-qlfile (ensure-qlfile-pathname object)
                       :install-deps install-deps
                       :concurrency concurrency)))))

(defun update-project (object &key projects
                                   (install-deps t)
                                   cache-directory
                                   concurrency)
  (when cache-directory
    (setf *cache-directory*
          (uiop:ensure-absolute-pathname
           (uiop:ensure-directory-pathname cache-directory)
           *default-pathname-defaults*))
    (initialize-cache))
  (etypecase object
    ((or symbol string)
     (update-project (asdf:find-system object)
                     :projects projects
                     :install-deps install-deps
                     :cache-directory cache-directory
                     :concurrency concurrency))
    (asdf:system
     (update-qlfile (asdf:system-relative-pathname object *default-qlfile*)
                    :quicklisp-home (asdf:system-relative-pathname object *qlot-directory*)
                    :projects projects
                    :install-deps install-deps
                    :concurrency concurrency))
    (pathname
     (update-qlfile (ensure-qlfile-pathname object)
                    :projects projects
                    :install-deps install-deps
                    :concurrency concurrency))))
