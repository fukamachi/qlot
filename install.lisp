(defpackage #:qlot/install
  (:use #:cl)
  (:import-from #:qlot/install/quicklisp
                #:install-quicklisp)
  (:import-from #:qlot/source
                #:source-dist
                #:source-dist-name
                #:source-version
                #:source-install-url
                #:freeze-source)
  (:import-from #:qlot/parser
                #:read-qlfile-for-install)
  (:import-from #:qlot/distify-protocol
                #:write-source-distinfo
                #:finalize-dist)
  (:import-from #:qlot/distify
                #:distify)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/logger
                #:message
                #:debug-log)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:with-autoload-on-missing
                #:directory-lisp-files
                #:lisp-file-dependencies)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  #+sbcl
  (:import-from #:sb-posix)
  (:export #:install-qlfile
           #:update-qlfile
           #:install-project
           #:update-project))
(in-package #:qlot/install)

(defvar *qlot-directory* #P".qlot/")
(defvar *default-qlfile* #P"qlfile")

(defun canonical-qlhome (qlhome &optional (base *default-pathname-defaults*))
  (setf qlhome (uiop:ensure-directory-pathname qlhome))
  (if (uiop:absolute-pathname-p qlhome)
      qlhome
      (merge-pathnames qlhome base)))

(defun install-dependencies (project-root qlhome)
  (with-package-functions #:ql-dist (find-system)
    (with-quicklisp-home qlhome
      (let ((all-dependencies '()))
        (with-directory (system-file system-name dependencies) project-root
          (message "Loading '~A'..." system-file)
          (let ((*standard-output* (make-broadcast-stream))
                (*trace-output* (make-broadcast-stream))
                (*error-output* (make-broadcast-stream)))
            (with-autoload-on-missing
              (asdf:load-asd system-file)))
          (when (typep (asdf:find-system system-name) 'asdf:package-inferred-system)
            (let ((pis-dependencies
                    (loop for file in (directory-lisp-files project-root)
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

        (format t "~&Ensuring ~D ~:*dependenc~[ies~;y~:;ies~] installed.~%" (length all-dependencies))
        (with-package-functions #:ql-dist (ensure-installed)
          (mapc #'ensure-installed all-dependencies))))))

(defun install-qlfile (qlfile &key quicklisp-home
                                   (install-deps t))
  (unless quicklisp-home
    (setf quicklisp-home
          (merge-pathnames *qlot-directory*
                           (uiop:pathname-directory-pathname qlfile))))
  (unless (uiop:file-exists-p qlfile)
    (error 'qlot-simple-error
           :format-control "File does not exist: ~A"
           :format-arguments (list qlfile)))

  (let ((qlhome (canonical-qlhome quicklisp-home)))
    (unless (and (uiop:directory-exists-p qlhome)
                 (uiop:file-exists-p (merge-pathnames "setup.lisp" qlhome)))
      (ensure-directories-exist qlhome)
      (install-quicklisp qlhome))

    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    (progv (list (intern (string '#:*proxy-url*) '#:ql-http))
        (list *proxy*)
      (apply-qlfile-to-qlhome qlfile qlhome))

    ;; Install project dependencies
    (when install-deps
      (let ((project-root (uiop:pathname-directory-pathname qlfile)))
        (install-dependencies project-root qlhome)))

    (message "Successfully installed.")))

(defun update-qlfile (qlfile &key quicklisp-home
                                  projects
                                  (install-deps t))
  (unless quicklisp-home
    (setf quicklisp-home
          (merge-pathnames *qlot-directory*
                           (uiop:pathname-directory-pathname qlfile))))
  (unless (uiop:file-exists-p qlfile)
    (error 'qlot-simple-error
           :format-control "Failed to update because the file '~A' does not exist."
           :format-arguments (list qlfile)))

  (let ((qlhome (canonical-qlhome quicklisp-home)))
    (unless (and (uiop:directory-exists-p qlhome)
                 (uiop:file-exists-p (merge-pathnames "setup.lisp" qlhome)))
      (error 'qlot-simple-error
             :format-control "Local Quicklisp is not installed yet."))

    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    (progv (list (intern (string '#:*proxy-url*) '#:ql-http))
        (list *proxy*)
      (apply-qlfile-to-qlhome qlfile qlhome :ignore-lock t :projects projects))

    ;; Install project dependencies
    (when install-deps
      (let ((project-root (uiop:pathname-directory-pathname qlfile)))
        (install-dependencies project-root qlhome)))

    (message "Successfully installed.")))

(defun install-release (release)
  (uiop:symbol-call '#:ql-dist '#:ensure-installed release)

  ;; Install Roswell scripts.
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
cd \"$CURRENT/../../\"
exec qlot exec /bin/sh \"$CURRENT/../~A\" \"$@\"
"
            (subseq (namestring script)
                    (length (namestring qlhome)))))
          #+sbcl (sb-posix:chmod to #o700)))))

  (values))

(defun install-all-releases (source)
  (unless (typep source 'source-dist)
    (with-package-functions #:ql-dist (find-dist provided-releases)
      (let ((dist (find-dist (source-dist-name source))))
        (let ((releases (provided-releases dist)))
          (dolist (release releases)
            (install-release release)))))))

(defun install-source (source tmp-dir)
  (distify source tmp-dir)
  (with-package-functions #:ql-dist (install-dist version)
    (let ((new-dist (install-dist (source-install-url source)
                                  :prompt nil
                                  :replace nil)))
      (setf (source-version source) (version new-dist))
      (install-all-releases source)
      new-dist)))

(defun update-source (source tmp-dir)
  (write-source-distinfo source tmp-dir)
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
              (finalize-dist source tmp-dir)
              (setf dist (find-dist (source-dist-name source))
                    new-dist (available-update dist))
              (let ((*trace-output* (make-broadcast-stream)))
                (update-in-place dist new-dist))
              (setf (source-version source) (version new-dist))
              (install-all-releases source))
            (progn
              (setf (source-version source) (version (find-dist (source-dist-name source))))
              (message "Already have dist ~S version ~S."
                       (source-dist-name source)
                       (source-version source))))
        new-dist))))

(defun dump-qlfile-lock (file sources)
  (uiop:with-output-file (out file :if-exists :supersede)
    (let ((*print-pretty* nil)
          (*print-case* :downcase))
      (loop for source in sources
            for (project-name . contents) = (freeze-source source)
            do (format out "~&(~S .~% (~{~S ~S~^~%  ~}))~%" project-name contents)))))

(defun apply-qlfile-to-qlhome (qlfile qlhome &key ignore-lock projects)
  (let ((sources (read-qlfile-for-install qlfile
                                          :ignore-lock ignore-lock
                                          :projects projects)))
    (let ((preference (get-universal-time))
          (system-qlhome (and (find :quicklisp *features*)
                              (symbol-value (intern (string '#:*quicklisp-home*) '#:ql)))))
      (dolist (source sources)
        (with-quicklisp-home qlhome
          (with-package-functions #:ql-dist (find-dist version)
            (let ((dist (find-dist (source-dist-name source))))
              (cond
                ((not dist)
                 (with-quicklisp-home system-qlhome
                   (with-qlot-server (source qlhome tmp-dir)
                     (debug-log "Using temporary directory '~A'" tmp-dir)
                     (install-source source tmp-dir))))
                ((and (slot-boundp source 'qlot/source/base::version)
                      (equal (version dist)
                             (source-version source)))
                 (message "Already have dist ~S version ~S."
                          (source-dist-name source)
                          (source-version source)))
                ((string= (source-dist-name source) "quicklisp")
                 (with-package-functions #:ql-dist (uninstall)
                   (uninstall (find-dist "quicklisp")))
                 (with-quicklisp-home system-qlhome
                   (with-qlot-server (source qlhome tmp-dir)
                     (debug-log "Using temporary directory '~A'" tmp-dir)
                     (install-source source tmp-dir))))
                (t
                 (with-quicklisp-home system-qlhome
                   (with-qlot-server (source qlhome tmp-dir)
                     (debug-log "Using temporary directory '~A'" tmp-dir)
                     (update-source source tmp-dir))))))))
        (with-quicklisp-home qlhome
          (with-package-functions #:ql-dist (find-dist (setf preference))
            (setf (preference (find-dist (source-dist-name source)))
                  (incf preference)))))
      (with-quicklisp-home qlhome
        (with-package-functions #:ql-dist (uninstall name all-dists)
          (dolist (dist (all-dists))
            (unless (find (name dist) sources :test #'string= :key #'source-dist-name)
              (message "Removing dist ~S." (name dist))
              (uninstall dist))))))

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

(defun install-project (object &key (install-deps t))
  (etypecase object
    ((or symbol string)
     (install-project (asdf:find-system object)))
    (asdf:system
      (install-qlfile (asdf:system-relative-pathname object *default-qlfile*)
                      :quicklisp-home (asdf:system-relative-pathname
                                       object *qlot-directory*)
                      :install-deps install-deps))
    (pathname
      (install-qlfile (ensure-qlfile-pathname object)
                      :install-deps install-deps))))

(defun update-project (object &key projects
                                   (install-deps t))
  (etypecase object
    ((or symbol string)
     (update-project (asdf:find-system object) :projects projects))
    (asdf:system
      (update-qlfile (asdf:system-relative-pathname object *default-qlfile*)
                     :quicklisp-home (asdf:system-relative-pathname object *qlot-directory*)
                     :projects projects
                     :install-deps install-deps))
    (pathname
      (update-qlfile (ensure-qlfile-pathname object)
                     :projects projects
                     :install-deps install-deps))))
