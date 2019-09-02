(defpackage #:qlot/install
  (:use #:cl)
  (:import-from #:qlot/install/quicklisp
                #:install-quicklisp)
  (:import-from #:qlot/source
                #:source-dist-name
                #:source-version
                #:source-distinfo-url
                #:freeze-source)
  (:import-from #:qlot/parser
                #:read-qlfile-for-install)
  (:import-from #:qlot/server
                #:with-qlot-server
                #:run-distify-source-process)
  (:import-from #:qlot/logger
                #:message
                #:debug-log)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
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

(defun install-qlfile (qlfile &key quicklisp-home)
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

    (apply-qlfile-to-qlhome qlfile qlhome)

    ;; TODO: Quickload project systems

    (message "Successfully installed.")))

(defun update-qlfile (qlfile &key quicklisp-home projects)
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

    (apply-qlfile-to-qlhome qlfile qlhome :ignore-lock t :projects projects)

    ;; TODO: Quickload project systems

    (message "Successfully installed.")))

(defun already-installed-p (source)
  (with-package-functions #:ql-dist (find-dist)
    (and (find-dist (source-dist-name source))
         t)))

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
qlot exec /bin/sh \"$CURRENT/../~A\" \"$@\"
"
            (subseq (namestring script)
                    (length (namestring qlhome)))))
          #+sbcl (sb-posix:chmod to #o700)))))

  (values))

(defun install-all-releases (source)
  (unless (equal (symbol-name (type-of source))
                 (string :source-ql-all))
    (with-package-functions #:ql-dist (find-dist provided-releases)
      (let ((dist (find-dist (source-dist-name source))))
        (let ((releases (provided-releases dist)))
          (dolist (release releases)
            (install-release release)))))))

(defun install-source (source tmp-dir)
  (run-distify-source-process source tmp-dir
                              :quicklisp-home (symbol-value (intern (string '#:*quicklisp-home*) '#:ql)))
  (with-package-functions #:ql-dist (install-dist version)
    (let ((new-dist (install-dist (source-distinfo-url source)
                                  :prompt nil
                                  :replace nil)))
      (setf (source-version source) (version new-dist))
      (install-all-releases source)
      new-dist)))

(defun update-source (source tmp-dir)
  (with-package-functions #:ql-dist (find-dist update-in-place available-update name version uninstall installed-releases distinfo-subscription-url (setf distinfo-subscription-url))
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
    (with-qlot-server (qlfile qlhome tmp-dir)
      (debug-log "Using temporary directory '~A'" tmp-dir)
      (with-quicklisp-home qlhome
        (let ((preference (get-universal-time)))
          (dolist (source sources)
            (if (already-installed-p source)
                (update-source source tmp-dir)
                (install-source source tmp-dir))
            (with-package-functions #:ql-dist (dist (setf preference))
              (setf (preference (dist (source-dist-name source)))
                    (incf preference))))
          (with-package-functions #:ql-dist (uninstall name all-dists)
            (dolist (dist (all-dists))
              (unless (find (name dist) sources :test #'string= :key #'source-dist-name)
                (message "Removing dist ~S." (name dist))
                (uninstall dist)))))))

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

(defun install-project (object)
  (etypecase object
    ((or symbol string)
     (install-project (asdf:find-system object)))
    (asdf:system
      (install-qlfile (asdf:system-relative-pathname object *default-qlfile*)
                      :quicklisp-home (asdf:system-relative-pathname object *qlot-directory*)))
    (pathname
      (install-qlfile (ensure-qlfile-pathname object)))))

(defun update-project (object &key projects)
  (etypecase object
    ((or symbol string)
     (update-project (asdf:find-system object) :projects projects))
    (asdf:system
      (update-qlfile (asdf:system-relative-pathname object)
                     :quicklisp-home (asdf:system-relative-pathname object *qlot-directory*)
                     :projects projects))
    (pathname
      (update-qlfile (ensure-qlfile-pathname object)
                     :projects projects))))
