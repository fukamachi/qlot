(in-package :cl-user)
(defpackage qlot/install
  (:use #:cl)
  (:import-from #:qlot/server
                #:with-qlot-server
                #:localhost)
  (:import-from #:qlot/parser
                #:prepare-qlfile)
  (:import-from #:qlot/tmp
                #:*tmp-directory*)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-dist-name
                #:source-version
                #:source-direct-dependencies
                #:freeze-source
                #:prepare
                #:update-available-p
                #:url-path-for
                #:project.txt

                ;; XXX
                #:*dependencies*
                #:system-file-systems)
  (:import-from #:qlot/shell
                #:safety-shell-command)
  (:import-from #:qlot/util
                #:*system-quicklisp-home*
                #:find-qlfile
                #:with-quicklisp-home
                #:with-local-quicklisp
                #:with-package-functions
                #:with-retrying
                #:*already-seen*
                #:all-required-systems
                #:pathname-in-directory-p
                #:generate-random-string
                #:project-systems)
  (:import-from #:qlot/proxy
                #:get-proxy)
  (:import-from #:cl-ppcre)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:absolute-pathname-p
                #:file-exists-p
                #:directory-files
                #:copy-file
                #:directory-exists-p
                #:directory-pathname-p
                #:pathname-directory-pathname
                #:delete-directory-tree)

  ;; We need to import from these packages explicitly
  ;; because they add make-source methods and these methods
  ;; should be available during 'qlot install' or 'qlot update'
  (:import-from #:qlot/source/ql)
  (:import-from #:qlot/source/git)
  (:import-from #:qlot/source/github)
  (:import-from #:qlot/source/http)

  #+sbcl
  (:import-from #:sb-posix)
  (:export #:install-quicklisp
           #:install-qlfile
           #:install-project))
(in-package #:qlot/install)

(defvar *current-lisp-path*
  (or
   #+ccl (car ccl:*command-line-argument-list*)
   #+sbcl (car sb-ext:*posix-argv*)
   #+allegro (car (system:command-line-arguments))
   #+clisp "clisp"
   #+cmu (car ext:*command-line-strings*)
   #+ecl (car (si:command-args))))

(defun install-quicklisp (&optional (path (merge-pathnames #P"quicklisp/" *default-pathname-defaults*)))
  (format t "~&Installing Quicklisp to ~A ...~%" path)
  (let ((*standard-output* (make-broadcast-stream))
        (quicklisp-file (merge-pathnames (format nil "quicklisp-~A.lisp"
                                                 (generate-random-string))
                                         *tmp-directory*)))
    (ensure-directories-exist *tmp-directory*)
    (progv (list (intern #.(string :*proxy-url*) :ql-http))
        (list (get-proxy))
      (with-package-functions :ql-http (http-fetch)
        (http-fetch "http://beta.quicklisp.org/quicklisp.lisp" quicklisp-file)))

    #+(or ccl sbcl allegro clisp cmu ecl)
    (let ((eval-option (or
                        #+ccl "--eval"
                        #+sbcl "--eval"
                        #+allegro "-e"
                        #+clisp "-x"
                        #+cmu "-eval"
                        #+ecl "-eval")))
      (safety-shell-command *current-lisp-path*

                            (append

                             #+ccl '("--no-init" "--quiet" "--batch")
                             #+sbcl '("--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
                             #+allegro '("--qq")
                             #+clisp '("-norc" "--quiet" "--silent" "-on-error" "exit")
                             #+cmu '("-noinit")
                             #+ecl '("-norc")

                             `(,eval-option
                               ,(prin1-to-string `(load ,quicklisp-file)))

                             `(,eval-option
                               ,(format nil "(quicklisp-quickstart:install :path #P\"~A\" ~@[:proxy \"~A\"~])" path (get-proxy)))

                             `(,eval-option
                               ,(prin1-to-string
                                 (quote
                                  #+ccl (ccl:quit)
                                  #+sbcl (sb-ext:exit)
                                  #+allegro (excl:exit :quiet t)
                                  #+clisp (ext:quit)
                                  #+cmucl (unix:unix-exit)
                                  #+ecl (ext:quit)
                                  #-(or ccl sbcl allegro clisp cmucl ecl) (cl-user::quit)))))))
    #-(or ccl sbcl allegro clisp cmu ecl)
    (progn
      (when (find-package :ql)
        (delete-package :ql))
      (asdf:clear-system :quicklisp)
      (load quicklisp-file)
      (with-package-functions :quicklisp-quickstart (install)
        (let ((proxy (get-proxy)))
          (if proxy
            (install :path path :proxy proxy)
            (install :path path))))))
  T)

(defun uninstall-all-dists (qlhome)
  (with-package-functions :ql-dist (uninstall all-dists)
    (with-quicklisp-home qlhome
      (mapc #'uninstall (all-dists)))))

(defun canonical-qlhome (qlhome &optional (base *default-pathname-defaults*))
  (setf qlhome (uiop:ensure-directory-pathname qlhome))
  (if (uiop:absolute-pathname-p qlhome)
      qlhome
      (merge-pathnames qlhome base)))

(defun install-qlfile (file &key (quicklisp-home #P"quicklisp/"))
  (unless (uiop:file-exists-p file)
    (error "File does not exist: ~A" file))

  (let ((qlhome (canonical-qlhome quicklisp-home (uiop:pathname-directory-pathname file))))

    (unless (uiop:directory-exists-p qlhome)
      (install-quicklisp qlhome))

    (unless (find-package :ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    (apply-qlfile-to-qlhome file qlhome)

    (format t "~&Successfully installed.~%")))

(defun update-qlfile (file &key (quicklisp-home #P"quicklisp/") projects)
  (unless (uiop:file-exists-p file)
    (error "File does not exist: ~A" file))

  (let ((qlhome (canonical-qlhome quicklisp-home (uiop:pathname-directory-pathname file))))

    (unless (uiop:directory-exists-p qlhome)
      (error "~S does not exist." qlhome))

    (unless (find-package :ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    (apply-qlfile-to-qlhome file qlhome :ignore-lock t :projects projects)

    (format t "~&Successfully updated.~%")))

(defun already-installed-p (source)
  (with-package-functions :ql-dist (find-dist)
    (and (find-dist (source-dist-name source))
         T)))

(defun source-update-available-p (source)
  (with-package-functions :ql-dist (find-dist version)
    (let ((dist (find-dist (source-dist-name source))))
      (unless dist
        (return-from source-update-available-p nil))

      (update-available-p source (version dist)))))

(defun install-source (source)
  (with-package-functions :ql-dist (install-dist)
    (format t "~&Installing dist ~S~:[~; version ~:*~S~].~%"
            (source-dist-name source)
            (and (slot-boundp source 'qlot/source::version)
                 (source-version source)))
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (locally
          (declare #+sbcl (sb-ext:muffle-conditions cl:warning))
        (handler-bind ((cl:warning #'muffle-warning))
          (install-dist (localhost (url-path-for source 'project.txt)) :prompt nil :replace nil))))))

(defun update-source (source)
  (with-package-functions :ql-dist (find-dist update-in-place available-update name version uninstall installed-releases distinfo-subscription-url (setf distinfo-subscription-url))
    (let ((dist (find-dist (source-dist-name source))))
      (setf (distinfo-subscription-url dist)
            (ppcre:regex-replace "^http://127\\.0\\.0\\.1:\\d+"
                                 (distinfo-subscription-url dist)
                                 (localhost)))
      (let ((new-dist (available-update dist)))
        (format t "~&Updating dist ~S version ~S -> ~S.~%"
                (name dist)
                (version dist)
                (version new-dist))
        (map nil #'uninstall (installed-releases dist))
        (let ((*trace-output* (make-broadcast-stream)))
          (update-in-place dist new-dist))))))

(defun project-lisp-files (dir)
  (append (uiop:directory-files dir "*.lisp")
          (loop for dir in (uiop:subdirectories dir)
                when (not (member (car (last (pathname-directory dir)))
                                  (append asdf/source-registry:*default-source-registry-exclusions*
                                          (list "quicklisp"))
                                  :test 'equal))
                append (project-lisp-files dir))))

(defun lisp-file-dependencies (file)
  (when (asdf/package-inferred-system::file-defpackage-form file)
    (asdf/package-inferred-system::package-inferred-system-file-dependencies file)))

(defun apply-qlfile-to-qlhome (file qlhome &key ignore-lock projects)
  (let ((*tmp-directory* (uiop:ensure-directory-pathname (merge-pathnames (generate-random-string)
                                                                          (merge-pathnames #P"tmp/qlot/" qlhome))))
        (all-sources (prepare-qlfile file :ignore-lock ignore-lock :projects projects)))

    (with-quicklisp-home qlhome
      (with-qlot-server all-sources
        (flet ((install-all-releases (source)
                 (unless (equal (symbol-name (type-of source))
                                (string :source-ql-all))
                   (let ((*standard-output* (make-broadcast-stream))
                         (*trace-output* (make-broadcast-stream)))
                     (with-package-functions :ql-dist (dist provided-releases ensure-installed base-directory)
                       (let ((releases (provided-releases (dist (source-dist-name source)))))
                         (dolist (release releases)
                           (ensure-installed release)

                           ;; Install Roswell scripts.
                           (let* ((ros-dir (merge-pathnames #P"roswell/" (base-directory release)))
                                  (bin-dir (merge-pathnames #P"bin/" qlhome))
                                  (scripts (uiop:directory-files ros-dir "*.*")))
                             (when scripts
                               (ensure-directories-exist bin-dir)
                               (dolist (script scripts)
                                 (let ((to (make-pathname
                                            :name (pathname-name script)
                                            :defaults bin-dir
                                            :type #+unix (if (equalp (pathname-type script) "ros")
                                                             nil
                                                             (pathname-type script))
                                                  #-unix (pathname-type script))))
                                   (with-open-file (stream to :direction :output
                                                              :if-exists :supersede
                                                              :if-does-not-exist :create)
                                     (format stream "#!/bin/sh
CURRENT=$(dirname $0)
cd \"$CURRENT/../../\"
qlot exec /bin/sh \"$CURRENT/../~A\" \"$@\"
"
                                             (subseq (namestring script)
                                                     (length (namestring qlhome)))))
                                   #+sbcl (sb-posix:chmod to #o700))))))))))))
          (loop for source in all-sources
                for preference from (get-universal-time)
                do (cond
                     ((not (already-installed-p source))
                      (install-source source)
                      (install-all-releases source))
                     ((source-update-available-p source)
                      (prepare source)
                      (if (string= (source-dist-name source) "quicklisp")
                          (with-package-functions :ql-dist (uninstall dist)
                            (uninstall (dist "quicklisp"))
                            (install-source source))
                          (update-source source))
                      (install-all-releases source))
                     (t (format t "~&Already have dist ~S version ~S.~%"
                                (source-dist-name source)
                                (source-version source))))
                   (with-package-functions :ql-dist (dist (setf preference))
                     (setf (preference (dist (source-dist-name source))) preference))))

        (with-package-functions :ql-dist (uninstall name all-dists)
          (let ((sources-map (make-hash-table :test 'equal)))
            (dolist (source all-sources)
              (setf (gethash (source-dist-name source) sources-map) t))
            (dolist (dist (all-dists))
              (unless (gethash (name dist) sources-map)
                (format t "~&Removing dist ~S.~%" (name dist))
                (uninstall dist)))))))

    (with-quicklisp-home qlhome
      (with-open-file (out (merge-pathnames (format nil "~A.lock" (file-namestring file))
                                            file)
                           :direction :output
                           :if-exists :supersede)
        (let ((*print-pretty* nil)
              (*print-case* :downcase))
          (loop for source in all-sources
                for (project-name . contents) = (freeze-source source)
                do (format out "~&(~S .~% (~{~S ~S~^~%  ~}))~%" project-name contents)))))

    ;; Quickload project systems.
    (format t "~&Calculating project dependencies...~%")
    (let* ((project-root (uiop:pathname-directory-pathname file))
           (systems (project-systems project-root))
           (ql:*quickload-verbose* nil))
      (with-package-functions :ql (quickload)
        (with-package-functions :ql-dist (ensure-installed find-system)
          (with-local-quicklisp (qlhome :systems systems)
            (let ((*already-seen* (make-hash-table :test 'equal)))
              (labels ((system-dependencies (system-name)
                         (unless (gethash system-name *already-seen*)
                           (let ((system (with-retrying (asdf:find-system system-name nil))))
                             (cond
                               ((or (null system)
                                    (not (equal (asdf:component-pathname system)
                                                (uiop:pathname-directory-pathname (first systems)))))
                                (cons
                                 system-name
                                 (all-required-systems system-name)))
                               (t
                                ;; Probably the user application's system.
                                ;; Continuing looking for it's dependencies
                                (setf (gethash system-name *already-seen*) t)
                                (mapcan #'system-dependencies
                                        (mapcar #'string-downcase
                                                (asdf::component-sideway-dependencies system))))))))
                       (find-system-with-fallback (system-name)
                         (or (find-system system-name)
                             (find-system (asdf:primary-system-name system-name)))))
                (let ((*dependencies* (make-hash-table :test 'equal)))
                  (let ((*macroexpand-hook* (lambda (&rest args)
                                              (declare (ignore args)))))
                    (mapcan #'system-file-systems systems))
                  (let ((deps '()))
                    (maphash (lambda (system dependencies)
                               (declare (ignore system))
                               (setf deps (append deps dependencies)))
                             *dependencies*)
                    (let ((defsystem-dependencies (delete-if-not #'find-system (delete-duplicates (mapcan #'system-dependencies deps) :test 'equal))))
                      (format t "~&Ensuring ~D defsystem ~:*dependenc~[ies~;y~:;ies~] installed.~%" (length defsystem-dependencies))
                      (when defsystem-dependencies
                        ;; XXX: Re-installing UIOP for preventing errors in case that bundled version is loaded and conflicts.
                        (when (find "uiop" defsystem-dependencies :test 'equal)
                          (unless (ignore-errors (asdf:find-system "uiop"))
                            (asdf:clear-system "uiop")
                            (quickload "uiop" :silent t)))
                        (quickload defsystem-dependencies :silent t)))))
                (let* ((systems (let ((*dependencies* (make-hash-table :test 'equal)))
                                  (mapcan #'system-file-systems systems)))
                       (dependencies
                         (delete-if (lambda (system)
                                      (or (member system systems :key #'asdf:component-name :test #'string-equal)
                                          (not (find-system system))))
                                    (delete-duplicates
                                     (mapcan #'system-dependencies
                                             (mapcar #'asdf:component-name systems))
                                     :test 'equal))))
                  (format t "~&Ensuring ~D ~:*dependenc~[ies~;y~:;ies~] installed.~%" (length dependencies))
                  (mapc #'ensure-installed
                        (mapcar #'find-system-with-fallback dependencies))

                  (when (find-if (lambda (s) (typep s 'asdf:package-inferred-system))
                                 systems)
                    (let ((pis-dependencies
                            (delete-duplicates
                             (mapcan #'system-dependencies
                                     (remove-if (lambda (name)
                                                  (or (find (asdf:primary-system-name name)
                                                            systems
                                                            :key #'asdf:component-name
                                                            :test 'string-equal)
                                                      (not (find-system name))))
                                                (delete-duplicates
                                                 (loop for file in (project-lisp-files project-root)
                                                       append (lisp-file-dependencies file))
                                                 :test 'equal)))
                             :test 'equal)))
                      (format t "~&Ensuring additional ~D ~:*dependenc~[ies~;y~:;ies~] installed.~%"
                              (length pis-dependencies))
                      (mapc #'ensure-installed
                            (mapcar #'find-system-with-fallback pis-dependencies)))))))))))

    #+windows
    (uiop:run-program (list "attrib"
                            "-r" "-h"
                            (format nil "~A*.*" (uiop:native-namestring *tmp-directory*))
                            "/s" "/d")
                      :error-output *error-output*
                      :ignore-error-status t)
    (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)))

(defgeneric install-project (object &rest args)
  (:method ((object symbol) &rest args)
    (apply #'install-project (asdf:find-system object) args))
  (:method ((object string) &rest args)
    (apply #'install-project (asdf:find-system object) args))
  (:method ((object asdf:system) &rest args &key quicklisp-home &allow-other-keys)
    (let ((system-dir (asdf:component-pathname object)))
      (unless quicklisp-home
        (setf args
              (list* :quicklisp-home (asdf:system-relative-pathname object #P"quicklisp/")
                     args)))
      (apply #'install-qlfile
             (find-qlfile system-dir)
             args)))
  (:method ((object pathname) &rest args &key quicklisp-home &allow-other-keys)
    (let* ((object (truename object))
           (dir (uiop:pathname-directory-pathname object)))
      (unless quicklisp-home
        (setf args
              (list* :quicklisp-home (merge-pathnames #P"quicklisp/" dir)
                     args)))
      (if (uiop:directory-pathname-p object)
          (apply #'install-qlfile (find-qlfile object) args)
          (apply #'install-qlfile object args)))))

(defgeneric update-project (object &rest args)
  (:method ((object symbol) &rest args)
    (apply #'update-project (asdf:find-system object) args))
  (:method ((object string) &rest args)
    (apply #'update-project (asdf:find-system object) args))
  (:method ((object asdf:system) &rest args &key quicklisp-home &allow-other-keys)
    (let ((system-dir (asdf:component-pathname object)))
      (unless quicklisp-home
        (setf args
              (list* :quicklisp-home (asdf:system-relative-pathname object #P"quicklisp/")
                     args)))
      (apply #'update-qlfile
             (find-qlfile system-dir :errorp nil)
             args)))
  (:method ((object pathname) &rest args &key quicklisp-home &allow-other-keys)
    (let* ((object (truename object))
           (dir (uiop:pathname-directory-pathname object)))
      (unless quicklisp-home
        (setf args
              (list* :quicklisp-home (merge-pathnames #P"quicklisp/" dir)
                     args)))
      (if (uiop:directory-pathname-p object)
          (apply #'update-qlfile (find-qlfile object) args)
          (apply #'update-qlfile object args)))))
