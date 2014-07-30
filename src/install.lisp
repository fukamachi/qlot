(in-package :cl-user)
(defpackage qlot.install
  (:use :cl
        :iterate)
  (:import-from :qlot.parser
                :parse-qlfile)
  (:import-from :qlot.server
                :localhost
                :start-server
                :stop-server)
  (:import-from :qlot.tmp
                :*tmp-directory*)
  (:import-from :qlot.source
                :source-dist-name
                :freeze-source
                :url-path-for
                :project.txt
                :install-source)
  (:import-from :qlot.http
                :download-file)
  (:import-from :qlot.shell
                :safety-shell-command)
  (:import-from :qlot.asdf
                :qlot-system
                :system-quicklisp-home)
  (:import-from :qlot.util
                :find-qlfile
                :with-quicklisp-home
                :with-package-functions)
  (:import-from :fad
                :pathname-as-directory
                :pathname-absolute-p
                :pathname-directory-pathname
                :generate-random-string
                :delete-directory-and-files)
  (:export :install-quicklisp
           :install-qlfile
           :install-project))
(in-package :qlot.install)

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
                                                 (fad::generate-random-string))
                                         *tmp-directory*)))
    (ensure-directories-exist *tmp-directory*)
    (download-file "http://beta.quicklisp.org/quicklisp.lisp"
                   quicklisp-file)

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
                               ,(format nil "(quicklisp-quickstart:install :path #P\"~A\")" path))

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
        (install :path path))))
  T)

(defun uninstall-all-dists (qlhome)
  (with-package-functions :ql-dist (uninstall all-dists)
    (with-quicklisp-home qlhome
      (mapc #'uninstall (all-dists)))))

(defun canonical-qlhome (qlhome &optional (base *default-pathname-defaults*))
  (setf qlhome (fad:pathname-as-directory qlhome))
  (if (fad:pathname-absolute-p qlhome)
      qlhome
      (merge-pathnames qlhome base)))

(defun install-qlfile (file &key (quicklisp-home #P"quicklisp/") use-snapshot)
  (unless (probe-file file)
    (error "File does not exist: ~A" file))

  (let ((qlhome (canonical-qlhome quicklisp-home (fad:pathname-directory-pathname file))))

    (unless (probe-file qlhome)
      (install-quicklisp qlhome))

    (unless (find-package :ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    (uninstall-all-dists qlhome)
    (apply-qlfile-to-qlhome file qlhome :use-snapshot use-snapshot)
    (format t "~&Successfully installed.~%")))

(defun update-qlfile (file &key (quicklisp-home #P"quicklisp/") use-snapshot)
  (unless (probe-file file)
    (error "File does not exist: ~A" file))

  (let ((qlhome (canonical-qlhome quicklisp-home (fad:pathname-directory-pathname file))))

    (unless (probe-file qlhome)
      (error "~S does not exist." qlhome))

    (unless (find-package :ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    (apply-qlfile-to-qlhome file qlhome :use-snapshot use-snapshot)
    (format t "~&Successfully updated.~%")))

(defun apply-qlfile-to-qlhome (file qlhome &key use-snapshot)
  (let ((*tmp-directory* (fad:pathname-as-directory (merge-pathnames (fad::generate-random-string)
                                                                     (merge-pathnames #P"tmp/qlot/" qlhome))))
        (sources (parse-qlfile file))
        (dists-map (make-hash-table :test 'equal))
        (time (get-universal-time)))
    (start-server sources)
    (with-package-functions :ql-dist (all-dists install-dist uninstall (setf preference) dist name distinfo-subscription-url (setf distinfo-subscription-url))
      (with-package-functions :ql (update-dist)
        (with-quicklisp-home qlhome
          (iter (for dist in (all-dists))
            (setf (gethash (name dist) dists-map) dist))
          (iter (for source in sources)
            (let ((dist (gethash (source-dist-name source) dists-map)))
              (cond
                (dist (remhash (source-dist-name source) dists-map)
                      (setf (distinfo-subscription-url dist)
                            (ppcre:regex-replace "^http://127\\.0\\.0\\.1:\\d+"
                                                 (distinfo-subscription-url dist)
                                                 (localhost)))
                      (update-dist dist :prompt nil))
                (T (install-dist (localhost (url-path-for source 'project.txt)) :prompt nil :replace t)
                   (install-source source))))
            (setf (preference (dist (source-dist-name source)))
                  (incf time)))
          (iter (for (dist-name dist) in-hashtable dists-map)
            (uninstall dist)))))
    (stop-server)

    (unless (or use-snapshot
                (string= (pathname-type file) "snapshot"))
      (with-quicklisp-home qlhome
        (with-open-file (out (merge-pathnames (format nil "~A.snapshot" (pathname-name file))
                                              file)
                             :direction :output
                             :if-exists :supersede)
          (format out "~{~A~&~}" (mapcar #'freeze-source sources)))))

    (when (probe-file *tmp-directory*)
      (fad:delete-directory-and-files *tmp-directory*))))

(defgeneric install-project (object &rest args)
  (:method ((object symbol) &rest args)
    (apply #'install-project (asdf:find-system object) args))
  (:method ((object string) &rest args)
    (apply #'install-project (asdf:find-system object) args))
  (:method ((object qlot-system) &rest args &key quicklisp-home &allow-other-keys)
    (unless quicklisp-home
      (setf args
            (list* :quicklisp-home
                   (system-quicklisp-home object)
                   args)))
    (apply #'install-project (asdf:component-pathname object) args))
  (:method ((object asdf:system) &rest args)
    (apply #'install-project (asdf:component-pathname object) args))
  (:method ((object pathname) &rest args &key use-snapshot &allow-other-keys)
    (let ((object (truename object)))
      (if (fad:directory-pathname-p object)
          (apply #'install-project (find-qlfile object :use-snapshot use-snapshot) args)
          (apply #'install-qlfile object args)))))

(defgeneric update-project (object &rest args)
  (:method ((object symbol) &rest args)
    (apply #'update-project (asdf:find-system object) args))
  (:method ((object string) &rest args)
    (apply #'update-project (asdf:find-system object) args))
  (:method ((object qlot-system) &rest args &key quicklisp-home &allow-other-keys)
    (unless quicklisp-home
      (setf args
            (list* :quicklisp-home
                   (system-quicklisp-home object)
                   args)))
    (apply #'update-project (asdf:component-pathname object) args))
  (:method ((object asdf:system) &rest args)
    (apply #'update-project (asdf:component-pathname object) args))
  (:method ((object pathname) &rest args)
    (let ((object (truename object)))
      (if (fad:directory-pathname-p object)
          (apply #'update-project (find-qlfile object) args)
          (apply #'update-qlfile object args)))))
