(in-package :cl-user)
(defpackage qlot.install
  (:use :cl)
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
                :url-path-for
                :project.txt
                :install-source)
  (:import-from :qlot.http
                :safety-http-request)
  (:import-from :qlot.shell
                :safety-shell-command)
  (:import-from :qlot.asdf
                :qlot-system
                :system-quicklisp-home)
  (:import-from :qlot.util
                :with-quicklisp-home
                :with-package-functions)
  (:import-from :fad
                :pathname-as-directory
                :pathname-absolute-p
                :pathname-directory-pathname
                :generate-random-string
                :delete-directory-and-files)
  (:import-from :alexandria
                :copy-stream)
  (:export :install-quicklisp
           :install-qlfile
           :find-qlfile
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
  (let ((stream (safety-http-request "http://beta.quicklisp.org/quicklisp.lisp"
                                     :want-stream t))
        (*standard-output* (make-broadcast-stream))
        (quicklisp-file (merge-pathnames (format nil "quicklisp-~A.lisp"
                                                 (fad::generate-random-string))
                                         *tmp-directory*)))
    (ensure-directories-exist *tmp-directory*)
    (with-open-file (out quicklisp-file :direction :output :if-does-not-exist :create)
      (alexandria:copy-stream stream out))

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

(defun install-qlfile (file &key (quicklisp-home #P"quicklisp/"))
  (unless (probe-file file)
    (error "File does not exist: ~A" file))

  (let ((qlhome (canonical-qlhome quicklisp-home (fad:pathname-directory-pathname file))))

    (unless (probe-file qlhome)
      (install-quicklisp qlhome))

    (unless (find-package :ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    (uninstall-all-dists qlhome)

    (let ((*tmp-directory* (fad:pathname-as-directory (merge-pathnames (fad::generate-random-string)
                                                                       (merge-pathnames #P"tmp/qlot/" qlhome))))
          (sources (parse-qlfile file))
          (time (get-universal-time)))
      (start-server sources)
      (with-package-functions :ql-dist (install-dist (setf preference) dist)
        (with-quicklisp-home qlhome
          (dolist (source sources)
            (format t "~&Installing a dist ~A~%" source)
            (install-dist (localhost (url-path-for source 'project.txt)) :prompt nil :replace t)
            (setf (preference (dist (source-dist-name source)))
                  (incf time))
            (install-source source))))
      (stop-server)
      (when (probe-file *tmp-directory*)
        (fad:delete-directory-and-files *tmp-directory*))
      (format t "~&Successfully installed.~%"))))

(defun find-qlfile (object &optional (errorp t))
  (check-type object pathname)
  (let ((qlfile (find-if #'fad:file-exists-p
                         (mapcar
                          (lambda (file) (merge-pathnames file object))
                          '("qlfile.txt" "qlfile")))))
    (when (and (not qlfile) errorp)
      (error "qlfile is not found at ~A" object))

    qlfile))

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
  (:method ((object pathname) &rest args)
    (let ((object (truename object)))
      (if (fad:directory-pathname-p object)
          (apply #'install-project (find-qlfile object) args)
          (apply #'install-qlfile object args)))))
