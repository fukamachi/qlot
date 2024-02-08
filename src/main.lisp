(defpackage #:qlot
  (:nicknames #:qlot/main)
  (:use #:cl)
  (:shadow #:remove)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/utils/shell
                #:*qlot-source-directory*)
  (:import-from #:qlot/utils
                #:ensure-package-loaded
                #:ensure-list)
  (:import-from #:qlot/utils/shell
                #:with-env-vars)
  (:import-from #:qlot/config
                #:load-qlot-config
                #:make-config)
  #+sbcl
  (:import-from #:sb-posix)
  (:export #:install-shell-command
           #:init
           #:install
           #:update
           #:bundle
           #:add
           #:remove))
(in-package #:qlot)

(defvar *project-root* nil)

(defun run-qlot-in-child-process (&rest args)
  (let* ((quicklisp-home (symbol-value (uiop:intern* '#:*quicklisp-home* '#:ql)))
         (config (or (load-qlot-config quicklisp-home)
                     (make-config))))
    (destructuring-bind (&key qlot-source-directory setup-file &allow-other-keys)
        config
      (assert (and qlot-source-directory setup-file))
      (let ((setup-file (merge-pathnames setup-file qlot-source-directory)))
        (unless (uiop:file-exists-p setup-file)
          (error "Failed to run Qlot"))
        (with-env-vars (("QLOT_SETUP_FILE" (uiop:native-namestring setup-file))
                        ("QLOT_NO_TERMINAL" "1"))
          (uiop:with-current-directory ((or *project-root*
                                            (uiop:pathname-parent-directory-pathname quicklisp-home)))
            (uiop:run-program `(,(uiop:native-namestring
                                  (merge-pathnames #P"scripts/run.sh" qlot-source-directory))
                                ,@(mapcar #'princ-to-string args))
                              :output :interactive
                              :error-output :interactive)))))))

(defun run-qlot-in-main-process (&rest args)
  (ensure-package-loaded :qlot/cli)
  (let ((*default-pathname-defaults* (or *project-root*
                                         *default-pathname-defaults*)))
    (with-env-vars (("QLOT_NO_TERMINAL" "1"))
      (apply #'uiop:symbol-call '#:qlot/cli '#:qlot-command (mapcar #'princ-to-string args)))))

(defun run-qlot (&rest args)
  (apply
   (if (find :qlot.project *features*)
       #'run-qlot-in-child-process
       #'run-qlot-in-main-process)
   args)
  (values))

(defun init (&key dist)
  (apply #'run-qlot "init"
         (if dist
             (list "--dist" dist)
             nil)))

(defun install (&key no-deps cache jobs init)
  (check-type jobs (or null (integer 1)))
  (apply #'run-qlot "install"
         (append
          (and no-deps '("--no-deps"))
          (and cache `("--cache" ,(uiop:native-namestring cache)))
          (and jobs `("--jobs" ,jobs))
          (and init '("--init")))))

(defun project-name-p (value)
  (stringp value))

(defun project-name-list-p (value)
  (and (consp value)
       (every #'project-name-p value)))

(deftype project-name () 'string)
(deftype project-name-list () '(satisfies project-name-list-p))

(defun update (projects &key no-deps cache jobs)
  (check-type projects (or project-name project-name-list))
  (check-type jobs (or null (integer 1)))
  (apply #'run-qlot "update"
         (append
          (ensure-list projects)
          (and no-deps '("--no-deps"))
          (and cache `("--cache" ,(uiop:native-namestring cache)))
          (and jobs `("--jobs" ,jobs)))))

(defun bundle (&key exclude)
  (check-type exclude (or null project-name project-name-list))
  (let ((exclude (ensure-list exclude)))
    (apply #'run-qlot "bundle"
           (loop for project in exclude
                 append (list "--exclude" project)))))

(defun add (name &rest args &key from no-install &allow-other-keys)
  (check-type name string)
  (unless from
    (setf from (if (find #\/ name :test 'char=)
                   "github"
                   "ql")))
  (setf args
        (loop for (k v) on args by #'cddr
              unless (member k '(:no-install :from))
              append (list k v)))
  (apply #'run-qlot "add"
         (append
          (and no-install '("--no-install"))
          '("--")
          (list from name)
          args)))

(defun remove (name-or-names &key no-install)
  (check-type name-or-names (or null project-name project-name-list))
  (let ((names (ensure-list name-or-names)))
    (apply #'run-qlot "remove"
           (append names
                   (and no-install '("--no-install"))))))

;; TODO: check, outdated

#-sbcl
(defun install-shell-command (destination &key quicklisp-home)
  (declare (ignore destination quicklisp-home))
  (error "This function is available only for SBCL."))

#+sbcl
(defun install-shell-command (destination &key quicklisp-home)
  (unless (typep destination '(or pathname string))
    (error "Requires a directory pathname but given ~S." destination))
  ;; Find a setup file
  (let ((setup-file
          (cond
            (quicklisp-home
             (or (and (uiop:directory-pathname-p quicklisp-home)
                      (uiop:file-exists-p (merge-pathnames "setup.lisp" quicklisp-home)))
                 (error "Invalid Quicklisp home: ~A" quicklisp-home)))
            ((uiop:file-exists-p
              (merge-pathnames ".bundle-libs/bundle.lisp"
                               *qlot-source-directory*)))
            ((uiop:file-exists-p
              (merge-pathnames ".qlot/setup.lisp"
                               *qlot-source-directory*)))
            ((find-package '#:ql)
             (or (uiop:file-exists-p (merge-pathnames "setup.lisp" (symbol-value (intern (string '#:*quicklisp-home*) '#:ql))))
                 (error "Invalid Quicklisp home")))
            (t
             (error "Requires Quicklisp to install, but it's not loaded and :quicklisp-home isn't given."))))
        (directory (uiop:ensure-directory-pathname destination)))
    (ensure-directories-exist directory)
    (let ((qlot-path (merge-pathnames "qlot" directory)))
      (message "Installing a shell command to '~A'." qlot-path)
      (with-open-file (out qlot-path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out "#!/bin/sh
export QLOT_SETUP_FILE=~A
exec ~Ascripts/run.sh \"$@\"~%"
                setup-file
                *qlot-source-directory*))
      #+sbcl (sb-posix:chmod qlot-path #o755)
      (message "Successfully installed!")))
  (values))
