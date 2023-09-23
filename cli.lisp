(defpackage #:qlot/cli
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors)
  (:import-from #:qlot/utils/cli
                #:exec
                #:which
                #:command-line-arguments)
  (:import-from #:qlot/utils
                #:generate-random-string)
  (:export #:install
           #:update
           #:add
           #:bundle
           #:main))
(in-package #:qlot/cli)

(defun install (&optional (object *default-pathname-defaults*) &rest args)
  (unless (find-package :qlot/install)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/install)))
  (destructuring-bind (&key install-deps cache) args
    (uiop:symbol-call '#:qlot/install '#:install-project
                      (pathname (or object *default-pathname-defaults*))
                      :install-deps install-deps
                      :cache-directory (and cache
                                            (uiop:ensure-absolute-pathname
                                              (uiop:ensure-directory-pathname cache)
                                              *default-pathname-defaults*)))))

(defun update (&optional (object *default-pathname-defaults*) &rest args)
  (unless (find-package :qlot/install)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/install)))
  (destructuring-bind (&key projects install-deps cache) args
    (uiop:symbol-call '#:qlot/install '#:update-project
                      (pathname (or object *default-pathname-defaults*))
                      :projects projects
                      :install-deps install-deps
                      :cache-directory (and cache
                                            (uiop:ensure-absolute-pathname
                                              (uiop:ensure-directory-pathname cache)
                                              *default-pathname-defaults*)))))

(defun add (args)
  ;; Use 'ql' as the default source
  (when (= 1 (length args))
    (setf args (cons "ql" args)))

  (unless (find-package :qlot/install)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/install)))
  (let ((qlfile (symbol-value (intern (string '#:*default-qlfile*) '#:qlot/install)))
        (qlfile.lock (merge-pathnames (format nil "qlfile-~A.lock" (generate-random-string))
                                      (uiop:temporary-directory))))
    (uiop:copy-file qlfile qlfile.lock)
    (uiop:with-output-file (out qlfile :if-exists :append :if-does-not-exist :create)
      (format out "~&~{~A~^ ~}~%" args)
      (message "Add '~{~A~^ ~}' to '~A'." args qlfile))
    (handler-bind ((error
                     (lambda (e)
                       (declare (ignore e))
                       (uiop:copy-file qlfile.lock qlfile))))
      (install))))

(defun bundle (&optional (project-root *default-pathname-defaults*) &rest args)
  (declare (ignore args))
  (unless (find-package :qlot/bundle)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/bundle)))
  (uiop:symbol-call '#:qlot/bundle '#:bundle-project
                    (uiop:ensure-directory-pathname project-root)))

(defun rename-quicklisp-to-dot-qlot (&optional (pwd *default-pathname-defaults*) enable-color)
  (fresh-line *error-output*)
  (when enable-color
    (format *error-output* "~C[33m" #\Esc))
  (format *error-output*
          "Project local Quicklisp directory has changed from 'quicklisp/' to '.qlot/' since v0.9.13.
See https://github.com/fukamachi/qlot/pull/78 for the details.")
  (when enable-color
    (format *error-output* "~C[0m" #\Esc))
  (fresh-line *error-output*)
  (when (y-or-n-p "Would you like Qlot to migrate?")
    (let ((*default-pathname-defaults* (or pwd
                                           *default-pathname-defaults*)))
      (rename-file #P"quicklisp/" (merge-pathnames #P".qlot/"))
      (when (uiop:directory-exists-p #P".git")
        (uiop:with-output-file (out #P".gitignore" :if-exists :append :if-does-not-exist :create)
          (format out "~&.qlot/~%")))
      t)))

(defun extend-source-registry (current-value dir-to-add)
  "According to ASDF documentation:
https://common-lisp.net/project/asdf/asdf/Shell_002dfriendly-syntax-for-configuration.html
environment variable can contain a string with directories to search systems in,
or a s-exp in DSL, describing sources of systems.

This function modifies such string by adding `dir-to-add' in such
way, so it will have a preference over other system sources specified
in CL_SOURCE_REGISTRY environment variable."
  (if current-value
      (cond ((and (> (length current-value)
                     0)
                  (char= (elt current-value 0)
                         #\())
             ;; If current-value is a lisp form
             (let ((data (read-from-string current-value)))
               (unless (eql (first data)
                            :source-registry)
                 (error "Source registry definition should start with :source-registry keyword"))

               (setf data
                     (append
                      (list :source-registry
                            (list :directory (list dir-to-add)))
                      (rest data)))

               ;; Now serialize it back to a string and return as result
               (write-to-string data
                                ;; We need this flag to keep the form as a one line
                                :pretty nil)))
            ;; When current-value is a string, then just
            ;; add qlot's path to the front
            (t (format nil "~A~C~A"
                       dir-to-add
                       (uiop/filesystem:inter-directory-separator)
                       current-value)))
      dir-to-add))

(defmacro case-equal (keyform &body cases)
  (let ((g-keyform (gensym "KEYFORM")))
    `(let ((,g-keyform ,keyform))
       (cond
         ,@(loop for (case . body) in cases
                 if (eq case 'otherwise)
                   collect `(t ,@body)
                 else
                   collect `((find ,g-keyform ',(if (listp case)
                                                    case
                                                    (list case))
                                   :test #'equal)
                             ,@body))))))

(defun split (div sequence)
  (let ((pos (position div sequence)))
    (if pos
        (list* (subseq sequence 0 pos)
               (split div (subseq sequence (1+ pos))))
        (list sequence))))

(defun print-usage ()
  (format *error-output*
          "~&Usage: ~A COMMAND [ARGS..]

COMMANDS:
    install
        Installs libraries to './.qlot'.

    update
        Makes './.qlot' up-to-date and update 'qlfile.lock'.
        Possible to update specific projects with --project option.
        ex) qlot update --project mito

    add [project name]
    add [source] [project name] [arg1, arg2..]
        Add a new library to qlfile and trigger 'qlot install'. (experimental)
        ex)
          $ qlot add mito       # Add 'ql mito'
          $ qlot add ql mito    # Same as the above
          $ qlot add ultralisp egao1980-cl-idna
          $ qlot add github datafly fukamachi/datafly

    run
        Starts REPL with the project local Quicklisp dists (Same as 'qlot exec ros run').

    exec [shell-args..]
        Invokes the following shell-command with the project local Quicklisp.

    bundle
        Bundles project dependencies to './.bundle-libs'.
        Load './.bundle-libs/bundle.lisp' to make them available.
        Read https://www.quicklisp.org/beta/bundles.html for the detail.

OPTIONS:
    --version
        Show the Qlot version
    --debug
        A flag to enable debug logging. (Only for 'install' or 'update')
    --no-deps
        Don't install dependencies of all systems from the current directory.
    --cache [directory]
        Keep intermediate files for fast reinstallation.
"
          "qlot"))

(defun print-version ()
  (format t "~&Qlot ~A~%"
          (asdf:component-version (asdf:find-system :qlot))))

(defun parse-argv (argv)
  (loop with target = nil
        with projects = '()
        with install-deps = t
        with cache = nil
        for option = (pop argv)
        while option
        do (case-equal option
             ("--project"
              (unless argv
                (qlot/errors:ros-command-error "--project requires a project name"))
              (setf projects
                    (append projects
                            (remove ""
                                    (split #\, (pop argv))
                                    :test 'equal))))
             ("--version"
              (print-version)
              (uiop:quit -1))
             ("--debug"
              (setf qlot/logger:*debug* t))
             ("--no-deps"
              (setf install-deps nil))
             ("--cache"
              (setf cache (pop argv)))
             (otherwise
              (if target
                  (qlot/errors:ros-command-error "'~A' is unknown option" option)
                  (setf target option))))
        finally
           (return
             (append (list target
                           :install-deps install-deps)
                     (when projects
                       (list :projects projects))
                     (when cache
                       (list :cache cache))))))

(defun use-local-quicklisp ()
  ;; Set QUICKLISP_HOME ./.qlot/
  (unless (uiop:getenv "QUICKLISP_HOME")
    (when (and (not (uiop:directory-exists-p #P".qlot/"))
               (uiop:directory-exists-p #P"quicklisp/")
               (uiop:file-exists-p #P"quicklisp/setup.lisp"))
      (rename-quicklisp-to-dot-qlot nil t))
    (setf (uiop:getenv "QUICKLISP_HOME")
          (uiop:native-namestring
            (or (uiop:directory-exists-p ".qlot/")
                (merge-pathnames #P".qlot/" (uiop:getcwd))))))
  (let ((path (uiop:ensure-directory-pathname
                (uiop:getenv-pathname "QUICKLISP_HOME"))))
    (unless (uiop:directory-exists-p path)
      (qlot/errors:ros-command-error "'~A' does not exist." path))
    (unless (uiop:file-exists-p (merge-pathnames "setup.lisp" path))
      (qlot/errors:ros-command-error "Invalid Quicklisp directory: '~A'"
                                     (uiop:getenv "QUICKLISP_HOME"))))

  ;; Overwrite CL_SOURCE_REGISTRY to the current directory
  (setf (uiop:getenv "CL_SOURCE_REGISTRY")
        (extend-source-registry
         (uiop:getenv "CL_SOURCE_REGISTRY")
         ;; Allow to find Qlot even in the subcommand with recursive 'qlot exec'.
         (uiop:native-namestring (asdf:system-source-directory :qlot)))))

(defun qlot-command (&optional $1 &rest argv)
  (handler-case
      (cond ((equal "install" $1)
             (apply #'install (parse-argv argv)))
            ((equal "update" $1)
             (apply #'qlot/cli:update (parse-argv argv)))
            ((equal "exec" $1)
             (use-local-quicklisp)

             (let ((command (or (which (first argv))
                                (first argv))))
               (exec (cons command (rest argv)))))
            ((equal "add" $1)
             (unless argv
               (qlot/errors:ros-command-error "requires a new library information."))
             (add argv))
            ((equal "bundle" $1)
             (apply #'bundle argv))
            ((equal "--version" $1)
             (print-version)
             (uiop:quit -1))
            ((null $1)
             (print-usage)
             (uiop:quit -1))
            (t (error 'qlot/errors:command-not-found :command $1)))
    #+sbcl (sb-sys:interactive-interrupt () (uiop:quit -1 t))
    (qlot/errors:command-not-found (e)
      (format *error-output* "~&~C[31m~A~C[0m~%" #\Esc e #\Esc)
      (print-usage)
      (uiop:quit -1))
    (qlot/errors:qlot-error (e)
      (format *error-output* "~&~C[31mqlot: ~A~C[0m~%" #\Esc e #\Esc)
      (uiop:quit -1))))

(defun main ()
  (destructuring-bind (&optional $0 $1 &rest argv)
      (command-line-arguments)
    (declare (ignore $0))
    (apply #'qlot-command $1 argv)))
