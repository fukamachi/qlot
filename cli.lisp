(defpackage #:qlot/cli
  (:use #:cl)
  (:shadow #:remove)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors)
  (:import-from #:qlot/color
                #:*enable-color*)
  (:import-from #:qlot/utils/cli
                #:exec
                #:which
                #:command-line-arguments
                #:ros-script-p)
  (:import-from #:qlot/utils
                #:starts-with
                #:generate-random-string)
  (:export #:install
           #:update
           #:init
           #:add
           #:remove
           #:bundle
           #:main))
(in-package #:qlot/cli)

(defun install (&optional targets &rest options)
  ;; Requires no targets or a single target
  (unless (or (null targets)
              (not (null (cdr targets))))
    (qlot/errors:ros-command-error "Too many arguments: ~{~A~^, ~}" targets))
  (unless (find-package :qlot/install)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/install)))
  (let ((object (or (first targets)
                    *default-pathname-defaults*)))
    (destructuring-bind (&key install-deps cache) options
      (uiop:symbol-call '#:qlot/install '#:install-project
                        (pathname object)
                        :install-deps install-deps
                        :cache-directory (and cache
                                              (uiop:ensure-absolute-pathname
                                               (uiop:ensure-directory-pathname cache)
                                               *default-pathname-defaults*))))))

(defun update (&optional targets &rest args)
  (unless (find-package :qlot/install)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/install)))
  (destructuring-bind (&key projects install-deps cache) args
    (when projects
      (qlot/errors:ros-command-warn "'--project' option is deprecated. Please use 'qlot update <name>' instead."))
    (uiop:symbol-call '#:qlot/install '#:update-project
                      (pathname *default-pathname-defaults*)
                      :projects (append targets projects)
                      :install-deps install-deps
                      :cache-directory (and cache
                                            (uiop:ensure-absolute-pathname
                                              (uiop:ensure-directory-pathname cache)
                                              *default-pathname-defaults*)))))

(defun init ()
  (unless (find-package :qlot/install)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/install)))
  (uiop:symbol-call '#:qlot/install '#:init-project *default-pathname-defaults*))

(defun add (args)
  ;; Complete the source type
  (unless (member (first args)
                  '("dist" "git" "github" "http" "local" "ql" "ultralisp")
                  :test 'equal)
    (setf args
          (if (find #\/ (first args) :test 'char=)
              (cons "github" args)
              (cons "ql" args))))

  (setf args
        (mapcar (lambda (arg)
                  (if (starts-with "--" arg)
                      (format nil ":~A" (subseq arg 2))
                      arg))
                args))

  (unless (find-package :qlot/add)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/add)))
  (unless (find-package :qlot/install)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/install)))
  (let ((qlfile (symbol-value (intern (string '#:*default-qlfile*) '#:qlot/install)))
        (qlfile.bak (merge-pathnames (format nil "qlfile-~A.bak" (generate-random-string))
                                     (uiop:temporary-directory))))
    (unless (uiop:file-exists-p qlfile)
      (message "Creating ~A" qlfile)
      (with-open-file (out qlfile :if-does-not-exist :create)
        (declare (ignorable out))))
    (uiop:copy-file qlfile qlfile.bak)
    (uiop:symbol-call '#:qlot/add '#:add-project args qlfile)
    (handler-bind ((error
                     (lambda (e)
                       (declare (ignore e))
                       (uiop:copy-file qlfile.bak qlfile))))
      (install))))

(defun remove (targets)
  (unless (find-package :qlot/add)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/add)))
  (unless (find-package :qlot/install)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/install)))
  (let ((qlfile (symbol-value (intern (string '#:*default-qlfile*) '#:qlot/install))))
    (when (uiop:file-exists-p qlfile)
      (let ((qlfile.bak (merge-pathnames (format nil "qlfile-~A.bak" (generate-random-string))
                                         (uiop:temporary-directory))))
        (uiop:copy-file qlfile qlfile.bak)
        (let ((removed-projects
                (uiop:symbol-call '#:qlot/add '#:remove-project targets qlfile)))
          (unless removed-projects
            (message "Nothing to remove in '~A'." qlfile)
            (return-from remove))

          (handler-bind ((error
                           (lambda (e)
                             (declare (ignore e))
                             (uiop:copy-file qlfile.bak qlfile))))
            (install)))))))

(defun bundle (&optional (project-root *default-pathname-defaults*) &rest args)
  (unless (find-package :qlot/bundle)
    (let ((*standard-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream)))
      (asdf:load-system :qlot/bundle)))
  (destructuring-bind (&key exclude) args
    (uiop:symbol-call '#:qlot/bundle '#:bundle-project
                      (uiop:ensure-directory-pathname project-root)
                      :exclude exclude)))

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
    init
        Initialize a project to start using Qlot.

    install
        Installs libraries to './.qlot'.

    update [project name..]
        Update specific projects and rewrite their versions in 'qlfile.lock'.

    add [project name]
    add [source] [project name] [arg1, arg2..]
        Add a new library to qlfile and trigger 'qlot install'. (experimental)
        ex)
          $ qlot add mito       # Add 'ql mito'
          $ qlot add ql mito    # Same as the above
          $ qlot add ultralisp egao1980-cl-idna
          $ qlot add github datafly fukamachi/datafly

    remove [project name..]
        Remove specific projects from 'qlfile' and trigger 'qlot install'.

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
  (loop with targets = nil
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
                            (cl:remove ""
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
              (if (and (<= 2 (length option))
                       (string= option "--" :end1 2))
                  (qlot/errors:ros-command-error "'~A' is unknown option" option)
                  (push option targets))))
        finally
           (return
             (append (list targets
                           :install-deps install-deps)
                     (when projects
                       (list :projects projects))
                     (when cache
                       (list :cache cache))))))

(defun parse-bundle-argv (argv)
  (loop with project-root = nil
        with exclude = '()
        for option = (pop argv)
        while option
        do (case-equal option
             ("--debug"
              (setf qlot/logger:*debug* t))
             ("--exclude"
              (unless argv
                (qlot/errors:ros-command-error "--exclude requires a system name"))
              (push (pop argv) exclude))
             (otherwise
              (if project-root
                  (qlot/errors:ros-command-error "'~A' is invalid argument" option)
                  (setf project-root option))))
        finally
        (return (list (or project-root *default-pathname-defaults*)
                      :exclude exclude))))

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
  (handler-bind ((qlot/errors:qlot-warning
                   (lambda (c)
                     (format *error-output*
                             "~&~C[33mWARNING: ~A~C[0m~%"
                             #\Esc c #\Esc)
                     (invoke-restart (find-restart 'continue c)))))
    (handler-case
        (cond ((equal "install" $1)
               (apply #'install (parse-argv argv)))
              ((equal "update" $1)
               (apply #'qlot/cli:update (parse-argv argv)))
              ((equal "init" $1)
               (qlot/cli:init))
              ((equal "exec" $1)
               (unless argv
                 (qlot/errors:ros-command-error "no command given to exec"))

               (use-local-quicklisp)

               (let ((command (or (which (first argv))
                                  (first argv))))
                 (unless (or (member (file-namestring command)
                                     '("ros")
                                     :test 'equal)
                             (ros-script-p command))
                   (qlot/errors:ros-command-error "exec must be followed by 'ros' or a Roswell script"))
                 (exec (cons command (rest argv)))))
              ((equal "add" $1)
               (unless argv
                 (qlot/errors:ros-command-error "requires a new library information."))
               (add argv))
              ((equal "remove" $1)
               (unless argv
                 (qlot/errors:ros-command-error "requires project names to remove"))
               (remove argv))
              ((equal "bundle" $1)
               (apply #'bundle (parse-bundle-argv argv)))
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
        (uiop:quit -1)))))

(defun main ()
  (destructuring-bind (&optional $0 $1 &rest argv)
      (command-line-arguments)
    (declare (ignore $0))
    (let ((*enable-color* t))
      (apply #'qlot-command
             (if (equal $1 "--") ;; Ignore the first '--'
                 argv
                 (cons $1 argv))))))
