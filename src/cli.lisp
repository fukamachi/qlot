(defpackage #:qlot/cli
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors
                #:missing-projects
                #:unnecessary-projects
                #:qlot-directory-not-found
                #:qlot-directory-invalid)
  (:import-from #:qlot/color
                #:*enable-color*
                #:color-text)
  (:import-from #:qlot/utils/cli
                #:exec
                #:which
                #:command-line-arguments
                #:ros-script-p)
  (:import-from #:qlot/utils/project
                #:check-local-quicklisp)
  (:import-from #:qlot/utils
                #:split-with
                #:ensure-list
                #:ensure-cons
                #:starts-with
                #:generate-random-string)
  (:export #:qlot-command
           #:main))
(in-package #:qlot/cli)

(defun error-message (control &rest args)
  (format *error-output*
          (apply #'color-text :red control args))
  (fresh-line *error-output*))

(defun warn-message (control &rest args)
  (format *error-output*
          (apply #'color-text :yellow control args))
  (fresh-line *error-output*))

(defun ensure-package-loaded (package-names)
  (let ((package-names (ensure-list package-names))
        (*standard-output* (make-broadcast-stream))
        (*trace-output* (make-broadcast-stream)))
    (dolist (package-name package-names)
      (check-type package-name keyword)
      (unless (find-package package-name)
        (asdf:load-system package-name)))))

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

(defun print-usage ()
  (format *error-output*
          "~&Usage: ~A COMMAND [ARGS..]

COMMANDS:
    init     Initialize a project to start using Qlot.
    install  Install libraries to './.qlot'.
    update   Update specific libraries and rewrite their versions in 'qlfile.lock'.
    add      Add a new library to qlfile and trigger 'qlot install'.
    remove   Remove specific projects from 'qlfile' and trigger 'qlot install'.
    check    Verify if dependencies are satisfied.
    exec     Invoke the following shell-command with the project local Quicklisp.
    bundle   Bundle project dependencies to './.bundle-libs'.

OPTIONS:
    --no-color
        Don't colorize the output
    --version
        Show the Qlot version
    --help
        Show help

Run 'qlot COMMAND --help' for more information on a subcommand.
"
          "qlot"))

(defun print-version ()
  (format t "~&Qlot ~A~%"
          (asdf:component-version (asdf:find-system :qlot))))

(defun append-load-setup-to-argv (argv)
  (flet ((runtime-option-p (option)
           (and (member option
                        '("--help" "--version" "--core" "--dynamic-space-size" "--control-stack-size" "--tls-limit")
                        :test 'equal)
                t))
         (option-string-p (option)
           (starts-with "--" option)))
    (let ((start-pos
            (position-if (lambda (option)
                           (and (option-string-p option)
                                (not (runtime-option-p option))))
                         argv)))
      (append (subseq argv 0 (and start-pos
                                  (1+ start-pos)))
              (list "--load" ".qlot/setup.lisp")
              (if start-pos
                  (subseq argv (1+ start-pos))
                  nil)))))

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
                (merge-pathnames #P".qlot/")))))
  (let ((path (uiop:ensure-directory-pathname
                (uiop:getenv-pathname "QUICKLISP_HOME"))))
    (unless (uiop:directory-exists-p path)
      (error 'qlot-directory-not-found :path path))
    (unless (uiop:file-exists-p (merge-pathnames "setup.lisp" path))
      (error 'qlot-directory-invalid :path path)))

  ;; Overwrite CL_SOURCE_REGISTRY to the current directory
  (setf (uiop:getenv "CL_SOURCE_REGISTRY")
        (extend-source-registry
         (uiop:getenv "CL_SOURCE_REGISTRY")
         ;; Allow to find Qlot even in the subcommand with recursive 'qlot exec'.
         (uiop:native-namestring (asdf:system-source-directory :qlot)))))

(defmacro do-options ((option argv &optional rest-options) &rest clauses)
  (let ((g-argv (gensym "ARGV")))
    `(loop with ,g-argv = (copy-seq ,argv)
           for ,option = (pop ,g-argv)
              ,@(when rest-options
                  `(for ,rest-options = ,g-argv))
           while ,option
           do (case-equal
                  ,option
                ("--no-color" (setf *enable-color* nil))
                ,@(mapcar (lambda (clause)
                            (destructuring-bind (case-expr &rest body)
                                clause
                              (destructuring-bind (option &optional var)
                                  (ensure-cons case-expr)
                                (assert (or (stringp option)
                                            (eq option 'otherwise)))
                                `(,option
                                  ,@(if var
                                        `((unless ,g-argv
                                            (qlot/errors:ros-command-error "~A requires a value" ,option))
                                          (let ((,var (pop ,g-argv)))
                                            ,@body))
                                        `((progn ,@body)))))))
                          clauses)
                ,@(unless (find 'otherwise clauses
                                :key #'first
                                :test 'eq)
                    `((otherwise (qlot-unknown-option ,option))))))))

(defun qlot-option-debug ()
  (setf qlot/logger:*debug* t))

(defun qlot-unknown-option (option)
  (qlot/errors:ros-command-error "'~A' is unknown option" option))

(defun qlot-command-install (argv)
  (let ((install-deps t)
        (cache nil))
    (do-options (option argv)
      ("--no-deps"
       (setf install-deps nil))
      (("--cache" cache-dir)
       (setf cache cache-dir))
      ("--debug"
       (qlot-option-debug))
      ("--help"
       (format *error-output*
               "~&qlot install - Install libraries to './.qlot'.

SYNOPSIS:
    qlot install [--no-deps] [--cache DIRECTORY]

OPTIONS:
    --no-deps
        Don't install dependencies of all systems from the current directory.
    --cache [directory]
        Keep intermediate files for fast reinstallation.
    --debug
        A flag to enable debug logging.
")
       (uiop:quit -1)))
    (ensure-package-loaded :qlot/install)
    (uiop:symbol-call '#:qlot/install '#:install-project
                      *default-pathname-defaults*
                      :install-deps install-deps
                      :cache-directory (and cache
                                            (uiop:ensure-absolute-pathname
                                             (uiop:ensure-directory-pathname cache)
                                             *default-pathname-defaults*)))))

(defun qlot-command-update (argv)
  (let ((install-deps t)
        (cache nil)
        (projects nil))
    (do-options (option argv)
      (("--project" name)
       (qlot/errors:ros-command-warn "'--project' option is deprecated. Please use 'qlot update <name>' instead.")
       (setf projects
             (append projects
                     (cl:remove ""
                                (split-with #\, name)
                                :test 'equal))))
      ("--no-deps"
       (setf install-deps nil))
      (("--cache" cache-dir)
       (setf cache cache-dir))
      ("--debug"
       (qlot-option-debug))
      ("--help"
       (format *error-output*
               "~&qlot update - Update specific libraries.

SYNOPSIS:
    qlot update [name...] [--no-deps] [--cache DIRECTORY]

OPTIONS:
    --no-deps
        Don't install dependencies of all systems from the current directory.
    --cache [directory]
        Keep intermediate files for fast reinstallation.
    --debug
        A flag to enable debug logging.
")
       (uiop:quit -1))
      (otherwise
       (if (starts-with "--" option)
           (qlot-unknown-option option)
           (setf projects
                 (append projects (list option))))))
    (ensure-package-loaded :qlot/install)
    (uiop:symbol-call '#:qlot/install '#:update-project
                      *default-pathname-defaults*
                      :projects projects
                      :install-deps install-deps
                      :cache-directory (and cache
                                            (uiop:ensure-absolute-pathname
                                             (uiop:ensure-directory-pathname cache)
                                             *default-pathname-defaults*))
                      :quiet (not (null projects)))))

(defun qlot-command-init (argv)
  (flet ((print-init-usage ()
           (format *error-output* "~&qlot init - Initialize a project for Qlot

SYNOPSIS:
    qlot init
")
           (uiop:quit -1)))

    (do-options (option argv)
      ("--help"
       (print-init-usage))
      (otherwise
       (error-message "qlot: extra arguments for 'qlot init'")
       (warn-message "Run 'qlot init --help' to see the usage.")
       (uiop:quit -1))))

  (ensure-package-loaded :qlot/install)
  (let* ((qlfile
           (uiop:symbol-call '#:qlot/install '#:init-project *default-pathname-defaults*))
         (qlfile.lock (make-pathname :type "lock"
                                     :defaults qlfile)))
    (unless (uiop:file-exists-p qlfile.lock)
      (warn-message "Run 'qlot install' to set up the project-local Quicklisp."))))

(defun qlot-command-exec (argv)
  (flet ((print-exec-usage ()
           (format *error-output* "~&qlot exec - Invoke the following shell-command with the project-local Quicklisp

SYNOPSYS:
    qlot exec [shell-args..]

EXAMPLES:
    qlot exec sbcl
    qlot exec ros run

NOTE:
    `qlot exec` only affects `ros`, `sbcl` or Roswell scripts.
    Since it's the same as loading `.qlot/setup.lisp`, you can still use the project-local Quicklisp by loading it, like: `ccl --load .qlot/setup.lisp`.
")
           (uiop:quit -1)))

    (unless argv
      (error-message "qlot: no command given to exec")
      (warn-message "Run 'qlot exec --help' to see the usage.")
      (uiop:quit -1))

    ;; Parse options
    (when (starts-with "--" (first argv))
      (block nil
        (do-options (option argv rest-argv)
          ("--help"
           (print-exec-usage))
          (otherwise
           (when (and (starts-with "--" option)
                      (not (equal "--" option)))
             (qlot-unknown-option option))
           (unless rest-argv
             (error-message "qlot: no command given to exec")
             (warn-message "Run 'qlot exec --help' to see the usage.")
             (uiop:quit -1))
           (setf argv rest-argv)
           (return))))))

  (check-local-quicklisp *default-pathname-defaults*)

  (use-local-quicklisp)

  (let ((command (or (which (first argv))
                     (first argv))))
    (exec
     (cons
      command
      (case-equal (file-namestring command)
        ("ros"
         (rest argv))
        ("sbcl"
         #+ros.init (setf (uiop:getenv "SBCL_HOME") "")
         (append-load-setup-to-argv (rest argv)))
        (otherwise
         (cond
           ((ros-script-p command)
            (rest argv))
           ((starts-with ".qlot/bin/" command)
            (qlot/errors:ros-command-warn "No need to exec scripts in .qlot/bin/.")
            (rest argv))
           (t
            (qlot/errors:ros-command-error "exec must be followed by 'ros' or a Roswell script")))))))))

(defun qlot-command-add (argv)
  (flet ((print-add-usage ()
           (format *error-output* "~&qlot add - Add a new library to qlfile and trigger 'qlot install'.

SYNOPSIS:
    qlot add [name] [arg1, arg2..]
    qlot add [username/repository] [arg1, arg2..]
    qlot add [source] [name] [arg1, arg2..]

DESCRIPTION:
    `qlot add` takes a definition as a line in qlfile syntax with the following extensions:

     * If source type is omitted, Qlot assumes that it's `ql` or `github` source.
       * `qlot add mito` = `qlot add ql mito`
       * `qlot add fukamachi/mito` = `qlot add github fukamachi/mito`
     * Shell options (ex. `--upstream`) is treated as a keyword.
       * `qlot add mito --upstream` = `qlot add mito :upstream`

EXAMPLES:
    qlot add mito
    qlot add mito --upstream
    qlot add fukamachi/mito
    qlot add fukamachi/mito --ref 8c795b
    qlot add ultralisp egao1980-cl-idna
    qlot add git datafly https://github.com/fukamachi/datafly
")
           (uiop:quit -1)))

    (unless argv
      (error-message "qlot: requires a new library information")
      (warn-message "Run 'qlot add --help' to see the usage.")
      (uiop:quit -1))

    ;; Parse options
    (when (starts-with "--" (first argv))
      (block nil
        (do-options (option argv rest-argv)
          ("--help"
           (print-add-usage))
          (otherwise
           (when (and (starts-with "--" option)
                      (not (equal "--" option)))
             (qlot-unknown-option option))
           (unless rest-argv
             (error-message "qlot: requires a new library information")
             (warn-message "Run 'qlot add --help' to see the usage.")
             (uiop:quit -1))
           (setf argv rest-argv)
           (return)))))

    ;; Complete the source type
    (unless (member (first argv)
                    '("dist" "git" "github" "http" "local" "ql" "ultralisp")
                    :test 'equal)
      (setf argv
            (if (find #\/ (first argv) :test 'char=)
                (cons "github" argv)
                (cons "ql" argv)))))

  (setf argv
        (mapcar (lambda (arg)
                  (if (starts-with "--" arg)
                      (format nil ":~A" (subseq arg 2))
                      arg))
                argv))

  (ensure-package-loaded '(:qlot/add :qlot/install))

  (let ((qlfile (symbol-value (intern (string '#:*default-qlfile*) '#:qlot/install)))
        (qlfile.bak (merge-pathnames (format nil "qlfile-~A.bak" (generate-random-string))
                                     (uiop:temporary-directory))))
    (unless (uiop:file-exists-p qlfile)
      (message "Creating ~A" qlfile)
      (with-open-file (out qlfile :if-does-not-exist :create)
        (declare (ignorable out))))
    (uiop:copy-file qlfile qlfile.bak)
    (uiop:symbol-call '#:qlot/add '#:add-project argv qlfile)
    (handler-bind ((error
                     (lambda (e)
                       (declare (ignore e))
                       (uiop:copy-file qlfile.bak qlfile))))
      (uiop:symbol-call '#:qlot/install '#:install-project
                        *default-pathname-defaults*
                        :install-deps nil
                        :quiet t))))

(defun qlot-command-remove (argv)
  (flet ((print-remove-usage ()
           (format *error-output* "~&qlot remove - Remove specific libraries.

SYNOPSIS:
    qlot remove [name...]
")
           (uiop:quit -1)))
    (unless argv
      (error-message "qlot: requires library names to remove")
      (warn-message "Run 'qlot remove --help' to see the usage.")
      (uiop:quit -1))

    ;; Parse options
    (when (starts-with "--" (first argv))
      (block nil
        (do-options (option argv rest-argv)
          ("--help"
           (print-remove-usage))
          (otherwise
           (when (and (starts-with "--" option)
                      (not (equal "--" option)))
             (qlot-unknown-option option))
           (unless rest-argv
             (error-message "qlot: requires library names to remove")
             (warn-message "Run 'qlot remove --help' to see the usage.")
             (uiop:quit -1))
           (setf argv rest-argv)
           (return))))))

  (ensure-package-loaded '(:qlot/add :qlot/install))
  (let ((qlfile (symbol-value (intern (string '#:*default-qlfile*) '#:qlot/install))))
    (when (uiop:file-exists-p qlfile)
      (let ((qlfile.bak (merge-pathnames (format nil "qlfile-~A.bak" (generate-random-string))
                                         (uiop:temporary-directory))))
        (uiop:copy-file qlfile qlfile.bak)
        (let ((removed-projects
                (uiop:symbol-call '#:qlot/add '#:remove-project argv qlfile)))
          (unless removed-projects
            (message "Nothing to remove in '~A'." qlfile)
            (return-from qlot-command-remove))

          (handler-bind ((error
                           (lambda (e)
                             (declare (ignore e))
                             (uiop:copy-file qlfile.bak qlfile))))
            (uiop:symbol-call '#:qlot/install '#:install-project
                              *default-pathname-defaults*
                              :install-deps nil
                              :quiet t)))))))

(defun qlot-command-check (argv)
  (flet ((print-check-usage ()
           (format *error-output* "~&qlot check - Verify if dependencies are installed.

SYNOPSIS:
    qlot check
")
           (uiop:quit -1)))

    ;; Parse options
    (do-options (option argv)
      ("--help"
       (print-check-usage))
      (otherwise
       (error-message "qlot: extra arguments for 'qlot check'")
       (warn-message "Run 'qlot check --help' to see the usage.")
       (uiop:quit -1))))

  (ensure-package-loaded :qlot/install)
  (handler-case
      (uiop:symbol-call '#:qlot/install '#:check-project *default-pathname-defaults*)
    ((or
      missing-projects
      unnecessary-projects) (e)
      (format *error-output* "~&~C[31m~A~C[0m~%" #\Esc e #\Esc)
      (format *error-output* "~C[33mMake it up-to-date with `qlot install`.~:*~C[0m~%" #\Esc)
      (uiop:quit 1))))

(defun qlot-command-bundle (argv)
  (flet ((print-bundle-usage ()
           (format *error-output* "~&qlot bundle - Bundle project dependencies.

SYNOPSIS:
    qlot bundle [[--exclude SYSTEM-NAME]..]

DESCRIPTION:
    This command bundles project dependencies to './.bundle-libs'.
    Load './.bundle-libs/bundle.lisp' to make them available.
    Read https://www.quicklisp.org/beta/bundles.html for the detail.

OPTIONS:
    --exclude <system name>
        Exclude specific system names and those dependencies.
        This option can be specified multiple times.
")
           (uiop:quit -1)))
    (let (exclude)
      (do-options (option argv)
        ("--debug"
         (qlot-option-debug))
        (("--exclude" name)
         (setf exclude
               (append exclude
                       (list name))))
        ("--help"
         (print-bundle-usage))
        (otherwise
         (error-message "qlot: '~A' is unknown option" option)
         (warn-message "Run 'qlot bundle --help' to see the usage.")
         (uiop:quit -1)))

      (ensure-package-loaded :qlot/bundle)
      (uiop:symbol-call '#:qlot/bundle '#:bundle-project
                        *default-pathname-defaults*
                        :exclude exclude))))

(defun qlot-command-toplevel (argv)
  (do-options (option argv)
    ("--version"
     (print-version)
     (uiop:quit -1))
    ("--help"
     (print-usage)
     (uiop:quit -1))
    (otherwise
     (error 'qlot/errors:command-not-found :command option)))
  (print-usage)
  (uiop:quit -1))

(defun qlot-command (&optional $1 &rest argv)
  (let ((*enable-color* t))
    (handler-bind ((qlot/errors:qlot-warning
                     (lambda (c)
                       (format *error-output*
                               "~&~C[33mWARNING: ~A~C[0m~%"
                               #\Esc c #\Esc)
                       (invoke-restart (find-restart 'continue c))))
                   (error
                     (lambda (c)
                       (let ((error.log
                               (uiop:with-temporary-file (:stream out
                                                          :pathname error.log
                                                          :direction :output
                                                          :prefix "qlot-error-"
                                                          :suffix ""
                                                          :type "log"
                                                          :keep t)
                                 (uiop:print-condition-backtrace c :stream out)
                                 error.log)))
                         (error-message "Unexpected error: ~A" c)
                         (message "This can be a bug of Qlot.~%Report it at https://github.com/fukamachi/qlot/issues/new/choose.")
                         (message "Please attach the stack trace dumped to '~A'." error.log))
                       (uiop:quit -1))))
      (handler-case
          (cond ((equal "install" $1)
                 (qlot-command-install argv))
                ((equal "update" $1)
                 (qlot-command-update argv))
                ((equal "init" $1)
                 (qlot-command-init argv))
                ((equal "exec" $1)
                 (qlot-command-exec argv))
                ((equal "add" $1)
                 (qlot-command-add argv))
                ((equal "remove" $1)
                 (qlot-command-remove argv))
                ((equal "check" $1)
                 (qlot-command-check argv))
                ((equal "bundle" $1)
                 (qlot-command-bundle argv))
                ((and $1 (starts-with "--" $1))
                 (qlot-command-toplevel (cons $1 argv)))
                ((null $1)
                 (qlot-command-toplevel nil))
                (t
                 (do-options (option argv)
                   ("--no-color" (setf *enable-color* nil))
                   (otherwise))
                 (error 'qlot/errors:command-not-found :command $1)))
        #+sbcl (sb-sys:interactive-interrupt () (uiop:quit -1 t))
        (qlot/errors:command-not-found (e)
          (error-message (princ-to-string e))
          (print-usage)
          (uiop:quit -1))
        (qlot/errors:qlfile-not-found (e)
          (error-message (princ-to-string e))
          (warn-message "Run 'qlot init' to start using Qlot.")
          (uiop:quit -1))
        ((or qlot/errors:qlfile-lock-not-found
             qlot/errors:qlot-directory-not-found
             qlot/errors:qlot-directory-invalid) (e)
          (error-message (princ-to-string e))
          (warn-message "Run 'qlot install' to set up the project-local Quicklisp.")
          (uiop:quit -1))
        (qlot/errors:qlot-error (e)
          (error-message "qlot: ~A" e)
          (uiop:quit -1))))))

(defun main ()
  (destructuring-bind (&optional $0 $1 &rest argv)
      (command-line-arguments)
    (declare (ignore $0))
    (apply #'qlot-command
           (if (equal $1 "--") ;; Ignore the first '--'
               argv
               (cons $1 argv)))))
