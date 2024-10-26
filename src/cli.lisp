(defpackage #:qlot/cli
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message
                #:debug-log
                #:*logger-message-stream*
                #:*terminal*
                #:clear-whisper)
  (:import-from #:qlot/errors
                #:missing-projects
                #:unnecessary-projects
                #:outdated-projects
                #:qlot-directory-not-found
                #:qlot-directory-invalid)
  (:import-from #:qlot/color
                #:*enable-color*
                #:color-text)
  (:import-from #:qlot/utils/cli
                #:exec
                #:command-line-arguments
                #:no-such-program)
  (:import-from #:qlot/utils/project
                #:*default-qlfile*
                #:find-project-root
                #:check-local-quicklisp)
  (:import-from #:qlot/utils
                #:split-with
                #:ensure-cons
                #:ensure-package-loaded
                #:starts-with
                #:generate-random-string)
  (:export #:qlot-command
           #:main))
(in-package #:qlot/cli)

(defun error-message (control &rest args)
  (princ (apply #'color-text :red control args)
         *error-output*)
  (fresh-line *error-output*))

(defun warn-message (control &rest args)
  (princ (apply #'color-text :yellow control args)
         *error-output*)
  (fresh-line *error-output*))

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
    init      Initialize a project to start using Qlot.
    install   Install libraries to './.qlot'.
    update    Update specific libraries and rewrite their versions in 'qlfile.lock'.
    add       Add a new library to qlfile and trigger 'qlot install'.
    remove    Remove specific projects from 'qlfile' and trigger 'qlot install'.
    check     Verify if dependencies are satisfied.
    outdated  Check available updates of libraries.
    exec      Invoke the following shell-command with the project local Quicklisp.
    bundle    Bundle project dependencies to './.bundle-libs'.

GLOBAL OPTIONS:
    --dir <directory>
        Directory to run the Qlot command
    --no-color
        Don't colorize the output
    --quiet
        Don't output except errors and warnings

TOPLEVEL OPTIONS:
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

(defun append-load-setup-to-sbcl-argv (argv)
  (flet ((runtime-option-p (option)
           (and (member option
                        '("--noinform" "--help" "--version" "--core" "--dynamic-space-size" "--control-stack-size" "--tls-limit")
                        :test 'equal)
                t))
         (option-string-p (option)
           (starts-with "--" option)))
    (let ((start-pos
            (position-if (lambda (option)
                           (and (option-string-p option)
                                (not (runtime-option-p option))))
                         argv)))
      (append (subseq argv 0 start-pos)
              (list "--load" ".qlot/setup.lisp")
              (if start-pos
                  (subseq argv start-pos)
                  nil)))))

(defun append-load-setup-to-allegro-argv (argv)
  (flet ((runtime-option-p (option)
           (and (or (member option '("-Q" "-:" "-I") :test 'equal)
                    (starts-with "+" option))
                t))
         (option-string-p (option)
           (starts-with "-" option)))
    (let ((start-pos
            (position-if (lambda (option)
                           (and (option-string-p option)
                                (not (runtime-option-p option))))
                         argv)))
      (append (subseq argv 0 start-pos)
              (list "-L" ".qlot/setup.lisp")
              (if start-pos
                  (subseq argv start-pos)
                  nil)))))

(defun use-local-quicklisp ()
  ;; Set QUICKLISP_HOME ./.qlot/
  (unless (uiop:getenvp "QUICKLISP_HOME")
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
         (uiop:getenvp "CL_SOURCE_REGISTRY")
         ;; Allow to find Qlot even in the subcommand with recursive 'qlot exec'.
         (uiop:native-namestring (asdf:system-source-directory :qlot)))))

(defun change-directory (dir)
  (let ((dir (uiop:ensure-directory-pathname dir)))
    (unless (uiop:directory-exists-p dir)
      (qlot/errors:ros-command-error "Directory '~A' does not exist." dir))
    (setf *default-pathname-defaults*
          (probe-file dir))))

(defmacro with-project-root (() &body body)
  (let ((project-root (gensym "PROJECT-ROOT")))
    `(let ((,project-root (or (find-project-root)
                              *default-pathname-defaults*)))
       (debug-log "Project root: ~A" ,project-root)
       (uiop:with-current-directory (,project-root)
         ,@body))))

(defmacro do-options ((option argv) &rest clauses)
  (check-type argv symbol)
  `(loop for ,option = (pop ,argv)
         while ,option
         do (case-equal
             ,option
             ("--no-color" (setf *enable-color* nil))
             ("--quiet" (setf *logger-message-stream* (make-broadcast-stream)))
             ("--dir"
              (unless ,argv
                (qlot/errors:ros-command-error "~A requires a value" ,option))
              (change-directory (pop ,argv)))
             ,@(mapcar (lambda (clause)
                         (destructuring-bind (case-expr &rest body)
                             clause
                           (destructuring-bind (option &optional var)
                               (ensure-cons case-expr)
                             (assert (or (stringp option)
                                         (eq option 'otherwise)))
                             `(,option
                               ,@(if var
                                     `((unless ,argv
                                         (qlot/errors:ros-command-error "~A requires a value" ,option))
                                       (let ((,var (pop ,argv)))
                                         ,@body))
                                     `((progn ,@body)))))))
                       clauses)
             ,@(unless (find 'otherwise clauses
                             :key #'first
                             :test 'eq)
                 `((otherwise (qlot-unknown-option ,option)))))))

(defun qlot-option-debug ()
  (setf qlot/logger:*debug* t))

(defun qlot-unknown-option (option)
  (if (starts-with "--" option)
      (qlot/errors:ros-command-error "'~A' is an unknown option" option)
      (qlot/errors:ros-command-error "'~A' is an extra argument" option)))

(defun qlot-command-install (argv)
  (let ((install-deps t)
        (cache nil)
        concurrency)
    (do-options (option argv)
      ("--no-deps"
       (setf install-deps nil))
      (("--cache" cache-dir)
       (setf cache cache-dir))
      (("--jobs" jobs)
       (unless (every (lambda (char)
                        (char<= #\0 char #\9))
                      jobs)
         (qlot/errors:ros-command-error "Invalid option value for --jobs: ~A" jobs))
       (setf concurrency (parse-integer jobs)))
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
    --jobs [concurrency]
        The number of threads to install simultaneously. (Default: 4)
    --debug
        A flag to enable debug logging.
")
       (uiop:quit -1))
      (otherwise
       (error-message "qlot: '~A' is an unknown argument" option)
       (unless (starts-with "--" option)
         (message (color-text :yellow "Did you mean:~%    qlot add ~A" option)))
       (uiop:quit -1)))
    (ensure-package-loaded :qlot/install)
    (uiop:symbol-call '#:qlot/install '#:install-project
                      *default-pathname-defaults*
                      :install-deps install-deps
                      :cache-directory (and cache
                                            (uiop:ensure-absolute-pathname
                                             (uiop:ensure-directory-pathname cache)
                                             *default-pathname-defaults*))
                      :concurrency concurrency)))

(defun qlot-command-update (argv)
  (let ((install-deps t)
        (cache nil)
        (projects nil)
        concurrency)
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
      (("--jobs" jobs)
       (unless (every (lambda (char)
                        (char<= #\0 char #\9))
                      jobs)
         (qlot/errors:ros-command-error "Invalid option value for --jobs: ~A" jobs))
       (setf concurrency (parse-integer jobs)))
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
    --jobs [concurrency]
        The number of threads to install simultaneously. (Default: 4)
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
                      :concurrency concurrency)))

(defun qlot-command-init (argv)
  (flet ((print-init-usage ()
           (format *error-output* "~&qlot init - Initialize a project for Qlot

SYNOPSIS:
    qlot init [--dist NAME-OR-URL]

OPTIONS:
    --dist [<name>|<url>]
        Add a dist to the initial qlfile.
")
           (uiop:quit -1)))

    (let (primary-dist)
      (do-options (option argv)
        ("--help"
         (print-init-usage))
        ("--dist"
         (when primary-dist
           (error-message "qlot: Can't specify multiple --dist")
           (uiop:quit -1))
         (setf primary-dist (pop argv)))
        (otherwise
         (error-message "qlot: extra arguments for 'qlot init'")
         (warn-message "Run 'qlot init --help' to see the usage.")
         (uiop:quit -1)))

      (ensure-package-loaded :qlot/init)
      (multiple-value-bind (qlfile created)
          (uiop:symbol-call '#:qlot/init '#:init-project *default-pathname-defaults*
                            :dist primary-dist)
        (let ((qlfile.lock (make-pathname :type "lock"
                                          :defaults qlfile)))
          (when created
            (message (color-text :green "Successfully initialized!")))
          (unless (uiop:file-exists-p qlfile.lock)
            (message "Run 'qlot install' to set up the project-local Quicklisp.")))))))

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

    ;; Parse options
    (when (and (first argv)
               (starts-with "--" (first argv)))
      (block nil
        (do-options (option argv)
          ("--help"
           (print-exec-usage))
          (otherwise
           (when (and (starts-with "--" option)
                      (not (equal "--" option)))
             (qlot-unknown-option option))
           (unless (equal "--" option)
             (push option argv))
           (return)))))

    (unless argv
      (error-message "qlot: no command given to exec")
      (warn-message "Run 'qlot exec --help' to see the usage.")
      (uiop:quit -1)))

  (check-local-quicklisp *default-pathname-defaults*)

  (ensure-package-loaded :qlot/check)
  (handler-case
      (uiop:symbol-call '#:qlot/check '#:check-project *default-pathname-defaults*
                        :quiet t)
    ((or missing-projects unnecessary-projects) ()
      (qlot/errors:ros-command-warn "Some installed libraries are different from specified versions.~%Run 'qlot install' to fix this problem.")))

  (use-local-quicklisp)

  (let ((command (first argv)))
    (when (starts-with ".qlot/bin/" command)
      (qlot/errors:ros-command-warn "No need to exec scripts in .qlot/bin/."))
    #+ros.init (setf (uiop:getenv "SBCL_HOME") "")

    (setf (rest argv)
          (case-equal (pathname-name command)
            ("sbcl"
             (append-load-setup-to-sbcl-argv (rest argv)))
            (("ccl" "ecl" "abcl" "clasp")
             (append (list "--load" ".qlot/setup.lisp")
                     (rest argv)))
            ("clisp"
             (append (list "-i" ".qlot/setup.lisp")
                     (rest argv)))
            (("allegro" "alisp")
             (append-load-setup-to-allegro-argv (rest argv)))
            (otherwise (rest argv))))

    (handler-case
        (exec argv)
      (no-such-program (e)
        (error-message (princ-to-string e))
        (uiop:quit -1)))))

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

OPTIONS:
    --no-install
        Don't invoke an installation after adding
")
           (uiop:quit -1)))

    (let (no-install)
      ;; Parse options
      (when (and (first argv)
                 (starts-with "--" (first argv)))
        (block nil
          (do-options (option argv)
            ("--help"
             (print-add-usage))
            ("--debug"
             (qlot-option-debug))
            ("--no-install"
             (setf no-install t))
            (otherwise
             (when (and (starts-with "--" option)
                        (not (equal "--" option)))
               (qlot-unknown-option option))
             (unless (equal "--" option)
               (push option argv))
             (return)))))

      (unless argv
        (error-message "qlot: requires a new library information")
        (warn-message "Run 'qlot add --help' to see the usage.")
        (uiop:quit -1))

      ;; Complete the source type
      (unless (member (first argv)
                      '("dist" "git" "github" "http" "local" "ql" "ultralisp")
                      :test 'equal)
        (setf argv
              (if (find #\/ (first argv) :test 'char=)
                  (cons "github" argv)
                  (cons "ql" argv))))

      (setf argv
            (mapcar (lambda (arg)
                      (if (starts-with "--" arg)
                          (format nil ":~A" (subseq arg 2))
                          arg))
                    argv))

      (ensure-package-loaded '(:qlot/add :qlot/install))
      (let ((qlfile *default-qlfile*)
            (qlfile.bak (merge-pathnames (format nil "qlfile-~A.bak" (generate-random-string))
                                         (uiop:temporary-directory))))
        (unless (uiop:file-exists-p qlfile)
          (message "Creating ~A" qlfile)
          (with-open-file (out qlfile :if-does-not-exist :create)
            (declare (ignorable out))))
        (uiop:copy-file qlfile qlfile.bak)
        (uiop:symbol-call '#:qlot/add '#:add-project argv qlfile)
        (unless no-install
          (handler-bind ((error
                           (lambda (e)
                             (declare (ignore e))
                             (uiop:copy-file qlfile.bak qlfile))))
            (uiop:symbol-call '#:qlot/install '#:install-project *default-pathname-defaults*
                              :install-deps nil)))))))

(defun qlot-command-remove (argv)
  (flet ((print-remove-usage ()
           (format *error-output* "~&qlot remove - Remove specific libraries.

SYNOPSIS:
    qlot remove [name...]

OPTIONS:
    --no-install
        Don't invoke an installation after removal
")
           (uiop:quit -1)))

    (let (names no-install)
      ;; Parse options
      (block nil
        (do-options (option argv)
          ("--help"
           (print-remove-usage))
          ("--debug"
           (qlot-option-debug))
          ("--no-install"
           (setf no-install t))
          (otherwise
           (when (equal "--" option)
             (setf names
                   (append names argv))
             (return))
           (when (starts-with "--" option)
             (qlot-unknown-option option))
           (setf names
                 (append names (list option))))))

      (unless names
        (error-message "qlot: requires library names to remove")
        (warn-message "Run 'qlot remove --help' to see the usage.")
        (uiop:quit -1))

      (ensure-package-loaded '(:qlot/add :qlot/install))
      (let ((qlfile *default-qlfile*))
        (when (uiop:file-exists-p qlfile)
          (let ((qlfile.bak (merge-pathnames (format nil "qlfile-~A.bak" (generate-random-string))
                                             (uiop:temporary-directory))))
            (uiop:copy-file qlfile qlfile.bak)
            (let ((removed-projects
                    (uiop:symbol-call '#:qlot/add '#:remove-project names qlfile)))
              (unless removed-projects
                (message "Nothing to remove in '~A'." qlfile)
                (return-from qlot-command-remove))

              (unless no-install
                (handler-bind ((error
                                 (lambda (e)
                                   (declare (ignore e))
                                   (uiop:copy-file qlfile.bak qlfile))))
                  (uiop:symbol-call '#:qlot/install '#:install-project
                                    *default-pathname-defaults*
                                    :install-deps nil))))))))))

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
      ("--debug"
       (qlot-option-debug))
      (otherwise
       (error-message "qlot: extra arguments for 'qlot check'")
       (warn-message "Run 'qlot check --help' to see the usage.")
       (uiop:quit -1))))

  (ensure-package-loaded :qlot/check)
  (uiop:symbol-call '#:qlot/check '#:check-project *default-pathname-defaults*))

(defun qlot-command-outdated (argv)
  (flet ((print-outdated-usage ()
           (format *error-output* "~&qlot outdated - Check available updates of libraries.

SYNOPSIS:
    qlot outdated [name...]
")
           (uiop:quit -1)))
    (let (projects)
      (do-options (option argv)
        ("--help"
         (print-outdated-usage))
        ("--debug"
         (qlot-option-debug))
        (otherwise
         (when (and (starts-with "--" option)
                    (not (equal "--" option)))
           (qlot-unknown-option option))
         (unless (equal "--" option)
           (push option projects))))

      (ensure-package-loaded :qlot/check)
      (uiop:symbol-call '#:qlot/check '#:available-update-project *default-pathname-defaults*
                        :projects projects))))

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

(defun %qlot-command (command &rest argv)
  (if (equal "init" command)
      (qlot-command-init argv)
      (with-project-root ()
        (cond ((equal "install" command)
               (qlot-command-install argv))
              ((equal "update" command)
               (qlot-command-update argv))
              ((equal "exec" command)
               (qlot-command-exec argv))
              ((equal "add" command)
               (qlot-command-add argv))
              ((equal "remove" command)
               (qlot-command-remove argv))
              ((equal "check" command)
               (qlot-command-check argv))
              ((equal "outdated" command)
               (qlot-command-outdated argv))
              ((equal "bundle" command)
               (qlot-command-bundle argv))
              ((and command (starts-with "--" command))
               (qlot-command-toplevel (cons command argv)))
              ((null command)
               (qlot-command-toplevel nil))
              (t
               (do-options (option argv)
                 ("--no-color" (setf *enable-color* nil))
                 (otherwise))
               (error 'qlot/errors:command-not-found :command command))))))

(defun qlot-command (&optional $1 &rest argv)
  (let* ((no-terminal-env (uiop:getenvp "QLOT_NO_TERMINAL"))
         (*terminal* (or (not no-terminal-env)
                         (uiop:getenvp "CI")))
         (*enable-color* *terminal*))
    (handler-bind ((qlot/errors:qlot-warning
                     (lambda (c)
                       (warn-message "WARNING: ~A" c)
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
                         (fresh-line)
                         (error-message "Unexpected error: ~A" c)
                         (message "This could be a bug in Qlot.~%Report it at https://github.com/fukamachi/qlot/issues/new/choose.")
                         (message "Please attach the stack trace dumped to '~A'." error.log))
                       (uiop:quit -1))))
      (handler-case
          (apply #'%qlot-command $1 argv)
        #+(or sbcl ecl clasp)
        (#+sbcl sb-sys:interactive-interrupt
         #+(or ecl clasp) ext:interactive-interrupt
          ()
          (clear-whisper)
          (uiop:quit -1 nil))
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
        ((or qlot/errors:missing-projects
             qlot/errors:unnecessary-projects) (e)
          (message (color-text :red (princ-to-string e)))
          (message (color-text :yellow "Make it up-to-date with `qlot install`."))
          (uiop:quit 1))
        (qlot/errors:outdated-projects ()
          (uiop:quit 1))
        (qlot/errors:qlot-error (e)
          (error-message "qlot: ~A" e)
          (uiop:quit -1))))))

(defun main ()
  (let ((argv (command-line-arguments)))
    (apply #'qlot-command
           (if (equal (first argv) "--") ;; Ignore the first '--'
               (rest argv)
               argv)))
  (uiop:quit))
