(defpackage #:qlot-tests/cli
  (:use #:cl #:rove)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:import-from #:qlot/errors
                #:qlfile-lock-not-found)
  (:import-from #:qlot/check
                #:check-project))
(in-package #:qlot-tests/cli)

(defun make-missing-lock-fixture (dir)
  "A project with an (empty) qlfile and a stub .qlot/setup.lisp but NO
   qlfile.lock. check-qlfile signals qlfile-lock-not-found before touching any
   dist, so a stub setup.lisp (enough for check-local-quicklisp) suffices."
  (ensure-directories-exist (merge-pathnames ".qlot/" dir))
  (flet ((touch (name)
           (with-open-file (s (merge-pathnames name dir)
                              :direction :output :if-does-not-exist :create))))
    (touch "qlfile")
    (touch ".qlot/setup.lisp")))

(defun known-system-source-directories ()
  "Absolute source directories of qlot/cli's and qlot/check's full load
   closures, so a child process started with a bare Lisp (no inherited registry)
   can resolve everything exec touches, wherever it lives (a project-local
   install or a global registry). We load qlot/check here too because exec loads
   it lazily and it pulls qlot/http -> dexador; collecting already-loaded
   systems (rather than ASDF's source-registry) is what works when deps are
   resolved by a search function, as under Quicklisp/Roswell."
  (asdf:load-system :qlot/cli)
  (asdf:load-system :qlot/check)
  (let ((dirs '()))
    (flet ((collect (sys)
             ;; Package-inferred sub-systems have no own source file; fall back
             ;; to the primary system's directory (where the .asd lives).
             (when sys
               (let ((dir (asdf:system-source-directory sys)))
                 (when dir
                   (pushnew (namestring dir) dirs :test #'equal))))))
      (dolist (name (asdf:already-loaded-systems))
        (collect (asdf:find-system name nil))
        (collect (asdf:find-system (asdf:primary-system-name name) nil))))
    (nreverse dirs)))

(defun run-exec-subprocess (project-dir inner-cmd)
  "Run qlot-command-exec in a fresh SBCL chdir'd to PROJECT-DIR, with INNER-CMD
   as the command. Returns (values stdout stderr exit-code). A subprocess is
   required because exec ends in execvp, replacing the process image.

   The child's source-registry inherits nothing and points at the source
   directories of every system ASDF can find here, so it resolves qlot/cli (the
   version under test) and its runtime deps without a network.

   When this process runs under Roswell (as it does in CI), the child is
   launched with `ros run' rather than a bare Lisp: qlot's fasls were compiled
   with the ROSWELL package present (e.g. src/utils/shell references it), so a
   bare Lisp cannot load them. Otherwise the child reuses this process's Lisp
   executable, since a bare \"sbcl\" is not necessarily on PATH."
  (let* ((dir-str (namestring project-dir))
         (setup-form
           (format nil
             "(progn (uiop:chdir ~A) ~
                     (setf *default-pathname-defaults* ~
                           (uiop:ensure-directory-pathname ~A)))"
             (prin1-to-string dir-str)
             (prin1-to-string dir-str)))
         (exec-form
           (format nil "(qlot/cli::qlot-command-exec (list ~{~A~^ ~}))"
                   (mapcar #'prin1-to-string inner-cmd)))
         (directory-forms
           (format nil "~{(:directory ~S)~^ ~}"
                   (known-system-source-directories)))
         (src-reg
           (format nil
             "(asdf:initialize-source-registry '(:source-registry ~A :ignore-inherited-configuration))"
             directory-forms))
         (lisp-args (list "--noinform" "--non-interactive"
                          "--eval" "(require :asdf)"
                          "--eval" src-reg
                          "--eval" "(asdf:load-system :qlot/cli)"
                          "--eval" setup-form
                          "--eval" exec-form))
         (command (if (find-package "ROSWELL")
                      (list* "ros" "run" "--" lisp-args)
                      (list* #+sbcl (namestring sb-ext:*runtime-pathname*)
                             #-sbcl "sbcl"
                             lisp-args))))
    (uiop:run-program command
                      :output :string
                      :error-output :string
                      :ignore-error-status t)))

;; check-project still signals on a missing lock; the fix only relaxes exec.
(deftest check-project-signals-on-missing-lock
  (testing "qlot/check:check-project signals qlfile-lock-not-found when qlfile.lock is absent"
    (with-tmp-directory (tmp)
      (make-missing-lock-fixture tmp)
      (ok (signals (check-project tmp) 'qlfile-lock-not-found)
          "check-project must signal qlfile-lock-not-found when qlfile.lock is absent"))))

;; Missing lock with .qlot/ present: exec warns about 'qlot install' but still
;; runs the command. Before the fix it aborted with qlfile-lock-not-found.
(deftest exec-warns-and-continues-when-lock-is-missing
  (with-tmp-directory (tmp)
    (make-missing-lock-fixture tmp)
    (multiple-value-bind (stdout stderr exit-code)
        (run-exec-subprocess tmp (list "sh" "-c" "echo QLOT-MISSING-LOCK-MARKER"))
      (testing "inner command runs to completion (exit 0)"
        (ok (zerop exit-code)
            (format nil "exit code should be 0 (got ~A); stderr: ~A"
                    exit-code (or stderr ""))))
      (testing "inner command output appears on stdout"
        (ok (search "QLOT-MISSING-LOCK-MARKER" (or stdout ""))
            "marker should appear on stdout"))
      (testing "qlot-install advisory is emitted to stderr"
        (ok (search "qlot install" (or stderr ""))
            "stderr should contain 'qlot install' advisory")))))
