(in-package :cl-user)
(defpackage shelly
  (:use :cl)
  (:import-from :cl-ppcre
                :split)
  (:import-from :shelly.core
                :run-repl)
  (:import-from :shelly.install
                :install
                :upgrade
                :dump-core
                :local-dump-core
                :rm-core)
  (:import-from :shelly.versions
                :release-versions)
  (:import-from :shelly.util
                :local-command-symbols
                :arglist)
  (:export :run-repl
           :install
           :upgrade
           :dump-core
           :local-dump-core
           :rm-core))
(in-package :shelly)

(cl-annot:enable-annot-syntax)

@export
(defun help (&optional command)
  "Show a list of Built-In Commands.
If `command' is specified, its usage will be displayed."
  (if command
      (let* ((parsed-command (let ((*package* (find-package :cl-user)))
                               (read-from-string command)))
             (arglist (arglist parsed-command)))
        (format t "~&Usage: ~A ~:[()~;~:*~(~A~)~]~{~&    ~A~}~%"
                command
                (if (eq arglist :not-available)
                    ""
                    arglist)
                (ppcre:split "\\n" (documentation parsed-command 'function))))
      (progn
        (format t "~&Built-In Commands:~%")
        (do-external-symbols (symbol :shelly)
          (let ((arglist (arglist symbol)))
            (format t "~&    ~(~A~) ~:[()~;~:*~(~A~)~]~%~{        ~A~^~%~}~2%"
                    symbol
                    (if (eq arglist :not-available)
                        ""
                        arglist)
                    (ppcre:split "\\n" (documentation symbol 'function)))))
        (let ((symbols (local-command-symbols)))
          (when symbols
            (format t "~&Local Commands:~%")
            (dolist (symbol symbols)
              (format t "~&    ~(~A~)~:[~;~:*~%~{        ~A~^~%~}~]~2%"
                      symbol
                      (ppcre:split "\\n" (documentation symbol 'function))))))))
  (values))

@export
(defun available-versions ()
  "Show all the possible Shelly versions."
  (format t "~{~&~A~%~}" (release-versions))
  (values))
