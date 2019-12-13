(defpackage #:qlot/cli
  (:use #:cl)
  (:export #:install
           #:update
           #:rename-quicklisp-to-dot-qlot
           #:extend-source-registry))
(in-package #:qlot/cli)

(defun install (&optional (object *default-pathname-defaults*) &rest args)
  (let ((*standard-output* (make-broadcast-stream))
        (*trace-output* (make-broadcast-stream)))
    (asdf:load-system :qlot/install))
  (destructuring-bind (&key install-deps) args
    (uiop:symbol-call '#:qlot/install '#:install-project
                      (pathname (or object *default-pathname-defaults*))
                      :install-deps install-deps)))

(defun update (&optional (object *default-pathname-defaults*) &rest args)
  (let ((*standard-output* (make-broadcast-stream))
        (*trace-output* (make-broadcast-stream)))
    (asdf:load-system :qlot/install))
  (destructuring-bind (&key projects install-deps) args
    (uiop:symbol-call '#:qlot/install '#:update-project
                      (pathname (or object *default-pathname-defaults*))
                      :projects projects
                      :install-deps install-deps)))

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
            (t (format nil "~A~A~A"
                       dir-to-add
                       (if (uiop/os:os-windows-p)
                           ";"
                           ":")
                       current-value)))
      dir-to-add))
