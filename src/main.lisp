(defpackage #:qlot
  (:nicknames #:qlot/main)
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/utils/shell
                #:*qlot-source-directory*)
  (:import-from #:qlot/utils
                #:ensure-package-loaded)
  (:import-from #:qlot/utils/shell
                #:with-env-vars)
  (:import-from #:qlot/config
                #:load-qlot-config)
  #+sbcl
  (:import-from #:sb-posix)
  (:export #:install-shell-command
           #:init
           #:install
           #:update
           #:with-local-quicklisp
           #:quickload
           #:bundle
           #:*proxy*
           #:*debug*
           #:*logger-message-stream*
           #:*logger-debug-stream*))
(in-package #:qlot)

(defvar *project-root* nil)

(defun run-qlot (&rest args)
  (let ((config (load-qlot-config)))
    (destructuring-bind (&key qlot-home setup-file) config
      (let* ((qlot-home (or qlot-home *qlot-source-directory*))
             (setup-file
              (and setup-file
                   (uiop:file-exists-p
                    (merge-pathnames setup-file qlot-home)))))
        (cond
          (setup-file
           (with-env-vars (("QLOT_SETUP_FILE" (uiop:native-namestring setup-file)))
             (uiop:with-current-directory (*project-root*)
               (uiop:run-program `(,(uiop:native-namestring
                                     (merge-pathnames #P"scripts/run.sh" qlot-home))
                                   ,@(mapcar #'princ-to-string args))
                                 :output :interactive
                                 :error-output :interactive))))
          (t
           (ensure-package-loaded :qlot/cli)
           (let ((*default-pathname-defaults* (or *project-root*
                                                  *default-pathname-defaults*)))
             (apply #'uiop:symbol-call '#:qlot/cli '#:qlot-command (mapcar #'princ-to-string args))))))))
  (values))

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
