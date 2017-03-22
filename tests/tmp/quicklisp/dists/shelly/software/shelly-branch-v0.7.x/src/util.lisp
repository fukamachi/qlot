(in-package :cl-user)
(defpackage shelly.util
  (:use :cl)
  (:import-from :asdf
                :getenv)
  (:import-from :shelly.error
                :shelly-command-not-found-error)
  (:import-from :shelly.impl
                :condition-undefined-function-name)
  (:import-from :cl-fad
                :file-exists-p
                :walk-directory
                :copy-file))
(in-package :shelly.util)

(cl-annot:enable-annot-syntax)

@export
(defun shelly-home ()
  (labels ((ensure-ends-slash (str)
             (if (char= (aref str (1- (length str))) #\/)
                 str
                 (concatenate 'string str "/"))))
    (if (getenv "SHELLY_HOME")
        (pathname (ensure-ends-slash (getenv "SHELLY_HOME")))
        (merge-pathnames ".shelly/" (user-homedir-pathname)))))

@export
(defun shadowing-use-package (packages-to-use &optional (package (find-package :cl-user)))
  (let ((packages-to-use (if (consp packages-to-use)
                             packages-to-use
                             (list packages-to-use))))
    (dolist (package-to-use packages-to-use)
      (do-external-symbols (symbol package-to-use)
        (shadowing-import symbol package)))))

@export
(defun load-systems (systems &key verbose)
  (handler-case (let ((*standard-output* (if verbose
                                             *standard-output*
                                             (make-broadcast-stream))))
                  #+quicklisp (ql:quickload systems :verbose nil :prompt nil)
                  #-quicklisp (dolist (system systems) (asdf:load-system system :verbose nil)))
    (#+quicklisp ql::system-not-found #-quicklisp asdf:missing-component (c)
      (terminate 1 "Error: ~A" c)))
  (let ((packages (remove-if-not #'find-package
                                 (if (consp systems)
                                     systems
                                     (list systems)))))
    (when packages
      (shadowing-use-package packages))))

@export
(defun add-load-path (directories)
  (setf asdf:*central-registry*
        (append (remove-if #'null (mapcar #'probe-file directories))
                asdf:*central-registry*)))

@export
(defun check-version (version)
  (let ((*standard-output* (make-broadcast-stream)))
    (unless (string= version (slot-value (asdf:find-system :shelly)
                                         'asdf:version))
      (format *error-output*
              "~&Warning: different version of Shelly was detected. Try \"shly install\".~%")
      (force-output *error-output*))
    (values)))

@export
(defun local-command-symbols (&optional (package (find-package :cl-user)))
  (let (symbols)
    (do-symbols (symbol package)
      (when (and (eq package (symbol-package symbol)) (fboundp symbol))
        (push symbol symbols)))
    symbols))

(defun load-shlyfile (shlyfile)
  (let ((shlyfile (if (fad:pathname-absolute-p shlyfile)
                      shlyfile
                      #+ccl
                      (merge-pathnames shlyfile (ccl:current-directory))
                      #-ccl
                      (asdf::truenamize shlyfile))))
    (pushnew (directory-namestring shlyfile)
             asdf:*central-registry*)
    (let ((*standard-output* (make-broadcast-stream))
          (*package* (find-package :cl-user)))
      (load shlyfile)
      (import (local-command-symbols) (find-package :cl-user)))))

@export
(defun load-local-shlyfile (&optional shlyfile)
  (when (and shlyfile
             (not (fad:file-exists-p shlyfile)))
    (terminate 1 "Error: No such shlyfile: \"~A\"" shlyfile))

  (let ((shlyfile (or shlyfile
                      (car (member-if #'fad:file-exists-p
                                      '(#P"shlyfile" #P"shlyfile.lisp" #P"shlyfile.cl"))))))
    (when shlyfile
      (load-shlyfile shlyfile))))

@export
(defun load-global-shlyfile ()
  (let ((shlyfile
          (car (member-if #'fad:file-exists-p
                          (mapcar #'(lambda (path)
                                      (merge-pathnames path (shelly-home)))
                                  '(#P"shlyfile" #P "shlyfile.lisp" #P"shlyfile.cl"))))))
    (when shlyfile
      (load-shlyfile shlyfile))))

@export
(defun copy-directory (from to &key overwrite)
  (let ((len (length (namestring (truename from)))))
    (fad:walk-directory
     from
     (lambda (x)
       (fad:copy-file
        x
        (ensure-directories-exist
         (merge-pathnames
          (subseq (namestring x) len)
          to))
        :overwrite overwrite)))))

@export
(defun arglist (fname)
  #+sbcl (require 'sb-introspect)
  #+(or sbcl ccl allegro clisp ecl abcl)
  (handler-case
      #+sbcl (funcall (intern (string :function-arglist) :sb-introspect)
                      fname)
      #+ccl (ccl:arglist fname)
      #+allegro (excl:arglist fname)
      #+clisp (ext:arglist fname)
      #+ecl (multiple-value-bind (arglist foundp)
                (ext:function-lambda-list fname)
              (if foundp arglist :not-available))
      #+abcl (multiple-value-bind (arglist foundp)
                 (sys::arglist fname)
               (if foundp arglist :not-available))
    (undefined-function (c)
      (error 'shelly-command-not-found-error
             :command (condition-undefined-function-name c)))
    (simple-error () :not-available))
  #-(or sbcl ccl allegro clisp ecl abcl) :not-available)

@export
(defun terminate (&optional (status 0) format-string &rest format-arguments)
  (declare (ignorable status))
  (when format-string
    (fresh-line *error-output*)
    (apply #'format *error-output* format-string format-arguments)
    (fresh-line *error-output*))
  #+ccl (ccl:quit status)
  #+sbcl (sb-ext:exit :code status)
  #+allegro (excl:exit status :quiet t)
  #+clisp (ext:quit status)
  #+cmucl (unix:unix-exit status)
  #+ecl (ext:quit status)
  #+abcl (ext:exit :status status)
  #-(or ccl sbcl allegro clisp cmucl ecl abcl) (cl-user::quit))
