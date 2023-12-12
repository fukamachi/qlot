(defpackage #:qlot
  (:nicknames #:qlot/main)
  (:use #:cl)
  (:import-from #:qlot/add)
  (:import-from #:qlot/bundle
                #:bundle-project)
  (:import-from #:qlot/check)
  (:import-from #:qlot/init
                #:init-project)
  (:import-from #:qlot/install
                #:install-project
                #:update-project
                #:*qlot-directory*)
  (:import-from #:qlot/logger
                #:message
                #:*debug*
                #:*logger-message-stream*
                #:*logger-debug-stream*)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils
                #:pathname-in-directory-p
                #:merge-hash-tables)
  (:import-from #:qlot/utils/shell
                #:*qlot-source-directory*)
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

#-sbcl
(defun install-shell-command (destination &key quicklisp-home)
  (declare (ignore destination quicklisp-home))
  (error "This function is available only for SBCL."))

#+sbcl
(defun install-shell-command (destination &key quicklisp-home)
  (check-type destination (or pathname string))
  (unless (and (typep destination '(or pathname string))
               (uiop:directory-pathname-p destination))
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
        (directory (uiop:ensure-pathname destination)))
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

(defun init (object)
  (init-project object))

(defun install (object)
  (install-project object))

(defun update (object)
  (update-project object))

(defun call-in-local-quicklisp (fn qlhome &key (central-registry '()))
  (unless (uiop:directory-exists-p qlhome)
    (error "Directory ~S does not exist." qlhome))

  (unless (uiop:file-exists-p (merge-pathnames #P"setup.lisp" qlhome))
    (error "~S is not a quicklisp directory." qlhome))

  (unless (find :ql *features*)
    (load (merge-pathnames #P"setup.lisp" qlhome)))

  (progv (list (intern (string '#:*quicklisp-home*) '#:ql)
               (intern (string '#:*local-project-directories*) '#:ql))
      (list qlhome
            (list (merge-pathnames #P"local-projects/" qlhome)))

    (let* ((asdf:*central-registry* central-registry)
           (asdf::*source-registry* (make-hash-table :test 'equal))
           (asdf::*default-source-registries*
             '(asdf::environment-source-registry
                asdf::system-source-registry
                asdf::system-source-registry-directory))
           (original-defined-systems #+asdf3.3 asdf::*registered-systems*
                                     #-asdf3.3 asdf::*defined-systems*)
           (#+asdf3.3 asdf::*registered-systems*
            #-asdf3.3 asdf::*defined-systems* (make-hash-table :test 'equal)))

      ;; Set systems already loaded to prevent reloading the same library in the local Quicklisp.
      (maphash (lambda (name system)
                 (let* ((system-object #+asdf3.3 system #-asdf3.3 (cdr system))
                        (system-path (asdf:system-source-directory system-object)))
                   (when (or (null system-path)
                             (pathname-in-directory-p system-path qlhome)
                             (typep system-object 'asdf:require-system))
                     (setf (gethash name #+asdf3.3 asdf::*registered-systems*
                                    #-asdf3.3 asdf::*defined-systems*) system))))
               original-defined-systems)

      (asdf:initialize-source-registry)

      (multiple-value-prog1 (funcall fn)
        ;; Make all systems that were actually loaded from the local quicklisp
        ;; visible through ASDF outside of the local environment.
        (merge-hash-tables #+asdf3.3 asdf::*registered-systems*
                           #-asdf3.3 asdf::*defined-systems* original-defined-systems)))))

(defun object-to-qlhome (object)
  (etypecase object
    ((or keyword string asdf:system)
     (asdf:system-relative-pathname object *qlot-directory*))
    (pathname
      (merge-pathnames *qlot-directory* (uiop:pathname-directory-pathname object)))))

(defmacro with-local-quicklisp ((object &key central-registry) &body body)
  (let ((g-object (gensym "OBJECT"))
        (g-qlhome (gensym "QLHOME"))
        (asd-specified-p (gensym "ASD-SPECIFIED-P")))
    `(let* ((,g-object ,object)
            (,g-qlhome (object-to-qlhome ,g-object))
            (,asd-specified-p (and (pathnamep ,g-object)
                                   (uiop:file-pathname-p ,g-object)
                                   (equal (pathname-type ,g-object) "asd"))))
       (call-in-local-quicklisp
         (lambda ()
           (when ,asd-specified-p
             (asdf:load-asd ,g-object))
           ,@body)
         ,g-qlhome
         :central-registry (append ,central-registry
                                   (list (asdf:system-source-directory :qlot)))))))

(defun quickload (systems &rest args &key verbose prompt explain &allow-other-keys)
  (declare (ignore verbose prompt explain))
  (let ((systems (if (consp systems)
                     systems
                     (list systems))))
    (dolist (system systems systems)
      (with-local-quicklisp (system)
        (apply #'uiop:symbol-call '#:ql '#:quickload system args)))))

(defun bundle (object)
  (bundle-project object))
