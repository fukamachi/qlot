(defpackage #:qlot/utils/asdf
  (:use #:cl)
  (:export #:with-autoload-on-missing
           #:directory-system-files
           #:system-file-systems
           #:with-directory
           #:system-dependencies))
(in-package #:qlot/utils/asdf)

(defun sbcl-contrib-p (name)
  (let ((name (princ-to-string name)))
    (and (<= 3 (length name))
         (string-equal name "sb-" :end1 3))))

(defmacro with-autoload-on-missing (&body body)
  (let ((retrying (gensym))
        (e (gensym)))
    `(let ((,retrying (make-hash-table :test 'equal)))
       (tagbody retry
         (handler-bind ((asdf:missing-component
                          (lambda (,e)
                            (unless (gethash (asdf::missing-requires ,e) ,retrying)
                              (setf (gethash (asdf::missing-requires ,e) ,retrying) t)
                              (asdf:clear-source-registry)
                              #+quicklisp (ql:quickload (asdf::missing-requires ,e) :silent t)
                              #-quicklisp (asdf:load-system (asdf::missing-requires ,e))
                              (go retry)))))
           ,@body)))))

(defun directory-system-files (directory)
  (labels ((asd-file-p (path)
             (and (equal (pathname-type path) "asd")
                  ;; KLUDGE: Ignore skeleton.asd of CL-Project
                  (not (search "skeleton" (pathname-name path)))))
           (collect-asd-files-in-directory (dir)
             (unless (find (car (last (pathname-directory dir)))
                           asdf::*default-source-registry-exclusions*
                           :test #'string=)
               (nconc
                 (mapcar #'truename
                         (remove-if-not #'asd-file-p
                                        (uiop:directory-files dir)))
                 (mapcan #'collect-asd-files-in-directory (uiop:subdirectories dir))))))
    (collect-asd-files-in-directory directory)))

(defvar *dependencies*)

(defun make-hook (old-hook)
  (labels ((dep-list-name (dep)
             (ecase (first dep)
               ((:version :require) (second dep))
               (:feature (third dep))))
           (normalize (dep)
             (real-system-name
              (cond
                ((and (consp dep)
                      (keywordp (car dep)))
                 (let ((name (dep-list-name dep)))
                   (etypecase name
                     ((or symbol string) (string-downcase name))
                     (list (ecase (first name)
                             (:require (normalize (second name))))))))
                ((or (symbolp dep)
                     (stringp dep))
                 (string-downcase dep))
                (t (error "Can't normalize dependency: ~S" dep)))))
           (real-system-name (name)
             (subseq name 0 (position #\/ name))))
    (lambda (fun form env)
      (when (and (consp form)
                 (eq (first form) 'asdf:defsystem))
        (destructuring-bind (system-name &rest system-form) (cdr form)
          (let ((defsystem-depends-on (getf system-form :defsystem-depends-on))
                (depends-on (getf system-form :depends-on))
                (weakly-depends-on (getf system-form :weakly-depends-on))
                (system-name (string-downcase system-name)))
            #+quicklisp
            (when defsystem-depends-on
              (let ((map (make-hash-table :test 'equal)))
                (labels ((get-deps (name)
                           (when (gethash name map)
                             (return-from get-deps nil))
                           (setf (gethash name map) t)
                           (let ((system (ql-dist:find-system name)))
                             (when system
                               (cons system
                                     (loop for system-name in (ql-dist:required-systems system)
                                           append (get-deps system-name)))))))
                  (mapc #'ql-dist:ensure-installed
                        (mapcan #'get-deps
                                (copy-seq defsystem-depends-on))))))
            (setf (gethash system-name *dependencies*)
                  (sort
                   (remove system-name
                           (remove-if #'sbcl-contrib-p
                                      (remove-duplicates
                                       (mapcar #'normalize
                                               (append defsystem-depends-on depends-on weakly-depends-on))
                                       :test #'equalp))
                           :test #'string=)
                   #'string<)))))
      (funcall old-hook fun form env))))

(defun system-file-systems (system-file)
  (handler-bind ((style-warning #'muffle-warning))
    (let ((*macroexpand-hook* (make-hook *macroexpand-hook*))
          (result '()))
      (with-autoload-on-missing
        (asdf:load-asd system-file))
      (asdf:map-systems
       (lambda (system)
         (when (equalp system-file (asdf:system-source-file system))
           (push system result))))
      (nreverse result))))

(defun system-dependencies (system-designator)
  (gethash (asdf:component-name (asdf:find-system system-designator)) *dependencies*))

(defmacro with-directory ((system-file directory &optional result-form) &body body)
  `(let ((*dependencies* (make-hash-table :test 'equal)))
     (dolist (,system-file (directory-system-files ,directory) ,result-form)
       ,@body)))
