(defpackage #:qlot/utils/project
  (:use #:cl)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:with-autoload-on-missing
                #:directory-lisp-files
                #:lisp-file-dependencies
                #:system-class-name)
  (:import-from #:qlot/utils
                #:with-package-functions
                #:starts-with
                #:split-with)
  (:import-from #:qlot/logger
                #:*debug*
                #:whisper
                #:message
                #:debug-log)
  (:import-from #:qlot/errors
                #:qlot-directory-not-found
                #:qlot-directory-invalid)
  (:export #:*qlot-directory*
           #:local-quicklisp-installed-p
           #:check-local-quicklisp
           #:local-quicklisp-home
           #:project-dependencies
           #:ensure-qlfile-pathname))
(in-package #:qlot/utils/project)

(defvar *default-qlfile* #P"qlfile")

(defvar *qlot-directory* #P".qlot/")
(defvar *default-qlfile* #P"qlfile")

(defun compile-exclude-rule (rule)
  (when (< 0 (length rule))
    (let ((rule-parts (split-with #\* rule))
          (starts-with-star
            (char= #\* (aref rule 0)))
          (ends-with-star
            (char= #\* (aref rule (1- (length rule))))))
      (lambda (str)
        (block out
          (when (< 0 (length str))
            (let ((current 0))
              (loop with initial = t
                    for part in rule-parts
                    do (when (or (null initial)
                                 starts-with-star)
                         (let ((pos (search part str :start2 current)))
                           (unless pos
                             (return-from out nil))
                           (setf current pos)))
                       (unless (starts-with part str :start current)
                         (return-from out nil))
                       (incf current (length part))
                       (setf initial nil))
              (if ends-with-star
                  t
                  (= current (length str))))))))))

(defun local-quicklisp-installed-p (project-root)
  (let ((qlhome (merge-pathnames *qlot-directory* project-root)))
    (when (and (uiop:directory-exists-p qlhome)
               (uiop:file-exists-p (merge-pathnames "setup.lisp" qlhome)))
      qlhome)))

(defun check-local-quicklisp (project-root)
  (let ((qlhome (merge-pathnames *qlot-directory* project-root)))
    (unless (uiop:directory-exists-p qlhome)
      (error 'qlot-directory-not-found :path qlhome))

    (unless (uiop:file-exists-p (merge-pathnames #P"setup.lisp" qlhome))
      (error 'qlot-directory-invalid :path qlhome))
    qlhome))

(defun local-quicklisp-home (project-root)
  (let ((project-root (uiop:ensure-directory-pathname project-root)))
    (uiop:ensure-absolute-pathname
      (merge-pathnames *qlot-directory* project-root)
      *default-pathname-defaults*)))

(defun project-dependencies (project-root &key exclude)
  (with-package-functions #:ql-dist (find-system name)
    (let* ((all-dependencies '())
           (pis-already-seen-files '())
           (loaded-asd-files '())
           (project-system-names '())
           (file-counts 0)
           (exclude-functions (mapcar #'compile-exclude-rule exclude))
           (test (and exclude-functions
                      (lambda (system-name)
                        (not (some (lambda (fn)
                                     (funcall fn system-name))
                                   exclude-functions))))))
      (with-directory (system-file system-name dependencies) project-root
        (pushnew system-name project-system-names :test 'equal)
        (when (or (null test)
                  (funcall test system-name))
          (unless (find system-file loaded-asd-files :test 'equal)
            (push system-file loaded-asd-files)
            (whisper "Loading '~A'..." system-file)
            (incf file-counts)
            (let ((errout *error-output*))
              (handler-bind ((error
                               (lambda (e)
                                 (uiop:print-condition-backtrace e :stream errout))))
                (let ((*standard-output* (make-broadcast-stream))
                      (*trace-output* (make-broadcast-stream))
                      (*error-output* (if *debug*
                                          *error-output*
                                          (make-broadcast-stream))))
                  (with-autoload-on-missing
                    (asdf:load-asd system-file))))))
          (block nil
            (let ((system-class-name (system-class-name system-name)))
              (when system-class-name
                (let ((system-class
                        (find-class (uiop:intern* system-class-name '#:asdf/interface) nil)))
                  (unless system-class
                    (warn "Class ~S couldn't be found." system-class-name)
                    (return))
                  (when (subtypep system-class (find-class 'asdf:package-inferred-system))
                    (let* ((lisp-files
                             (set-difference
                              (directory-lisp-files (uiop:pathname-directory-pathname system-file))
                              pis-already-seen-files
                              :test 'equal))
                           (pis-dependencies
                             (loop for file in lisp-files
                                   for (file-deps pkg-name) = (multiple-value-list
                                                               (lisp-file-dependencies file :test test))
                                   when pkg-name
                                   append (progn (debug-log "'~A' requires ~S" pkg-name file-deps)
                                                 file-deps))))
                      (setf dependencies
                            (delete-duplicates
                             (remove-if-not (or test #'identity)
                                            (nconc dependencies pis-dependencies))
                             :test 'equal))
                      (setf pis-already-seen-files
                            (append pis-already-seen-files lisp-files))))))))
          (let ((dependencies (remove-if-not #'find-system dependencies)))
            (debug-log "'~A' requires ~S" system-name dependencies)
            (setf all-dependencies
                  (nconc all-dependencies dependencies)))))
      (message "Loaded ~A system files." file-counts)
      (with-package-functions #:ql-dist (required-systems name)
        (let ((already-seen (make-hash-table :test 'equal)))
          (labels ((find-system-with-fallback (system-name)
                     (or (find-system system-name)
                         (find-system (asdf:primary-system-name system-name))))
                   (system-dependencies (system-name)
                     (unless (gethash system-name already-seen)
                       (setf (gethash system-name already-seen) t)
                       (let ((system (find-system-with-fallback system-name)))
                         (when system
                           (cons system
                                 (mapcan #'system-dependencies (copy-seq (required-systems system)))))))))
            (setf all-dependencies
                  (delete-duplicates
                   (loop for dependency in all-dependencies
                         append (system-dependencies dependency))
                   :key #'name
                   :test 'string=)))))

      (remove-if (lambda (dep)
                   (find (name dep) project-system-names :test 'equal))
                 all-dependencies))))

(defun ensure-qlfile-pathname (object)
  (cond
    ((uiop:file-exists-p object)
     object)
    ((uiop:directory-exists-p (uiop:ensure-directory-pathname object))
     (merge-pathnames *default-qlfile* (uiop:ensure-directory-pathname object)))
    (t object)))
