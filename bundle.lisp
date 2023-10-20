(defpackage #:qlot/bundle
  (:use #:cl)
  (:import-from #:qlot/utils/project
                #:project-dependencies
                #:local-quicklisp-installed-p
                #:local-quicklisp-home)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/utils
                #:with-package-functions
                #:starts-with
                #:split-with)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:export #:bundle-project))
(in-package #:qlot/bundle)

(defvar *bundle-directory* #P".bundle-libs/")

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

(defun %bundle-project (project-root &key exclude)
  (assert (uiop:absolute-pathname-p project-root))

  (unless (local-quicklisp-installed-p project-root)
    (error 'qlot-simple-error
           :format-control "Local Quicklisp is not installed yet."))

  (let ((quicklisp-home (local-quicklisp-home project-root)))
    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" quicklisp-home)))

    (with-quicklisp-home quicklisp-home
      (let* ((exclude-functions (mapcar #'compile-exclude-rule exclude))
             (dependencies (project-dependencies project-root
                                                 :test
                                                 (lambda (system-name)
                                                   (not (some (lambda (fn)
                                                                (funcall fn system-name))
                                                              exclude-functions)))))
             (dep-releases (with-package-functions #:ql-dist (release name)
                             (delete-duplicates
                              (mapcar #'release dependencies)
                              :key #'name
                              :test 'equal)))
             (bundle-directory (merge-pathnames *bundle-directory* project-root)))
        (when dep-releases
          (message "Bundling ~D ~:*dependenc~[ies~;y~:;ies~]..."
                   (length dep-releases))
          (with-package-functions #:ql-dist (name)
            (let ((dependency-names (mapcar #'name dependencies)))
              (with-package-functions #:ql (bundle-systems)
                (bundle-systems dependency-names
                                :to bundle-directory))))
          (message "Successfully bundled at '~A'." bundle-directory))))))

(defun bundle-project (object &key exclude)
  (etypecase object
    ((or symbol string)
     (bundle-project (asdf:find-system object) :exclude exclude))
    (asdf:system
     (%bundle-project (asdf:system-source-directory object) :exclude exclude))
    (pathname
     (%bundle-project object :exclude exclude))))
