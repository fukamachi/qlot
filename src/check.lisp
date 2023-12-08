(defpackage #:qlot/check
  (:use #:cl)
  (:import-from #:qlot/source
                #:source=
                #:source-project-name
                #:source-dist-name
                #:source-version
                #:source-local
                #:defrost-source)
  (:import-from #:qlot/parser
                #:find-lock
                #:parse-qlfile
                #:parse-qlfile-lock
                #:read-qlfile-for-install)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/logger
                #:message
                #:progress
                #:clear-progress)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/utils/qlot
                #:dump-source-registry-conf)
  (:import-from #:qlot/utils/project
                #:check-local-quicklisp
                #:*qlot-directory*
                #:*default-qlfile*
                #:ensure-qlfile-pathname
                #:local-quicklisp-home)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:import-from #:qlot/errors
                #:qlfile-not-found
                #:qlfile-lock-not-found
                #:missing-projects
                #:unnecessary-projects)
  (:export #:check-qlfile
           #:check-project
           #:available-update-project))
(in-package #:qlot/check)

(defun check-qlfile (qlfile &key quiet)
  (unless (uiop:file-exists-p qlfile)
    (error 'qlfile-not-found :path qlfile))

  (let* ((sources (read-qlfile-for-install qlfile :silent t))
         (qlfile.lock (find-lock qlfile))
         (project-root (uiop:pathname-directory-pathname qlfile))
         (qlhome (merge-pathnames *qlot-directory* project-root)))

    (unless (uiop:file-exists-p qlfile.lock)
      (error 'qlfile-lock-not-found :path qlfile.lock))

    (check-local-quicklisp project-root)

    ;; Check if qlfile.lock is up-to-date
    (let* ((lock-sources (parse-qlfile-lock qlfile.lock))
           (lock-sources (mapcar #'defrost-source lock-sources))
           (old-sources
             (remove-if (lambda (source)
                          (let ((lock-source
                                  (find (source-project-name source) lock-sources
                                        :key #'source-project-name
                                        :test #'string=)))
                            (and lock-source
                                 (source= source lock-source))))
                        sources)))
      (when old-sources
        (error 'missing-projects
               :projects (mapcar #'source-project-name old-sources))))

    (unless (find-package '#:ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    ;; Check if all dists are installed and up-to-date
    (let ((source-registry-up-to-date
            (or (not (find-if (lambda (source)
                                (typep source 'source-local))
                              sources))
                (and (uiop:file-exists-p (merge-pathnames #P"source-registry.conf" qlhome))
                     (let ((source-registry-conf
                             (with-output-to-string (s)
                               (dump-source-registry-conf s sources))))
                       (equal source-registry-conf
                              (uiop:read-file-string (merge-pathnames #P"source-registry.conf" qlhome))))))))
      (with-quicklisp-home qlhome
        (with-package-functions #:ql-dist (find-dist version)
          (let ((old-sources
                  (remove-if (lambda (source)
                               (if (typep source 'source-local)
                                   source-registry-up-to-date
                                   (let ((dist (find-dist (source-dist-name source))))
                                     (and dist
                                          (slot-boundp source 'qlot/source/base::version)
                                          (equal (version dist)
                                                 (source-version source))))))
                             sources)))
            (when old-sources
              (error 'missing-projects
                     :projects (mapcar #'source-project-name old-sources)))))
        (with-package-functions #:ql-dist (all-dists name)
          (let ((extra-dists
                  (remove-if (lambda (dist-name)
                               (find dist-name sources :test #'string= :key #'source-dist-name))
                             (mapcar #'name (all-dists)))))
            (when extra-dists
              (error 'unnecessary-projects
                     :projects extra-dists)))))))
  (unless quiet
    (message "Lock file is up-to-date.")))

(defun check-project (object &key quiet)
  (etypecase object
    ((or symbol string)
     (check-project (asdf:find-system object) :quiet quiet))
    (asdf:system
     (check-qlfile (asdf:system-relative-pathname object *default-qlfile*) :quiet quiet))
    (pathname
     (check-qlfile (ensure-qlfile-pathname object) :quiet quiet))))

(defun available-update-project (object &key projects)
  (etypecase object
    ((or symbol string)
     (available-update-project (asdf:find-system object)))
    (asdf:system
     (available-update-project (asdf:system-relative-pathname object *default-qlfile*)))
    (pathname
     (let* ((qlfile (ensure-qlfile-pathname object))
            (sources (parse-qlfile qlfile))
            ;; TODO: Check if the specified project is in qlfile
            (sources
              (if (null projects)
                  sources
                  (remove-if-not (lambda (source)
                                   (find (source-project-name source) projects
                                         :test 'equal))
                                 sources)))
            (quicklisp-home (local-quicklisp-home object))
            (new-update-projects '()))
       (unless (find-package '#:ql)
         (load (merge-pathnames #P"setup.lisp" quicklisp-home)))
       (with-quicklisp-home quicklisp-home
         (with-tmp-directory (tmp-dir)
           (dolist (source sources (reverse new-update-projects))
             (with-package-functions #:ql-dist (find-dist version available-update)
               (let ((dist (find-dist (source-dist-name source))))
                 (if dist
                     (progn
                       (progress "Checking update of ~S..." (source-project-name source))
                       (with-qlot-server (source :destination tmp-dir
                                                 :distinfo-only t
                                                 :silent t)
                         (if (available-update dist)
                             (progn
                               (push (source-project-name source) new-update-projects)
                               (message "New version of ~S is available.~%  Current: ~A~%  Latest:  ~A"
                                        (source-project-name source)
                                        (version dist)
                                        (source-version source)))
                             (clear-progress))))
                     (message "~S is not installed yet. Skipped."
                              (source-project-name source))))))))))))
