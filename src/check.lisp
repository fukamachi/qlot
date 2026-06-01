(defpackage #:qlot/check
  (:use #:cl)
  (:import-from #:qlot/source
                #:source=
                #:source-project-name
                #:source-dist-name
                #:source-identifier
                #:source-version
                #:source-local
                #:source-asdf
                #:make-source)
  (:import-from #:qlot/parser
                #:find-lock
                #:parse-qlfile
                #:parse-qlfile-lock
                #:read-qlfile-for-install
                #:read-qlfile-for-outdated)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/logger
                #:whisper
                #:clear-whisper)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home
                #:quicklisp-distinfo-url)
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
                #:unnecessary-projects
                #:outdated-projects)
  (:export #:check-qlfile
           #:check-qlfile-lock-current
           #:check-project
           #:available-update-project))
(in-package #:qlot/check)

(defun source-identifier-for-report (source)
  (let ((identifier (source-identifier source)))
    (cond
      ((and (stringp identifier)
            (or (uiop:string-prefix-p "https://" identifier)
                (uiop:string-prefix-p "http://" identifier)))
       (let* ((without-query (subseq identifier 0 (or (position #\? identifier)
                                                       (length identifier))))
              (trimmed (string-right-trim "/" without-query))
              (slash (position #\/ trimmed :from-end t))
              (name (if slash
                        (subseq trimmed (1+ slash))
                        trimmed))
              (dot (position #\. name :from-end t)))
         (if dot
             (subseq name 0 dot)
             name)))
      (t
       identifier))))

(defun source-name-for-report (source)
  (or (source-project-name source)
      (source-dist-name source)
      (source-identifier-for-report source)))

(defun qlfile-sources-for-lock-check (qlfile)
  (let ((default-ql-source (make-source :dist "quicklisp" (quicklisp-distinfo-url)))
        (sources (parse-qlfile qlfile)))
    (unless (find "quicklisp" sources
                  :key #'source-dist-name
                  :test #'string=)
      (push default-ql-source sources))
    sources))

(defun check-qlfile-lock-current (qlfile)
  (unless (uiop:file-exists-p qlfile)
    (error 'qlfile-not-found :path qlfile))
  (let ((qlfile.lock (find-lock qlfile)))
    (unless (uiop:file-exists-p qlfile.lock)
      (error 'qlfile-lock-not-found :path qlfile.lock))
    (let* ((sources (qlfile-sources-for-lock-check qlfile))
           (lock-sources (parse-qlfile-lock qlfile.lock))
           (missing
            (loop for source in sources
                  for lock-source = (find (source-identifier source) lock-sources
                                          :key #'source-identifier
                                          :test #'string=)
                  unless (and lock-source
                              (source= source lock-source))
                  collect (source-name-for-report source)))
           (unnecessary
            (loop for lock-source in lock-sources
                  for source = (find (source-identifier lock-source) sources
                                     :key #'source-identifier
                                     :test #'string=)
                  unless (and source
                              (source= source lock-source))
                  collect (source-name-for-report lock-source))))
      (when missing
        (error 'missing-projects :projects missing))
      (when unnecessary
        (error 'unnecessary-projects :projects unnecessary)))))

(defun check-qlfile (qlfile &key quiet)
  (unless (uiop:file-exists-p qlfile)
    (error 'qlfile-not-found :path qlfile))

  (let* ((sources (read-qlfile-for-install qlfile :silent t))
         (project-root (uiop:pathname-directory-pathname qlfile))
         (qlhome (merge-pathnames *qlot-directory* project-root)))

    (check-qlfile-lock-current qlfile)

    (check-local-quicklisp project-root)

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
                               (typecase source
                                 (source-local
                                  source-registry-up-to-date)
                                 (source-asdf
                                  (let ((asdf-dir (merge-pathnames #P"local-projects/asdf/" qlhome)))
                                    (and (uiop:directory-exists-p asdf-dir)
                                         (let ((version-file (merge-pathnames #P"version.lisp-expr" asdf-dir)))
                                           ;; XXX: Skip if the version file doesn't exist.
                                           (or (not (uiop:file-exists-p version-file))
                                               (ignore-errors
                                                 (equal (uiop:read-file-form version-file)
                                                        (source-version source))))))))
                                 (otherwise
                                  (let ((dist (find-dist (source-dist-name source))))
                                    (and dist
                                         (slot-boundp source 'qlot/source/base::version)
                                         (equal (version dist)
                                                (source-version source)))))))
                             sources)))
            (when old-sources
              (error 'missing-projects
                     :projects (mapcar #'source-project-name old-sources)))))
        (with-package-functions #:ql-dist (all-dists name)
          (let ((extra-dists
                  (append
                   (remove-if (lambda (dist-name)
                                (find dist-name sources :test #'string= :key #'source-dist-name))
                              (mapcar #'name (all-dists)))
                   (and (not (find-if (lambda (source)
                                        (typep source 'source-asdf))
                                      sources))
                        (let ((asdf-dir (merge-pathnames #P"local-projects/asdf/" qlhome)))
                          (and (uiop:directory-exists-p asdf-dir)
                               (list "asdf")))))))
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
     (check-local-quicklisp object)
     (let* ((qlfile (ensure-qlfile-pathname object))
            (sources (read-qlfile-for-outdated qlfile))
            (sources
              (if (null projects)
                  sources
                  (remove-if-not (lambda (source)
                                   (find (source-project-name source) projects
                                         :test 'equal))
                                 sources)))
            (quicklisp-home (local-quicklisp-home object))
            (new-update-projects '()))
       (when projects
         (let ((missing (set-difference projects (mapcar #'source-project-name sources)
                                        :test 'equal)))
           (when missing
             (error 'missing-projects :projects missing))))
       (unless (find-package '#:ql)
         (load (merge-pathnames #P"setup.lisp" quicklisp-home)))
       (with-quicklisp-home quicklisp-home
         (with-tmp-directory (tmp-dir)
           (dolist (source sources)
             (with-package-functions #:ql-dist (find-dist version available-update)
               (let ((dist (find-dist (source-dist-name source))))
                 (if dist
                     (progn
                       (whisper "Checking update of ~S..." (source-project-name source))
                       (with-qlot-server (source :destination tmp-dir
                                                 :distinfo-only t
                                                 :silent t)
                         (let ((new-dist (available-update dist)))
                           (if new-dist
                               (progn
                                 (push (source-project-name source) new-update-projects)
                                 (message "New version of ~S is available.~%  Current: ~A~%  Latest:  ~A"
                                          (source-project-name source)
                                          (version dist)
                                          (version new-dist)))
                               (clear-whisper)))))
                     (message "~S is not installed yet. Skipped."
                              (source-project-name source))))))
           (when new-update-projects
             (error 'outdated-projects
                    :projects (reverse new-update-projects)))))))))
