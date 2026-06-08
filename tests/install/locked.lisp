(defpackage #:qlot-tests/install/locked
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install
                #:install-qlfile)
  (:import-from #:qlot/modes
                #:*locked*)
  (:import-from #:qlot/errors
                #:missing-projects
                #:unnecessary-projects)
  (:import-from #:qlot/check
                #:check-qlfile-lock-current)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/install/locked)

(defun write-text-file (path content)
  (ensure-directories-exist path)
  (with-open-file (out path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (write-string content out)))

(defun file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

;;; A minimal lock entry for the implicit quicklisp source that
;;; qlfile-sources-for-lock-check prepends when the qlfile has no
;;; explicit quicklisp/dist entry.
;;;
;;; qlfile-sources-for-lock-check builds:
;;;   (make-source :dist "quicklisp" (quicklisp-distinfo-url))
;;;   → source-dist with :distribution "https://beta.quicklisp.org/dist/quicklisp.txt"
;;;              and :%version :latest
;;; source= for source-dist-project compares %version (both :latest) and
;;; distribution URLs (both https://…).  The lock entry below matches.
(defparameter *ql-lock-entry*
  "(\"quicklisp\" . (:class qlot.source.dist:source-dist :initargs (:project-name \"quicklisp\" :%version :latest :distribution \"https://beta.quicklisp.org/dist/quicklisp.txt\") :version \"2018-02-28\"))")

;;; *locked* + qlfile declares a dep absent from lock → missing-projects.
;;;
;;; check-qlfile-lock-current must signal missing-projects naming the new project,
;;; and the lock file must be unchanged on disk.
(deftest locked-missing-project-signals-error
  (with-tmp-directory (tmp)
    (let ((qlfile  (merge-pathnames #P"qlfile"  tmp))
          (lockfile (merge-pathnames #P"qlfile.lock" tmp))
          (qlhome  (merge-pathnames #P".qlot/" tmp)))
      ;; qlfile adds a project that the lock does not know about
      (write-text-file qlfile "ql new-absent-project :latest")
      ;; lock has only the implicit quicklisp — new-absent-project is missing
      (write-text-file lockfile *ql-lock-entry*)
      (let ((lock-bytes-before (file-bytes lockfile)))
        (let ((caught (nth-value 1
                        (ignore-errors
                          (let ((*locked* t))
                            (install-qlfile qlfile :quicklisp-home qlhome))))))
          (ok (typep caught 'missing-projects)
              "locked install with new qlfile dep must signal missing-projects")
          (when (typep caught 'missing-projects)
            (let ((report (with-output-to-string (s) (princ caught s))))
              (ok (search "new-absent-project" report :test #'char-equal)
                  "missing-projects report must name the absent project")))
          (ok (equalp lock-bytes-before (file-bytes lockfile))
              "qlfile.lock must be unchanged after missing-projects"))))))

;;; *locked* + lock has a dep removed from qlfile → unnecessary-projects.
;;;
;;; check-qlfile-lock-current must signal unnecessary-projects naming the removed
;;; project, and the lock file must be unchanged.
(deftest locked-unnecessary-project-signals-error
  (with-tmp-directory (tmp)
    (let ((qlfile  (merge-pathnames #P"qlfile"  tmp))
          (lockfile (merge-pathnames #P"qlfile.lock" tmp))
          (qlhome  (merge-pathnames #P".qlot/" tmp)))
      ;; qlfile is empty — only the implicit quicklisp is considered
      (write-text-file qlfile "")
      ;; lock has quicklisp + a project that no longer appears in qlfile
      (write-text-file lockfile
                       (concatenate 'string
                                    *ql-lock-entry*
                                    "
(\"extra-removed-project\" . (:class qlot.source.ql:source-ql :initargs (:project-name \"extra-removed-project\" :%version :latest) :version \"ql-2018-08-31\"))"))
      (let ((lock-bytes-before (file-bytes lockfile)))
        (let ((caught (nth-value 1
                        (ignore-errors
                          (let ((*locked* t))
                            (install-qlfile qlfile :quicklisp-home qlhome))))))
          (ok (typep caught 'unnecessary-projects)
              "locked install with lock-only dep must signal unnecessary-projects")
          (when (typep caught 'unnecessary-projects)
            (let ((report (with-output-to-string (s) (princ caught s))))
              (ok (search "extra-removed-project" report :test #'char-equal)
                  "unnecessary-projects report must name the removed project")))
          (ok (equalp lock-bytes-before (file-bytes lockfile))
              "qlfile.lock must be unchanged after unnecessary-projects"))))))

;;; *locked* + qlfile constraint differs from lock under source= → missing-projects.
;;;
;;; When the qlfile requests cl-ppcre at "2019-01-01" but the lock has it at "2018-08-31",
;;; source= fails (%version mismatch), so cl-ppcre appears in the "missing" list
;;; (the missing check runs before the unnecessary check, so missing-projects fires first).
;;; Lock must be unchanged on disk.
(deftest locked-changed-constraint-signals-error
  (with-tmp-directory (tmp)
    (let ((qlfile  (merge-pathnames #P"qlfile"  tmp))
          (lockfile (merge-pathnames #P"qlfile.lock" tmp))
          (qlhome  (merge-pathnames #P".qlot/" tmp)))
      ;; qlfile requests cl-ppcre at a specific version
      (write-text-file qlfile "ql cl-ppcre 2019-01-01")
      ;; lock pins cl-ppcre at a DIFFERENT version — source= returns false
      (write-text-file lockfile
                       (concatenate 'string
                                    *ql-lock-entry*
                                    "
(\"cl-ppcre\" . (:class qlot.source.ql:source-ql :initargs (:project-name \"cl-ppcre\" :%version \"2018-08-31\") :version \"ql-2018-08-31\"))"))
      (let ((lock-bytes-before (file-bytes lockfile)))
        (let ((caught (nth-value 1
                        (ignore-errors
                          (let ((*locked* t))
                            (install-qlfile qlfile :quicklisp-home qlhome))))))
          (ok (typep caught 'missing-projects)
              "locked install with changed version constraint must signal missing-projects")
          (when (typep caught 'missing-projects)
            (let ((report (with-output-to-string (s) (princ caught s))))
              (ok (search "cl-ppcre" report :test #'char-equal)
                  "missing-projects report must name the diverged project")))
          (ok (equalp lock-bytes-before (file-bytes lockfile))
              "qlfile.lock must be unchanged after missing-projects"))))))

;;; *locked* + qlfile exactly matches lock → full install completes, lock bytes unchanged.
;;;
;;; This exercises the FULL install path (apply-qlfile-to-qlhome), not the read-only
;;; check-qlfile-lock-current predicate alone: the predicate never reaches the
;;; dump-qlfile-lock write at the end of the install, so it cannot catch a *locked*
;;; install that re-serializes (and thereby rewrites) a matching qlfile.lock.  Driven
;;; against a warm cache in the style of offline-warm-cache-success so no network is
;;; needed; the lock pins quicklisp to the version pre-populated in the cache.
(deftest locked-matching-qlfile-full-install-keeps-lock
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           ;; cache-key for source-dist "quicklisp" @ the version pinned in *ql-lock-entry*
           (meta-dir (merge-pathnames
                      #P"metadata/dist/quicklisp/2018-02-28/" cache-dir))
           (src-dir (merge-pathnames
                     #P"sources/dist/quicklisp/2018-02-28/" cache-dir)))
      ;; empty qlfile — the implicit quicklisp source is added by read-qlfile-for-install
      (write-text-file qlfile "")
      ;; lock exactly matches and pins the version we pre-populate in the cache
      (write-text-file lockfile *ql-lock-entry*)
      ;; pre-populate the cache so the install needs no network
      (ensure-directories-exist meta-dir)
      (ensure-directories-exist src-dir)
      (write-text-file (merge-pathnames "distinfo.txt" meta-dir)
                       "name: quicklisp
version: 2018-02-28
")
      (write-text-file (merge-pathnames "systems.txt" meta-dir) "")
      (write-text-file (merge-pathnames "releases.txt" meta-dir) "")
      (let ((lock-bytes-before (file-bytes lockfile)))
        (let* ((qlot/cache:*cache-directory* cache-dir)
               (*locked* t)
               (err (nth-value 1
                      (ignore-errors
                        (qlot/install::apply-qlfile-to-qlhome qlfile qlhome)))))
          (ok (null err)
              "full *locked* install against a matching lock + warm cache completes without error")
          (ok (equalp lock-bytes-before (file-bytes lockfile))
              "qlfile.lock must be byte-identical after a full *locked* install"))))))
