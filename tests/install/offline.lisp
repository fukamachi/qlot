(defpackage #:qlot-tests/install/offline
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install
                #:install-qlfile)
  (:import-from #:qlot/modes
                #:*offline*)
  (:import-from #:qlot/errors
                #:qlot-error)
  (:import-from #:qlot/cache
                #:*cache-directory*)
  (:import-from #:qlot/utils/git
                #:git-clone)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/install/offline)

(defun write-text-file (path content)
  (ensure-directories-exist path)
  (with-open-file (out path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (write-string content out)))

;;; With *offline* true and all sources present in the shared cache,
;;; apply-qlfile-to-qlhome must complete successfully without any network
;;; access, and the restored dist metadata must exist in qlhome.
;;;
;;; The quicklisp source-dist uses the cache layout:
;;;   metadata/dist/<project>/<version>/{distinfo,systems,releases}.txt
;;;   sources/dist/<project>/<version>/   (empty dir for non-ql sources)
(deftest offline-warm-cache-success
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           ;; cache-key for source-dist "quicklisp" @ version below:
           ;; ("dist" "quicklisp" "offline-warm-cache-test-v1")
           (meta-dir (merge-pathnames
                      #P"metadata/dist/quicklisp/offline-warm-cache-test-v1/"
                      cache-dir))
           (src-dir (merge-pathnames
                     #P"sources/dist/quicklisp/offline-warm-cache-test-v1/"
                     cache-dir)))
      ;; Empty qlfile: default quicklisp source is added by read-qlfile-for-install
      (write-text-file qlfile "")
      ;; Lock pins quicklisp to the version we pre-populate in the cache.
      ;; The distribution URL must match quicklisp-distinfo-url (https://...).
      ;; Single-line format is valid — the lock reader does not care about newlines.
      (write-text-file lockfile
                       "(\"quicklisp\" . (:class qlot.source.dist:source-dist :initargs (:project-name \"quicklisp\" :%version :latest :distribution \"https://beta.quicklisp.org/dist/quicklisp.txt\") :version \"offline-warm-cache-test-v1\"))")
      ;; Pre-populate the cache: metadata files + empty sources directory.
      ;; The content of the metadata files does not need to be valid dist
      ;; format; only their existence is checked by cache-exists-p.
      (ensure-directories-exist meta-dir)
      (ensure-directories-exist src-dir)
      (write-text-file (merge-pathnames "distinfo.txt" meta-dir)
                       "name: quicklisp
version: offline-warm-cache-test-v1
")
      (write-text-file (merge-pathnames "systems.txt" meta-dir) "")
      (write-text-file (merge-pathnames "releases.txt" meta-dir) "")
      ;; Run the offline install against the warm cache.
      ;; *cache-directory* must be rebound before apply-qlfile-to-qlhome so
      ;; that the bt2:*default-special-bindings* capture propagates it to
      ;; worker threads (install.lisp line 513).
      (let ((*cache-directory* cache-dir)
            (*offline* t))
        (let* ((err (nth-value 1
                      (ignore-errors
                        (qlot/install::apply-qlfile-to-qlhome qlfile qlhome)))))
          (ok (null err)
              "offline install against warm cache completes without error")
          (ok (uiop:file-exists-p
               (merge-pathnames #P"dists/quicklisp/distinfo.txt" qlhome))
              "dist metadata is installed in qlhome after warm-cache restore"))))))

;;; With *offline* true and no qlfile.lock present, apply-qlfile-to-qlhome
;;; must fail fast with an actionable error that names the missing lock
;;; before attempting any network access.  The report must contain "lock" —
;;; the bare word "offline" alone is insufficient, distinguishing this
;;; fail-fast from an offline-network-access signal that would arise only if
;;; fetch/get were actually reached.
(deftest offline-no-qlfile-lock
  (with-tmp-directory (tmp)
    (let ((qlfile (merge-pathnames #P"qlfile" tmp))
          (qlhome (merge-pathnames #P".qlot/" tmp))
          (network-trap-triggered nil)
          (ona-sym (find-symbol "OFFLINE-NETWORK-ACCESS" :qlot/errors)))
      (write-text-file qlfile "ql :all :latest")
      ;; No qlfile.lock is written — offline mode must detect this early.
      (let* ((caught (nth-value 1
                       (ignore-errors
                         (handler-bind
                             ((error (lambda (c)
                                       (when (and ona-sym (typep c ona-sym))
                                         (setf network-trap-triggered t)))))
                           (let ((*offline* t))
                             (qlot/install::apply-qlfile-to-qlhome qlfile qlhome)))))))
        (ng network-trap-triggered
            "no HTTP call was made (fail-fast fires before any network access)")
        (ok caught
            "signals an error when *offline* is true and qlfile.lock is absent")
        (when caught
          (let ((report (with-output-to-string (s) (princ caught s))))
            (ok (search "lock" report :test #'char-equal)
                "error report mentions the missing lock file")))))))

;;; With *offline* true and a source not present in the shared cache,
;;; install must signal offline-cache-miss (not fall through to a network
;;; install branch).  The condition report must name the missing project
;;; and its requested version.  No qlot/http call must be made — an
;;; explicit network trap detects any attempt to reach the network.
(deftest offline-cache-miss
  (let ((ocm-sym (find-symbol "OFFLINE-CACHE-MISS" :qlot/errors))
        (ona-sym (find-symbol "OFFLINE-NETWORK-ACCESS" :qlot/errors)))
    (ok ocm-sym
        "qlot/errors:offline-cache-miss must be defined")
    (when ocm-sym
      (with-tmp-directory (tmp)
        (let ((qlfile (merge-pathnames #P"qlfile" tmp))
              (lockfile (merge-pathnames #P"qlfile.lock" tmp))
              (qlhome (merge-pathnames #P".qlot/" tmp))
              (network-trap-triggered nil))
          ;; Pin quicklisp to a unique version that can never be in the cache.
          (write-text-file qlfile "ql :all :latest")
          (write-text-file lockfile
                           "(\"quicklisp\" . (:class qlot.source.dist:source-dist :initargs (:project-name \"quicklisp\" :%version :latest :distribution \"http://beta.quicklisp.org/dist/quicklisp.txt\") :version \"offline-cache-miss-unique-version\"))")
          ;; handler-bind is inside ignore-errors so it is established after
          ;; ignore-errors and therefore checked first on the handler chain.
          ;; It sets network-trap-triggered if offline-network-access fires,
          ;; then returns normally so ignore-errors catches the condition.
          (let* ((caught
                  (nth-value 1
                    (ignore-errors
                      (handler-bind
                          ((error (lambda (c)
                                    (when (and ona-sym (typep c ona-sym))
                                      (setf network-trap-triggered t)))))
                        (let ((*offline* t))
                          (install-qlfile qlfile :quicklisp-home qlhome)))))))
            (ng network-trap-triggered
                "no qlot/http call was made (network trap not triggered)")
            (ok (and caught (typep caught ocm-sym))
                "install signals offline-cache-miss when source is absent from cache")
            (when (and caught (typep caught ocm-sym))
              (let ((report (with-output-to-string (s) (princ caught s))))
                (ok (search "quicklisp" report :test #'char-equal)
                    "offline-cache-miss report names the missing project")
                (ok (search "offline-cache-miss-unique-version" report :test #'char-equal)
                    "offline-cache-miss report names the missing version")))))))))

;;; With *offline* true and an ASDF source whose local checkout directory
;;; does not yet exist, install must signal an offline error — not invoke
;;; git-clone.  The lockfile contains ONLY the asdf source (no ql/dist
;;; sources) so the parallel worker has nothing to do and the ASDF
;;; git-clone guard is the FIRST point of failure.
;;; (An existing asdf-dir with the right tag is a local-only op and is
;;; allowed even in offline mode.)
(deftest offline-asdf-no-local-dir
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (git-clone-trap-triggered nil)
           (ona-sym (find-symbol "OFFLINE-NETWORK-ACCESS" :qlot/errors))
           (ocm-sym (find-symbol "OFFLINE-CACHE-MISS" :qlot/errors)))
      ;; Minimal qlfile + lock with ONLY the asdf source.
      ;; No ql/dist entries -> parallel worker has nothing to do,
      ;; so the ASDF git-clone guard is the first failure point.
      (write-text-file qlfile "asdf 3.3.7")
      ;; :version goes inside :initargs so make-instance sets the version slot;
      ;; source-asdf's source= reads source-version which requires the slot bound.
      (write-text-file lockfile
                       "(\"asdf\" . (:class qlot/source/asdf:source-asdf :initargs (:version \"3.3.7\") :version \"3.3.7\"))")
      ;; The local asdf checkout directory must not exist before the test.
      (ng (uiop:directory-exists-p (merge-pathnames #P"local-projects/asdf/" qlhome))
          "asdf-dir must be absent before the test")
      ;; Trap git-clone: if invoked, the ASDF offline guard failed to fire.
      (let ((original-git-clone (symbol-function 'git-clone)))
        (unwind-protect
             (progn
               (setf (symbol-function 'git-clone)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (setf git-clone-trap-triggered t)
                       (error "git-clone invoked in offline mode (test trap)")))
               ;; No ql/dist sources -> parallel worker is empty.
               ;; ASDF dir absent -> offline guard fires before git-clone.
               (let* ((caught (nth-value 1
                                (ignore-errors
                                  (let ((*offline* t))
                                    (qlot/install::apply-qlfile-to-qlhome qlfile qlhome))))))
                 (ng git-clone-trap-triggered
                     "git-clone was never invoked — ASDF offline guard fired first")
                 (ok (and caught (typep caught 'qlot-error))
                     "install signals a qlot-error when asdf-dir is absent in offline mode")
                 (ok (and caught ona-sym (typep caught ona-sym))
                     "the signaled condition is offline-network-access (ASDF guard, not cache-miss)")
                 (ng (and caught ocm-sym (typep caught ocm-sym))
                     "error is NOT offline-cache-miss — ASDF guard fired, not ql/dist cache miss")
                 (ng (uiop:directory-exists-p (merge-pathnames #P"local-projects/asdf/" qlhome))
                     "asdf-dir not created — git-clone was blocked")))
          (setf (symbol-function 'git-clone) original-git-clone))))))

;;; With *offline* true and ignore-lock true (the update-style path that
;;; re-resolves qlfile sources without consulting qlfile.lock),
;;; apply-qlfile-to-qlhome must fail fast with offline-network-access before any
;;; network access — re-resolving sources from the qlfile would require the
;;; network, which offline mode forbids.  This pins the guard at the top of
;;; apply-qlfile-to-qlhome so an accidental removal or mis-spelled condition is
;;; caught.
(deftest offline-ignore-lock-blocks-update
  (with-tmp-directory (tmp)
    (let ((qlfile (merge-pathnames #P"qlfile" tmp))
          (qlhome (merge-pathnames #P".qlot/" tmp))
          (ona-sym (find-symbol "OFFLINE-NETWORK-ACCESS" :qlot/errors)))
      (write-text-file qlfile "ql :all :latest")
      (let ((caught (nth-value 1
                      (ignore-errors
                        (let ((*offline* t))
                          (qlot/install::apply-qlfile-to-qlhome
                           qlfile qlhome :ignore-lock t))))))
        (ok (and caught ona-sym (typep caught ona-sym))
            "*offline* + ignore-lock signals offline-network-access before any network access")))))
