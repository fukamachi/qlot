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
                #:*cache-directory*
                #:*cache-enabled*
                #:normalize-git-url
                #:split-path)
  (:import-from #:qlot/utils/git
                #:git-clone
                #:git-switch-tag)
  (:import-from #:qlot/utils/shell
                #:safety-shell-command)
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

(defun write-git-asdf-checkout (directory)
  (ensure-directories-exist directory)
  (safety-shell-command "git" `("init" ,(uiop:native-namestring directory) "--quiet"))
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "config" "user.email" "qlot-tests@example.invalid"))
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "config" "user.name" "Qlot Tests"))
  (write-text-file (merge-pathnames "version.txt" directory) "3.3.6")
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "add" "version.txt"))
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "commit" "--quiet" "-m" "Add ASDF 3.3.6"))
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "tag" "3.3.6"))
  (write-text-file (merge-pathnames "version.txt" directory) "3.3.7")
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "commit" "--quiet" "-am" "Add ASDF 3.3.7"))
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "tag" "3.3.7"))
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "checkout" "3.3.6" "--quiet"))
  (values))

(defun git-head-at-tag-p (directory tag)
  (let ((head (string-right-trim
               '(#\Newline #\Return)
               (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                             "rev-parse" "HEAD"))))
        (tag-ref (string-right-trim
                  '(#\Newline #\Return)
                  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                                "rev-parse" ,tag)))))
    (string= head tag-ref)))

;;; Compute the expected sources cache directory for the ASDF source at VERSION.
;;; Mirrors the cache-key structure for source-git (git + normalized URL parts +
;;; version) that the implementation must add for source-asdf.  Used by tests to
;;; pre-populate and verify the cache without calling cache-key directly.
(defun asdf-expected-cache-path (cache-dir version)
  (let* ((url "https://gitlab.common-lisp.net/asdf/asdf.git")
         (normalized (normalize-git-url url))
         (parts (split-path normalized))
         (key-components (append (list "git") parts (list version))))
    (reduce (lambda (path component)
              (merge-pathnames
               (make-pathname :directory (list :relative component))
               path))
            key-components
            :initial-value (merge-pathnames #P"sources/" cache-dir))))

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

;;; With *offline* true, empty cache, and no local-projects/asdf/, install must
;;; signal offline-cache-miss (reporting "asdf" and the requested version) —
;;; not offline-network-access.  The ASDF cache layer means an absent local dir
;;; with an empty cache is a cache miss, not a raw network denial.  git-clone
;;; must never be invoked.
(deftest offline-asdf-no-local-dir
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           (git-clone-trap-triggered nil)
           (ocm-sym (find-symbol "OFFLINE-CACHE-MISS" :qlot/errors)))
      ;; Minimal qlfile + lock with ONLY the asdf source so the ASDF
      ;; code path is the first (and only) point of failure.
      (write-text-file qlfile "asdf 3.3.7")
      ;; :version inside :initargs so make-instance sets the version slot;
      ;; source-asdf's source= reads source-version which requires the slot bound.
      (write-text-file lockfile
                       "(\"asdf\" . (:class qlot/source/asdf:source-asdf :initargs (:version \"3.3.7\") :version \"3.3.7\"))")
      (ng (uiop:directory-exists-p (merge-pathnames #P"local-projects/asdf/" qlhome))
          "asdf-dir must be absent before the test")
      (ensure-directories-exist cache-dir)
      ;; Trap git-clone: must never be invoked when the cache miss guard fires.
      (let ((original-git-clone (symbol-function 'git-clone)))
        (unwind-protect
             (progn
               (setf (symbol-function 'git-clone)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (setf git-clone-trap-triggered t)
                       (error "git-clone invoked in offline mode (test trap)")))
               (let* ((caught (nth-value 1
                                (ignore-errors
                                  (let ((*cache-directory* cache-dir)
                                        (*offline* t))
                                    (qlot/install::apply-qlfile-to-qlhome qlfile qlhome))))))
                 (ng git-clone-trap-triggered
                     "git-clone was never invoked — ASDF cache miss guard fired first")
                 (ok (and caught (typep caught 'qlot-error))
                     "install signals a qlot-error when asdf cache is empty and dir absent in offline mode")
                 (ok (and caught ocm-sym (typep caught ocm-sym))
                     "install signals offline-cache-miss (not offline-network-access) for ASDF cold cache")
                 (when (and caught ocm-sym (typep caught ocm-sym))
                   (let ((report (with-output-to-string (s) (princ caught s))))
                     (ok (search "asdf" report :test #'char-equal)
                         "offline-cache-miss report names the project (asdf)")
                     (ok (search "3.3.7" report :test #'char-equal)
                         "offline-cache-miss report names the requested version")))
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

;;; With *offline* true, ASDF cache pre-populated for the pinned version, and no
;;; local-projects/asdf/, install must restore ASDF into local-projects/asdf/
;;; and complete without signaling.  git-clone must not be invoked (warm cache
;;; takes the restore path) and git fetch must not be invoked (offline guard in
;;; git-switch-tag must suppress the network call).
;;;
;;; The cache-restored directory is a real local git repository with the pinned
;;; tag already present, so git-switch-tag itself is exercised without stubbing
;;; checkout.  This replaces a prior version that stubbed git-switch-tag to
;;; (values), which masked the missing guard.
(deftest offline-asdf-warm-cache
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           (asdf-cache-dir (asdf-expected-cache-path cache-dir "3.3.7"))
           (git-clone-trap-triggered nil)
           (git-fetch-trap-triggered nil)
           (original-git-clone (symbol-function 'git-clone))
           (original-ssc (symbol-function 'safety-shell-command)))
      (write-text-file qlfile "asdf 3.3.7")
      (write-text-file lockfile
                       "(\"asdf\" . (:class qlot/source/asdf:source-asdf :initargs (:version \"3.3.7\") :version \"3.3.7\"))")
      ;; Pre-populate the expected ASDF cache entry with a real local git repo.
      ;; The pinned tag is already present locally, so offline checkout can
      ;; succeed without fetching from origin.
      (write-git-asdf-checkout asdf-cache-dir)
      (ng (uiop:directory-exists-p (merge-pathnames #P"local-projects/asdf/" qlhome))
          "asdf-dir must be absent before the test")
      (unwind-protect
           (progn
             ;; Trap safety-shell-command for git fetch: offline mode must never
             ;; perform a network fetch.  Before the fix git-switch-tag calls this
             ;; unconditionally; after the fix the *offline* guard skips it.
             (setf (symbol-function 'safety-shell-command)
                   (lambda (program args &rest rest)
                     (when (and (string= program "git")
                                (listp args)
                                (member "fetch" args :test #'string=))
                       (setf git-fetch-trap-triggered t)
                       (error "git fetch invoked in offline mode (test trap)"))
                     (apply original-ssc program args rest)))
             ;; Trap git-clone: warm cache must make the clone path unreachable.
             (setf (symbol-function 'git-clone)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (setf git-clone-trap-triggered t)
                     (error "git-clone invoked — warm-cache restore path did not fire (test trap)")))
             (let* ((err (nth-value 1
                           (ignore-errors
                             (let ((*cache-directory* cache-dir)
                                   (*offline* t))
                               (qlot/install::apply-qlfile-to-qlhome qlfile qlhome))))))
               (ok (null err)
                   "offline install with warm ASDF cache completes without error")
               (ng git-clone-trap-triggered
                   "git-clone was not invoked — ASDF was restored from cache")
               (ng git-fetch-trap-triggered
                   "git fetch was not invoked in offline mode — offline guard in git-switch-tag")
               (ok (uiop:directory-exists-p
                    (merge-pathnames #P"local-projects/asdf/" qlhome))
                   "local-projects/asdf/ exists after warm-cache restore")
               (ok (git-head-at-tag-p (merge-pathnames #P"local-projects/asdf/" qlhome)
                                      "3.3.7")
                   "local-projects/asdf/ is checked out at the pinned tag 3.3.7")))
        (setf (symbol-function 'git-clone) original-git-clone)
        (setf (symbol-function 'safety-shell-command) original-ssc)))))

;;; With *offline* nil and empty cache, after install the ASDF source's cache
;;; entry must exist — the online clone warms the cache for future offline use.
;;; git-clone is stubbed to write a fixed checkout into local-projects/asdf/;
;;; the test asserts the cache directory was populated (state observation).
(deftest online-asdf-warms-cache
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           (asdf-cache-dir (asdf-expected-cache-path cache-dir "3.3.7"))
           (original-git-clone (symbol-function 'git-clone))
           (original-git-switch-tag (symbol-function 'git-switch-tag)))
      (write-text-file qlfile "asdf 3.3.7")
      (write-text-file lockfile
                       "(\"asdf\" . (:class qlot/source/asdf:source-asdf :initargs (:version \"3.3.7\") :version \"3.3.7\"))")
      (ensure-directories-exist cache-dir)
      (unwind-protect
           (progn
             ;; Stub git-clone to write a fake checkout into the destination
             ;; without hitting the network.
             (setf (symbol-function 'git-clone)
                   (lambda (remote-url destination &rest args)
                     (declare (ignore remote-url args))
                     (ensure-directories-exist destination)
                     (write-text-file (merge-pathnames "asdf.lisp" destination)
                                      "; stub asdf checkout")
                     (values)))
             ;; Stub git-switch-tag (guards against unexpected real git ops).
             (setf (symbol-function 'git-switch-tag)
                   (lambda (&rest args) (declare (ignore args)) (values)))
             (let* ((err (nth-value 1
                           (ignore-errors
                             (let ((*cache-directory* cache-dir))
                               (qlot/install::apply-qlfile-to-qlhome qlfile qlhome))))))
               (ok (null err)
                   "online install with empty cache completes without error")
               (ok (uiop:directory-exists-p asdf-cache-dir)
                   "ASDF cache entry exists after online install — clone warmed the cache")))
        (setf (symbol-function 'git-clone) original-git-clone)
        (setf (symbol-function 'git-switch-tag) original-git-switch-tag)))))

;;; With *cache-enabled* nil (QLOT_NO_CACHE equivalent), the ASDF block must
;;; neither read nor write the cache.
;;;
;;; (a) Offline + no local dir: still signals an offline error rather than
;;;     silently restoring from cache.
;;; (b) Online + stub git-clone: the ASDF cache entry is absent after install
;;;     (cache write was skipped).
(deftest asdf-cache-disabled
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           (asdf-cache-dir (asdf-expected-cache-path cache-dir "3.3.7")))
      (write-text-file qlfile "asdf 3.3.7")
      (write-text-file lockfile
                       "(\"asdf\" . (:class qlot/source/asdf:source-asdf :initargs (:version \"3.3.7\") :version \"3.3.7\"))")
      (ensure-directories-exist cache-dir)

      ;; Sub-test (a): cache disabled + offline + no local dir → offline error.
      (testing "cache disabled + offline + no local dir signals an offline error"
        (let* ((caught (nth-value 1
                         (ignore-errors
                           (let ((*cache-directory* cache-dir)
                                 (*cache-enabled* nil)
                                 (*offline* t))
                             (qlot/install::apply-qlfile-to-qlhome qlfile qlhome))))))
          (ok (and caught (typep caught 'qlot-error))
              "an offline error is still raised when cache is disabled and dir absent")))

      ;; Sub-test (b): cache disabled + online → cache not populated after install.
      (let ((original-git-clone (symbol-function 'git-clone))
            (original-git-switch-tag (symbol-function 'git-switch-tag)))
        (unwind-protect
             (progn
               (setf (symbol-function 'git-clone)
                     (lambda (remote-url destination &rest args)
                       (declare (ignore remote-url args))
                       (ensure-directories-exist destination)
                       (write-text-file (merge-pathnames "asdf.lisp" destination)
                                        "; stub")
                       (values)))
               (setf (symbol-function 'git-switch-tag)
                     (lambda (&rest args) (declare (ignore args)) (values)))
               (ignore-errors
                 (let ((*cache-directory* cache-dir)
                       (*cache-enabled* nil))
                   (qlot/install::apply-qlfile-to-qlhome qlfile qlhome)))
               (testing "online install with cache disabled does not populate the ASDF cache"
                 (ng (uiop:directory-exists-p asdf-cache-dir)
                     "ASDF cache dir must not exist — cache was disabled during install")))
          (setf (symbol-function 'git-clone) original-git-clone)
          (setf (symbol-function 'git-switch-tag) original-git-switch-tag))))))

;;; When local-projects/asdf/ already exists, offline install must switch to the
;;; pinned version without calling git-clone and without performing a git fetch.
;;; Regression: ensures the new cache layer does not displace the existing-dir
;;; code path, and that git-switch-tag's offline guard suppresses the fetch.
;;;
;;; git-switch-tag itself is NOT stubbed so the offline guard inside it is
;;; exercised against a real local git repository whose pinned tag is already
;;; present.
(deftest offline-asdf-existing-local-dir
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           (asdf-dir (merge-pathnames #P"local-projects/asdf/" qlhome))
           (git-clone-trap-triggered nil)
           (git-fetch-trap-triggered nil)
           (original-git-clone (symbol-function 'git-clone))
           (original-ssc (symbol-function 'safety-shell-command)))
      (write-text-file qlfile "asdf 3.3.7")
      (write-text-file lockfile
                       "(\"asdf\" . (:class qlot/source/asdf:source-asdf :initargs (:version \"3.3.7\") :version \"3.3.7\"))")
      (ensure-directories-exist cache-dir)
      ;; Pre-create a local ASDF checkout at a different tag (simulates a prior install).
      (write-git-asdf-checkout asdf-dir)
      (ok (uiop:directory-exists-p asdf-dir)
          "asdf-dir exists before the test (regression precondition)")
      (ok (git-head-at-tag-p asdf-dir "3.3.6")
          "asdf-dir starts checked out at a different tag")
      (unwind-protect
           (progn
             ;; Trap safety-shell-command for git fetch: offline mode must not fetch.
             (setf (symbol-function 'safety-shell-command)
                   (lambda (program args &rest rest)
                     (when (and (string= program "git")
                                (listp args)
                                (member "fetch" args :test #'string=))
                       (setf git-fetch-trap-triggered t)
                       (error "git fetch invoked in offline mode (test trap)"))
                     (apply original-ssc program args rest)))
             ;; Trap git-clone: existing-dir path must not clone.
             (setf (symbol-function 'git-clone)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (setf git-clone-trap-triggered t)
                     (error "git-clone invoked — existing-dir path should have been taken (test trap)")))
             (let* ((err (nth-value 1
                           (ignore-errors
                             (let ((*cache-directory* cache-dir)
                                   (*offline* t))
                               (qlot/install::apply-qlfile-to-qlhome qlfile qlhome))))))
               (ok (null err)
                   "offline install with existing asdf-dir completes without error")
               (ng git-clone-trap-triggered
                   "git-clone was not invoked — existing-dir path was taken")
               (ng git-fetch-trap-triggered
                   "git fetch was not invoked — offline guard prevents network access")
               (ok (git-head-at-tag-p asdf-dir "3.3.7")
                   "local-projects/asdf/ is checked out at the pinned tag 3.3.7")))
        (setf (symbol-function 'git-clone) original-git-clone)
        (setf (symbol-function 'safety-shell-command) original-ssc)))))

;;; With *offline* nil (the default), git-switch-tag must still perform a git
;;; fetch before checking out the tag.  This is a regression guard: the offline
;;; guard added by the fix must be conditional on *offline*, not unconditional.
;;;
;;; safety-shell-command is intercepted to record the fetch call and return
;;; success without running real git (the fake directory is not a repository).
;;; git-checkout is stubbed for the same reason.
(deftest online-asdf-tag-switch-fetches
  (with-tmp-directory (tmp)
    (let* ((fake-dir (uiop:ensure-directory-pathname
                      (merge-pathnames #P"fake-asdf/" tmp)))
           (git-fetch-called nil)
           (original-ssc (symbol-function 'safety-shell-command))
           (original-git-checkout (symbol-function 'qlot/utils/git::git-checkout)))
      (ensure-directories-exist fake-dir)
      (unwind-protect
           (progn
             (setf (symbol-function 'safety-shell-command)
                   (lambda (program args &rest rest)
                     (if (and (string= program "git")
                              (listp args)
                              (member "fetch" args :test #'string=))
                         (progn
                           (setf git-fetch-called t)
                           "")
                         (apply original-ssc program args rest))))
             (setf (symbol-function 'qlot/utils/git::git-checkout)
                   (lambda (dir tag) (declare (ignore dir tag)) (values)))
             (let ((err (nth-value 1
                          (ignore-errors
                            (let ((*offline* nil))
                              (git-switch-tag fake-dir "3.3.7"))))))
               (ok (null err)
                   "git-switch-tag with *offline* nil completes without error")
               (ok git-fetch-called
                   "git fetch is invoked when *offline* is nil (online path preserved)")))
        (setf (symbol-function 'safety-shell-command) original-ssc)
        (setf (symbol-function 'qlot/utils/git::git-checkout) original-git-checkout)))))
