(defpackage #:qlot-tests/install/env-modes
  (:use #:cl
        #:rove)
  (:import-from #:qlot/modes
                #:*offline*
                #:*locked*)
  (:import-from #:qlot/errors
                #:offline-network-access
                #:offline-cache-miss
                #:locked-operation-rejected)
  (:import-from #:qlot/cache
                #:*cache-directory*
                #:*cache-enabled*)
  ;; Importing a (non-conflicting) symbol from each of these systems forces ASDF
  ;; to load them before this file compiles, so the fully-qualified trap symbols
  ;; (qlot/http:get etc.) resolve at read time.  We deliberately do NOT import or
  ;; reference qlot/cli at read time: it is feature-gated (:ros.installing) in
  ;; qlot.asd, so a read-time reference would fail to compile on a clean image.
  ;; Command functions are reached via uiop:symbol-call at runtime instead.
  (:import-from #:qlot/http
                #:fetch)
  (:import-from #:qlot/utils/git
                #:git-clone
                #:git-switch-tag)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  ;; Needed for bootstrapping .qlot/ in update/add tests.
  (:import-from #:qlot/install
                #:install-qlfile))
(in-package #:qlot-tests/install/env-modes)

(defun write-text-file (path content)
  (ensure-directories-exist path)
  (with-open-file (out path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (write-string content out)))

;;; A minimal lock entry for the implicit quicklisp default source-dist.
;;; Mirrors the entry used by tests/install/offline.lisp.
(defparameter *ql-default-lock-entry*
  "(\"quicklisp\" . (:class qlot.source.dist:source-dist :initargs (:project-name \"quicklisp\" :%version :latest :distribution \"https://beta.quicklisp.org/dist/quicklisp.txt\") :version \"env-modes-test-v1\"))")

;;; A second lock entry pinning to a version that is NOT in the warm cache.
;;; Used to trigger offline-cache-miss when the dist is installed at v1 but
;;; the lock demands v2.
(defparameter *ql-v2-lock-entry*
  "(\"quicklisp\" . (:class qlot.source.dist:source-dist :initargs (:project-name \"quicklisp\" :%version :latest :distribution \"https://beta.quicklisp.org/dist/quicklisp.txt\") :version \"env-modes-test-v2\"))")

;;; --- Network traps -------------------------------------------------------
;;; Rebinding the real network/git entry points so that ANY attempt to reach the
;;; network during a test signals immediately instead of hanging on a real
;;; download.  An offline-mode test passes only if the offline guard fires BEFORE
;;; one of these traps; an online (env-unset) test expects a trap to fire,
;;; proving the offline tests are not vacuous.

(define-condition network-trap-fired (error)
  ((what :initarg :what :reader network-trap-what))
  (:report (lambda (c s)
             (format s "NETWORK TRAP: ~A was invoked (a real network/git call was attempted)"
                     (network-trap-what c)))))

(defmacro with-network-traps (&body body)
  (let ((g (gensym)) (f (gensym)) (c (gensym)) (s (gensym)))
    `(let ((,g (symbol-function 'qlot/http:get))
           (,f (symbol-function 'qlot/http:fetch))
           (,c (symbol-function 'qlot/utils/git:git-clone))
           (,s (symbol-function 'qlot/utils/git:git-switch-tag)))
       (flet ((trap (name)
                (lambda (&rest args)
                  (declare (ignore args))
                  (error 'network-trap-fired :what name))))
         (unwind-protect
              (progn
                (setf (symbol-function 'qlot/http:get) (trap "qlot/http:get")
                      (symbol-function 'qlot/http:fetch) (trap "qlot/http:fetch")
                      (symbol-function 'qlot/utils/git:git-clone) (trap "git-clone")
                      (symbol-function 'qlot/utils/git:git-switch-tag) (trap "git-switch-tag"))
                ,@body)
           (setf (symbol-function 'qlot/http:get) ,g
                 (symbol-function 'qlot/http:fetch) ,f
                 (symbol-function 'qlot/utils/git:git-clone) ,c
                 (symbol-function 'qlot/utils/git:git-switch-tag) ,s))))))

;;; --- Environment helpers -------------------------------------------------

(defun call-with-env (alist thunk)
  "Set each (NAME . VALUE) in ALIST (NIL clears via empty string, which
uiop:getenvp treats as unset), run THUNK, then restore prior values."
  (let ((saved (loop for (name . nil) in alist
                     collect (cons name (uiop:getenv name)))))
    (unwind-protect
         (progn
           (loop for (name . value) in alist
                 do (setf (uiop:getenv name) (or value "")))
           (funcall thunk))
      (loop for (name . old) in saved
            do (setf (uiop:getenv name) (or old ""))))))

(defmacro with-env ((&rest alist) &body body)
  `(call-with-env (list ,@(loop for (k . v) in alist collect `(cons ,k ,v)))
                  (lambda () ,@body)))

(defun run-command (command-name argv)
  "Load qlot/cli at runtime (it is feature-gated, so not loaded by default in the
test image) and invoke the named internal command function with ARGV."
  (asdf:load-system :qlot/cli)
  (uiop:symbol-call '#:qlot/cli command-name argv))

;;; --- Warm-cache helper ---------------------------------------------------
;;; Pre-populate the qlot dist cache for the quicklisp dist at VERSION so that
;;; apply-qlfile-to-qlhome can restore from cache without network access.

(defun populate-warm-cache (cache-dir version)
  "Create the metadata + sources directories for quicklisp dist VERSION in
CACHE-DIR so that cache-exists-p returns true for that entry."
  (let ((meta-dir (merge-pathnames
                   (make-pathname :directory `(:relative "metadata" "dist" "quicklisp" ,version))
                   cache-dir))
        (src-dir (merge-pathnames
                  (make-pathname :directory `(:relative "sources" "dist" "quicklisp" ,version))
                  cache-dir)))
    (ensure-directories-exist meta-dir)
    (ensure-directories-exist src-dir)
    (write-text-file (merge-pathnames "distinfo.txt" meta-dir)
                     (format nil "name: quicklisp~%version: ~A~%" version))
    (write-text-file (merge-pathnames "systems.txt" meta-dir) "")
    (write-text-file (merge-pathnames "releases.txt" meta-dir) "")))

;;; --- Bootstrap helper ---------------------------------------------------
;;; Install a minimal .qlot/ into TMP using only the warm cache (no network).
;;; The bootstrap DIRECTLY binds *offline* to T (does not set QLOT_OFFLINE in
;;; the env) so the env remains unset for the real test body.

(defun bootstrap-qlot (tmp cache-dir lock-entry)
  "Bootstrap a .qlot/ installation in TMP/  using the warm cache.
Writes a qlfile (empty) and qlfile.lock containing LOCK-ENTRY, then calls
install-qlfile offline.  After this, TMP/.qlot/ exists and the quicklisp
dist referenced by LOCK-ENTRY is installed from cache."
  (write-text-file (merge-pathnames #P"qlfile" tmp) "")
  (write-text-file (merge-pathnames #P"qlfile.lock" tmp) lock-entry)
  (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
        (*cache-directory* (uiop:ensure-directory-pathname cache-dir))
        (*offline* t))
    (ignore-errors
      (install-qlfile (merge-pathnames #P"qlfile" tmp)))))

;;; --- Gate 1: production wiring + offline update fail-fast -----------------
;;;
;;; `qlot update` re-resolves sources from the qlfile (ignore-lock), which needs
;;; the network to discover latest versions, so with QLOT_OFFLINE it must FAIL
;;; FAST.  The env var must be honored on the update path (not only via the CLI
;;; argv parser) -- the test sets QLOT_OFFLINE in the environment and does NOT
;;; bind *offline*, so a pass proves the command initialized the mode from env.
;;;
;;; The test first bootstraps .qlot/ offline via the warm cache so that
;;; update-qlfile's check-local-quicklisp passes.  Without this bootstrap
;;; the update command raises qlot-directory-not-found before reaching the
;;; offline guard.
(deftest env-offline-update-fails-fast
  (with-tmp-directory (tmp)
    (let* ((cache-dir (uiop:ensure-directory-pathname (merge-pathnames #P"cache/" tmp))))
      ;; Warm cache for v1 and bootstrap .qlot/ using that cached version.
      (populate-warm-cache cache-dir "env-modes-test-v1")
      (bootstrap-qlot tmp cache-dir *ql-default-lock-entry*)
      ;; Now .qlot/ exists.  Test update with QLOT_OFFLINE from env.
      (with-env (("QLOT_OFFLINE" . "1") ("QLOT_LOCKED" . nil) ("QLOT_NO_CACHE" . nil))
        (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
              (*cache-directory* cache-dir)
              (*offline* nil)
              (*locked* nil))
          (with-network-traps
            (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-update '())))))
              (ok (and err (typep err 'offline-network-access))
                  "QLOT_OFFLINE + qlot update signals offline-network-access (env honored on update path)")
              (ok (not (typep err 'network-trap-fired))
                  "no real network/git call was attempted -- the offline guard fired first"))))))))

;;; --- Gate 2: offline add fail-fast ---------------------------------------
;;;
;;; `qlot add` of a source re-installs from the qlfile; with QLOT_OFFLINE and a
;;; cached dist at version V1, but the lock demanding V2 (not in cache), qlot
;;; must fail fast with offline-cache-miss.
;;;
;;; The test bootstraps .qlot/ with V1 in the cache, then overwrites the lock
;;; to demand V2 before calling the add command.  With QLOT_OFFLINE the dist
;;; exists at V1 but the lock wants V2, which is absent from the cache, so
;;; offline-cache-miss fires before any network access.
(deftest env-offline-add-fails-fast
  (with-tmp-directory (tmp)
    (let* ((cache-dir (uiop:ensure-directory-pathname (merge-pathnames #P"cache/" tmp))))
      ;; Warm cache for v1, bootstrap .qlot/ with v1 installed.
      (populate-warm-cache cache-dir "env-modes-test-v1")
      (bootstrap-qlot tmp cache-dir *ql-default-lock-entry*)
      ;; Overwrite the lock to demand v2 (NOT in cache).  The add command will
      ;; run install-qlfile offline with *offline* from env; the quicklisp dist
      ;; is installed at v1 but the lock says v2, triggering offline-cache-miss.
      (write-text-file (merge-pathnames #P"qlfile.lock" tmp) *ql-v2-lock-entry*)
      (with-env (("QLOT_OFFLINE" . "1") ("QLOT_LOCKED" . nil) ("QLOT_NO_CACHE" . nil))
        (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
              (*cache-directory* cache-dir)
              (*offline* nil)
              (*locked* nil))
          (with-network-traps
            (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-add '("ql" "alexandria"))))))
              (ok (and err (or (typep err 'offline-network-access)
                               (typep err 'offline-cache-miss)))
                  "QLOT_OFFLINE + qlot add with cache miss signals an offline error (env honored on add path)")
              (ok (not (typep err 'network-trap-fired))
                  "no real network/git call was attempted"))))))))

;;; --- Gate 3: env-unset default takes the online path (discriminator) ------
;;;
;;; Proves the offline tests above are not vacuous: with QLOT_OFFLINE unset, the
;;; update path does NOT fail fast, reaches the network, and so trips a trap.
;;;
;;; The test bootstraps .qlot/ first so update-qlfile's check-local-quicklisp
;;; passes (same bootstrap requirement as env-offline-update-fails-fast).
(deftest env-unset-update-takes-online-path
  (with-tmp-directory (tmp)
    (let* ((cache-dir (uiop:ensure-directory-pathname (merge-pathnames #P"cache/" tmp))))
      ;; Bootstrap .qlot/ so check-local-quicklisp passes.
      (populate-warm-cache cache-dir "env-modes-test-v1")
      (bootstrap-qlot tmp cache-dir *ql-default-lock-entry*)
      (with-env (("QLOT_OFFLINE" . nil) ("QLOT_LOCKED" . nil) ("QLOT_NO_CACHE" . nil))
        (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
              (*cache-directory* cache-dir)
              (*offline* nil)
              (*locked* nil))
          (with-network-traps
            (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-update '())))))
              (ok (typep err 'network-trap-fired)
                  "with QLOT_OFFLINE unset, qlot update takes the online path (a real network call is attempted)"))))))))

;;; --- Gate 4: offline remove is cache-only and succeeds --------------------
;;;
;;; `qlot remove` reinstalls the remaining deps from the lock; with QLOT_OFFLINE
;;; and a warm cache for those deps it completes with no network access.  Here
;;; the qlfile has one removable ql source; after removal only the implicit
;;; quicklisp default remains, and it is pre-populated in the cache.
(deftest env-offline-remove-cache-only-succeeds
  (with-tmp-directory (tmp)
    (let* ((cache-dir (uiop:ensure-directory-pathname (merge-pathnames #P"cache/" tmp)))
           (meta-dir (merge-pathnames #P"metadata/dist/quicklisp/env-modes-test-v1/" cache-dir))
           (src-dir (merge-pathnames #P"sources/dist/quicklisp/env-modes-test-v1/" cache-dir)))
      ;; one removable source; the implicit quicklisp default remains after removal
      (write-text-file (merge-pathnames #P"qlfile" tmp) "ql alexandria")
      ;; lock pins the remaining quicklisp default to the cached version
      (write-text-file (merge-pathnames #P"qlfile.lock" tmp) *ql-default-lock-entry*)
      ;; warm cache for the quicklisp default (existence is what cache-exists-p checks)
      (ensure-directories-exist meta-dir)
      (ensure-directories-exist src-dir)
      (write-text-file (merge-pathnames "distinfo.txt" meta-dir)
                       "name: quicklisp
version: env-modes-test-v1
")
      (write-text-file (merge-pathnames "systems.txt" meta-dir) "")
      (write-text-file (merge-pathnames "releases.txt" meta-dir) "")
      (with-env (("QLOT_OFFLINE" . "1") ("QLOT_LOCKED" . nil) ("QLOT_NO_CACHE" . nil))
        (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
              (*cache-directory* cache-dir)
              (*offline* nil)
              (*locked* nil))
          (with-network-traps
            (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-remove '("alexandria"))))))
              (ok (not (typep err 'network-trap-fired))
                  "QLOT_OFFLINE + warm cache: qlot remove reinstalled the remaining deps from cache with no real network")
              (ok (null err)
                  "qlot remove completes offline without signalling"))))))))

;;; --- Gate 4b: offline remove HONORS the env (discriminator) ---------------
;;;
;;; The success test above (full warm cache + matching lock) cannot tell offline
;;; mode apart from an online run that happened to need no network -- both just
;;; restore from cache.  This companion forces a discriminator: the remaining
;;; quicklisp default is pinned to V2, which is NOT in the cache.  With
;;; QLOT_OFFLINE honored on the remove path the reinstall fails fast with
;;; offline-cache-miss before any network; if remove ignored the env it would run
;;; online and fetch V2, tripping a network trap.  So the (not network-trap-fired)
;;; assertion fails precisely when qlot-command-remove omits the initialize-modes
;;; wiring -- making the remove-path env-wiring observable.
(deftest env-offline-remove-honors-env
  (with-tmp-directory (tmp)
    (let* ((cache-dir (uiop:ensure-directory-pathname (merge-pathnames #P"cache/" tmp))))
      ;; Warm cache for v1, bootstrap .qlot/ with v1 installed.
      (populate-warm-cache cache-dir "env-modes-test-v1")
      (bootstrap-qlot tmp cache-dir *ql-default-lock-entry*)
      ;; A removable source; after removal the implicit quicklisp default remains.
      (write-text-file (merge-pathnames #P"qlfile" tmp) "ql alexandria")
      ;; Pin the remaining default to v2 (NOT in cache): an offline reinstall must
      ;; fail fast with offline-cache-miss; an online run would fetch v2 -> trap.
      (write-text-file (merge-pathnames #P"qlfile.lock" tmp) *ql-v2-lock-entry*)
      (with-env (("QLOT_OFFLINE" . "1") ("QLOT_LOCKED" . nil) ("QLOT_NO_CACHE" . nil))
        (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
              (*cache-directory* cache-dir)
              (*offline* nil)
              (*locked* nil))
          (with-network-traps
            (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-remove '("alexandria"))))))
              (ok (not (typep err 'network-trap-fired))
                  "qlot remove honors QLOT_OFFLINE -- no real network/git call (an online run would fetch v2 and trip a trap)")
              (ok (and err (or (typep err 'offline-cache-miss)
                               (typep err 'offline-network-access)))
                  "offline remove of an uncached remaining dep fails fast with an offline error"))))))))

;;; --- Gate 5: QLOT_LOCKED + qlot update signals locked-operation-rejected ---
;;;
;;; `qlot update` re-resolves sources from the qlfile (ignore-lock), which is
;;; explicitly prohibited in locked mode.  The guard fires in update-qlfile at
;;; line 314 (when *locked*) and again in apply-qlfile-to-qlhome at line 496
;;; (when *locked* + ignore-lock).  The test sets QLOT_LOCKED in the environment
;;; and does NOT bind *locked*, so a pass proves the command initializes the mode
;;; from env (not only via the CLI argv parser).
;;;
;;; The test bootstraps .qlot/ first so check-local-quicklisp passes.  Without
;;; the bootstrap, update-qlfile raises qlot-directory-not-found before the
;;; locked guard fires -- the same requirement as env-offline-update-fails-fast.
(deftest env-locked-update-fails-fast
  (with-tmp-directory (tmp)
    (let* ((cache-dir (uiop:ensure-directory-pathname (merge-pathnames #P"cache/" tmp))))
      ;; Warm cache for v1 and bootstrap .qlot/ using that cached version.
      (populate-warm-cache cache-dir "env-modes-test-v1")
      (bootstrap-qlot tmp cache-dir *ql-default-lock-entry*)
      ;; Now .qlot/ exists.  Test update with QLOT_LOCKED from env.
      (with-env (("QLOT_LOCKED" . "1") ("QLOT_OFFLINE" . nil) ("QLOT_NO_CACHE" . nil))
        (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
              (*cache-directory* cache-dir)
              (*locked* nil)
              (*offline* nil))
          (with-network-traps
            (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-update '())))))
              (ok (and err (typep err 'locked-operation-rejected))
                  "QLOT_LOCKED + qlot update signals locked-operation-rejected (env honored on update path)")
              (ok (not (typep err 'network-trap-fired))
                  "no real network/git call was attempted -- the locked guard fired first"))))))))

;;; --- Gate 5b: QLOT_LOCKED unset takes the online path (discriminator) ------
;;;
;;; Proves env-locked-update-fails-fast is not vacuous: with QLOT_LOCKED unset,
;;; the update path does NOT fail fast, reaches the network, and trips a trap.
;;; Mirrors the structure of env-unset-update-takes-online-path for the offline
;;; discriminator.
(deftest env-locked-unset-update-takes-online-path
  (with-tmp-directory (tmp)
    (let* ((cache-dir (uiop:ensure-directory-pathname (merge-pathnames #P"cache/" tmp))))
      ;; Bootstrap .qlot/ so check-local-quicklisp passes.
      (populate-warm-cache cache-dir "env-modes-test-v1")
      (bootstrap-qlot tmp cache-dir *ql-default-lock-entry*)
      (with-env (("QLOT_LOCKED" . nil) ("QLOT_OFFLINE" . nil) ("QLOT_NO_CACHE" . nil))
        (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
              (*cache-directory* cache-dir)
              (*locked* nil)
              (*offline* nil))
          (with-network-traps
            (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-update '())))))
              (ok (typep err 'network-trap-fired)
                  "with QLOT_LOCKED unset, qlot update takes the online path (a real network call is attempted)"))))))))

;;; --- Gate 6: QLOT_LOCKED blocks qlot add / qlot remove before mutating qlfile ---
;;;
;;; Locked mode forbids changing the dependency set.  add/remove must fail fast
;;; with locked-operation-rejected BEFORE the qlfile is touched, so the file is
;;; left byte-identical and the user gets an actionable error rather than the
;;; confusing missing-projects / unnecessary-projects that the lock-currency
;;; check would otherwise surface for the library just added or removed.

(deftest env-locked-add-fails-fast
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp)))
      (write-text-file qlfile "ql alexandria")
      (let ((before (uiop:read-file-string qlfile)))
        (with-env (("QLOT_LOCKED" . "1") ("QLOT_OFFLINE" . nil) ("QLOT_NO_CACHE" . nil))
          (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
                (*locked* nil)
                (*offline* nil))
            (with-network-traps
              (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-add '("ql" "mito"))))))
                (ok (and err (typep err 'locked-operation-rejected))
                    "QLOT_LOCKED + qlot add signals locked-operation-rejected (env honored on add path)")
                (ok (not (typep err 'network-trap-fired))
                    "no real network/git call was attempted -- the locked guard fired first")
                (ok (equal before (uiop:read-file-string qlfile))
                    "qlfile is left byte-identical after a rejected locked add")))))))))

(deftest env-locked-remove-fails-fast
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp)))
      (write-text-file qlfile "ql alexandria")
      (let ((before (uiop:read-file-string qlfile)))
        (with-env (("QLOT_LOCKED" . "1") ("QLOT_OFFLINE" . nil) ("QLOT_NO_CACHE" . nil))
          (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname tmp))
                (*locked* nil)
                (*offline* nil))
            (with-network-traps
              (let ((err (nth-value 1 (ignore-errors (run-command '#:qlot-command-remove '("alexandria"))))))
                (ok (and err (typep err 'locked-operation-rejected))
                    "QLOT_LOCKED + qlot remove signals locked-operation-rejected (env honored on remove path)")
                (ok (not (typep err 'network-trap-fired))
                    "no real network/git call was attempted -- the locked guard fired first")
                (ok (equal before (uiop:read-file-string qlfile))
                    "qlfile is left byte-identical after a rejected locked remove")))))))))
