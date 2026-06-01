(defpackage #:qlot-tests/install/offline-evicted-cache
  (:use #:cl
        #:rove)
  (:import-from #:qlot/modes
                #:*offline*)
  (:import-from #:qlot/errors
                #:offline-cache-conflict
                #:offline-cache-miss)
  (:import-from #:qlot/cache
                #:*cache-directory*
                #:*cache-enabled*
                #:validate-dist-installation)
  (:import-from #:qlot/install
                #:invalidate-broken-dist)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/install/offline-evicted-cache)

(defun write-text-file (path content)
  (ensure-directories-exist path)
  (with-open-file (out path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (write-string content out)))

(defun dist-lock-entry (version)
  (format nil
          "(\"quicklisp\" . (:class qlot.source.dist:source-dist \
:initargs (:project-name \"quicklisp\" :%version :latest \
:distribution \"https://beta.quicklisp.org/dist/quicklisp.txt\") \
:version ~S))"
          version))

;;; -----------------------------------------------------------------------
;;; AC1 + AC2: Offline install with evicted cache must be an idempotent no-op.
;;;
;;; Scenario:
;;;   1. First offline install with a warm cache restores the dist and
;;;      creates a software/mylib → cache/sources/.../mylib symlink.
;;;   2. Cache is evicted (deleted) so the symlink becomes dangling.
;;;   3. Second offline install must NOT signal offline-cache-miss (AC1) and
;;;      must NOT delete the dist directory via invalidate-broken-dist (AC2).
;;;
;;; Without the fix:
;;;   - sources-to-install filter calls invalidate-broken-dist (deletes dist dir)
;;;   - install loop hits the *offline* guard → signals offline-cache-miss
;;; -----------------------------------------------------------------------
(deftest offline-evicted-cache-noop
  (with-tmp-directory (tmp)
    (let* ((qlfile  (merge-pathnames #P"qlfile"  tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome  (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           (version "evicted-cache-test-v1")
           ;; cache-key for source-dist "quicklisp" @ version: ("dist" "quicklisp" version)
           (meta-dir (merge-pathnames
                      (format nil "metadata/dist/quicklisp/~A/" version)
                      cache-dir))
           (src-dir  (merge-pathnames
                      (format nil "sources/dist/quicklisp/~A/" version)
                      cache-dir))
           ;; Subdir inside sources-cache so restore-from-cache creates a real symlink.
           (lib-src-dir (merge-pathnames "mylib/" src-dir)))
      (write-text-file qlfile "")
      (write-text-file lockfile (dist-lock-entry version))

      ;; Warm metadata cache.
      (ensure-directories-exist meta-dir)
      (write-text-file (merge-pathnames "distinfo.txt" meta-dir)
                       (format nil "name: quicklisp~%version: ~A~%" version))
      (write-text-file (merge-pathnames "systems.txt"  meta-dir) "")
      (write-text-file (merge-pathnames "releases.txt" meta-dir) "")
      ;; Warm sources cache: one fake lib dir → restore-from-cache creates a symlink.
      (ensure-directories-exist lib-src-dir)
      (write-text-file (merge-pathnames "mylib.lisp" lib-src-dir) "; stub")

      ;; --- STEP 1: first offline install with warm cache ---
      (let ((*cache-directory* cache-dir)
            (*offline* t))
        (let ((err (nth-value 1
                     (ignore-errors
                       (qlot/install::apply-qlfile-to-qlhome qlfile qlhome)))))
          (ok (null err)
              "first offline install (warm cache) succeeds — precondition")))

      ;; Preconditions: dist dir and software/ with symlink.
      (let ((dist-dir  (merge-pathnames #P"dists/quicklisp/" qlhome))
            (sw-dir    (merge-pathnames #P"dists/quicklisp/software/" qlhome)))
        (ok (uiop:directory-exists-p dist-dir)
            "dist directory exists after first install")
        (ok (uiop:directory-exists-p sw-dir)
            "software/ directory exists after first install")

        ;; --- STEP 2: evict the cache → symlinks become dangling ---
        (uiop:delete-directory-tree cache-dir :validate t)

        ;; Confirm validate-dist-installation now returns NIL (dangling symlinks).
        ;; This is the precondition that currently triggers the bug.
        (ng (validate-dist-installation dist-dir)
            "validate-dist-installation returns nil after eviction (precondition for the bug)")

        ;; --- STEP 3: second offline install with evicted cache ---
        ;; Use a fresh (non-existent) cache-dir so cache-exists-p returns NIL.
        (let* ((*cache-directory* (uiop:ensure-directory-pathname
                                   (merge-pathnames #P"cache2/" tmp)))
               (*offline* t)
               (invalidate-broken-called nil)
               (original-invalidate (symbol-function 'invalidate-broken-dist)))
          (unwind-protect
               (progn
                 ;; Intercept invalidate-broken-dist: it must NOT fire when *offline*.
                 ;; Without the fix, it fires and deletes dist-dir regardless of *offline*.
                 ;; We cannot rely on directory-existence alone because invalidate-broken-dist
                 ;; has (ignore-errors ...) inside and can fail silently — the dir would
                 ;; survive but the bug path would still have run.
                 (setf (symbol-function 'invalidate-broken-dist)
                       (lambda (path)
                         (setf invalidate-broken-called t)
                         (funcall original-invalidate path)))
                 (let ((err (nth-value 1
                              (ignore-errors
                                (qlot/install::apply-qlfile-to-qlhome qlfile qlhome)))))
                   ;; AC1: idempotent no-op — must not signal offline-cache-miss.
                   (ok (null err)
                       "AC1: second offline install with evicted cache completes without error")
                   ;; AC2 (mechanism): invalidate-broken-dist must not be called when *offline*.
                   ;; This is the direct check for Fix A — verifies the filter short-circuits
                   ;; before calling invalidate-broken-dist, not just that the dir happens to exist.
                   (ng invalidate-broken-called
                       "AC2: invalidate-broken-dist was not called under *offline*")
                   ;; AC2 (effect): dist directory must survive.
                   (ok (uiop:directory-exists-p dist-dir)
                       "AC2: dist directory preserved — invalidate-broken-dist did not run under *offline*")))
            (setf (symbol-function 'invalidate-broken-dist) original-invalidate)))))))

;;; -----------------------------------------------------------------------
;;; AC3: Direct dynamic binding of *offline* t and *cache-enabled* nil must
;;; signal offline-cache-conflict at the top of apply-qlfile-to-qlhome,
;;; before any per-source work.
;;;
;;; initialize-modes only detects this conflict via env vars.  Directly
;;; binding the vars bypasses that guard.  The fix adds a check inside
;;; apply-qlfile-to-qlhome (or equivalent choke point) so the conflict is
;;; caught regardless of how the vars were set.
;;;
;;; Without the fix: each source hits the *offline* guard → offline-cache-miss.
;;; After the fix:   offline-cache-conflict fires before per-source work.
;;; -----------------------------------------------------------------------
(deftest offline-direct-binding-conflict
  (with-tmp-directory (tmp)
    (let* ((qlfile   (merge-pathnames #P"qlfile"       tmp))
           (lockfile  (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome   (merge-pathnames #P".qlot/"       tmp))
           (version  "direct-binding-conflict-v1"))
      (write-text-file qlfile "")
      (write-text-file lockfile (dist-lock-entry version))

      ;; Direct bindings — NOT going through initialize-modes / env vars.
      (let ((*offline*       t)
            (*cache-enabled* nil))
        ;; AC3: must signal offline-cache-conflict, NOT offline-cache-miss.
        (ok (signals
              (qlot/install::apply-qlfile-to-qlhome qlfile qlhome)
              'offline-cache-conflict)
            "AC3: direct binding of *offline* t + *cache-enabled* nil signals offline-cache-conflict")))))
