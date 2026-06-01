(defpackage #:qlot-tests/install/frozen
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install
                #:install-qlfile)
  (:import-from #:qlot/modes
                #:*offline*
                #:*locked*)
  (:import-from #:qlot/errors
                #:offline-cache-miss)
  (:import-from #:qlot/cache
                #:*cache-directory*)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/install/frozen)

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

;;; --frozen = --offline + --locked.  With both *offline* and *locked* true,
;;; a warm cache, and a lock that exactly matches qlfile, install must:
;;;  - complete without error (offline warm-cache contract)
;;;  - leave qlfile.lock byte-identical (locked contract)
;;;  - install the dist metadata into qlhome
;;;
;;; The two flags are independent in the implementation; this test verifies
;;; that composing them does not introduce an unexpected interaction — e.g. a
;;; locked check that blocks the offline path, or an offline guard that
;;; overwrites the lock.
(deftest frozen-warm-cache-success
  (with-tmp-directory (tmp)
    (let* ((qlfile (merge-pathnames #P"qlfile" tmp))
           (lockfile (merge-pathnames #P"qlfile.lock" tmp))
           (qlhome (merge-pathnames #P".qlot/" tmp))
           (cache-dir (uiop:ensure-directory-pathname
                       (merge-pathnames #P"cache/" tmp)))
           (version "frozen-warm-cache-test-v1")
           (meta-dir (merge-pathnames
                      (uiop:strcat "metadata/dist/quicklisp/" version "/")
                      cache-dir))
           (src-dir (merge-pathnames
                     (uiop:strcat "sources/dist/quicklisp/" version "/")
                     cache-dir)))
      ;; Empty qlfile: default quicklisp source is added by read-qlfile-for-install.
      (write-text-file qlfile "")
      ;; Lock pins quicklisp to the version we pre-populate in the cache.
      ;; URL must use https:// to match quicklisp-distinfo-url.
      (write-text-file lockfile
                       (format nil
                               "(\"quicklisp\" . (:class qlot.source.dist:source-dist ~
                                :initargs (:project-name \"quicklisp\" :%version :latest ~
                                :distribution \"https://beta.quicklisp.org/dist/quicklisp.txt\") ~
                                :version ~S))"
                               version))
      ;; Pre-populate the shared cache with the metadata files the offline
      ;; install needs.  Content need not be valid dist format; only
      ;; existence is checked by cache-exists-p / validate-dist-installation.
      (ensure-directories-exist meta-dir)
      (ensure-directories-exist src-dir)
      (write-text-file (merge-pathnames "distinfo.txt" meta-dir)
                       (format nil "name: quicklisp~%version: ~A~%" version))
      (write-text-file (merge-pathnames "systems.txt" meta-dir) "")
      (write-text-file (merge-pathnames "releases.txt" meta-dir) "")
      (let ((lock-bytes-before (file-bytes lockfile)))
        (let* ((*cache-directory* cache-dir)
               (*offline* t)
               (*locked* t)
               (err (nth-value 1
                      (ignore-errors
                        (qlot/install::apply-qlfile-to-qlhome qlfile qlhome)))))
          (ok (null err)
              "frozen (offline+locked) install against warm cache + matching lock completes without error")
          (ok (uiop:file-exists-p
               (merge-pathnames #P"dists/quicklisp/distinfo.txt" qlhome))
              "dist metadata is installed in qlhome after frozen warm-cache install")
          (ok (equalp lock-bytes-before (file-bytes lockfile))
              "qlfile.lock is byte-identical after frozen install (locked contract preserved)"))))))

;;; README.markdown must document all three install modes and both env vars.
;;; This test fails until the implementation phase adds the documentation.
(deftest frozen-readme-documents-modes
  (let* ((readme (asdf:system-relative-pathname :qlot #P"README.markdown"))
         (content (uiop:read-file-string readme)))
    (ok (search "--offline" content :test #'char-equal)
        "README.markdown documents --offline flag")
    (ok (search "--locked" content :test #'char-equal)
        "README.markdown documents --locked flag")
    (ok (search "--frozen" content :test #'char-equal)
        "README.markdown documents --frozen flag")
    (ok (search "QLOT_OFFLINE" content)
        "README.markdown documents QLOT_OFFLINE env var")
    (ok (search "QLOT_LOCKED" content)
        "README.markdown documents QLOT_LOCKED env var")))

;;; With both *offline* and *locked* true and a source not present in the
;;; shared cache, install must signal offline-cache-miss (not fall through
;;; to a network install or a locked-check error).  No network access may
;;; be made — an explicit offline-network-access trap verifies this.
(deftest frozen-cache-miss
  (let ((ocm-sym (find-symbol "OFFLINE-CACHE-MISS" :qlot/errors))
        (ona-sym (find-symbol "OFFLINE-NETWORK-ACCESS" :qlot/errors)))
    (ok ocm-sym "qlot/errors:offline-cache-miss must be defined")
    (when ocm-sym
      (with-tmp-directory (tmp)
        (let ((qlfile (merge-pathnames #P"qlfile" tmp))
              (lockfile (merge-pathnames #P"qlfile.lock" tmp))
              (qlhome (merge-pathnames #P".qlot/" tmp))
              (network-trap-triggered nil))
          ;; Pin quicklisp to a version that will never be in the cache.
          (write-text-file qlfile "ql :all :latest")
          (write-text-file lockfile
                           "(\"quicklisp\" . (:class qlot.source.dist:source-dist :initargs (:project-name \"quicklisp\" :%version :latest :distribution \"http://beta.quicklisp.org/dist/quicklisp.txt\") :version \"frozen-cache-miss-unique-version\"))")
          ;; handler-bind is nested inside ignore-errors so it is established
          ;; after ignore-errors on the handler chain and is checked first.
          ;; It records offline-network-access without altering control flow,
          ;; letting ignore-errors absorb any condition that propagates.
          (let* ((caught
                  (nth-value 1
                    (ignore-errors
                      (handler-bind
                          ((error (lambda (c)
                                    (when (and ona-sym (typep c ona-sym))
                                      (setf network-trap-triggered t)))))
                        (let ((*offline* t)
                              (*locked* t))
                          (install-qlfile qlfile :quicklisp-home qlhome)))))))
            (ng network-trap-triggered
                "frozen mode must not trigger any network access on a cache miss")
            (ok (and caught (typep caught ocm-sym))
                "frozen mode signals offline-cache-miss when source is absent from cache")
            (when (and caught (typep caught ocm-sym))
              (let ((report (with-output-to-string (s) (princ caught s))))
                (ok (search "quicklisp" report :test #'char-equal)
                    "offline-cache-miss report names the missing project")
                (ok (search "frozen-cache-miss-unique-version" report :test #'char-equal)
                    "offline-cache-miss report names the missing version")))))))))
