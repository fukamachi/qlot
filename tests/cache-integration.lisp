(defpackage #:qlot-tests/cache-integration
  (:use #:cl
        #:rove)
  (:import-from #:qlot/cache
                #:*cache-directory*
                #:*cache-enabled*
                #:cache-exists-p
                #:clear-cache)
  (:import-from #:qlot/install
                #:install-qlfile)
  (:import-from #:qlot/source/base
                #:make-source
                #:source-version)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/cache-integration)

(defun copy-cache-test-qlfile (tmp-dir)
  "Copy the cache test qlfile and lockfile to TMP-DIR."
  (uiop:copy-file (asdf:system-relative-pathname :qlot #P"tests/data/qlfile-cache-test")
                  (merge-pathnames #P"qlfile" tmp-dir))
  (uiop:copy-file (asdf:system-relative-pathname :qlot #P"tests/data/qlfile-cache-test.lock")
                  (merge-pathnames #P"qlfile.lock" tmp-dir)))

(defun strip-trailing-slash (path)
  "Remove trailing slash from a path string."
  (let ((str (if (pathnamep path) (namestring path) path)))
    (if (and (< 1 (length str))
             (char= (char str (1- (length str))) #\/))
        (subseq str 0 (1- (length str)))
        str)))

(defun symlink-p (path)
  "Return T if PATH is a symbolic link."
  #+sbcl
  (handler-case
      ;; Must strip trailing slash - lstat with trailing slash follows symlinks!
      (let ((stat (sb-posix:lstat (strip-trailing-slash path))))
        (= (logand (sb-posix:stat-mode stat) #o170000)
           #o120000))
    (error () nil))
  #-sbcl
  ;; Fallback: Check if truename differs from path
  (let ((truename (ignore-errors (truename path))))
    (and truename
         (not (equal (namestring (uiop:ensure-directory-pathname path))
                     (namestring truename))))))

(defun count-symlinks-in-software (qlhome)
  "Count the number of symlinks in the software directories under qlhome/dists."
  (let ((count 0))
    (dolist (dist-dir (uiop:subdirectories (merge-pathnames #P"dists/" qlhome)))
      (let ((software-dir (merge-pathnames "software/" dist-dir)))
        (when (uiop:directory-exists-p software-dir)
          (dolist (entry (uiop:subdirectories software-dir))
            (when (symlink-p entry)
              (incf count))))))
    count))

(deftest test-fresh-install-populates-cache
  "Fresh install with empty cache should populate the cache."
  (with-tmp-directory (cache-dir)
    (with-tmp-directory (project-dir)
      (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
            (*cache-enabled* t))
        (copy-cache-test-qlfile project-dir)
        (let ((qlhome (merge-pathnames #P".qlot/" project-dir))
              (qlfile (merge-pathnames #P"qlfile" project-dir)))
          ;; First install - should download and cache
          (install-qlfile qlfile :quicklisp-home qlhome)
          ;; Verify installation
          (ok (uiop:directory-exists-p (merge-pathnames "dists/quicklisp/" qlhome))
              "Quicklisp dist should be installed")
          (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome))
              "cl-ppcre dist should be installed")
          ;; Verify cache was populated
          (let ((source (make-source :ql "cl-ppcre" :latest)))
            (setf (source-version source) "ql-2018-08-31")
            (ok (cache-exists-p source)
                "Cache should be populated after fresh install")))))))

(deftest test-reinstall-uses-cache
  "Reinstall with populated cache should use cached sources."
  (with-tmp-directory (cache-dir)
    (with-tmp-directory (project-dir)
      (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
            (*cache-enabled* t))
        (copy-cache-test-qlfile project-dir)
        (let ((qlhome (merge-pathnames #P".qlot/" project-dir))
              (qlfile (merge-pathnames #P"qlfile" project-dir)))
          ;; First install - populates cache
          (install-qlfile qlfile :quicklisp-home qlhome)
          ;; Delete the .qlot directory to simulate fresh project
          (uiop:delete-directory-tree qlhome :validate t)
          ;; Second install - should use cache
          (install-qlfile qlfile :quicklisp-home qlhome)
          ;; Verify installation
          (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome))
              "cl-ppcre dist should be reinstalled from cache")
          ;; Verify symlinks are used
          (let ((symlink-count (count-symlinks-in-software qlhome)))
            (ok (< 0 symlink-count)
                (format nil "Should use symlinks (found ~A)" symlink-count))))))))

(deftest test-cache-miss-after-clear
  "After clearing cache, sources should be re-downloaded."
  (with-tmp-directory (cache-dir)
    (with-tmp-directory (project-dir)
      (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
            (*cache-enabled* t))
        (copy-cache-test-qlfile project-dir)
        (let ((qlhome (merge-pathnames #P".qlot/" project-dir))
              (qlfile (merge-pathnames #P"qlfile" project-dir)))
          ;; First install - populates cache
          (install-qlfile qlfile :quicklisp-home qlhome)
          (let ((source (make-source :ql "cl-ppcre" :latest)))
            (setf (source-version source) "ql-2018-08-31")
            (ok (cache-exists-p source)
                "Cache should be populated after first install"))
          ;; Clear the cache
          (clear-cache)
          (let ((source (make-source :ql "cl-ppcre" :latest)))
            (setf (source-version source) "ql-2018-08-31")
            (ng (cache-exists-p source)
                "Cache should be empty after clear-cache"))
          ;; Delete .qlot and reinstall
          (uiop:delete-directory-tree qlhome :validate t)
          (install-qlfile qlfile :quicklisp-home qlhome)
          ;; Verify cache is populated again
          (let ((source (make-source :ql "cl-ppcre" :latest)))
            (setf (source-version source) "ql-2018-08-31")
            (ok (cache-exists-p source)
                "Cache should be repopulated after reinstall")))))))

(deftest test-no-cache-flag
  "With *cache-enabled* set to NIL, cache should be bypassed."
  (with-tmp-directory (cache-dir)
    (with-tmp-directory (project-dir)
      (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
            (*cache-enabled* nil))  ; Cache disabled
        (copy-cache-test-qlfile project-dir)
        (let ((qlhome (merge-pathnames #P".qlot/" project-dir))
              (qlfile (merge-pathnames #P"qlfile" project-dir)))
          ;; Install with cache disabled
          (install-qlfile qlfile :quicklisp-home qlhome)
          ;; Verify installation works
          (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome))
              "cl-ppcre dist should be installed even with cache disabled")
          ;; Verify cache was NOT populated
          (let ((source (make-source :ql "cl-ppcre" :latest))
                (*cache-enabled* t))  ; Need to enable to check
            (setf (source-version source) "ql-2018-08-31")
            (ng (cache-exists-p source)
                "Cache should NOT be populated when cache is disabled")))))))

(deftest test-multiple-projects-share-cache
  "Multiple projects should share the same cache."
  (with-tmp-directory (cache-dir)
    (with-tmp-directory (project1-dir)
      (with-tmp-directory (project2-dir)
        (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
              (*cache-enabled* t))
          ;; Setup both projects
          (copy-cache-test-qlfile project1-dir)
          (copy-cache-test-qlfile project2-dir)
          (let ((qlhome1 (merge-pathnames #P".qlot/" project1-dir))
                (qlhome2 (merge-pathnames #P".qlot/" project2-dir))
                (qlfile1 (merge-pathnames #P"qlfile" project1-dir))
                (qlfile2 (merge-pathnames #P"qlfile" project2-dir)))
            ;; Install first project
            (install-qlfile qlfile1 :quicklisp-home qlhome1)
            ;; Install second project - should use cache
            (install-qlfile qlfile2 :quicklisp-home qlhome2)
            ;; Verify both installations
            (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome1))
                "First project should have cl-ppcre installed")
            (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome2))
                "Second project should have cl-ppcre installed")
            ;; Verify second project uses symlinks
            (let ((symlink-count (count-symlinks-in-software qlhome2)))
              (ok (< 0 symlink-count)
                  (format nil "Second project should use symlinks (found ~A)" symlink-count)))))))))

;;; Helper functions for overlap tests

(defun copy-overlap-qlfile (variant tmp-dir)
  "Copy the overlap test qlfile VARIANT (a, b, or c) to TMP-DIR."
  (let ((base-name (format nil "qlfile-cache-overlap-~A" variant)))
    (uiop:copy-file (asdf:system-relative-pathname :qlot (format nil "tests/data/~A" base-name))
                    (merge-pathnames #P"qlfile" tmp-dir))
    (uiop:copy-file (asdf:system-relative-pathname :qlot (format nil "tests/data/~A.lock" base-name))
                    (merge-pathnames #P"qlfile.lock" tmp-dir))))

(deftest test-partially-overlapping-dependencies
  "Projects with partially overlapping dependencies should share common deps.
Project A: cl-ppcre
Project B: cl-ppcre + split-sequence
Both should share cl-ppcre from cache."
  (with-tmp-directory (cache-dir)
    (with-tmp-directory (project-a-dir)
      (with-tmp-directory (project-b-dir)
        (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
              (*cache-enabled* t))
          ;; Setup projects with different but overlapping deps
          (copy-overlap-qlfile "a" project-a-dir)  ; cl-ppcre only
          (copy-overlap-qlfile "b" project-b-dir)  ; cl-ppcre + split-sequence
          (let ((qlhome-a (merge-pathnames #P".qlot/" project-a-dir))
                (qlhome-b (merge-pathnames #P".qlot/" project-b-dir))
                (qlfile-a (merge-pathnames #P"qlfile" project-a-dir))
                (qlfile-b (merge-pathnames #P"qlfile" project-b-dir)))
            ;; Install project A first - downloads cl-ppcre
            (install-qlfile qlfile-a :quicklisp-home qlhome-a)
            (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome-a))
                "Project A should have cl-ppcre installed")
            ;; Install project B - should reuse cl-ppcre from cache, download split-sequence
            (install-qlfile qlfile-b :quicklisp-home qlhome-b)
            (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome-b))
                "Project B should have cl-ppcre installed")
            (ok (uiop:directory-exists-p (merge-pathnames "dists/split-sequence/" qlhome-b))
                "Project B should have split-sequence installed")
            ;; Verify project B uses symlinks for shared dependency
            (let ((symlink-count (count-symlinks-in-software qlhome-b)))
              (ok (< 0 symlink-count)
                  (format nil "Project B should use symlinks for cached deps (found ~A)" symlink-count)))))))))

(deftest test-three-projects-chain-sharing
  "Three projects with chain of overlapping dependencies.
Project A: cl-ppcre
Project B: cl-ppcre + split-sequence
Project C: split-sequence + alexandria
Each project shares some dependencies with its neighbor."
  (with-tmp-directory (cache-dir)
    (with-tmp-directory (project-a-dir)
      (with-tmp-directory (project-b-dir)
        (with-tmp-directory (project-c-dir)
          (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
                (*cache-enabled* t))
            ;; Setup three projects with chain overlap
            (copy-overlap-qlfile "a" project-a-dir)  ; cl-ppcre
            (copy-overlap-qlfile "b" project-b-dir)  ; cl-ppcre + split-sequence
            (copy-overlap-qlfile "c" project-c-dir)  ; split-sequence + alexandria
            (let ((qlhome-a (merge-pathnames #P".qlot/" project-a-dir))
                  (qlhome-b (merge-pathnames #P".qlot/" project-b-dir))
                  (qlhome-c (merge-pathnames #P".qlot/" project-c-dir))
                  (qlfile-a (merge-pathnames #P"qlfile" project-a-dir))
                  (qlfile-b (merge-pathnames #P"qlfile" project-b-dir))
                  (qlfile-c (merge-pathnames #P"qlfile" project-c-dir)))
              ;; Install project A - downloads cl-ppcre
              (install-qlfile qlfile-a :quicklisp-home qlhome-a)
              (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome-a))
                  "Project A: cl-ppcre installed")
              ;; Install project B - reuses cl-ppcre, downloads split-sequence
              (install-qlfile qlfile-b :quicklisp-home qlhome-b)
              (ok (uiop:directory-exists-p (merge-pathnames "dists/cl-ppcre/" qlhome-b))
                  "Project B: cl-ppcre installed (from cache)")
              (ok (uiop:directory-exists-p (merge-pathnames "dists/split-sequence/" qlhome-b))
                  "Project B: split-sequence installed")
              ;; Install project C - reuses split-sequence, downloads alexandria
              (install-qlfile qlfile-c :quicklisp-home qlhome-c)
              (ok (uiop:directory-exists-p (merge-pathnames "dists/split-sequence/" qlhome-c))
                  "Project C: split-sequence installed (from cache)")
              (ok (uiop:directory-exists-p (merge-pathnames "dists/alexandria/" qlhome-c))
                  "Project C: alexandria installed")
              ;; Verify projects B and C use symlinks
              (let ((symlinks-b (count-symlinks-in-software qlhome-b))
                    (symlinks-c (count-symlinks-in-software qlhome-c)))
                (ok (< 0 symlinks-b)
                    (format nil "Project B should use symlinks (found ~A)" symlinks-b))
                (ok (< 0 symlinks-c)
                    (format nil "Project C should use symlinks (found ~A)" symlinks-c))))))))))

(deftest test-reinstall-preserves-symlinks
  "Reinstalling a project should preserve symlinks to cached releases."
  (with-tmp-directory (cache-dir)
    (with-tmp-directory (project-dir)
      (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
            (*cache-enabled* t))
        (copy-cache-test-qlfile project-dir)
        (let ((qlhome (merge-pathnames #P".qlot/" project-dir))
              (qlfile (merge-pathnames #P"qlfile" project-dir)))
          ;; First install - populates cache
          (install-qlfile qlfile :quicklisp-home qlhome)
          (let ((symlinks-before (count-symlinks-in-software qlhome)))
            ;; Delete .qlot and reinstall
            (uiop:delete-directory-tree qlhome :validate t)
            (install-qlfile qlfile :quicklisp-home qlhome)
            ;; Verify symlinks are used on reinstall
            (let ((symlinks-after (count-symlinks-in-software qlhome)))
              (ok (< 0 symlinks-after)
                  (format nil "Reinstall should use symlinks (found ~A)" symlinks-after))
              ;; Second install should have at least as many symlinks
              (ok (<= symlinks-before symlinks-after)
                  (format nil "Reinstall symlinks (~A) >= first install (~A)"
                          symlinks-after symlinks-before)))))))))
