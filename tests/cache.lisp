(defpackage #:qlot-tests/cache
  (:use #:cl
        #:rove)
  (:import-from #:qlot/cache
                #:*cache-directory*
                #:*cache-enabled*
                #:cache-key
                #:cache-exists-p
                #:restore-from-cache
                #:save-to-cache
                #:cache-metadata-path
                #:cache-sources-path
                #:normalize-git-url
                #:split-path
                #:url-has-credentials-p
                #:staging-path
                #:validate-dist-installation)
  (:import-from #:qlot/source/base
                #:make-source
                #:source-version)
  (:import-from #:qlot/source/git
                #:source-git-ref)
  (:import-from #:qlot/source/github
                #:source-github-ref)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/cache)

(defun make-symlink (target link)
  #+sbcl
  (sb-posix:symlink (namestring target) (namestring link))
  #-sbcl
  (uiop:run-program (list "ln" "-s"
                          (uiop:native-namestring target)
                          (uiop:native-namestring link))))

(deftest test-normalize-git-url
  (testing "HTTPS URLs"
    (ok (equal "github.com/foo/bar"
               (normalize-git-url "https://github.com/foo/bar.git")))
    (ok (equal "github.com/foo/bar"
               (normalize-git-url "https://github.com/foo/bar")))
    (ok (equal "gitlab.com/group/subgroup/project"
               (normalize-git-url "https://gitlab.com/group/subgroup/project.git"))))
  (testing "SSH URLs"
    (ok (equal "github.com/foo/bar"
               (normalize-git-url "git@github.com:foo/bar.git")))
    (ok (equal "gitlab.com/user/project"
               (normalize-git-url "ssh://git@gitlab.com/user/project.git")))
    (ok (equal "bitbucket.org/team/repo"
               (normalize-git-url "git@bitbucket.org:team/repo.git"))))
  (testing "Edge cases"
    (ok (equal "github.com/foo/bar"
               (normalize-git-url "https://github.com/foo/bar/")))
    (ok (equal "bitbucket.org/user/repo"
               (normalize-git-url "https://BITBUCKET.ORG/user/repo")))
    (ok (equal "github.com/foo/bar"
               (normalize-git-url "http://github.com/foo/bar")))))

(deftest test-split-path
  (ok (equal '("github.com" "foo" "bar")
             (split-path "github.com/foo/bar")))
  (ok (equal '("a" "b" "c")
             (split-path "a/b/c")))
  (ok (equal '("single")
             (split-path "single")))
  (ok (null (split-path ""))))

(deftest test-url-has-credentials-p
  (testing "URLs with credentials"
    (ok (url-has-credentials-p "https://user:token@github.com/foo/bar"))
    (ok (url-has-credentials-p "https://user:pass@gitlab.com/foo/bar.git"))
    (ok (url-has-credentials-p "https://oauth2:token@github.com/foo/bar")))
  (testing "URLs without credentials"
    (ng (url-has-credentials-p "https://github.com/foo/bar"))
    (ng (url-has-credentials-p "git@github.com:foo/bar.git"))
    (ng (url-has-credentials-p "ssh://git@github.com/foo/bar"))
    (ng (url-has-credentials-p nil))))

(deftest test-staging-path
  (let ((path #P"/home/user/.cache/qlot/metadata/ql/quicklisp/alexandria-20250622/"))
    (ok (equal #P"/home/user/.cache/qlot/metadata/ql/quicklisp/alexandria-20250622.staging/"
               (staging-path path)))))

(deftest test-cache-key-source-ql
  (let ((source (make-source :ql "alexandria" :latest)))
    (setf (source-version source) "20250622")
    (ok (equal '("ql" "quicklisp" "alexandria-20250622")
               (cache-key source)))))

(deftest test-cache-key-source-git
  (let ((source (make-source :git "mylib" "https://github.com/user/mylib.git"
                             :ref "abc123")))
    (setf (source-git-ref source) "abc123def456")
    (ok (equal '("git" "github.com" "user" "mylib" "abc123def456")
               (cache-key source)))))

(deftest test-cache-key-source-github
  (let ((source (make-source :github "user/repo" :ref "abc123")))
    (setf (source-github-ref source) "abc123def456")
    (ok (equal '("github" "user" "repo" "abc123def456")
               (cache-key source)))))

(deftest test-cache-exists-p-empty
  (with-tmp-directory (tmp)
    (let ((*cache-directory* (uiop:ensure-directory-pathname tmp))
          (*cache-enabled* t))
      (let ((source (make-source :ql "nonexistent" :latest)))
        (setf (source-version source) "20250622")
        (ng (cache-exists-p source))))))

(deftest test-validate-dist-installation
  (testing "Valid installation"
    (with-tmp-directory (tmp-dir)
      (let ((dist-path (uiop:ensure-directory-pathname tmp-dir)))
        (ensure-directories-exist (merge-pathnames "software/mylib/" dist-path))
        (with-open-file (out (merge-pathnames "software/mylib/test.lisp" dist-path)
                             :direction :output
                             :if-does-not-exist :create)
          (write-line ";; test" out))
        (ok (validate-dist-installation dist-path)))))
  (testing "Broken symlink"
    (with-tmp-directory (tmp-dir)
      (let ((dist-path (uiop:ensure-directory-pathname tmp-dir)))
        (ensure-directories-exist (merge-pathnames "software/" dist-path))
        (let ((link (merge-pathnames "software/broken" dist-path)))
          (make-symlink #P"/nonexistent/path" link))
        (ng (validate-dist-installation dist-path))))))

(deftest test-restore-from-cache
  (with-tmp-directory (cache-root)
    (let ((*cache-directory* (uiop:ensure-directory-pathname cache-root))
          (*cache-enabled* t)
          (source (make-source :ql "alexandria" :latest)))
      (setf (source-version source) "20250622")
      (let* ((metadata (cache-metadata-path source))
             (sources (cache-sources-path source))
             (project-dir (merge-pathnames "alexandria-20250622/" sources)))
        (ensure-directories-exist metadata)
        (dolist (file '("distinfo.txt" "systems.txt" "releases.txt"))
          (with-open-file (s (merge-pathnames file metadata)
                             :direction :output
                             :if-does-not-exist :create)
            (write-line "dummy" s)))
        (ensure-directories-exist project-dir)
        (with-open-file (s (merge-pathnames "alexandria.asd" project-dir)
                           :direction :output
                           :if-does-not-exist :create)
          (write-line ";; asd" s))
        (with-tmp-directory (dist-path)
          (let ((result (restore-from-cache source dist-path)))
            (ok result)
            (ok (uiop:file-exists-p (merge-pathnames "distinfo.txt" dist-path)))
            (let ((link (merge-pathnames "software/alexandria-20250622/" dist-path)))
              (ok (uiop:directory-exists-p link))
              (ok (or (equal (truename link) (truename project-dir))
                      (uiop:file-exists-p (merge-pathnames "alexandria.asd" link)))))
            (ok (uiop:file-exists-p (merge-pathnames "installed/releases/alexandria-20250622.txt"
                                                     dist-path)))
            (ok (uiop:file-exists-p (merge-pathnames "installed/systems/alexandria.txt"
                                                     dist-path)))))))))

(deftest test-save-to-cache
  (with-tmp-directory (cache-root)
    (let ((*cache-directory* (uiop:ensure-directory-pathname cache-root))
          (*cache-enabled* t)
          (source (make-source :ql "serapeum" :latest)))
      (setf (source-version source) "20240601")
      (with-tmp-directory (dist-path)
        (let ((metadata-dir dist-path)
              (software-dir (merge-pathnames "software/serapeum-20240601/" dist-path)))
          (dolist (file '("distinfo.txt" "systems.txt" "releases.txt"))
            (with-open-file (s (merge-pathnames file metadata-dir)
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
              (write-line file s)))
          (ensure-directories-exist software-dir)
          (with-open-file (s (merge-pathnames "serapeum.asd" software-dir)
                             :direction :output
                             :if-does-not-exist :create)
            (write-line ";; asd" s))
          (save-to-cache source dist-path)
          (ok (cache-exists-p source))
          (let* ((cache-src (cache-sources-path source))
                 (cache-project (merge-pathnames "serapeum-20240601/" cache-src))
                 (link (merge-pathnames "software/serapeum-20240601/" dist-path)))
            (ok (uiop:directory-exists-p cache-project))
            (ok (uiop:directory-exists-p link))
            (ok (or (equal (truename link) (truename cache-project))
                    (uiop:file-exists-p (merge-pathnames "serapeum.asd" link))))))))))

;;; ==========================================================================
;;; Tests for cache-key with unbound version slots
;;; These tests ensure that cache-key returns NIL when version is unbound,
;;; rather than signalling an error.
;;; ==========================================================================

(deftest test-cache-key-source-ql-unbound-version
  (testing "cache-key returns NIL when version slot is unbound"
    (let ((source (make-source :ql "test-lib" :latest)))
      ;; Do NOT set the version - leave it unbound
      (ok (not (slot-boundp source 'qlot/source/base::version))
          "Version slot should be unbound")
      (ok (null (cache-key source))
          "cache-key should return NIL for unbound version"))))

(deftest test-cache-key-source-ultralisp-unbound-version
  (testing "cache-key returns NIL when version slot is unbound"
    (let ((source (make-source :ultralisp "test-lib")))
      ;; Do NOT set the version - leave it unbound
      (ok (not (slot-boundp source 'qlot/source/base::version))
          "Version slot should be unbound")
      (ok (null (cache-key source))
          "cache-key should return NIL for unbound version"))))

(deftest test-cache-key-source-dist-unbound-version
  (testing "cache-key returns NIL when version slot is unbound"
    (let ((source (make-source :dist "https://dist.ultralisp.org/")))
      ;; Do NOT set the version - leave it unbound
      (ok (not (slot-boundp source 'qlot/source/base::version))
          "Version slot should be unbound")
      (ok (null (cache-key source))
          "cache-key should return NIL for unbound version"))))

(deftest test-cache-key-source-ultralisp-with-version
  (testing "cache-key returns correct key when version is set"
    (let ((source (make-source :ultralisp "test-lib")))
      (setf (source-version source) "20240101")
      (ok (equal '("ql" "ultralisp" "test-lib-20240101")
                 (cache-key source))))))

(deftest test-cache-key-source-dist-with-version
  (testing "cache-key returns correct key when version is set"
    (let ((source (make-source :dist "https://dist.ultralisp.org/")))
      (setf (source-version source) "20240101")
      (let ((key (cache-key source)))
        (ok (listp key) "cache-key should return a list")
        (ok (equal "dist" (first key)) "First element should be 'dist'")))))

;;; ==========================================================================
;;; Tests for cache-exists-p with unbound version
;;; ==========================================================================

(deftest test-cache-exists-p-unbound-version
  (testing "cache-exists-p returns NIL (not error) when version is unbound"
    (with-tmp-directory (tmp)
      (let ((*cache-directory* (uiop:ensure-directory-pathname tmp))
            (*cache-enabled* t))
        (let ((source (make-source :ql "test-lib" :latest)))
          ;; Do NOT set the version - leave it unbound
          (ok (not (slot-boundp source 'qlot/source/base::version))
              "Version slot should be unbound")
          ;; This should return NIL, not signal an error
          (ok (null (cache-exists-p source))
              "cache-exists-p should return NIL for unbound version"))))))

(deftest test-cache-exists-p-unbound-version-ultralisp
  (testing "cache-exists-p returns NIL for ultralisp source with unbound version"
    (with-tmp-directory (tmp)
      (let ((*cache-directory* (uiop:ensure-directory-pathname tmp))
            (*cache-enabled* t))
        (let ((source (make-source :ultralisp "test-lib")))
          (ok (not (slot-boundp source 'qlot/source/base::version)))
          (ok (null (cache-exists-p source))))))))

(deftest test-cache-exists-p-unbound-version-dist
  (testing "cache-exists-p returns NIL for dist source with unbound version"
    (with-tmp-directory (tmp)
      (let ((*cache-directory* (uiop:ensure-directory-pathname tmp))
            (*cache-enabled* t))
        (let ((source (make-source :dist "https://example.com/dist/my-dist.txt")))
          (ok (not (slot-boundp source 'qlot/source/base::version)))
          (ok (null (cache-exists-p source))))))))

;;; ==========================================================================
;;; Tests for symlink-p function
;;; ==========================================================================

(deftest test-symlink-p-regular-file
  (testing "symlink-p returns NIL for regular files"
    (with-tmp-directory (tmp)
      (let ((file (merge-pathnames "test.txt" tmp)))
        (with-open-file (s file :direction :output :if-does-not-exist :create)
          (write-line "test" s))
        (ng (qlot/cache::symlink-p file)
            "symlink-p should return NIL for regular files")))))

(deftest test-symlink-p-directory
  (testing "symlink-p returns NIL for directories"
    (with-tmp-directory (tmp)
      (let ((dir (merge-pathnames "subdir/" tmp)))
        (ensure-directories-exist dir)
        (ng (qlot/cache::symlink-p dir)
            "symlink-p should return NIL for directories")))))

(deftest test-symlink-p-symlink-to-file
  (testing "symlink-p returns T for symlinks to files"
    (with-tmp-directory (tmp)
      (let ((target (merge-pathnames "target.txt" tmp))
            (link (merge-pathnames "link.txt" tmp)))
        (with-open-file (s target :direction :output :if-does-not-exist :create)
          (write-line "test" s))
        (make-symlink target link)
        (ok (qlot/cache::symlink-p link)
            "symlink-p should return T for symlinks to files")))))

(deftest test-symlink-p-symlink-to-directory
  (testing "symlink-p returns T for symlinks to directories"
    (with-tmp-directory (tmp)
      (let ((target (merge-pathnames "target-dir/" tmp))
            (link (merge-pathnames "link-dir" tmp)))
        (ensure-directories-exist target)
        (make-symlink target link)
        (ok (qlot/cache::symlink-p link)
            "symlink-p should return T for symlinks to directories")))))

(deftest test-symlink-p-with-trailing-slash
  (testing "symlink-p handles paths with trailing slashes correctly"
    (with-tmp-directory (tmp)
      (let ((target (merge-pathnames "target-dir/" tmp))
            (link (merge-pathnames "link-dir" tmp)))
        (ensure-directories-exist target)
        (make-symlink target link)
        ;; Test with trailing slash (this was a bug - trailing slash caused lstat to follow)
        (ok (qlot/cache::symlink-p (uiop:ensure-directory-pathname link))
            "symlink-p should work with trailing slash")))))

(deftest test-symlink-p-nonexistent
  (testing "symlink-p returns NIL for nonexistent paths"
    (with-tmp-directory (tmp)
      (let ((nonexistent (merge-pathnames "does-not-exist" tmp)))
        (ng (qlot/cache::symlink-p nonexistent)
            "symlink-p should return NIL for nonexistent paths")))))

(deftest test-symlink-p-broken-symlink
  (testing "symlink-p returns T for broken symlinks"
    (with-tmp-directory (tmp)
      (let ((target (merge-pathnames "nonexistent-target" tmp))
            (link (merge-pathnames "broken-link" tmp)))
        ;; Create symlink to nonexistent target
        (make-symlink target link)
        (ok (qlot/cache::symlink-p link)
            "symlink-p should return T for broken symlinks")))))

;;; ==========================================================================
;;; Tests for strip-trailing-slash
;;; ==========================================================================

(deftest test-strip-trailing-slash
  (testing "strip-trailing-slash removes trailing slash"
    (ok (equal "/path/to/dir" (qlot/cache::strip-trailing-slash "/path/to/dir/"))
        "Should remove trailing slash"))
  (testing "strip-trailing-slash leaves paths without trailing slash unchanged"
    (ok (equal "/path/to/file" (qlot/cache::strip-trailing-slash "/path/to/file"))
        "Should leave path without trailing slash unchanged"))
  (testing "strip-trailing-slash handles single slash"
    (ok (equal "/" (qlot/cache::strip-trailing-slash "/"))
        "Should not remove single slash"))
  (testing "strip-trailing-slash handles pathnames"
    (ok (equal "/path/to/dir" (qlot/cache::strip-trailing-slash #P"/path/to/dir/"))
        "Should handle pathname objects")))

;;; ==========================================================================
;;; Tests for create-symlink
;;; ==========================================================================

(deftest test-create-symlink-basic
  (testing "create-symlink creates a working symlink"
    (with-tmp-directory (tmp)
      (let ((target (merge-pathnames "target.txt" tmp))
            (link (merge-pathnames "link.txt" tmp)))
        (with-open-file (s target :direction :output :if-does-not-exist :create)
          (write-line "content" s))
        (qlot/cache::create-symlink target link)
        (ok (uiop:file-exists-p link)
            "Symlink should exist")
        (ok (qlot/cache::symlink-p link)
            "Should be a symlink")
        (ok (equal (with-open-file (s link) (read-line s))
                   "content")
            "Should read through symlink")))))

(deftest test-create-symlink-replaces-existing
  (testing "create-symlink replaces existing file/symlink"
    (with-tmp-directory (tmp)
      (let ((target1 (merge-pathnames "target1.txt" tmp))
            (target2 (merge-pathnames "target2.txt" tmp))
            (link (merge-pathnames "link.txt" tmp)))
        (with-open-file (s target1 :direction :output :if-does-not-exist :create)
          (write-line "content1" s))
        (with-open-file (s target2 :direction :output :if-does-not-exist :create)
          (write-line "content2" s))
        ;; Create first symlink
        (qlot/cache::create-symlink target1 link)
        (ok (equal "content1" (with-open-file (s link) (read-line s))))
        ;; Replace with new target
        (qlot/cache::create-symlink target2 link)
        (ok (equal "content2" (with-open-file (s link) (read-line s)))
            "Should read from new target after replacement")))))

;;; ==========================================================================
;;; Tests for remove-path
;;; ==========================================================================

(deftest test-remove-path-file
  (testing "remove-path removes regular files"
    (with-tmp-directory (tmp)
      (let ((file (merge-pathnames "test.txt" tmp)))
        (with-open-file (s file :direction :output :if-does-not-exist :create)
          (write-line "test" s))
        (ok (uiop:file-exists-p file))
        (qlot/cache::remove-path file)
        (ng (uiop:file-exists-p file)
            "File should be removed")))))

(deftest test-remove-path-directory
  (testing "remove-path removes directories"
    (with-tmp-directory (tmp)
      (let ((dir (merge-pathnames "subdir/" tmp)))
        (ensure-directories-exist (merge-pathnames "file.txt" dir))
        (with-open-file (s (merge-pathnames "file.txt" dir)
                           :direction :output :if-does-not-exist :create)
          (write-line "test" s))
        (ok (uiop:directory-exists-p dir))
        (qlot/cache::remove-path dir)
        (ng (uiop:directory-exists-p dir)
            "Directory should be removed")))))

(deftest test-remove-path-symlink-to-directory
  (testing "remove-path removes symlinks to directories (without following)"
    (with-tmp-directory (tmp)
      (let ((target (merge-pathnames "target-dir/" tmp))
            (link (merge-pathnames "link-dir" tmp)))
        (ensure-directories-exist (merge-pathnames "file.txt" target))
        (with-open-file (s (merge-pathnames "file.txt" target)
                           :direction :output :if-does-not-exist :create)
          (write-line "test" s))
        (make-symlink target link)
        (ok (qlot/cache::symlink-p link) "Should be a symlink before removal")
        (ok (uiop:directory-exists-p target) "Target should exist")
        (qlot/cache::remove-path link)
        (ng (probe-file link) "Symlink should be removed")
        (ok (uiop:directory-exists-p target)
            "Target directory should NOT be removed")))))

(deftest test-remove-path-nonexistent
  (testing "remove-path handles nonexistent paths gracefully"
    (with-tmp-directory (tmp)
      (let ((nonexistent (merge-pathnames "does-not-exist" tmp)))
        ;; Should not signal an error
        (ok (null (qlot/cache::remove-path nonexistent))
            "remove-path should return NIL for nonexistent paths")))))

;;; ==========================================================================
;;; Tests for save-to-cache with symlinks
;;; ==========================================================================

(deftest test-save-to-cache-skips-symlinks
  (testing "save-to-cache skips symlinked directories in software/"
    (with-tmp-directory (cache-root)
      (let ((*cache-directory* (uiop:ensure-directory-pathname cache-root))
            (*cache-enabled* t)
            (source (make-source :ql "mylib" :latest)))
        (setf (source-version source) "20240601")
        (with-tmp-directory (dist-path)
          ;; Create metadata files
          (dolist (file '("distinfo.txt" "systems.txt" "releases.txt"))
            (with-open-file (s (merge-pathnames file dist-path)
                               :direction :output
                               :if-does-not-exist :create)
              (write-line file s)))
          ;; Create a real software directory
          (let ((real-dir (merge-pathnames "software/real-lib-20240601/" dist-path)))
            (ensure-directories-exist real-dir)
            (with-open-file (s (merge-pathnames "real.asd" real-dir)
                               :direction :output
                               :if-does-not-exist :create)
              (write-line "; real" s)))
          ;; Create a symlink in software/ (simulating release-level cache)
          (let* ((external-target (merge-pathnames "external-target/" cache-root))
                 ;; Note: symlink path should not have trailing slash
                 (symlink-dir (merge-pathnames "software/symlinked-lib" dist-path)))
            (ensure-directories-exist external-target)
            (with-open-file (s (merge-pathnames "external.asd" external-target)
                               :direction :output
                               :if-does-not-exist :create)
              (write-line "; external" s))
            ;; Ensure software/ directory exists first
            (ensure-directories-exist (merge-pathnames "software/" dist-path))
            (make-symlink external-target symlink-dir))
          ;; Save to cache
          (save-to-cache source dist-path)
          ;; Verify: real-lib should be in cache, symlinked-lib should NOT be
          (let ((cache-src (cache-sources-path source)))
            (ok (uiop:directory-exists-p (merge-pathnames "real-lib-20240601/" cache-src))
                "Real directory should be cached")
            (ng (uiop:directory-exists-p (merge-pathnames "symlinked-lib/" cache-src))
                "Symlinked directory should NOT be cached")))))))
