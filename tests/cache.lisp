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
