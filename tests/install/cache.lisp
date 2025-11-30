(defpackage #:qlot-tests/install/cache
  (:use #:cl
        #:rove)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:import-from #:qlot/source/base
                #:make-source)
  (:import-from #:qlot/install
                #:source-dist-path
                #:register-dist-with-quicklisp
                #:invalidate-broken-dist
                #:format-cache-status)
  (:import-from #:qlot/cache
                #:validate-dist-installation))
(in-package #:qlot-tests/install/cache)

(defun make-symlink (target link)
  "Create a symbolic link at LINK pointing to TARGET."
  #+sbcl
  (sb-posix:symlink (namestring target) (namestring link))
  #-sbcl
  (uiop:run-program (list "ln" "-s"
                          (uiop:native-namestring target)
                          (uiop:native-namestring link))))

(deftest source-dist-path-test
  (with-tmp-directory (tmp)
    (let* ((qlhome (merge-pathnames #P".qlot/" tmp))
           (source (make-source :ql "quicklisp" :latest))
           (path (source-dist-path source qlhome)))
      (ok (uiop:pathname-equal path
                               (merge-pathnames #P"dists/quicklisp/" qlhome))))))

(deftest register-dist-with-quicklisp-test
  (with-tmp-directory (tmp)
    (let* ((qlhome (merge-pathnames #P".qlot/" tmp))
           (dist-dir (merge-pathnames #P"dists/example/" qlhome))
           (source (make-source :dist "example" "https://example.com/dist.txt")))
      (ensure-directories-exist dist-dir)
      (dolist (file '("distinfo.txt" "systems.txt" "releases.txt"))
        (with-open-file (out (merge-pathnames file dist-dir)
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
          (write-line "dummy" out)))
      (register-dist-with-quicklisp source qlhome)
      (ok (uiop:file-exists-p (merge-pathnames "enabled.txt" dist-dir)))
      (ok (uiop:file-exists-p (merge-pathnames "preference.txt" dist-dir)))
      (ok (search "example"
                  (uiop:read-file-string (merge-pathnames "enabled.txt" dist-dir))))
      (ok (search "100"
                  (uiop:read-file-string (merge-pathnames "preference.txt" dist-dir)))))))

(deftest invalidate-broken-dist-test
  (with-tmp-directory (tmp)
    (let* ((qlhome (merge-pathnames #P".qlot/" tmp))
           (dist-dir (merge-pathnames #P"dists/broken/" qlhome)))
      (ensure-directories-exist dist-dir)
      (with-open-file (out (merge-pathnames "distinfo.txt" dist-dir)
                           :direction :output
                           :if-does-not-exist :create)
        (write-line "dummy" out))
      (ok (uiop:directory-exists-p dist-dir))
      (invalidate-broken-dist dist-dir)
      (ng (uiop:directory-exists-p dist-dir)))))

(deftest broken-symlink-recovery-test
  (testing "invalidate-broken-dist removes dist directory when symlinks are broken"
    (with-tmp-directory (tmp)
      (let* ((qlhome (merge-pathnames #P".qlot/" tmp))
             (dist-dir (merge-pathnames #P"dists/example/" qlhome))
             (software-dir (merge-pathnames #P"software/" dist-dir))
             (cache-dir (merge-pathnames #P"cache/" tmp))
             (cached-lib (merge-pathnames #P"mylib/" cache-dir))
             (link-path (merge-pathnames #P"mylib" software-dir)))
        ;; Setup: Create cache directory with content
        (ensure-directories-exist cached-lib)
        (with-open-file (out (merge-pathnames "main.lisp" cached-lib)
                             :direction :output
                             :if-does-not-exist :create)
          (write-line "(defpackage :mylib)" out))
        ;; Create dist directory with required files
        (ensure-directories-exist software-dir)
        (dolist (file '("distinfo.txt" "systems.txt" "releases.txt"))
          (with-open-file (out (merge-pathnames file dist-dir)
                               :direction :output
                               :if-does-not-exist :create)
            (write-line "dummy" out)))
        ;; Create symlink from software dir to cached lib
        (make-symlink cached-lib link-path)
        ;; Verify installation is valid while cache exists
        (ok (validate-dist-installation dist-dir)
            "Installation should be valid with intact symlink")
        ;; Simulate user deleting the global cache
        (uiop:delete-directory-tree cache-dir :validate t)
        ;; Verify installation is now invalid (broken symlink)
        (ng (validate-dist-installation dist-dir)
            "Installation should be invalid after cache deletion")
        ;; Verify invalidate-broken-dist removes the broken dist
        (ok (uiop:directory-exists-p dist-dir)
            "Dist directory should exist before invalidation")
        (invalidate-broken-dist dist-dir)
        (ng (uiop:directory-exists-p dist-dir)
            "Dist directory should be removed after invalidation")))))

(deftest format-cache-status-test
  (let ((source (make-source :ql "alexandria" :latest)))
    (setf (qlot/source/base:source-version source) "20250622")
    (testing "Cache hit shows 'from cache'"
      (let ((message (format-cache-status source :hit :new 0.5)))
        (ok (search "alexandria" message))
        (ok (search "from cache" message))
        (ok (search "Installed" message))))
    (testing "Cache miss shows no 'from cache'"
      (let ((message (format-cache-status source :miss :new 0.5)))
        (ok (search "alexandria" message))
        (ng (search "from cache" message))
        (ok (search "Installed" message))))
    (testing "Update shows 'Updated'"
      (let ((message (format-cache-status source :hit :update 0.5)))
        (ok (search "Updated" message))
        (ok (search "from cache" message))))))
