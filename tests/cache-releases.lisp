(defpackage #:qlot-tests/cache-releases
  (:use #:cl
        #:rove)
  (:import-from #:qlot/cache
                #:*cache-directory*
                #:*cache-enabled*
                #:canonicalize-dist-url
                #:release-cache-key
                #:release-cache-path
                #:release-cache-exists-p
                #:releases-directory)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/cache-releases)

(deftest test-canonicalize-dist-url
  (testing "Quicklisp dist URL"
    (ok (equal "dist.quicklisp.org/quicklisp"
               (canonicalize-dist-url "https://dist.quicklisp.org/dist/quicklisp.txt"))))
  (testing "Ultralisp dist URL"
    (ok (equal "dist.ultralisp.org/ultralisp"
               (canonicalize-dist-url "http://dist.ultralisp.org/dist/ultralisp.txt"))))
  (testing "Custom dist URL"
    (ok (equal "example.com/my-dist"
               (canonicalize-dist-url "https://example.com/dist/my-dist.txt"))))
  (testing "URL without /dist/ prefix"
    (ok (equal "example.com/path/to/distinfo"
               (canonicalize-dist-url "https://example.com/path/to/distinfo.txt"))))
  (testing "NIL input"
    (ng (canonicalize-dist-url nil)))
  (testing "Empty string"
    (ng (canonicalize-dist-url ""))))

(deftest test-release-cache-key
  (testing "Basic cache key generation"
    (let ((key (release-cache-key "dist.quicklisp.org/quicklisp" "alexandria" "abc123")))
      (ok (equal '("dist.quicklisp.org" "quicklisp" "alexandria" "abc123") key)))))

(deftest test-release-cache-path
  (with-tmp-directory (cache-dir)
    (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir)))
      (testing "Path generation"
        (let ((path (release-cache-path "dist.quicklisp.org/quicklisp" "alexandria" "abc123")))
          (ok (uiop:string-suffix-p (namestring path) "releases/dist.quicklisp.org/quicklisp/alexandria/abc123/")))))))

(deftest test-release-cache-exists-p-empty
  (with-tmp-directory (cache-dir)
    (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
          (*cache-enabled* t))
      (testing "Returns NIL for non-existent cache"
        (ng (release-cache-exists-p "dist.quicklisp.org/quicklisp" "alexandria" "abc123"))))))

(deftest test-release-cache-exists-p-with-content
  (with-tmp-directory (cache-dir)
    (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
          (*cache-enabled* t))
      ;; Create a fake cached release
      (let* ((path (release-cache-path "dist.quicklisp.org/quicklisp" "alexandria" "abc123"))
             (prefix-dir (merge-pathnames "alexandria-20231021-git/" path)))
        (ensure-directories-exist (merge-pathnames "test.asd" prefix-dir))
        (with-open-file (out (merge-pathnames "test.asd" prefix-dir)
                             :direction :output
                             :if-does-not-exist :create)
          (write-line "; test" out)))
      (testing "Returns T for existing cache with content"
        (ok (release-cache-exists-p "dist.quicklisp.org/quicklisp" "alexandria" "abc123"))))))

(deftest test-release-cache-disabled
  (with-tmp-directory (cache-dir)
    (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir))
          (*cache-enabled* nil))  ; Cache disabled
      ;; Create a fake cached release
      (let* ((path (release-cache-path "dist.quicklisp.org/quicklisp" "alexandria" "abc123"))
             (prefix-dir (merge-pathnames "alexandria-20231021-git/" path)))
        (ensure-directories-exist (merge-pathnames "test.asd" prefix-dir))
        (with-open-file (out (merge-pathnames "test.asd" prefix-dir)
                             :direction :output
                             :if-does-not-exist :create)
          (write-line "; test" out)))
      (testing "Returns NIL when cache is disabled"
        (ng (release-cache-exists-p "dist.quicklisp.org/quicklisp" "alexandria" "abc123"))))))

(deftest test-releases-directory
  (with-tmp-directory (cache-dir)
    (let ((*cache-directory* (uiop:ensure-directory-pathname cache-dir)))
      (testing "Returns correct releases directory"
        (ok (uiop:string-suffix-p (namestring (releases-directory)) "releases/"))))))
