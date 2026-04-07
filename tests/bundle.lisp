(defpackage #:qlot-tests/bundle
  (:use #:cl
        #:rove)
  (:import-from #:qlot/bundle
                #:local-project-paths-from-registry-conf
                #:create-local-project-symlinks)
  (:import-from #:qlot/cache
                #:symlink-p)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/bundle)

(defun write-registry-conf (dir &rest tree-entries)
  "Write a source-registry.conf to DIR with the given :tree entries."
  (let ((conf-file (merge-pathnames #P"source-registry.conf" dir)))
    (with-open-file (out conf-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (let ((*print-pretty* nil) (*print-case* :downcase))
        (prin1 `(:source-registry
                 :ignore-inherited-configuration
                 (:also-exclude ".qlot")
                 (:also-exclude ".bundle-libs")
                 (:directory "/some/qlot/src/")
                 ,@tree-entries)
               out)))
    conf-file))

(deftest local-project-paths-from-registry-conf-tests
  (testing "returns nil when source-registry.conf does not exist"
    (with-tmp-directory (tmp)
      (ok (null (local-project-paths-from-registry-conf tmp)))))

  (testing "returns nil when source-registry.conf has no :tree entries"
    (with-tmp-directory (tmp)
      (write-registry-conf tmp)
      (ok (null (local-project-paths-from-registry-conf tmp)))))

  (testing "extracts :here relative path"
    (with-tmp-directory (tmp)
      ;; (:here #P"../mylib/") is relative to the .qlot dir (tmp)
      (let ((resolved (uiop:ensure-directory-pathname
                       (merge-pathnames #P"../mylib/" tmp))))
        (write-registry-conf tmp `(:tree (:here #P"../mylib/")))
        (let ((paths (local-project-paths-from-registry-conf tmp)))
          (ok (= 1 (length paths)))
          (ok (uiop:pathname-equal resolved (first paths)))))))

  (testing "extracts absolute pathname"
    (with-tmp-directory (tmp)
      (with-tmp-directory (local-proj)
        (write-registry-conf tmp `(:tree ,local-proj))
        (let ((paths (local-project-paths-from-registry-conf tmp)))
          (ok (= 1 (length paths)))
          (ok (uiop:pathname-equal local-proj (first paths)))))))

  (testing "extracts multiple :tree entries"
    (with-tmp-directory (tmp)
      (with-tmp-directory (proj-a)
        (with-tmp-directory (proj-b)
          (write-registry-conf tmp
                               `(:tree ,proj-a)
                               `(:tree ,proj-b))
          (let ((paths (local-project-paths-from-registry-conf tmp)))
            (ok (= 2 (length paths))))))))

  (testing "skips :directory entries (not local projects)"
    (with-tmp-directory (tmp)
      ;; Only :tree entries should be returned; :directory (qlot src) is skipped
      (write-registry-conf tmp)
      (let ((paths (local-project-paths-from-registry-conf tmp)))
        (ok (null paths)))))

  (testing "extracts :home directive path"
    (with-tmp-directory (tmp)
      ;; (:home #P"mylib/") represents ~/mylib/ - should be resolved
      (write-registry-conf tmp `(:tree (:home #P"mylib/")))
      (let ((paths (local-project-paths-from-registry-conf tmp)))
        (ok (= 1 (length paths)))
        ;; The resolved path should end in mylib/ under the user home
        (ok (uiop:pathname-equal
             (merge-pathnames #P"mylib/" (user-homedir-pathname))
             (first paths))))))

  (testing "returns nil for malformed source-registry.conf"
    (with-tmp-directory (tmp)
      ;; Write garbage that is not valid s-expression
      (let ((conf-file (merge-pathnames #P"source-registry.conf" tmp)))
        (with-open-file (out conf-file
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (write-string "this is not valid ( lisp" out)))
      (ok (null (local-project-paths-from-registry-conf tmp)))))

  (testing "returns nil for empty source-registry.conf"
    (with-tmp-directory (tmp)
      (let ((conf-file (merge-pathnames #P"source-registry.conf" tmp)))
        (with-open-file (out conf-file
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          ;; Write empty file
          ))
      (ok (null (local-project-paths-from-registry-conf tmp)))))

  (testing "returned paths are directory pathnames"
    (with-tmp-directory (tmp)
      (with-tmp-directory (local-proj)
        (write-registry-conf tmp `(:tree ,local-proj))
        (let ((paths (local-project-paths-from-registry-conf tmp)))
          (ok (= 1 (length paths)))
          ;; Must be a directory pathname (trailing slash)
          (ok (uiop:directory-pathname-p (first paths))))))))

(deftest create-local-project-symlinks-tests
  (testing "no-op with empty local project list"
    (with-tmp-directory (bundle-dir)
      (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-dir))
      (ok (null (create-local-project-symlinks '() bundle-dir)))))

  (testing "creates symlink named after local project directory"
    (with-tmp-directory (bundle-dir)
      (with-tmp-directory (proj)
        (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-dir))
        (create-local-project-symlinks (list proj) bundle-dir)
        (let* ((proj-name (car (last (pathname-directory proj))))
               (link (merge-pathnames (format nil "local-projects/~A" proj-name)
                                      bundle-dir)))
          (ok (symlink-p link))))))

  (testing "symlink target resolves to the actual project directory"
    (with-tmp-directory (bundle-dir)
      (with-tmp-directory (proj)
        (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-dir))
        (create-local-project-symlinks (list proj) bundle-dir)
        (let* ((proj-name (car (last (pathname-directory proj))))
               (link (merge-pathnames (format nil "local-projects/~A" proj-name)
                                      bundle-dir)))
          ;; truename through the symlink should resolve to the actual dir
          (ok (uiop:pathname-equal (uiop:ensure-directory-pathname (truename link))
                                   proj))))))

  (testing "creates symlinks for multiple local projects"
    (with-tmp-directory (bundle-dir)
      (with-tmp-directory (proj-a)
        (with-tmp-directory (proj-b)
          (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-dir))
          (create-local-project-symlinks (list proj-a proj-b) bundle-dir)
          (let* ((name-a (car (last (pathname-directory proj-a))))
                 (name-b (car (last (pathname-directory proj-b))))
                 (link-a (merge-pathnames (format nil "local-projects/~A" name-a) bundle-dir))
                 (link-b (merge-pathnames (format nil "local-projects/~A" name-b) bundle-dir)))
            (ok (symlink-p link-a))
            (ok (symlink-p link-b)))))))

  (testing "is idempotent when called twice"
    (with-tmp-directory (bundle-dir)
      (with-tmp-directory (proj)
        (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-dir))
        (create-local-project-symlinks (list proj) bundle-dir)
        ;; Second call should not signal an error
        (let ((errored nil))
          (handler-case
              (create-local-project-symlinks (list proj) bundle-dir)
            (error (c)
              (declare (ignore c))
              (setf errored t)))
          (ok (not errored) "second call does not error"))
        (let* ((proj-name (car (last (pathname-directory proj))))
               (link (merge-pathnames (format nil "local-projects/~A" proj-name)
                                      bundle-dir)))
          (ok (symlink-p link))))))

  (testing "skips non-existent local project paths"
    (with-tmp-directory (bundle-dir)
      (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-dir))
      (let ((nonexistent #P"/tmp/does-not-exist-qlot-test/"))
        (ok (null (create-local-project-symlinks (list nonexistent) bundle-dir)))
        ;; No symlink should have been created
        (ok (null (uiop:directory-files (merge-pathnames #P"local-projects/" bundle-dir)))))))

  (testing "creates local-projects/ subdirectory when it does not exist"
    (with-tmp-directory (bundle-dir)
      (with-tmp-directory (proj)
        ;; Intentionally do NOT create local-projects/ beforehand
        (create-local-project-symlinks (list proj) bundle-dir)
        (let* ((proj-name (car (last (pathname-directory proj))))
               (link (merge-pathnames (format nil "local-projects/~A" proj-name)
                                      bundle-dir)))
          (ok (uiop:directory-exists-p (merge-pathnames #P"local-projects/" bundle-dir))
              "local-projects/ directory was created")
          (ok (symlink-p link))))))

  (testing "overwrites existing non-symlink file at symlink location"
    (with-tmp-directory (bundle-dir)
      (with-tmp-directory (proj)
        (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-dir))
        (let* ((proj-name (car (last (pathname-directory proj))))
               (link-path (merge-pathnames (format nil "local-projects/~A" proj-name)
                                           bundle-dir)))
          ;; Place a regular file where the symlink should go
          (with-open-file (out link-path
                               :direction :output
                               :if-does-not-exist :create)
            (write-string "placeholder" out))
          (ok (uiop:file-exists-p link-path) "precondition: regular file exists")
          ;; Should replace the file with a symlink
          (create-local-project-symlinks (list proj) bundle-dir)
          (ok (symlink-p link-path)
              "regular file replaced by symlink")
          (ok (uiop:pathname-equal (uiop:ensure-directory-pathname (truename link-path))
                                   proj)
              "symlink points to correct target")))))

  (testing "stub returning nil would fail — symlink must actually exist"
    ;; This test guards against a trivial stub implementation.
    ;; It verifies the symlink is a real filesystem entry, not just a return value check.
    (with-tmp-directory (bundle-dir)
      (with-tmp-directory (proj)
        (ensure-directories-exist (merge-pathnames #P"local-projects/" bundle-dir))
        (create-local-project-symlinks (list proj) bundle-dir)
        (let* ((proj-name (car (last (pathname-directory proj))))
               (link (merge-pathnames (format nil "local-projects/~A" proj-name)
                                      bundle-dir)))
          ;; Verify the symlink exists as an actual filesystem entry
          (ok (symlink-p link) "symlink exists on disk")
          ;; Verify we can list it in the directory
          (let ((entries (uiop:subdirectories (merge-pathnames #P"local-projects/" bundle-dir))))
            (ok (<= 1 (length entries))
                "at least one entry in local-projects/"))
          ;; Verify reading through the symlink works
          (ok (uiop:directory-exists-p link)
              "symlink target is accessible as directory"))))))
