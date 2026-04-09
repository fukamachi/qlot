(defpackage #:qlot-tests/bundle
  (:use #:cl
        #:rove)
  (:import-from #:qlot/bundle
                #:local-project-paths
                #:create-local-project-symlinks)
  (:import-from #:qlot/cache
                #:symlink-p)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/bundle)

(defun write-qlfile (dir &rest lines)
  "Write a qlfile to DIR with the given lines."
  (let ((qlfile (merge-pathnames #P"qlfile" dir)))
    (with-open-file (out qlfile
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (dolist (line lines)
        (write-line line out)))
    qlfile))

(deftest local-project-paths-tests
  (testing "returns nil when qlfile does not exist"
    (with-tmp-directory (tmp)
      (ok (null (local-project-paths tmp)))))

  (testing "returns nil when qlfile has no local entries"
    (with-tmp-directory (tmp)
      (write-qlfile tmp "github foo/bar")
      (ok (null (local-project-paths tmp)))))

  (testing "extracts local project path"
    (with-tmp-directory (tmp)
      (let ((lib-dir (merge-pathnames #P"libs/mylib/" tmp)))
        (ensure-directories-exist lib-dir)
        (write-qlfile tmp "local mylib ./libs/mylib/")
        (let ((paths (local-project-paths tmp)))
          (ok (= 1 (length paths)))
          (ok (uiop:pathname-equal lib-dir (first paths)))))))

  (testing "extracts absolute path"
    (with-tmp-directory (tmp)
      (with-tmp-directory (lib-dir)
        (write-qlfile tmp (format nil "local mylib ~A" (namestring lib-dir)))
        (let ((paths (local-project-paths tmp)))
          (ok (= 1 (length paths)))
          (ok (uiop:pathname-equal lib-dir (first paths)))))))

  (testing "extracts multiple local entries"
    (with-tmp-directory (tmp)
      (let ((lib-a (merge-pathnames #P"libs/a/" tmp))
            (lib-b (merge-pathnames #P"libs/b/" tmp)))
        (ensure-directories-exist lib-a)
        (ensure-directories-exist lib-b)
        (write-qlfile tmp
                      "local a ./libs/a/"
                      "github foo/bar"
                      "local b ./libs/b/")
        (let ((paths (local-project-paths tmp)))
          (ok (= 2 (length paths)))))))

  (testing "returned paths are directory pathnames"
    (with-tmp-directory (tmp)
      (let ((lib-dir (merge-pathnames #P"libs/mylib/" tmp)))
        (ensure-directories-exist lib-dir)
        (write-qlfile tmp "local mylib ./libs/mylib/")
        (let ((paths (local-project-paths tmp)))
          (ok (= 1 (length paths)))
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
