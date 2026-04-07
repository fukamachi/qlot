(defpackage #:qlot-tests/bundle
  (:use #:cl
        #:rove)
  (:import-from #:qlot/bundle
                #:local-project-paths-from-registry-conf)
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
