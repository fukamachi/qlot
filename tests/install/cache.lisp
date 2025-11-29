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
                #:format-cache-status))
(in-package #:qlot-tests/install/cache)

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

(deftest format-cache-status-test
  (let ((source (make-source :ql "alexandria" :latest)))
    (dolist (status '(:hit :miss :skip :disabled))
      (let ((message (format-cache-status source status 0.5)))
        (ok (search "alexandria" message))
        (ok (typep message 'string))))))
