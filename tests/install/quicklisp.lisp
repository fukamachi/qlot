(defpackage #:qlot-tests/install/quicklisp
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install/quicklisp
                #:install-quicklisp
                #:install-quicklisp-from-subdir)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/install/quicklisp)

(deftest install-quicklisp-tests
  (ok (uiop:directory-exists-p (asdf:system-relative-pathname :qlot #P"quicklisp-client/")))
  (ok (uiop:directory-files (asdf:system-relative-pathname :qlot #P"quicklisp-client/")))
  (testing "install-quicklisp-from-subdir"
    (with-tmp-directory (tmp-dir)
      (ok (install-quicklisp-from-subdir tmp-dir))
      (ok (uiop:file-exists-p (merge-pathnames #P"setup.lisp" tmp-dir)))
      (ok (uiop:directory-exists-p (merge-pathnames #P"quicklisp/" tmp-dir)))
      (ok (uiop:file-exists-p (merge-pathnames #P"quicklisp/quicklisp.asd" tmp-dir)))
      (ok (uiop:directory-exists-p (merge-pathnames #P"dists/" tmp-dir)))))
  (testing "install-quicklisp"
    (with-tmp-directory (tmp-dir)
      (ok (install-quicklisp tmp-dir))
      (ok (uiop:file-exists-p (merge-pathnames #P"setup.lisp" tmp-dir)))
      (ok (uiop:directory-exists-p (merge-pathnames #P"local-init/" tmp-dir)))
      (ok (uiop:file-exists-p (merge-pathnames #P"local-init/qlot-10-https.lisp" tmp-dir)))
      (ok (uiop:file-exists-p (merge-pathnames #P"local-init/qlot-99-setup.lisp" tmp-dir))))))
