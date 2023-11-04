(defpackage #:qlot/tests/install/quicklisp
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install/quicklisp
                #:install-quicklisp
                #:install-quicklisp-with-installer
                #:install-quicklisp-from-subdir)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot/tests/install/quicklisp)

(deftest install-quicklisp-tests
  (testing "install-quicklisp-with-installer"
    (with-tmp-directory (tmp-dir)
      (ok (install-quicklisp-with-installer tmp-dir))
      (ok (uiop:file-exists-p (merge-pathnames #P"setup.lisp" tmp-dir)))
      (ok (uiop:directory-exists-p (merge-pathnames #P"quicklisp/" tmp-dir)))
      (ok (uiop:file-exists-p (merge-pathnames #P"quicklisp/quicklisp.asd" tmp-dir)))
      (ok (uiop:directory-exists-p (merge-pathnames #P"dists/quicklisp/" tmp-dir)))))
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
      (ok (uiop:file-exists-p (merge-pathnames #P"local-init/https.lisp" tmp-dir)))
      (ok (uiop:file-exists-p (merge-pathnames #P"local-init/qlot-setup.lisp" tmp-dir))))))
