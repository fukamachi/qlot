(defpackage #:qlot/tests/install/quicklisp
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install/quicklisp
                #:fetch-installer
                #:install-quicklisp)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot/tests/install/quicklisp)

(deftest fetch-installer-tests
  (with-tmp-directory (tmp-dir)
    (ok (fetch-installer tmp-dir))
    (ok (= 1 (length (uiop:directory-files tmp-dir "quicklisp-*.lisp"))))))

(deftest install-quicklisp-tests
  (with-tmp-directory (tmp-dir)
    (ok (install-quicklisp tmp-dir))
    (ok (uiop:file-exists-p (merge-pathnames #P"setup.lisp" tmp-dir)))))
