(defpackage #:qlot/tests/install/quicklisp
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install/quicklisp
                #:install-quicklisp)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot/tests/install/quicklisp)

(deftest install-quicklisp-tests
  (with-tmp-directory (tmp-dir)
    (ok (install-quicklisp tmp-dir))
    (ok (uiop:file-exists-p (merge-pathnames #P"setup.lisp" tmp-dir)))))
