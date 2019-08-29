(defpackage #:qlot/tests/distify
  (:use #:cl
        #:rove
        #:qlot/distify)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory))
(in-package #:qlot/tests/distify)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-tests
  (let ((sources (list (make-source :git "clack" "https://github.com/fukamachi/clack.git")
                       (make-source :git "cl-dbi" "https://github.com/fukamachi/cl-dbi" :tag "0.9.0")
                       (make-source :git "utopian" "https://github.com/fukamachi/utopian" :branch "next")
                       (make-source :ql "log4cl" "2014-03-17"))))
    (distify sources *tmp-directory*)
    (ok (uiop:file-exists-p (merge-pathnames #P"clack.txt" *tmp-directory*)))
    (ok (uiop:file-exists-p (merge-pathnames #P"cl-dbi.txt" *tmp-directory*)))
    (ok (uiop:file-exists-p (merge-pathnames #P"utopian.txt" *tmp-directory*)))
    (ok (uiop:file-exists-p (merge-pathnames #P"log4cl.txt" *tmp-directory*)))))
