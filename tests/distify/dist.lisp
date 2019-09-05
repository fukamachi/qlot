(defpackage #:qlot/tests/distify/dist
  (:use #:cl
        #:rove
        #:qlot/distify/dist)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:qlot/tests/distify/dist)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-dist-tests
  (let ((source (make-source :dist "ultralisp" "http://dist.ultralisp.org/")))
    (distify-dist source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "ultralisp"
                                       :type "txt"
                                       :defaults *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (let ((data (parse-distinfo-file distinfo.txt)))
        (ok (equal (aget data "name") "ultralisp")))))

  (let ((source (make-source :dist "ultralisp" "http://dist.ultralisp.org/" "20190904101505")))
    (distify-dist source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "ultralisp"
                                       :type "txt"
                                       :defaults *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (let ((data (parse-distinfo-file distinfo.txt)))
        (ok (equal (aget data "name") "ultralisp"))
        (ok (equal (aget data "version") "20190904101505"))))))
