(defpackage #:qlot/tests/distify/ultralisp
  (:use #:cl
        #:rove
        #:qlot/distify/ql)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:qlot/tests/distify/ultralisp)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-ultralisp-tests
  (let ((source (make-source :ultralisp "fukamachi-quri" :latest)))
    (distify-ql source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "fukamachi-quri"
                                       :type "txt"
                                       :defaults *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (let ((data (parse-distinfo-file distinfo.txt)))
        (ok (equal (aget data "name") "fukamachi-quri"))))))
