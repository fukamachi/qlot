(defpackage #:qlot/tests/distify/ultralisp
  (:use #:cl
        #:rove
        #:qlot/distify)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:qlot/tests/distify/ultralisp)

(deftest distify-ultralisp-tests
  (let ((source (make-source :ultralisp "fukamachi-quri" :latest)))
    (with-qlot-server (source nil *default-pathname-defaults*)
      (distify source *default-pathname-defaults*)

      (let ((distinfo.txt (make-pathname :name "fukamachi-quri"
                                         :type "txt"
                                         :defaults *default-pathname-defaults*)))
        (ok (uiop:file-exists-p distinfo.txt))
        (let ((data (parse-distinfo-file distinfo.txt)))
          (ok (equal (aget data "name") "fukamachi-quri")))))))
