(defpackage #:qlot/tests/distify/dist
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
(in-package #:qlot/tests/distify/dist)

(deftest distify-dist-tests
  (let ((source (make-source :dist "ultralisp" "http://dist.ultralisp.org/")))
    (with-qlot-server (source nil *default-pathname-defaults*)
      (distify source *default-pathname-defaults*)

      (let ((distinfo.txt (make-pathname :name "ultralisp"
                                         :type "txt"
                                         :defaults *default-pathname-defaults*)))
        (ok (uiop:file-exists-p distinfo.txt))
        (let ((data (parse-distinfo-file distinfo.txt)))
          (ok (equal (aget data "name") "ultralisp"))))))

  (let ((source (make-source :dist "ultralisp" "http://dist.ultralisp.org/" "20190904101505")))
    (with-qlot-server (source nil *default-pathname-defaults*)
      (distify source *default-pathname-defaults*)

      (let ((distinfo.txt (make-pathname :name "ultralisp"
                                         :type "txt"
                                         :defaults *default-pathname-defaults*)))
        (ok (uiop:file-exists-p distinfo.txt))
        (let ((data (parse-distinfo-file distinfo.txt)))
          (ok (equal (aget data "name") "ultralisp"))
          (ok (equal (aget data "version") "20190904101505")))))))
