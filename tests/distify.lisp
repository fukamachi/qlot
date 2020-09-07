(defpackage #:qlot/tests/distify
  (:use #:cl
        #:rove
        #:qlot/distify)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory))
(in-package #:qlot/tests/distify)

(deftest distify-tests
  (let ((sources (list (make-source :git "clack" "https://github.com/fukamachi/clack.git")
                       (make-source :git "cl-dbi" "https://github.com/fukamachi/cl-dbi" :tag "0.9.0")
                       (make-source :git "utopian" "https://github.com/fukamachi/utopian" :branch "next")
                       (make-source :ql "log4cl" "2014-03-17"))))
    ;; Note: this will distify /all/ of these sources into the same
    ;; temp directory. Multi-source distification is not well supported.
    (with-qlot-server (sources nil *tmp-directory*)
      (distify sources *tmp-directory*)
      (ok (uiop:file-exists-p (merge-pathnames #P"clack.txt" *tmp-directory*)))
      (ok (uiop:file-exists-p (merge-pathnames #P"cl-dbi.txt" *tmp-directory*)))
      (ok (uiop:file-exists-p (merge-pathnames #P"utopian.txt" *tmp-directory*)))
      (ok (uiop:file-exists-p (merge-pathnames #P"log4cl.txt" *tmp-directory*))))))
