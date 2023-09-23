(defpackage #:qlot/tests/utils/ql
  (:use #:cl
        #:rove
        #:qlot/utils/ql))
(in-package #:qlot/tests/utils/ql)

(deftest quicklisp-distinfo-url-tests
  (ok (equal (quicklisp-distinfo-url)
             "https://beta.quicklisp.org/dist/quicklisp.txt"))
  (ok (equal (quicklisp-distinfo-url "2019-08-13")
             "https://beta.quicklisp.org/dist/quicklisp/2019-08-13/distinfo.txt"))
  (ok (signals (quicklisp-distinfo-url "XXXX-XX-XX")))
  (ok (signals (quicklisp-distinfo-url 1))))
