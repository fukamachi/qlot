(defpackage #:qlot-tests/utils/asdf
  (:use #:cl
        #:rove)
  (:import-from #:qlot/utils/asdf
                #:with-directory))
(in-package #:qlot-tests/utils/asdf)

(deftest with-directory-tests
  (let ((system-hash (make-hash-table :test 'equal)))
    (with-directory (system-file system-name dependencies)
        (asdf:system-source-directory :qlot)
      (declare (ignore dependencies))
      (push system-name (gethash system-file system-hash)))
    (ok (null (set-difference (gethash (asdf:system-source-file :qlot) system-hash)
                              '("qlot" "qlot/command" "qlot/tests")
                              :test 'equal)))
    (ok (null (set-difference (gethash (asdf:system-source-file :qlot-tests) system-hash)
                              '("qlot-tests")
                              :test 'equal)))))
