(defpackage #:qlot/tests/sources/github
  (:use #:cl
        #:rove
        #:qlot/source/github)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/source/base
                #:source-version))
(in-package #:qlot/tests/sources/github)

(deftest source-github-tests
    (let* ((ref "adb6d04f1b9ea99fa7f18044df4c86b6c68023af")
           ;; Source with a branch name.
           (source-branch (make-source :github
                                       "quri"
                                       "fukamachi/quri"
                                       :branch "master"))
           ;; Source with a branch name and a locked version set.
           (source-locked (let ((src (make-source :github
                                                  "quri"
                                                  "fukamachi/quri"
                                                  :branch "master")))
                            (setf (source-version src) (concatenate 'string "github-" ref))
                            src)))
      ;; Non-locked sources fetch archives by ref name.
      (ok (equal (source-github-url source-branch)
                 "https://github.com/fukamachi/quri/archive/master.tar.gz"))
      ;; Locked versions fetch archives by (locked) ref, since other names
      ;; names may point elsewhere by the time fetch happens.
      (ok (equal (source-github-url source-locked)
                 (format nil "https://github.com/fukamachi/quri/archive/~A.tar.gz" ref)))))
