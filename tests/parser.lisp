(defpackage #:qlot/tests/parser
  (:use #:cl
        #:rove
        #:qlot/parser)
  (:import-from #:qlot/parser
                #:parse-qlfile-line)
  (:import-from #:qlot/source/base
                #:source-project-name
                #:source-version)
  (:import-from #:qlot/source/git
                #:source-git))
(in-package #:qlot/tests/parser)

(defun test-qlfile (name)
  (merge-pathnames name (asdf:system-relative-pathname :qlot #P"tests/data/")))

(deftest parse-qlfile-line-test
  (let ((source (parse-qlfile-line "git lsx https://github.com/fukamachi/lsx")))
    (ok (typep source 'source-git))
    (ok (equal (source-project-name source) "lsx")))
  (ok (typep (parse-qlfile-line "git myapp http://myapp.com/\\#/myapp.git")
             'source-git)
      "can escape a sharp")

  (ok (signals
        (parse-qlfile-line "source"))
      "invalid source")
  (ok (equal (parse-qlfile-line "# This is a comment.") nil)
      "# comment")
  (ok (equal (parse-qlfile-line " # This is a comment.") nil)
      " # comment")
  (ok (equal (parse-qlfile-line "; This is a comment.") nil)
      "; comment")
  (ok (equal (parse-qlfile-line " ; This is a comment.") nil)
      " ; comment")
  (ok (equal (parse-qlfile-line ";; This is a comment.") nil)
      ";; comment")
  (ok (equal (parse-qlfile-line " ;; This is a comment.") nil)
      " ;; comment"))
