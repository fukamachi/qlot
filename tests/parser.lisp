(defpackage #:qlot/tests/parser
  (:use #:cl
        #:qlot/parser
        #:rove)
  (:import-from #:qlot/source
                #:source-project-name
                #:prepare)
  (:import-from #:qlot/source/ql
                #:source-ql
                #:source-ql-all
                #:source-ql-version)
  (:import-from #:qlot/source/git
                #:source-git)
  (:import-from #:qlot/parser
                #:parse-qlfile
                #:parse-qlfile-line)
  (:import-from #:qlot/error
                #:qlot-qlfile-error))
(in-package #:qlot/tests/parser)

(defun test-qlfile (name)
  (merge-pathnames name (asdf:system-relative-pathname :qlot #P"tests/data/")))

(deftest parse-qlfile-line-test
  (let ((source (parse-qlfile-line "ql log4cl 2014-03-17")))
    (ok (typep source 'source-ql))
    (ok (equal (source-project-name source) "log4cl"))
    (ok (equal (source-ql-version source) "2014-03-17")))

  (let ((source (parse-qlfile-line "ql :all 2014-12-17")))
    (ok (typep source 'source-ql-all))
    (ok (equal (source-project-name source) "quicklisp"))
    (prepare source)
    (ok (equal (source-ql-version source) "2014-12-17")))

  (ok (signals
          (parse-qlfile-line "source")
          'qlot-qlfile-error)
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
      " ;; comment")
  (ok (typep (parse-qlfile-line "git myapp http://myapp.com/\\#/myapp.git")
             'source-git)
      "can escape a sharp"))

(deftest parse-qlfile-test
  (let ((parsed (parse-qlfile (test-qlfile #P"qlfile"))))
    (ok (equal (length parsed) 5)))

  (ok (signals (parse-qlfile (test-qlfile #P"qlfile.error"))
          'qlot-qlfile-error))

  ;; https://github.com/fukamachi/qlot/issues/18
  (testing "CRLF"
    (let ((parsed (parse-qlfile (test-qlfile #P"qlfile-crlf"))))
      (ok (equal (length parsed) 5)))))
