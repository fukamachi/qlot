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
                #:source-git)
  (:import-from #:qlot/source/ql
                #:source-ql
                #:source-ql-all
                #:source-distribution)
  (:import-from #:qlot/errors
                #:qlfile-parse-failed))
(in-package #:qlot/tests/parser)

(defun test-qlfile (name)
  (merge-pathnames name (asdf:system-relative-pathname :qlot #P"tests/data/")))

(deftest parse-qlfile-line-test
  (testing "git source"
    (let ((source (parse-qlfile-line "git lsx https://github.com/fukamachi/lsx")))
      (ok (typep source 'source-git))
      (ok (equal (source-project-name source) "lsx")))
    (ok (typep (parse-qlfile-line "git myapp http://myapp.com/\\#/myapp.git")
               'source-git)
        "can escape a sharp"))

  (testing "ql source"
    (let ((source (parse-qlfile-line "ql :all :latest")))
      (ok (typep source 'source-ql-all))
      (ok (equal (source-project-name source) "quicklisp"))
      (ok (equal (source-distribution source) "http://beta.quicklisp.org/dist/quicklisp.txt")))
    (let ((source (parse-qlfile-line "ql log4cl 2014-03-17")))
      (ok (typep source 'source-ql))
      (ok (equal (source-project-name source) "log4cl"))
      (ok (equal (source-distribution source) "http://beta.quicklisp.org/dist/quicklisp/2014-03-17/distinfo.txt"))))

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

(deftest parse-qlfile-tests
  (let ((parsed (parse-qlfile (test-qlfile #P"qlfile"))))
    (ok (equal (length parsed) 4)))

  (ok (signals (parse-qlfile (test-qlfile #P"qlfile.error"))
               'qlfile-parse-failed))

  ;; https://github.com/fukamachi/qlot/issues/18
  (testing "CRLF"
    (let ((parsed (parse-qlfile (test-qlfile #P"qlfile-csrf"))))
      (ok (equal (length parsed) 4)))))

(deftest parse-qlfile-lock-tests
  (let ((parsed (parse-qlfile-lock (test-qlfile #P"qlfile.lock"))))
    (ok (equal (length parsed) 5)))

  (ok (signals (parse-qlfile-lock (test-qlfile #P"qlfile.error.lock"))
               'qlfile-parse-failed)))
