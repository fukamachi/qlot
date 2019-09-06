(defpackage #:qlot/tests/parser
  (:use #:cl
        #:rove
        #:qlot/parser)
  (:import-from #:qlot/parser
                #:parse-qlfile-line)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-version
                #:source-git
                #:source-ql
                #:source-dist
                #:source-distribution)
  (:import-from #:qlot/errors
                #:qlfile-parse-failed
                #:unknown-source
                #:invalid-definition))
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
      (ok (typep source 'source-dist))
      (ok (equal (source-project-name source) "quicklisp"))
      (ok (equal (source-distribution source) "http://beta.quicklisp.org/dist/quicklisp.txt")))
    (let ((source (parse-qlfile-line "ql log4cl 2014-03-17")))
      (ok (typep source 'source-ql))
      (ok (equal (source-project-name source) "log4cl"))
      (ok (equal (source-distribution source) "http://beta.quicklisp.org/dist/quicklisp.txt"))))

  (ok (signals
        (parse-qlfile-line "source")
        'unknown-source)
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
    (let ((parsed (parse-qlfile (test-qlfile #P"qlfile-crlf"))))
      (ok (equal (length parsed) 4)))))

(deftest parse-qlfile-lock-tests
  (let ((parsed (parse-qlfile-lock (test-qlfile #P"qlfile.lock"))))
    (ok (equal (length parsed) 5)))

  (ok (signals (parse-qlfile-lock (test-qlfile #P"qlfile.error.lock"))
               'qlfile-parse-failed)))

(deftest read-qlfile-for-install-tests
  (let ((sources (read-qlfile-for-install (test-qlfile #P"qlfile") :ignore-lock t)))
    (ok (typep (first sources) 'source-dist))
    (ng (slot-boundp (first sources) 'qlot/source/base::version))
    (ok (= (length sources) 5)))
  (let ((sources (read-qlfile-for-install (test-qlfile #P"qlfile"))))
    (ok (typep (first sources) 'source-dist))
    (ok (slot-boundp (first sources) 'qlot/source/base::version))
    (ok (string= (source-version (first sources)) "2019-08-13"))
    (ok (= (length sources) 5))))
