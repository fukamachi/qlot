(in-package :cl-user)
(defpackage qlot-test.parser
  (:use :cl
        :qlot.parser
        :prove)
  (:import-from :qlot.source
                :source-project-name
                :prepare)
  (:import-from :qlot.source.ql
                :source-ql
                :source-ql-all
                :source-ql-version)
  (:import-from :qlot.source.git
                :source-git)
  (:import-from :qlot.parser
                :parse-qlfile
                :parse-qlfile-line)
  (:import-from :qlot.error
                :qlot-qlfile-error))
(in-package :qlot-test.parser)

(defun test-qlfile (name)
  (merge-pathnames name (asdf:system-relative-pathname :qlot #P"t/data/")))

(plan 12)

(diag "parse-qlfile-line")

(let ((source (parse-qlfile-line "ql log4cl 2014-03-17")))
  (is-type source 'source-ql)
  (is (source-project-name source) "log4cl")
  (is (source-ql-version source) "2014-03-17"))

(let ((source (parse-qlfile-line "ql :all 2014-12-17")))
  (is-type source 'source-ql-all)
  (is (source-project-name source) "quicklisp")
  (prepare source)
  (is (source-ql-version source) "2014-12-17"))

(is-error (parse-qlfile-line "source")
          'qlot-qlfile-error
          "invalid source")
(is (parse-qlfile-line "# This is a comment.")
    nil
    "comment")
(is (parse-qlfile-line " # This is a comment.")
    nil
    "comment")
(is-type (parse-qlfile-line "git myapp http://myapp.com/\\#/myapp.git")
         'source-git
         "can escape a sharp")

(let ((parsed (parse-qlfile (test-qlfile #P"qlfile"))))
  (is (length parsed) 5))

(is-error (parse-qlfile (test-qlfile #P"qlfile.error"))
          'qlot-qlfile-error)

(finalize)
