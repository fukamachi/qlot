(defsystem "qlot"
  :class :package-inferred-system
  :version "1.2.13"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "A project-local library installer"
  :depends-on ("qlot/main")
  :output-files (image-op (o c)
                  (output-files o :qlot/command))
  :in-order-to ((test-op (test-op "qlot/tests"))
                (build-op (program-op "qlot/command"))))

(defsystem "qlot/command"
  :depends-on ("qlot/cli"
               "qlot/main"
               "qlot/install")
  :build-operation "program-op"
  :build-pathname "qlot"
  :entry-point "qlot/cli::main")

(defsystem "qlot/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "qlot/tests/main"
               "qlot/tests/parser"
               "qlot/tests/distify/ql"
               "qlot/tests/distify/git"
               "qlot/tests/distify/http"
               "qlot/tests/distify/github"
               "qlot/tests/distify/dist"
               "qlot/tests/distify/ultralisp"
               "qlot/tests/distify"
               "qlot/tests/server"
               "qlot/tests/install/quicklisp"
               "qlot/tests/install"
               "qlot/tests/utils"
               "qlot/tests/utils/ql")
  :perform (test-op (op c) (symbol-call :rove :run c)))
