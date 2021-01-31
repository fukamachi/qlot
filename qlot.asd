(defsystem "qlot"
  :class :package-inferred-system
  :version "0.10.8"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "A project-local library installer"
  :depends-on ("qlot/main")
  :in-order-to ((test-op (test-op "qlot/tests"))))

(defsystem "qlot/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "qlot/tests/parser"
               "qlot/tests/sources/github"
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
