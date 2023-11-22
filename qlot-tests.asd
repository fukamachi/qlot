(defsystem "qlot-tests"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "qlot-tests/main"
               "qlot-tests/parser"
               "qlot-tests/distify/ql"
               "qlot-tests/distify/git"
               "qlot-tests/distify/http"
               "qlot-tests/distify/github"
               "qlot-tests/distify/dist"
               "qlot-tests/distify/ultralisp"
               "qlot-tests/distify"
               "qlot-tests/server"
               "qlot-tests/install/quicklisp"
               "qlot-tests/install"
               "qlot-tests/utils"
               "qlot-tests/utils/ql")
  :perform (test-op (op c) (symbol-call :rove :run c)))
