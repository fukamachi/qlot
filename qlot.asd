(defsystem "qlot"
  :class :package-inferred-system
  :version "0.9.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "A project-local library installer"
  :depends-on ("qlot/main")
  :in-order-to ((test-op (test-op "qlot/tests"))))

(defsystem "qlot/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "qlot/tests/parser"
               "qlot/tests/main")
  :perform (test-op (op c) (symbol-call :rove :run c)))
