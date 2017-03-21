#-asdf3.1 (error "Qlot requires ASDF 3.1")
(asdf:defsystem qlot
  :class :package-inferred-system
  :version "0.3.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("qlot/main")
  :in-order-to ((test-op (test-op qlot-test))))
