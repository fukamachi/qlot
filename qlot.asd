(defsystem "qlot"
  :class :package-inferred-system
  :version "1.3.4"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "A project-local library installer"
  :pathname "src"
  :depends-on ("qlot/main"
               #+ros.installing "qlot/distify"
               #+ros.installing "qlot/cli")
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
  :depends-on ("qlot-tests")
  :in-order-to ((test-op (test-op "qlot-tests"))))
