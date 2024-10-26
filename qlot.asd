(defsystem "qlot"
  :class :package-inferred-system
  :version "1.5.15"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "A project-local library installer"
  :pathname "src"
  :depends-on ("qlot/main"
               (:feature :ros.installing "qlot/cli")
               (:feature :ros.installing "qlot/fetch")
               (:feature (:not :qlot.project) "qlot/subcommands"))
  :output-files (image-op (o c)
                  (output-files o :qlot/command))
  :in-order-to ((test-op (test-op "qlot/tests"))
                (build-op (program-op "qlot/command"))))

(defsystem "qlot/subcommands"
  :pathname "src"
  :depends-on ("qlot/add"
               "qlot/bundle"
               "qlot/check"
               "qlot/install"))

(defsystem "qlot/command"
  :depends-on ("qlot/cli"
               "qlot/main")
  :build-operation "program-op"
  :build-pathname "qlot"
  :entry-point "qlot/cli::main")

(defsystem "qlot/tests"
  :depends-on ("qlot-tests")
  :in-order-to ((test-op (test-op "qlot-tests"))))

(register-system-packages "lparallel" '(:lparallel :lparallel.queue))
