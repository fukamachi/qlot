(defsystem "qlot"
  :class :package-inferred-system
  :version "1.4.2"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "A project-local library installer"
  :pathname "src"
  :depends-on ("qlot/main"
               (:feature :ros.installing "qlot/cli")
               (:feature :ros.installing "qlot/fetch")
               (:feature (:not :qlot.project) "qlot/add")
               (:feature (:not :qlot.project) "qlot/bundle")
               (:feature (:not :qlot.project) "qlot/check")
               (:feature (:not :qlot.project) "qlot/install"))
  :output-files (image-op (o c)
                  (output-files o :qlot/command))
  :in-order-to ((test-op (test-op "qlot/tests"))
                (build-op (program-op "qlot/command"))))

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
