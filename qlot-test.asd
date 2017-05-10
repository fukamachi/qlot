#|
  This file is a part of qlot project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage qlot-test-asd
  (:use :cl :asdf))
(in-package :qlot-test-asd)

(defsystem qlot-test
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:qlot
               :qlot/install
               :qlot/source/git
               :qlot/source/github
               :qlot/source/ql
               :qlot/source/http
               :qlot/server
               :uiop
               :prove)
  :components ((:module "tests"
                :components
                ((:test-file "parser")
                 (:test-file "main"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)))
