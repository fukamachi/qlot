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
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "parser")
                 (:test-file "server")
                 (:test-file "qlot"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
