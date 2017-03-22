#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi-test-asd
  (:use :cl :asdf))
(in-package :dbi-test-asd)

(defsystem dbi-test
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi
               :cl-test-more
               :closer-mop
               :cl-syntax
               :cl-syntax-annot
               :trivial-types)
  :components ((:module "src"
                :components
                ((:file "test")))
               (:module "t"
                :depends-on ("src")
                :components
                ((:test-file "driver")
                 (:module "dbd"
                  :components
                  ((:test-file "sqlite3")
                   (:test-file "postgres")
                   (:test-file "mysql"))))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)))
