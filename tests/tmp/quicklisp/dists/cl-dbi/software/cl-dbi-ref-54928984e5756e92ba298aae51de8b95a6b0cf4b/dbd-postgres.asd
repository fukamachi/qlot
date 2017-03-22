#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Database driver for PostgreSQL.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-postgres-asd
  (:use :cl :asdf))
(in-package :dbd-postgres-asd)

(defsystem dbd-postgres
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi
               :cl-postgres
               :cl-syntax
               :cl-syntax-annot)
  :components ((:module "src/dbd"
                :components
                ((:file "postgres"))))
  :description "Database driver for PostgreSQL.")
