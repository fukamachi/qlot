#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Database driver for SQLite3.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-sqlite3-asd
  (:use :cl :asdf))
(in-package :dbd-sqlite3-asd)

(defsystem dbd-sqlite3
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi
               :sqlite
               :cl-syntax
               :cl-syntax-annot
               :uiop)
  :components ((:module "src/dbd"
                :components
                ((:file "sqlite3"))))
  :description "Database driver for SQLite3.")
