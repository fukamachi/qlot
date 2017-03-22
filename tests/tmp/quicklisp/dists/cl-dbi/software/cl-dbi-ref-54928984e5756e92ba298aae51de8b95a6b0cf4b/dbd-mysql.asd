#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Database driver for MySQL.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-mysql-asd
  (:use :cl :asdf))
(in-package :dbd-mysql-asd)

(defsystem dbd-mysql
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi
               :cl-mysql
               :cl-syntax
               :cl-syntax-annot)
  :components ((:module "src/dbd"
                :components
                ((:file "mysql" :depends-on ("mysql/error"))
                 (:file "mysql/error"))))
  :description "Database driver for MySQL.")
