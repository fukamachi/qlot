#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-sqlite3-test
  (:use :cl
        :cl-test-more
        :dbi.test))
(in-package :dbd-sqlite3-test)

(dbi.test:run-driver-tests :sqlite3 :database-name ":memory:")
