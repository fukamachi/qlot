#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-mysql-test
  (:use :cl
        :cl-test-more
        :dbi))
(in-package :dbd-mysql-test)

(dbi.test:run-driver-tests :mysql :database-name "cl-dbi" :username "nobody" :password "nobody")
