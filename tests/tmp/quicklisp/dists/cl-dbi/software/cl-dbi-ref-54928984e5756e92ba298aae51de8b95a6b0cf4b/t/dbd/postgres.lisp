#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-postgres-test
  (:use :cl
        :cl-test-more
        :dbi.test))
(in-package :dbd-postgres-test)

(dbi.test:run-driver-tests :postgres :database-name "cl-dbi" :username "nobody" :password "nobody")
