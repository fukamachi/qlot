#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi-test.driver
  (:use :cl
        :cl-test-more
        :dbi
        :dbi.driver))
(in-package :dbi-test.driver)

(plan 6)

(is (find-driver :imaginedb) nil :test #'eq
    "find-driver: which doesn't exist")

(defclass <dbd-imaginedb> (<dbi-driver>) () )
(defmethod make-connection ((class <dbd-imaginedb>) &rest params)
  (declare (ignore params))
  (make-instance '<dbi-connection>))

(is (c2mop:subclassp (find-driver :imaginedb) '<dbi-driver>)
    t
    "find-driver: which exists")

(ok (find (find-class '<dbd-imaginedb>) (list-all-drivers))
    "list-all-drivers")

(defparameter *connection* (connect :imaginedb))

(is-type *connection* '<dbi-connection>
         "connect")

(defparameter *query*
              (prepare *connection* "SELECT * FROM kyoto WHERE type = ?"))

(is-type *query*
         '<dbi-query>
         "prepare")

(is (funcall (slot-value *query* 'dbi.driver::prepared) "cafe")
    "SELECT * FROM kyoto WHERE type = 'cafe'"
    "prepare-sql")

(c2mop:remove-direct-subclass (find-class '<dbi-driver>) (find-class '<dbd-imaginedb>))

(finalize)
