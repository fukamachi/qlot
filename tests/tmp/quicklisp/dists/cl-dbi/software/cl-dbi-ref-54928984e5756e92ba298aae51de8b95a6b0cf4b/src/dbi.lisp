#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi
  (:use :cl
        :dbi.error)
  (:nicknames :cl-dbi)
  (:import-from :dbi.driver
                :list-all-drivers
                :find-driver
                :connection-driver-type
                :connection-database-name
                :make-connection
                :disconnect
                :prepare
                :execute
                :fetch
                :fetch-all
                :do-sql
                :begin-transaction
                :commit
                :rollback
                :ping)
  (:export :list-all-drivers
           :find-driver
           :connection-driver-type
           :connection-database-name
           :disconnect
           :prepare
           :execute
           :fetch
           :fetch-all
           :do-sql
           :begin-transaction
           :commit
           :rollback
           :ping

           :<dbi-error>
           :<dbi-warning>
           :<dbi-interface-error>
           :<dbi-unimplemented-error>
           :<dbi-database-error>
           :<dbi-data-error>
           :<dbi-operational-error>
           :<dbi-integrity-error>
           :<dbi-internal-error>
           :<dbi-programming-error>
           :<dbi-notsupported-error>))
(in-package :dbi)

(cl-syntax:use-syntax :annot)

@export
(defun connect (driver-name &rest params &key database-name &allow-other-keys)
  "Open a connection to the database which corresponds to `driver-name`."
  (let ((driver (find-driver driver-name)))
    (unless driver
      (load-driver driver-name)
      (setf driver (find-driver driver-name)))

    (unless driver
      (error 'simple-error
             :format-control "Driver ~A is not found."
             :format-arguments driver-name))

    (apply #'make-connection (make-instance driver) params)))

(defun load-driver (driver-name)
  (let ((driver-system (intern (format nil "DBD-~A" driver-name) :keyword)))
    #+quicklisp (ql:quickload driver-system :verbose nil)
    #-quicklisp
    (asdf:load-system driver-system :verbose nil)))

@export
(defmacro with-transaction (conn &body body)
  "Start a transaction and commit at the end of this block. If the evaluation `body` is interrupted, the transaction is rolled back automatically."
  (let ((ok (gensym "TRANSACTION-OK"))
        (conn-var (gensym "CONN-VAR")))
    `(let (,ok
           (,conn-var ,conn))
       (begin-transaction ,conn-var)
       (unwind-protect (multiple-value-prog1
                         (progn ,@body)
                         (setf ,ok t))
         (if ,ok
             (commit ,conn-var)
             (rollback ,conn-var))))))

@export
(defmacro with-connection ((conn-sym &rest rest) &body body)
  `(let ((,conn-sym (connect ,@rest)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,conn-sym))))
