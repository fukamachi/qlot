#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.error
  (:use :cl))
(in-package :dbi.error)

(cl-syntax:use-syntax :annot)

@export
(define-condition <dbi-error> (simple-error) ()
  (:documentation "Base class of all other error exceptions. Use this to catch all DBI
errors."))

@export
(define-condition <dbi-warning> (simple-warning) ()
  (:documentation "For important warnings like data truncation, etc."))

@export
(define-condition <dbi-interface-error> (<dbi-error>) ()
  (:documentation "Exception for errors related to the DBI interface rather than the
database itself."))

@export
(define-condition <dbi-unimplemented-error> (<dbi-interface-error>)
  ((method-name :initarg :method-name :type (or symbol string)))
  (:documentation "Exception raised if the DBD driver has not specified a mandatory method.")
  (:report
   (lambda (condition stream)
     (format stream
             "`~A' must be implemented."
             (slot-value condition 'method-name)))))

@export
(define-condition <dbi-database-error> (<dbi-error>)
  ((message :initarg :message)
   (error-code :initarg :error-code))
  (:documentation "Exception for errors related to the database.")
  (:report
   (lambda (condition stream)
     (format stream
             "DB Error: ~A (Code: ~A)"
             (slot-value condition 'message)
             (slot-value condition 'error-code)))))

@export
(define-condition <dbi-data-error> (<dbi-database-error>) ()
  (:documentation "Exception for errors due to problems with the processed
data such as division for zero, numeric value out of range, etc."))

@export
(define-condition <dbi-operational-error> (<dbi-database-error>) ()
  (:documentation "Exception for errors related to the database's operation which are not
necessarily under the control of the programmer.  This includes such
things as unexpected disconnect, datasource name not found, transaction
could not be processed, a memory allocation error occured during
processing, etc."))

@export
(define-condition <dbi-integrity-error> (<dbi-database-error>) ()
  (:documentation "Exception raised when the relational integrity of the database
is affected, e.g. a foreign key check fails."))

@export
(define-condition <dbi-internal-error> (<dbi-database-error>) ()
  (:documentation "Exception raised when the database encounters an internal error,
e.g. the cursor is not valid anymore, the transaction is out of sync."))

@export
(define-condition <dbi-programming-error> (<dbi-database-error>) ()
  (:documentation "Exception raised for programming errors, e.g. table not found
or already exists, syntax error in SQL statement, wrong number
of parameters specified, etc."))

@export
(define-condition <dbi-notsupported-error> (<dbi-database-error>)
  ((method-name :initarg :method-name :type (or symbol string)))
  (:documentation "Exception raised if e.g. commit() is called for a database which do not
support transactions.")
  (:report
   (lambda (condition stream)
     (format stream
             "`~A' isn't supported on the current driver."
             (slot-value condition 'method-name)))))
