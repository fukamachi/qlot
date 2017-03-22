#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.driver
  (:use :cl
        :annot.class
        :split-sequence)
  (:import-from :c2mop
                :class-direct-subclasses)
  (:import-from :dbi.error
                :<dbi-unimplemented-error>
                :<dbi-notsupported-error>))
(in-package :dbi.driver)

(cl-syntax:use-syntax :annot)

(declaim (optimize (speed 3)))

@export
(defclass <dbi-driver> () ()
  (:documentation "Base class for DB driver."))

@export
@export-accessors
(defclass <dbi-connection> ()
     ((auto-commit :type boolean
                   :initarg :auto-commit
                   :initform t)
      (database-name :initarg :database-name
                     :accessor connection-database-name)
      (%handle :initarg :handle
               :accessor connection-handle))
  (:documentation "Base class for managing DB connection."))

@export
(defmethod connection-driver-type ((conn <dbi-connection>))
  (let ((package (package-name (symbol-package (type-of conn)))))
    (cond
      ((string= package #.(string :dbd.mysql))    :mysql)
      ((string= package #.(string :dbd.postgres)) :postgres)
      ((string= package #.(string :dbd.sqlite3))  :sqlite3))))

@export
(defmethod make-connection ((driver <dbi-driver>) &key)
  "Create a instance of `<dbi-connection>` for the `driver`.
This method must be implemented in each drivers."
  @ignore driver
  (error '<dbi-unimplemented-error>
         :method-name 'make-connection))

@export
(defmethod disconnect ((conn <dbi-connection>))
  @ignore conn
  (error '<dbi-unimplemented-error>
         :method-name 'disconnect))

@export
(defun find-driver (driver-name)
  "Find a driver class named as `driver-name`.
`driver-name` is a string designer.
Driver should be named like '<DBD-SOMETHING>' for a database 'something'."
  (find-if
   (lambda (class)
     (string= (format nil "<DBD-~:@(~A~)>" driver-name)
              (class-name class)))
   (list-all-drivers)))

@export
(defun list-all-drivers ()
  "Return a list of direct subclasses for `<dbi-driver>`."
  (c2mop:class-direct-subclasses (find-class '<dbi-driver>)))

@export
@export-accessors
(defclass <dbi-query> ()
     ((connection :type <dbi-connection>
                  :initarg :connection
                  :initform nil
                  :accessor query-connection)
      (prepared :type t
                :initarg :prepared
                :accessor query-prepared))
  (:documentation "Class that represents a prepared DB query."))

@export
(defgeneric prepare (conn sql &key))

@export
(defmethod prepare ((conn <dbi-connection>) (sql string) &key (query-class '<dbi-query>))
  "Preparing executing SQL statement and returns a instance of `<dbi-query>`.
This method may be overrided by subclasses."
  (make-instance query-class
     :connection conn
     :prepared (prepare-sql conn sql)))

@export
(defmethod execute ((query <dbi-query>) &rest params)
  "Execute `query` with `params` and return the results."
  (execute-using-connection
   (query-connection query)
   query
   params))

@export
(defmethod fetch ((query <dbi-query>))
  "Fetch the first row from `query` which is returned by `execute`."
  (fetch-using-connection (query-connection query) query))

@export
(defmethod fetch-all ((query <dbi-query>))
  "Fetch all rest rows from `query`."
  (loop for result = (fetch query)
        while result
        collect result))

@export
(defmethod fetch-using-connection ((conn <dbi-connection>) (query <dbi-query>))
  (error '<dbi-unimplemented-error>
         :method-name 'fetch-using-connection))

@export
(defmethod do-sql ((conn <dbi-connection>) (sql string) &rest params)
  "Do preparation and execution at once.
This method may be overrided by subclasses."
  (apply #'execute (prepare conn sql) params)
  nil)

@export
(defmethod execute-using-connection ((conn <dbi-connection>) (query <dbi-query>) params)
  "Execute `query` in `conn`.
This method must be implemented in each drivers."
  @ignore (conn query params)
  (error '<dbi-unimplemented-error>
         :method-name 'execute-using-connection))

@export
(defmethod begin-transaction :around ((conn <dbi-connection>))
  "Turn `auto-commit` off automatically before starting a transaction."
  (symbol-macrolet ((auto-commit (slot-value conn 'auto-commit)))
     (let ((saved auto-commit))
       (setf auto-commit nil)
       (unwind-protect (call-next-method)
         (setf auto-commit saved)))))

@export
(defmethod begin-transaction ((conn <dbi-connection>))
  "Start a transaction."
  @ignore conn
  (error '<dbi-notsupported-error>
         :method-name 'begin-transaction))

@export
(defmethod commit ((conn <dbi-connection>))
  "Commit changes and end the current transaction."
  @ignore conn
  (error '<dbi-notsupported-error>
         :method-name 'commit))

@export
(defmethod rollback ((conn <dbi-connection>))
  "Rollback all changes and end the current transaction."
  @ignore conn
  (error '<dbi-notsupported-error>
         :method-name 'rollback))

@export
(defmethod ping ((conn <dbi-connection>))
  "Check if the database server is still running and the connection to it is still working."
  @ignore conn
  (error '<dbi-notsupported-error>
         :method-name 'ping))

@export
(defmethod escape-sql ((conn <dbi-connection>) (sql string))
  "Return escaped `sql`.
This method may be overrided by subclasses when needed.
For example, in case of MySQL and PostgreSQL, backslashes must be escaped by doubling it."
  (with-output-to-string (out)
    (loop for c across sql
          if (char= c #\')
            do (write-sequence "''" out)
          else
            do (write-char c out))))

(defmethod prepare-sql ((conn <dbi-connection>) (sql string))
  "Create a function that takes parameters, binds them into a query and returns SQL as a string."
  (labels ((param-to-sql (param)
             (typecase param
               (string (concatenate 'string "'" (escape-sql conn param) "'"))
               (null "NULL")
               (t (princ-to-string param)))))
    (let ((sql-parts (split-sequence #\? sql)))
      (lambda (&rest params)
        (if params
            (with-output-to-string (out)
              (loop for (part . rest) on sql-parts
                    do
                 (let ((param (pop params)))
                   (write-sequence
                    (if rest
                        (concatenate 'string part (param-to-sql param))
                        part)
                    out))))
            sql)))))
