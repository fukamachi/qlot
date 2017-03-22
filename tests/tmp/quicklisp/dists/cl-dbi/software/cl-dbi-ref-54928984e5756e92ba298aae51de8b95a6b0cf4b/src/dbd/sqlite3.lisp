#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd.sqlite3
  (:use :cl
        :dbi.driver
        :dbi.error
        :sqlite)
  (:shadowing-import-from :dbi.driver
                          :disconnect)
  (:import-from :uiop/filesystem
                :file-exists-p))
(in-package :dbd.sqlite3)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbd-sqlite3> (<dbi-driver>) ())

@export
(defclass <dbd-sqlite3-connection> (<dbi-connection>) ())

(defmethod make-connection ((driver <dbd-sqlite3>) &key database-name busy-timeout)
  (make-instance '<dbd-sqlite3-connection>
     :database-name database-name
     :handle (connect database-name :busy-timeout busy-timeout)))

(defclass <dbd-sqlite3-query> (<dbi-query>)
  (%first-result))

(defmethod prepare ((conn <dbd-sqlite3-connection>) (sql string) &key)
  (handler-case
      (make-instance '<dbd-sqlite3-query>
         :connection conn
         :prepared (prepare-statement (connection-handle conn) sql))
    (sqlite-error (e)
      (if (eq (sqlite-error-code e) :error)
          (error '<dbi-programming-error>
                 :message (sqlite-error-message e)
                 :error-code (sqlite-error-code e))
          (error '<dbi-database-error>
                 :message (sqlite-error-message e)
                 :error-code (sqlite-error-code e))))))

(defmethod execute-using-connection ((conn <dbd-sqlite3-connection>) (query <dbd-sqlite3-query>) params)
  (let ((prepared (query-prepared query)))
    (reset-statement prepared)
    (clear-statement-bindings prepared)
    (let ((count 0))
      (dolist (param params)
        (bind-parameter prepared (incf count) param)))
    (setf (slot-value query '%first-result)
          (fetch-using-connection conn query))
    query))

(defmethod do-sql ((conn <dbd-sqlite3-connection>) (sql string) &rest params)
  (handler-case
      (apply #'execute-non-query (connection-handle conn) sql params)
    (sqlite-error (e)
      (if (eq (sqlite-error-code e) :error)
          (error '<dbi-programming-error>
                 :message (sqlite-error-message e)
                 :error-code (sqlite-error-code e))
          (error '<dbi-database-error>
                 :message (sqlite-error-message e)
                 :error-code (sqlite-error-code e))))))

(defmethod fetch-using-connection ((conn <dbd-sqlite3-connection>) (query <dbd-sqlite3-query>))
  @ignore conn
  (if (slot-boundp query '%first-result)
      (prog1
          (slot-value query '%first-result)
        (slot-makunbound query '%first-result))
      (let ((prepared (query-prepared query)))
        (when (handler-case (step-statement prepared)
                (sqlite-error (e)
                  @ignore e
                  nil))
          (loop for column in (statement-column-names prepared)
                for i from 0
                append (list (intern column :keyword)
                             (statement-column-value prepared i)))))))

(defmethod disconnect ((conn <dbd-sqlite3-connection>))
  (sqlite:disconnect (connection-handle conn)))

(defmethod begin-transaction ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "BEGIN TRANSACTION"))

(defmethod commit ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "COMMIT TRANSACTION"))

(defmethod rollback ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "ROLLBACK TRANSACTION"))

(defmethod ping ((conn <dbd-sqlite3-connection>))
  "Return T if the database file exists or the database is in-memory."
  (let* ((handle (connection-handle conn))
         (database-path (sqlite::database-path handle)))
    (cond
      ((string= database-path ":memory:") T)
      ((uiop:file-exists-p database-path) T)
      (T nil))))
