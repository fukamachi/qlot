#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd.postgres
  (:use :cl
        :dbi.driver
        :dbi.error
        :cl-postgres)
  (:import-from :cl-postgres
                :connection-socket
                :send-parse)
  (:import-from :cl-postgres-error
                :database-error
                :syntax-error-or-access-violation
                :database-error-message
                :database-error-code

                :admin-shutdown
                :crash-shutdown
                :cannot-connect-now))
(in-package :dbd.postgres)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbd-postgres> (<dbi-driver>) ())

@export
(defclass <dbd-postgres-connection> (<dbi-connection>) ())

(defmethod make-connection ((driver <dbd-postgres>) &key database-name username password (host "localhost") (port 5432) (use-ssl :no))
  (make-instance '<dbd-postgres-connection>
     :database-name database-name
     :handle (open-database database-name username password host port use-ssl)))

@export
(defclass <dbd-postgres-query> (<dbi-query>)
     ((name :initarg :name)
      (%result :initform nil)))

(defmethod prepare ((conn <dbd-postgres-connection>) (sql string) &key)
  (let ((name (symbol-name (gensym "PREPARED-STATEMENT"))))
    (setf sql
          (with-output-to-string (s)
            (loop with i = 0
                  with escaped = nil
                  for c across sql
                  if (and (char= c #\\) (not escaped))
                    do (setf escaped t)
                  else do (setf escaped nil)
                  if (and (char= c #\?) (not escaped))
                    do (format s "$~D" (incf i))
                  else do (write-char c s))))
    (handler-case
        (make-instance '<dbd-postgres-query>
           :connection conn
           :name name
           :prepared (prepare-query (connection-handle conn) name sql))
      (syntax-error-or-access-violation (e)
        (error '<dbi-programming-error>
               :message (database-error-message e)
               :error-code (database-error-code e)))
      (database-error (e)
        (error '<dbi-database-error>
               :message (database-error-message e)
               :error-code (database-error-code e))))))

(defmethod execute-using-connection ((conn <dbd-postgres-connection>) (query <dbd-postgres-query>) params)
  (handler-case
      (exec-prepared (connection-handle conn)
                     (slot-value query 'name)
                     params
                     ;; TODO: lazy fetching
                     (row-reader (fields)
                       (let ((result
                               (loop while (next-row)
                                     collect (loop for field across fields
                                                   collect (intern (field-name field) :keyword)
                                                   collect (next-field field)))))
                         (setf (slot-value query '%result)
                               result)
                         query)))
    (syntax-error-or-access-violation (e)
      (error '<dbi-programming-error>
             :message (database-error-message e)
             :error-code (database-error-code e)))
    (database-error (e)
      (error '<dbi-database-error>
             :message (database-error-message e)
             :error-code (database-error-code e)))))

(defmethod fetch ((query <dbd-postgres-query>))
  (pop (slot-value query '%result)))

(defmethod disconnect ((conn <dbd-postgres-connection>))
  (close-database (connection-handle conn)))

(defmethod begin-transaction ((conn <dbd-postgres-connection>))
  (do-sql conn "BEGIN"))

(defmethod commit ((conn <dbd-postgres-connection>))
  (do-sql conn "COMMIT"))

(defmethod rollback ((conn <dbd-postgres-connection>))
  (do-sql conn "ROLLBACK"))

(defmethod ping ((conn <dbd-postgres-connection>))
  (let ((handle (connection-handle conn)))
    (handler-case
        (and (database-open-p handle)
             (progn
               (cl-postgres::send-parse (cl-postgres::connection-socket handle)
                                        (symbol-name (gensym "PING"))
                                        "")
               t))
      ((cl-postgres-error:admin-shutdown
        cl-postgres-error:crash-shutdown
        cl-postgres-error:cannot-connect-now) (e)
        @ignore e
        nil))))
