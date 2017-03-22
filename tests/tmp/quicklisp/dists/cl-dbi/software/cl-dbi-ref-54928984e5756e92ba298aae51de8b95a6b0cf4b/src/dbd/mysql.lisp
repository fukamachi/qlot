#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd.mysql
  (:use :cl
        :dbi.driver
        :cl-mysql
        :annot.class)
  (:shadow :result-set-fields
           :next-row)
  (:shadowing-import-from :dbi.driver
                          :disconnect
                          :ping)
  (:import-from :dbd.mysql.error
                :with-error-handler)
  (:import-from :cl-mysql-system
                :mysql-error
                :mysql-error-errno
                :connect-to-server
                :return-or-close
                :owner-pool
                :+server-gone-error+))
(in-package :dbd.mysql)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbd-mysql> (<dbi-driver>) ())

@export
(defclass <dbd-mysql-connection> (<dbi-connection>) ())

(defmethod make-connection ((driver <dbd-mysql>) &key host database-name username password port socket client-flag)
  (make-instance '<dbd-mysql-connection>
     :database-name database-name
     :handle (connect :host host
                      :database database-name
                      :user username
                      :password password
                      :port port
                      :socket socket
                      :client-flag client-flag)))

@export
@export-accessors
(defclass <dbd-mysql-query> (<dbi-query>)
     ((%result :initform nil)
      (store :initarg :store :initform T
             :accessor mysql-use-store)))

(defstruct (mysql-result-list (:constructor make-mysql-result-list (result-set fields)))
  (result-set nil :type list)
  (fields nil :type list))

(defun result-set-fields (result)
  (if (mysql-result-list-p result)
      (slot-value result 'fields)
      (car (cl-mysql:result-set-fields result))))

(defun next-row (result)
  (if (mysql-result-list-p result)
      (pop (slot-value result 'result-set))
      (cl-mysql:next-row result)))

(defmethod prepare ((conn <dbd-mysql-connection>) (sql string) &key (store T))
  (let ((query (call-next-method conn sql :query-class '<dbd-mysql-query>)))
    (setf (mysql-use-store query) store)
    query))

(defmethod execute-using-connection ((conn <dbd-mysql-connection>) (query <dbd-mysql-query>) params)
  (let ((result
          (with-error-handler conn
            (query (apply (query-prepared query) params)
                   :database (connection-handle conn)
                   :store (mysql-use-store query)))))
    (if (mysql-use-store query)
        (setf result
              (apply #'make-mysql-result-list (car result)))
        (progn
          (return-or-close (owner-pool result) result)
          (next-result-set result)))
    (setf (slot-value query '%result) result)
    query))

(defmethod fetch-using-connection ((conn <dbd-mysql-connection>) query)
  (loop with result = (slot-value query '%result)
        for val in (next-row result)
        for (name . nil) in (result-set-fields result)
        append (list (intern name :keyword) val)))

(defmethod escape-sql ((conn <dbd-mysql-connection>) (sql string))
  (escape-string sql :database (connection-handle conn)))

(defmethod disconnect ((conn <dbd-mysql-connection>))
  (cl-mysql:disconnect (connection-handle conn)))

(defmethod begin-transaction ((conn <dbd-mysql-connection>))
  (do-sql conn "START TRANSACTION"))

(defmethod commit ((conn <dbd-mysql-connection>))
  (do-sql conn "COMMIT"))

(defmethod rollback ((conn <dbd-mysql-connection>))
  (do-sql conn "ROLLBACK"))

(defmethod ping ((conn <dbd-mysql-connection>))
  (handler-case (cl-mysql:ping :database (connection-handle conn))
    (mysql-error (e)
      (if (= +server-gone-error+ (mysql-error-errno e))
          nil
          (signal e)))))
