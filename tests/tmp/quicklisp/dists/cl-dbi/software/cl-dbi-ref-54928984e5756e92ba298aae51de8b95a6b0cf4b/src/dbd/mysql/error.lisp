#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd.mysql.error
  (:use :cl)
  (:import-from :dbi.error
                :<dbi-programming-error>
                :<dbi-database-error>)
  (:import-from :dbi.driver
                :connection-handle)
  (:import-from :cl-mysql-system
                :mysql-error
                :mysql-error-errno
                :mysql-error-message
                :release
                :connections
                :in-use))
(in-package :dbd.mysql.error)

(cl-syntax:use-syntax :annot)

(defparameter *mysql-programming-error-code*
  (list 1044 ;; ER_DBACCESS_DENIED_ERROR
        1049 ;; ER_BAD_DB_ERROR
        1050 ;; ER_TABLE_EXISTS_ERROR
        1051 ;; ER_BAD_TABLE_ERROR
        1054 ;; ER_BAD_FIELD_ERROR
        1055 ;; ER_WRONG_FIELD_WITH_GROUP
        1056 ;; ER_WRONG_GROUP_FIELD
        1057 ;; ER_WRONG_SUM_SELECT
        1059 ;; ER_TOO_LONG_IDENT
        1060 ;; ER_DUP_FIELDNAME
        1061 ;; ER_DUP_KEYNAME
        1063 ;; ER_WRONG_FIELD_SPEC
        1064 ;; ER_PARSE_ERROR
        1066 ;; ER_NONUNIQ_TABLE
        1067 ;; ER_INVALID_DEFAULT
        1068 ;; ER_MULTIPLE_PRI_KEY
        1069 ;; ER_TOO_MANY_KEYS
        1070 ;; ER_TOO_MANY_KEY_PARTS
        1071 ;; ER_TOO_LONG_KEY
        1072 ;; ER_KEY_COLUMN_DOES_NOT_EXITS
        1073 ;; ER_BLOB_USED_AS_KEY
        1074 ;; ER_TOO_BIG_FIELDLENGTH
        1075 ;; ER_WRONG_AUTO_KEY
        1082 ;; ER_NO_SUCH_INDEX
        1083 ;; ER_WRONG_FIELD_TERMINATORS
        1084 ;; ER_BLOBS_AND_NO_TERMINATED
        1090 ;; ER_CANT_REMOVE_ALL_FIELDS
        1091 ;; ER_CANT_DROP_FIELD_OR_KEY
        1101 ;; ER_BLOB_CANT_HAVE_DEFAULT
        1102 ;; ER_WRONG_DB_NAME
        1103 ;; ER_WRONG_TABLE_NAME
        1104 ;; ER_TOO_BIG_SELECT
        1106 ;; ER_UNKNOWN_PROCEDURE
        1107 ;; ER_WRONG_PARAMCOUNT_TO_PROCEDURE
        1109 ;; ER_UNKNOWN_TABLE
        1110 ;; ER_FIELD_SPECIFIED_TWICE
        1112 ;; ER_UNSUPPORTED_EXTENSION
        1113 ;; ER_TABLE_MUST_HAVE_COLUMNS
        1115 ;; ER_UNKNOWN_CHARACTER_SET
        1118 ;; ER_TOO_BIG_ROWSIZE
        1120 ;; ER_WRONG_OUTER_JOIN
        1121 ;; ER_NULL_COLUMN_IN_INDEX
        1131 ;; ER_PASSWORD_ANONYMOUS_USER
        1132 ;; ER_PASSWORD_NOT_ALLOWED
        1133 ;; ER_PASSWORD_NO_MATCH
        1138 ;; ER_INVALID_USE_OF_NULL
        1139 ;; ER_REGEXP_ERROR
        1140 ;; ER_MIX_OF_GROUP_FUNC_AND_FIELDS
        1141 ;; ER_NONEXISTING_GRANT
        1142 ;; ER_TABLEACCESS_DENIED_ERROR
        1143 ;; ER_COLUMNACCESS_DENIED_ERROR
        1144 ;; ER_ILLEGAL_GRANT_FOR_TABLE
        1145 ;; ER_GRANT_WRONG_HOST_OR_USER
        1146 ;; ER_NO_SUCH_TABLE
        1147 ;; ER_NONEXISTING_TABLE_GRANT
        1148 ;; ER_NOT_ALLOWED_COMMAND
        1149 ;; ER_SYNTAX_ERROR
        1162 ;; ER_TOO_LONG_STRING
        1163 ;; ER_TABLE_CANT_HANDLE_BLOB
        1164 ;; ER_TABLE_CANT_HANDLE_AUTO_INCREMENT
        1166 ;; ER_WRONG_COLUMN_NAME
        1167 ;; ER_WRONG_KEY_COLUMN
        1170 ;; ER_BLOB_KEY_WITHOUT_LENGTH
        1171 ;; ER_PRIMARY_CANT_HAVE_NULL
        1172 ;; ER_TOO_MANY_ROWS
        1173 ;; ER_REQUIRES_PRIMARY_KEY
        1177 ;; ER_CHECK_NO_SUCH_TABLE
        1178 ;; ER_CHECK_NOT_IMPLEMENTED
        1203 ;; ER_TOO_MANY_USER_CONNECTIONS
        1211 ;; ER_NO_PERMISSION_TO_CREATE_USER
        1226 ;; ER_USER_LIMIT_REACHED
        1230 ;; ER_NO_DEFAULT
        1231 ;; ER_WRONG_VALUE_FOR_VAR
        1232 ;; ER_WRONG_TYPE_FOR_VAR
        1234 ;; ER_CANT_USE_OPTION_HERE
        1235 ;; ER_NOT_SUPPORTED_YET
        1239 ;; ER_WRONG_FK_DEF
        1247 ;; ER_ILLEGAL_REFERENCE
        1248 ;; ER_DERIVED_MUST_HAVE_ALIAS
        1250 ;; ER_TABLENAME_NOT_ALLOWED_HERE
        1252 ;; ER_SPATIAL_CANT_HAVE_NULL
        1253 ;; ER_COLLATION_CHARSET_MISMATCH
        1280 ;; ER_WRONG_NAME_FOR_INDEX
        1281 ;; ER_WRONG_NAME_FOR_CATALOG
        1286 ;; ER_UNKNOWN_STORAGE_ENGINE
        ))

@export
(defmacro with-error-handler (conn &body body)
  `(handler-case (progn ,@body)
     (mysql-error (e)
       (unwind-protect
            (error (if (member (mysql-error-errno e)
                               *mysql-programming-error-code*
                               :test #'eql)
                       '<dbi-programming-error>
                       '<dbi-database-error>)
                   :message (mysql-error-message e)
                   :error-code (mysql-error-errno e))
         ;; KLUDGE: I think this should be done in cl-mysql.
         ;;   cl-mysql doesn't release the connection when a MySQL error has occurred.
         ;;   Though I can't tell which connection is used for the query,
         ;;   I assume the first one is the one.
         (let* ((handle (connection-handle ,conn))
                (using-connections (cl-mysql-system:connections handle))
                (connection (and (> (length using-connections) 0)
                                 (aref using-connections 0))))
           (when (and connection (in-use connection))
             (cl-mysql-system:release handle connection)))))))
