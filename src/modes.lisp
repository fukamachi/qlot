(defpackage #:qlot/modes
  (:use #:cl)
  (:import-from #:qlot/errors
                #:offline-cache-conflict)
  (:export #:*offline*
           #:*locked*
           #:initialize-modes))
(in-package #:qlot/modes)

(defvar *offline* nil)
(defvar *locked* nil)

(defun initialize-modes ()
  (setf *offline* (uiop:getenvp "QLOT_OFFLINE")
        *locked* (uiop:getenvp "QLOT_LOCKED"))
  (when (and *offline*
             (uiop:getenvp "QLOT_NO_CACHE"))
    (error 'offline-cache-conflict))
  (values))
