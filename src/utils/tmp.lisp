(defpackage #:qlot/utils/tmp
  (:use #:cl)
  (:import-from #:qlot/utils
                #:generate-random-string)
  (:export #:tmp-directory
           #:delete-tmp-directory
           #:with-tmp-directory))
(in-package #:qlot/utils/tmp)

(defun tmp-directory (&optional (prefix "qlot-"))
  (let ((directory (merge-pathnames (format nil "~A~A/" prefix (generate-random-string))
                                    (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    (truename directory)))

(defun delete-tmp-directory (tmp-dir)
  #+(or mswindows win32)
  (uiop:run-program (list "attrib"
                          "-r" "-h"
                          (format nil "~A*.*" (uiop:native-namestring tmp-dir))
                          "/s" "/d")
                    :error-output *error-output*
                    :ignore-error-status t)
  (uiop:delete-directory-tree tmp-dir :validate t :if-does-not-exist :ignore))

(defmacro with-tmp-directory ((tmp-dir &key (prefix "qlot-")) &body body)
  `(let ((,tmp-dir (tmp-directory ,prefix)))
     (unwind-protect (progn ,@body)
       (delete-tmp-directory ,tmp-dir))))
