(defpackage #:qlot/utils/tmp
  (:use #:cl)
  (:import-from #:qlot/utils
                #:generate-random-string)
  (:export #:tmp-directory
           #:with-tmp-directory))
(in-package #:qlot/utils/tmp)

(defun tmp-directory (&optional (prefix "qlot-"))
  (let ((directory (merge-pathnames (format nil "~A~A/" prefix (generate-random-string))
                                    (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(defmacro with-tmp-directory ((tmp-dir &key (prefix "qlot-")) &body body)
  `(let ((,tmp-dir (tmp-directory ,prefix)))
     (unwind-protect (progn ,@body)
       #+windows
       (uiop:run-program (list "attrib"
                               "-r" "-h"
                               (format nil "~A*.*" (uiop:native-namestring ,tmp-dir))
                               "/s" "/d")
                         :error-output *error-output*
                         :ignore-error-status t)
       (uiop:delete-directory-tree ,tmp-dir :validate t :if-does-not-exist :ignore))))
