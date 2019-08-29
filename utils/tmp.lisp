(defpackage #:qlot/utils/tmp
  (:use #:cl)
  (:import-from #:qlot/utils
                #:generate-random-string)
  (:export #:tmp-directory))
(in-package #:qlot/utils/tmp)

(defun tmp-directory (&optional (prefix "qlot-"))
  (let ((directory (merge-pathnames (format nil "~A~A/" prefix (generate-random-string))
                                    (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))
