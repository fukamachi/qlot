(defpackage #:qlot/utils/digest
  (:use #:cl)
  (:import-from #:qlot/utils/shell
                #:safety-shell-command
                #:shell-command-error)
  (:import-from #:qlot/utils
                #:split-with
                #:slurp-file)
  (:import-from #:md5)
  (:import-from #:sha1)
  (:export #:md5
           #:sha1sum))
(in-package #:qlot/utils/digest)

(defun hex-string (byte-array)
  (check-type byte-array (vector (unsigned-byte 8)))
  (flet ((int-to-hex (v)
           (format nil "~(~X~)" v)))
    (apply #'concatenate 'string (map 'list #'int-to-hex byte-array))))

(defun md5 (value)
  (hex-string
    (etypecase value
      (pathname (md5:md5sum-file value))
      (stream (md5:md5sum-stream value))
      (string (md5:md5sum-string value))
      (sequence (md5:md5sum-sequence value)))))

(defun sha1sum (file)
  (handler-case
      (first
        (split-with #\Space
                    (safety-shell-command "sha1sum" (list (uiop:native-namestring file)))
                    :limit 1))
    (shell-command-error ()
      (string-downcase
        (sha1:sha1-hex (slurp-file file))))))
