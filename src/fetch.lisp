(defpackage #:qlot/fetch
  (:use #:cl)
  (:import-from #:qlot/http)
  (:import-from #:qlot/utils/cli
                #:command-line-arguments))
(in-package #:qlot/fetch)

(defun file-size (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (file-length in)))

(defun fetch-file (url file &rest args &key quietly &allow-other-keys)
  (declare (ignore args))
  (ensure-directories-exist file)
  (unless quietly
    (format t "~&; Fetching '~A'.~%" url))
  (let ((now (get-internal-real-time)))
    (multiple-value-bind (body-stream status headers)
        (qlot/http:get url :want-stream t :force-binary t)
      (declare (ignore status))
      (let* ((body-size (gethash "content-length" headers))
             (body-size (and body-size
                             (ignore-errors
                               (parse-integer body-size))))
             (buffer (make-array 1024 :element-type '(unsigned-byte 8)))
             (current-read-bytes 0)
             (current-progress 0)
             (width 80))
        (with-open-file (out file
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8)
                             :direction :output)
          (loop for read-bytes = (read-sequence buffer body-stream)
                until (zerop read-bytes)
                do (incf current-read-bytes read-bytes)
                   (write-sequence buffer out :end read-bytes)
                   (when (and body-size (not quietly))
                     (let ((new-progress (round
                                          (* width
                                             (min 1.0
                                                  (/ current-read-bytes body-size))))))
                       (loop repeat (- new-progress current-progress)
                             do (write-char #\#))
                       (force-output)
                       (setf current-progress new-progress)))))))
    (unless quietly
      (format t "~&; Done '~A' (~$KB) in ~A seconds.~%"
              (file-namestring file)
              (/ (file-size file) 1024)
              (coerce
               (/ (- (get-internal-real-time) now)
                  internal-time-units-per-second)
               'float))))
  file)

(defun parse-options (args)
  (loop for arg in args
        if (equal arg "--quiet")
        append (list :quietly t)))

(defun main ()
  (destructuring-bind (&optional $1 &rest argv)
      (command-line-arguments)
    (let ((args (if (equal $1 "--")
                    argv
                    (cons $1 argv))))
      (unless (<= 2 (length args))
        (format *error-output* "~&Error: Invalid number of arguments.~%"))
      (destructuring-bind (url file &rest option-args)
          args
        (apply #'fetch-file url file
               (parse-options option-args)))))
  (uiop:quit))
