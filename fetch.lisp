(defpackage #:qlot/fetch
  (:use #:cl)
  (:import-from #:qlot/utils/http)
  (:import-from #:qlot/utils/cli
                #:command-line-arguments))
(in-package #:qlot/fetch)

(defun file-size (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (file-length in)))

;; TODO: show progress
(defun fetch-file (url file)
  (ensure-directories-exist file)
  (format t "~&; Fetching '~A'.~%" url)
  (let ((now (get-internal-real-time)))
    (qdex:fetch url file)
    (format t "~&; Done '~A' (~$KB) in ~A seconds.~%"
            (file-namestring file)
            (/ (file-size file) 1024)
            (coerce
             (/ (- (get-internal-real-time) now)
                internal-time-units-per-second)
             'float)))
  file)

(defun main ()
  (destructuring-bind (&optional $0 $1 &rest argv)
      (command-line-arguments)
    (let ((args (if (equal $1 "--")
                    argv
                    (cons $1 argv))))
      (unless (= (length args) 2)
        (format *error-output* "~&Error: Invalid number of arguments.~%"))
      (apply #'fetch-file args))))
