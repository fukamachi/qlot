(defpackage #:qlot/utils/ql
  (:use #:cl)
  (:import-from #:qlot/utils
                #:split-with)
  (:export #:quicklisp-distinfo-url
           #:parse-distinfo-stream
           #:parse-distinfo-file
           #:parse-space-delimited-stream
           #:parse-space-delimited-file
           #:with-quicklisp-home))
(in-package #:qlot/utils/ql)

(defparameter *quicklisp-distinfo*
  "http://beta.quicklisp.org/dist/quicklisp.txt")

(defparameter *quicklisp-versioned-distinfo*
  "http://beta.quicklisp.org/dist/quicklisp/{{version}}/distinfo.txt")

(defun replace-version (value version)
  (check-type value string)
  (with-output-to-string (*standard-output*)
    (loop with i = 0
          for pos = (search "{{version}}" value :start2 i)
          do (princ (subseq value i pos))
          if pos
            do (princ version)
               (setf i (+ pos (length "{{version}}")))
          while pos)))

(defun check-version (version-string)
  (or (null version-string)
      (and (stringp version-string)
           (= 10 (length version-string))
           (flet ((check-num-char (from-idx to-idx)
                    (loop for i from from-idx to to-idx
                          unless (char<= #\0 (aref version-string i) #\9)
                          do (return nil)
                          finally (return t))))
             (and (check-num-char 0 3)
                  (char= #\- (aref version-string 4))
                  (check-num-char 5 6)
                  (char= #\- (aref version-string 7))
                  (check-num-char 8 9))))))

(defun quicklisp-distinfo-url (&optional version)
  (assert (check-version version))
  (if version
      (replace-version *quicklisp-versioned-distinfo* version)
      *quicklisp-distinfo*))

(defun parse-distinfo-stream (stream)
  (loop for line = (read-line stream nil nil)
        while line
        for (key val) = (split-with #\: line :limit 2)
        collect (cons key (string-left-trim '(#\Space) val))))

(defun parse-distinfo-file (file)
  (uiop:with-input-file (in file)
    (parse-distinfo-stream in)))

(defun parse-space-delimited-stream (stream &key (test #'identity))
  (loop for line = (read-line stream nil nil)
        while line
        for data = (split-with #\Space line)
        when (and (char/= (aref line 0) #\#)
                  (funcall test data))
        collect data))

(defun parse-space-delimited-file (file &key (test #'identity))
  (uiop:with-input-file (in file)
    (parse-space-delimited-stream in :test test)))

(defmacro with-quicklisp-home (qlhome &body body)
  `(progv (list (intern #.(string :*quicklisp-home*) :ql))
       (list ,qlhome)
     ,@body))
