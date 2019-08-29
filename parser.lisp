(defpackage #:qlot/parser
  (:use #:cl)
  (:import-from #:qlot/source
                #:make-source
                #:source-defrost-args)
  (:import-from #:qlot/utils
                #:make-keyword
                #:split-with)
  (:export #:parse-qlfile
           #:parse-qlfile-lock))
(in-package #:qlot/parser)

(defun trim-comment (value)
  (check-type value string)
  (do ((i 0 (1+ i)))
      ((= i (length value)) value)
    (let ((char (aref value i)))
      (cond
        ((char= char #\\)
         ;; Skip the next char
         (incf i))
        ((or (char= char #\#)
             (char= char #\;))
         (return (subseq value 0 i)))))))

(defun parse-qlfile-line (line)
  (flet ((canonical-line (line)
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (trim-comment line))))
    (setf line (canonical-line line))
    (when (string= line "")
      (return-from parse-qlfile-line))

    (destructuring-bind (source-type &rest args)
        (split-with #\Space line)
      (cond
        ((string-equal source-type
                       "dist")
         (unless (= (length args) 2)
           (error "Distribution's definition should contain it's name and url, like that: dist ultralisp http://dist.ultralisp.org/"))
         ;; TODO
         (error "TODO: Implement a custom dist"))
        (t
         (apply #'make-source
                (make-keyword source-type)
                (mapcar (lambda (arg)
                          (if (char= (aref arg 0) #\:)
                              (make-keyword (subseq arg 1))
                              arg))
                        args)))))))

(defun parse-qlfile (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil nil)
          while line
          for source = (parse-qlfile-line line)
          when source
            collect source)))

(defun parse-qlfile-lock (file &key (test #'identity))
  (loop for (project-name . args) in (uiop:read-file-forms file)
        when (funcall test project-name)
        collect
        (let ((source (apply #'make-instance (getf args :class) (getf args :initargs))))
          (setf (source-defrost-args source)
                (loop for (k v) on args by #'cddr
                      unless (member k '(:class :initargs))
                        append (list k v)))
          source)))
