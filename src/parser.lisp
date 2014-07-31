(in-package :cl-user)
(defpackage qlot.parser
  (:use :cl
        :iterate)
  (:import-from :qlot.source
                :make-source
                :prepare
                :find-source-class
                :source-project-name
                :source-direct-dependencies)
  (:import-from :qlot.error
                :qlot-qlfile-error)
  (:export :parse-qlfile
           :prepare-qlfile))
(in-package :qlot.parser)

(defun parse-qlfile-line (line)
  (labels ((trim-comment (line)
             (ppcre:regex-replace "(?<!\\\\)#.*" line ""))
           (canonical-line (line)
             (string-trim '(#\Space #\Tab)
                          (trim-comment line))))
    (setf line (canonical-line line))
    (when (string= line "")
      (return-from parse-qlfile-line))
    (destructuring-bind (source-type &rest args)
        (ppcre:split "\\s+" line)
      (let ((package-name (format nil "~A.~:@(~A~)" #.(string :qlot.source) source-type)))
        (unless (find-package package-name)
          (error 'qlot-qlfile-error
                 :format-control "Invalid source type: ~S~2%  ~A"
                 :format-arguments (list source-type line)))
        (apply #'make-source
               (find-source-class source-type)
               (mapcar (lambda (arg)
                         (if (char= (aref arg 0) #\:)
                             (intern (string-upcase (subseq arg 1)) :keyword)
                             arg))
                       args))))))

(defun parse-qlfile (file)
  (with-open-file (in file)
    (iter (for line = (read-line in nil nil))
      (while line)
      (for source = (handler-bind ((error
                                     (lambda (e)
                                       (error 'qlot-qlfile-error
                                              :format-control "Error while parsing qlfile: ~A~%  at ~S~2%  ~A"
                                              :format-arguments (list file line e)))))
                      (parse-qlfile-line line)))
      (when source
        (collect source)))))

(defparameter *prepared-sources* nil)

(defmacro with-prepared-transaction (&body body)
  `(let ((*prepared-sources* (make-hash-table :test 'equal)))
     ,@body))

(defun prepare-qlfile (file)
  (labels ((prepare-source (source)
             (when (gethash (source-project-name source) *prepared-sources*)
               (warn "Project named '~S' is already prepared. Ignored."
                     (source-project-name source))
               (return-from prepare-source '()))

             (prepare source)
             (setf (gethash (source-project-name source) *prepared-sources*) t)

             (append (iter (for dep in (reverse (source-direct-dependencies source)))
                       (appending (prepare-source dep)))
                     (list source))))
    (with-prepared-transaction
      (iter (for source in (parse-qlfile file))
        (appending (prepare-source source))))))
