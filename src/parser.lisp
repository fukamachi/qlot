(in-package :cl-user)
(defpackage qlot.parser
  (:use :cl
        :iterate)
  (:import-from :qlot.source
                :make-source
                :find-source-class
                :prepare
                :defrost-source
                :source-project-name
                :source-version
                :source-dist-name
                :source-defrost-args
                :source-equal)
  (:import-from :qlot.source.ql
                :source-ql)
  (:import-from :qlot.error
                :qlot-qlfile-error)
  (:import-from :alexandria
                :delete-from-plist)
  (:export :parse-qlfile
           :parse-qlfile-lock
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

(defun parse-qlfile-lock (file)
  (iter (for (project-name . args) in-file file)
    (let ((source (apply #'make-instance (getf args :class) (getf args :initargs))))
      (setf (source-defrost-args source)
            (delete-from-plist args :class :initargs))
      (collect source))))

(defun merging-lock-sources (sources lock-sources)
  (flet ((make-sources-map (sources)
           (let ((hash (make-hash-table :test 'equal)))
             (iter (for source in sources)
               (setf (gethash (source-project-name source) hash) source))
             hash)))
    (let ((lock-sources-map (make-sources-map lock-sources)))
      (iter (for source in sources)
        (for lock-source = (gethash (source-project-name source)
                                    lock-sources-map))
        (collect
            (if (source-equal source lock-source)
                (progn
                  (defrost-source lock-source)
                  lock-source)
                (progn
                  (prepare source)
                  source)))))))

(defun prepare-qlfile (file &key ignore-lock)
  (let ((default-ql-source (make-source 'source-ql :all :latest))
        (lock-file (and (not ignore-lock)
                        (probe-file
                         (make-pathname :defaults file
                                        :name (file-namestring file)
                                        :type "lock"))))
        (sources (parse-qlfile file)))
    (unless (find "quicklisp" sources
                  :key #'source-dist-name
                  :test #'string=)
      (push default-ql-source sources))
    (setf sources
          (if lock-file
              (merging-lock-sources sources
                                    (parse-qlfile-lock lock-file))
              (mapc #'prepare sources)))
    sources))
