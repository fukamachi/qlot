(defpackage #:qlot/parser
  (:use #:cl)
  (:import-from #:qlot/source
                #:make-source
                #:find-source-class
                #:defrost-source
                #:source-project-name
                #:source-version
                #:source-dist-name
                #:source-defrost-args
                #:source-equal)
  (:import-from #:qlot/source/ql
                #:source-ql)
  (:import-from #:qlot/error
                #:qlot-qlfile-error)
  (:import-from #:cl-ppcre)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:uiop
                #:file-exists-p)
  (:import-from #:alexandria
                #:delete-from-plist)
  (:export #:parse-qlfile
           #:parse-qlfile-lock
           #:prepare-qlfile))
(in-package #:qlot/parser)

(defun parse-qlfile-line (line)
  (labels ((trim-comment (line)
             (ppcre:regex-replace "(?<!\\\\)[#|;].*" line ""))
           (canonical-line (line)
             (string-trim '(#\Space #\Tab #\Newline #\Return)
                          (trim-comment line))))
    (setf line (canonical-line line))
    (when (string= line "")
      (return-from parse-qlfile-line))
    (destructuring-bind (source-type &rest args)
        (split-sequence #\Space line :remove-empty-subseqs t)
      (apply #'make-source
             (handler-case
                 (find-source-class source-type)
               (error (e)
                 (error 'qlot-qlfile-error
                        :format-control "~A"
                        :format-arguments (list e))))
             (mapcar (lambda (arg)
                       (if (char= (aref arg 0) #\:)
                           (intern (string-upcase (subseq arg 1)) :keyword)
                           arg))
                     args)))))

(defun parse-qlfile (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil nil)
          while line
          for source = (handler-bind ((error
                                        (lambda (e)
                                          (error 'qlot-qlfile-error
                                                 :format-control "Error while parsing qlfile: ~A~%  at ~S~2%  ~A"
                                                 :format-arguments (list file line e)))))
                         (parse-qlfile-line line))
          when source
            collect source)))

(defun parse-qlfile-lock (file)
  (loop for (project-name . args) in (handler-case (uiop:read-file-forms file)
                                       (package-error (e)
                                         (let ((system-name
                                                 (string-downcase
                                                  (substitute #\/ #\. (package-error-package e)))))
                                           #+quicklisp (ql:quickload system-name :silent t)
                                           #-quicklisp (asdf:load-system system-name)
                                           (uiop:read-file-forms file))))
        for source = (apply #'make-instance (getf args :class) (getf args :initargs))
        do (setf (source-defrost-args source)
                 (delete-from-plist args :class :initargs))
        collect source))

(defun merging-lock-sources (sources lock-sources)
  (flet ((make-sources-map (sources)
           (let ((hash (make-hash-table :test 'equal)))
             (dolist (source sources)
               (setf (gethash (source-project-name source) hash) source))
             hash)))
    (let ((lock-sources-map (make-sources-map lock-sources)))
      (loop for source in sources
            for lock-source = (gethash (source-project-name source)
                                       lock-sources-map)
            collect
            (if (source-equal source lock-source)
                (progn
                  (defrost-source lock-source)
                  lock-source)
                source)))))

(defun prepare-qlfile (file &key ignore-lock)
  (format t "~&Reading '~A'...~%" file)
  (let ((default-ql-source (make-source 'source-ql :all :latest))
        (lock-file (and (not ignore-lock)
                        (uiop:file-exists-p
                         (make-pathname :defaults file
                                        :name (file-namestring file)
                                        :type "lock"))))
        (sources (parse-qlfile file)))
    (unless (find "quicklisp" sources
                  :key #'source-dist-name
                  :test #'string=)
      (push default-ql-source sources))
    (when lock-file
      (setf sources
            (merging-lock-sources sources
                                  (parse-qlfile-lock lock-file))))
    sources))
