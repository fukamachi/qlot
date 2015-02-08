(in-package :cl-user)
(defpackage qlot.parser
  (:use :cl
        :iterate)
  (:import-from :qlot.source
                :make-source
                :find-source-class
                :defrost-source
                :prepare
                :source-project-name
                :source-version
                :source-dist-name
                :source-direct-dependencies
                :source-defrost-args
                :source-equal
                :source-compatible)
  (:import-from :qlot.source.ql
                :source-ql)
  (:import-from :qlot.error
                :qlot-qlfile-error
                :qlot-sources-incompatible)
  (:import-from :fad
                :file-exists-p)
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
                source))))))

(defun merge-dependency-sources (sources dependency-sources)
  (let ((new-sources nil))
    (iter (for dep-source in dependency-sources)
          (for orig-source = (find (source-dist-name dep-source) sources
                                   :key #'source-dist-name
                                   :test #'string=))
          (if orig-source
              (unless (source-compatible orig-source dep-source)
                (error 'qlot-sources-incompatible
                       :format-control "Sources ~A and ~A are not compatible."
                       :format-arguments (list orig-source dep-source)))
              (push dep-source new-sources)))
    (values (append new-sources sources) new-sources)))

(defun prepare-dependency-qlfiles (all-sources dependencies)
  ;; First, prepare all the dependencies.
  (mapc #'prepare all-sources)
  ;; Now walk all the dependencies, merging their dependencies into new-sources.
  (iter (for dependency in dependencies)
        (for direct-dependencies = (source-direct-dependencies dependency))
        (for (values merged-sources new-dependencies)
             = (merge-dependency-sources all-sources direct-dependencies))
        (setf all-sources merged-sources)
        (appending new-dependencies into new-sources)
        (finally (return (values all-sources new-sources)))))

(defun prepare-qlfile (file &key ignore-lock)
  (let ((default-ql-source (make-source 'source-ql :all :latest))
        (lock-file (and (not ignore-lock)
                        (fad:file-exists-p
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
    (iter (for (values all-sources new-sources)
               first (prepare-dependency-qlfiles sources sources)
               then (prepare-dependency-qlfiles all-sources new-sources))
          (while new-sources)
          (finally (return all-sources)))))
