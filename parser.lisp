(defpackage #:qlot/parser
  (:use #:cl)
  (:import-from #:qlot/source
                #:make-source
                #:source-project-name
                #:source-defrost-args
                #:defrost-source
                #:source=)
  (:import-from #:qlot/utils
                #:make-keyword
                #:split-with)
  (:export #:parse-qlfile
           #:parse-qlfile-lock
           #:read-qlfile-for-install))
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
            (if (source= source lock-source)
                (progn
                  (defrost-source lock-source)
                  lock-source)
                source)))))

(defun read-qlfile-for-install (qlfile &key ignore-lock projects)
  "Read 'qlfile' (or 'qlfile.lock' if exists) and return sources.
  This adds the latest 'quicklisp' dist implicitly if no 'quicklisp' project exists in the file.
  If :ignore-lock is T, read 'qlfile' even when 'qlfile.lock' exists.
  If :projects is specified, read only those projects from qlfile.lock."
  (format t "~&Reading '~A'...~%" qlfile)
  (let ((sources (parse-qlfile qlfile))
        (qlfile-lock (and (or (not ignore-lock) projects)
                          (uiop:file-exists-p
                            (make-pathname :defaults qlfile
                                           :name (file-namestring qlfile)
                                           :type "lock")))))
    (if qlfile-lock
        (merging-lock-sources sources
                              (parse-qlfile-lock qlfile-lock
                                                 :test (lambda (name)
                                                         (not (find name projects :test 'equal)))))
        (cons (make-source :ql :all :latest)
              sources))))
