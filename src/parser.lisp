(defpackage #:qlot/parser
  (:use #:cl)
  (:import-from #:qlot/source
                #:make-source
                #:source-identifier
                #:source-dist-name
                #:source-defrost-args
                #:defrost-source
                #:source=)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors
                #:qlfile-parse-failed
                #:duplicate-project)
  (:import-from #:qlot/utils
                #:make-keyword)
  (:import-from #:qlot/utils/ql
                #:quicklisp-distinfo-url)
  (:export #:parse-qlfile
           #:parse-qlfile-lock
           #:parse-qlfile-line
           #:find-lock
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

(defun skip-while (s skip-chars)
  (loop
    (let ((char (read-char s nil nil)))
      (unless char
        (return))
      (unless (member char skip-chars)
        (return char)))))

(defun read-until (s allowed-chars)
  (loop for char = (read-char s nil nil)
        while (and char
                   (not (member char allowed-chars)))
        collect char into chars
        finally
           (when char
             (unread-char char s))
           (return (values (coerce chars 'string) char))))

(defun read-double-quoted (s)
  (prog1 (read-until s '(#\"))
    (read-char s)))

(defun read-list-elem (s)
  (with-output-to-string (res)
    (loop
      (multiple-value-bind (token last-char)
          (read-until s '(#\( #\)))
        (case last-char
          ('nil
           (return))
          (#\(
           (princ token res)
           (princ (read-char s) res)
           (princ (read-list-elem s) res))
          (#\)
           (format res "~A)" token)
           (read-char s)
           (return)))))))

(defun read-token (s)
  (let ((char (skip-while s '(#\Space #\Tab))))
    (when char
      (case char
        (#\( (format nil "(~A" (read-list-elem s)))
        (#\" (read-double-quoted s))
        (otherwise
         (format nil "~C~A" char
                 (read-until s '(#\Space #\Tab #\" #\())))))))

(defun parse-qlfile-line (line)
  (flet ((canonical-line (line)
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (trim-comment line))))
    (setf line (canonical-line line))
    (when (string= line "")
      (return-from parse-qlfile-line))

    (destructuring-bind (source-type &rest args)
        (with-input-from-string (s line)
          (loop for token = (read-token s)
                while token
                collect token))
      (apply #'make-source
             (make-keyword source-type)
             (mapcar (lambda (arg)
                       (if (char= (aref arg 0) #\:)
                           (make-keyword (subseq arg 1))
                           arg))
                     args)))))

(defun parse-qlfile (file)
  (with-open-file (in file)
    (loop for lineno from 1
          for line = (read-line in nil nil)
          while line
          for source = (handler-bind ((error
                                        (lambda (e)
                                          (error 'qlfile-parse-failed
                                                 :file file
                                                 :lineno lineno
                                                 :line line
                                                 :error e))))
                         (parse-qlfile-line line))
          when source
          collect
             (if (find (source-identifier source) sources
                       :test #'string=
                       :key #'source-identifier)
                 (error 'qlfile-parse-failed
                        :file file
                        :lineno lineno
                        :line line
                        :error
                        (make-condition 'duplicate-project
                                        :name (source-identifier source)))
                 source)
          into sources
          finally (return sources))))

(defmacro with-handling-parse-error ((file lineno) &body body)
  `(handler-bind ((error
                    (lambda (e)
                      (error 'qlfile-parse-failed
                             :lineno ,lineno
                             :file ,file
                             :error e))))
     ,@body))

(defun read-qlfile-lock (file)
  (with-handling-parse-error (file 1)   ;; Can't tell the right lineno
    (uiop:with-safe-io-syntax ()
      (uiop:read-file-forms file))))

(defun parse-qlfile-lock (file &key (test #'identity))
  (loop with lineno = 1
        for (project-name . args) in (read-qlfile-lock file)
        when (funcall test project-name)
        collect
        (let ((source (with-handling-parse-error (file lineno)
                        (apply #'make-instance (getf args :class)
                               :project-name project-name
                               (getf args :initargs)))))
          (setf (source-defrost-args source)
                (loop for (k v) on args by #'cddr
                      unless (member k '(:class :initargs))
                      append (list k v)))
          source)
        ;; XXX: It could be wrong if the lock file is modified manually
        do (incf lineno (1+ (/ (length args) 2)))))

(defun merging-lock-sources (sources lock-sources)
  (flet ((make-sources-map (sources)
           (let ((hash (make-hash-table :test 'equal)))
             (dolist (source sources)
               (setf (gethash (source-identifier source) hash) source))
             hash)))
    (let ((lock-sources-map (make-sources-map lock-sources)))
      (loop for source in sources
            for lock-source = (gethash (source-identifier source)
                                       lock-sources-map)
            collect
            (if (source= source lock-source)
                (progn
                  (defrost-source lock-source)
                  lock-source)
                source)))))

(defun find-lock (qlfile)
  (make-pathname :defaults qlfile
                 :name (file-namestring qlfile)
                 :type "lock"))

(defun read-qlfile-for-install (qlfile &key ignore-lock projects silent)
  "Read 'qlfile' (or 'qlfile.lock' if exists) and return sources.
  This adds the latest 'quicklisp' dist implicitly if no 'quicklisp' project exists in the file.
  If :ignore-lock is T, read 'qlfile' even when 'qlfile.lock' exists.
  If :projects is specified, read only those projects from qlfile.lock."
  (unless silent
    (message "Reading '~A'..." qlfile))
  (let ((default-ql-source (make-source :dist "quicklisp" (quicklisp-distinfo-url)))
        (sources (parse-qlfile qlfile))
        (qlfile-lock (and (or (not ignore-lock) projects)
                          (find-lock qlfile))))
    (unless (find "quicklisp" sources
                  :key #'source-dist-name
                  :test #'string=)
      (push default-ql-source sources))
    (if (uiop:file-exists-p qlfile-lock)
        (merging-lock-sources sources
                              (parse-qlfile-lock qlfile-lock
                                                 :test (lambda (name)
                                                         (not (find name projects :test 'equal)))))
        sources)))
