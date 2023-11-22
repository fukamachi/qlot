(defpackage #:qlot/secure-downloader
  (:use #:cl)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/logger
                #:progress)
  (:import-from #:qlot/utils
                #:https-of)
  (:import-from #:qlot/utils/shell
                #:*qlot-source-directory*
                #:launch-lisp)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:export #:with-secure-installer
           #:with-download-logs
           #:without-download-logs))
(in-package #:qlot/secure-downloader)

(defvar *install-process* nil)
(defvar *enable-logging* t)

(defun check-install-process ()
  (assert *install-process*)
  (unless (uiop:process-alive-p *install-process*)
    (let ((error-log
            (uiop:with-temporary-file (:pathname error-log
                                       :stream errout
                                       :direction :output
                                       :type "log"
                                       :prefix "qlot-install-"
                                       :suffix ""
                                       :keep t)
              (princ
               (uiop:slurp-stream-string
                (uiop:process-info-error-output *install-process*))
               errout)
              error-log)))
      (error 'qlot-simple-error
             :format-control "Qlot secure downloader was unexpectedly terminated.~%See error logs at '~A'."
             :format-arguments (list error-log)))))

(defun https-fetch (url file &rest args)
  (declare (ignore args))
  (check-install-process)
  (let ((url (https-of url)))
    (progress "Downloading ~S." url)
    (let ((stream (uiop:process-info-input *install-process*)))
      (format stream "~A~%~A~%" url file)
      (force-output stream))
    (let ((result (read-line (uiop:process-info-output *install-process*) nil nil)))
      (if result
          (progn
            (progress "Downloaded ~S." url)
            (values t (probe-file result)))
          (progn
            (progress "Failed to download ~S." url)
            (values nil nil))))))

(defun launch-fetch-process ()
  (let ((url-var (intern (string :url) :cl-user))
        (file-var (intern (string :file) :cl-user)))
    (launch-lisp `((loop
                     (let ((,url-var (read-line))
                           (,file-var (read-line)))
                       (uiop:symbol-call :qlot/http :fetch ,url-var
                                         ,file-var)
                       (format t "~A~%" ,file-var)
                       (force-output))))

                 :load (or (probe-file (asdf:system-relative-pathname :qlot #P".bundle-libs/bundle.lisp"))
                           #+quicklisp (merge-pathnames #P"setup.lisp" ql:*quicklisp-home*))
                 :systems '("qlot/http")
                 :source-registry (or *qlot-source-directory*
                                      (asdf:system-source-directory :qlot)))))

(defmacro with-download-logs (&body body)
  `(let ((*enable-logging* t)) ,@body))

(defmacro without-download-logs (&body body)
  `(let ((*enable-logging* nil)) ,@body))

(defmacro with-secure-installer ((&key no-logs) &body body)
  `(let ((*install-process* (launch-fetch-process))
         (*enable-logging* (not ,no-logs)))
     (unwind-protect
          (progv (list (intern #.(string '#:*fetch-scheme-functions*) '#:ql-http)
                       (intern #.(string '#:*proxy-url*) '#:ql-http))
              (list
               (append `(("http" . https-fetch)
                         ("https" . https-fetch))
                       (symbol-value (intern #.(string '#:*fetch-scheme-functions*) '#:ql-http)))
               *proxy*)
            ,@body)
       (when (uiop:process-alive-p *install-process*)
         (uiop:terminate-process *install-process*)))))
