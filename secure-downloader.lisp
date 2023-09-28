(defpackage #:qlot/secure-downloader
  (:use #:cl)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/logger
                #:progress)
  (:import-from #:qlot/utils
                #:https-of)
  (:import-from #:qlot/utils/shell
                #:launch-lisp)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:export #:with-secure-installer
           #:with-download-logs
           #:without-download-logs))
(in-package #:qlot/secure-downloader)

(defvar *install-process* nil)
(defvar *enable-logging* t)

(defun https-fetch (url file &rest args)
  (declare (ignore args))
  (assert *install-process*)
  (let ((url (https-of url)))
    (progress "Downloading ~S." url)
    (let ((stream (uiop:process-info-input *install-process*)))
      (format stream "~A~%~A~%" url file)
      (force-output stream))
    (let ((result (read-line (uiop:process-info-output *install-process*))))
      (progress "Downloaded ~S." url)
      (values t (probe-file result)))))

(defun launch-fetch-process ()
  (let ((url-var (intern (string :url) :cl-user))
        (file-var (intern (string :file) :cl-user)))
    (launch-lisp (append
                  (when (find :quicklisp *features*)
                    (list
                     `(let ((*error-output* (make-broadcast-stream)))
                        (load ,(merge-pathnames #P"setup.lisp"
                                                (symbol-value (intern (string '#:*quicklisp-home*) '#:ql)))))))
                  `((uiop:symbol-call :ql :quickload :qlot/utils/http :silent t)
                    (loop
                      (let ((,url-var (read-line))
                            (,file-var (read-line)))
                        (uiop:symbol-call :qlot/utils/http :fetch ,url-var
                                          ,file-var)
                        (format t "~A~%" ,file-var)
                        (force-output)))))
                 :without-quicklisp t)))

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
