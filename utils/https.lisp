(defpackage #:qlot/utils/https
  (:use #:cl)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/shell
                #:launch-lisp)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:export #:https-of
           #:http-fetch
           #:with-secure-installer))
(in-package #:qlot/utils/https)

(defvar *install-process* nil)

(defun https-of (url)
  (if (search "http://" url :end2 7)
      (format nil "https://~A" (subseq url 7))
      url))

(defun https-fetch (url file &rest args)
  (declare (ignore args))
  (assert *install-process*)
  (let ((url (https-of url)))
    (format t "~&Downloading '~A'..." url)
    (force-output)
    (let ((stream (uiop:process-info-input *install-process*)))
      (format stream "~A~%~A~%" url file)
      (force-output stream))
    (let ((result (read-line (uiop:process-info-output *install-process*))))
      (format t "~CDownloaded '~A'.   ~%" #\Return url)
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
                  `((uiop:symbol-call :ql :quickload :dexador :silent t)
                    (loop
                      (let ((,url-var (read-line))
                            (,file-var (read-line)))
                        (uiop:symbol-call :dexador :fetch ,url-var
                                          ,file-var
                                          :if-exists :supersede
                                          :proxy ,*proxy*)
                        (format t "~A~%" ,file-var)
                        (force-output)))))
                 :without-quicklisp t)))

(defmacro with-secure-installer (() &body body)
  `(let ((*install-process* (launch-fetch-process)))
     (unwind-protect
          (progv (list (intern #.(string :*fetch-scheme-functions*) '#:ql-http))
              (list
               (append `(("http" . https-fetch)
                         ("https" . https-fetch))
                       (symbol-value (intern #.(string :*fetch-scheme-functions*) '#:ql-http))))
            ,@body)
       (when (uiop:process-alive-p *install-process*)
         (uiop:terminate-process *install-process*)))))
