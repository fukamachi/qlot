(in-package :cl-user)
(defpackage qlot.http
  (:use :cl)
  (:import-from :uiop
                :file-exists-p)
  (:import-from :alexandria
                :with-gensyms
                :once-only
                :copy-stream)
  (:export :http-request
           :safety-http-request
           :http-request-failed
           :download-file
           :retry-download))
(in-package :qlot.http)

(define-condition http-request-failed (simple-error)
  ((status :initarg :status :type integer)
   (url :initarg :url :type string))
  (:report
   (lambda (condition stream)
     (format stream "HTTP Request to ~A has failed (Code=~D)."
             (slot-value condition 'url)
             (slot-value condition 'status)))))

(defun http-request (url &rest args)
  (apply #'drakma:http-request
         url
         (append args
                 (list :user-agent (format nil "qlot/~A"
                                           (asdf::component-version (asdf:find-system :qlot)))))))

(defun safety-http-request (url &rest args)
  (multiple-value-bind (body status)
      (apply #'http-request url args)
    (unless (= status 200)
      (error 'http-request-failed
             :url url
             :status status))

    body))

(defun download-file (url output &key (element-type '(unsigned-byte 8)))
  (format t "~&Downloading ~S to ~S...~%" url output)
  (let (stream)
    (tagbody downloading
       (restart-case (setf stream (safety-http-request url :want-stream t))
         (retry-download ()
           :report "Retry to download."
           (when (uiop:file-exists-p output)
             (delete-file output))
           (go downloading))))
    (with-open-file (out output
                         :direction :output :if-exists :supersede
                         :element-type element-type)
      (alexandria:copy-stream stream out
                              :element-type element-type))))
