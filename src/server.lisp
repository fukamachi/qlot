(defpackage #:qlot/server
  (:use #:cl)
  (:import-from #:qlot/distify
                #:distify)
  (:import-from #:qlot/logger
                #:*enable-whisper*)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:export #:with-qlot-server))
(in-package #:qlot/server)

(defvar *handler*)

(defun qlot-fetch (url file &key follow-redirects quietly maximum-redirects)
  (declare (ignore follow-redirects quietly maximum-redirects))
  (let ((result (funcall *handler* url)))
    (format *error-output* "~&[DEBUG qlot-fetch] url=~A result=~A~%" url result)
    (etypecase result
      (pathname
       (let ((src-size (with-open-file (in result :element-type '(unsigned-byte 8)) (file-length in))))
         (uiop:copy-file result file)
         (let ((dst-size (with-open-file (in file :element-type '(unsigned-byte 8)) (file-length in))))
           (format *error-output* "~&[DEBUG qlot-fetch] copied ~A -> ~A (src=~A bytes, dst=~A bytes)~%"
                   result file src-size dst-size))))
      (null
       (format *error-output* "~&[DEBUG qlot-fetch] handler returned NIL for ~A~%" url))))
  (values (make-instance (intern (string '#:header) '#:ql-http) :status 200)
          (probe-file file)))

(defun make-handler (destination)
  (lambda (url)
    (when (and (stringp url)
               (<= (length "qlot://localhost/")
                   (length url))
               (string= "qlot://localhost/" url :end2 (length "qlot://localhost/")))
      (let* ((path (subseq url (length "qlot://localhost/")))
             (file (merge-pathnames path destination)))
        (when (uiop:file-exists-p file)
          file)))))

(defmacro with-qlot-server ((source &key destination distinfo-only quicklisp-home silent) &body body)
  (declare (ignore quicklisp-home))
  (let ((g-source (gensym "SOURCE"))
        (fetch-scheme-functions (gensym "FETCH-SCHEME-FUNCTIONS"))
        (g-destination (gensym "DESTINATION")))
    `(let ((,g-source ,source)
           (,fetch-scheme-functions (intern (string '#:*fetch-scheme-functions*) '#:ql-http)))
       (,@(if destination
              `(let ((,g-destination ,destination)))
              `(with-tmp-directory (,g-destination)))
        (let ((*enable-whisper* (not ,silent)))
          (distify ,g-source ,g-destination :distinfo-only ,distinfo-only))
        (progv (list ,fetch-scheme-functions '*handler*)
            (list (cons '("qlot" . qlot-fetch)
                        (symbol-value ,fetch-scheme-functions))
                  (make-handler ,g-destination))
          ,@body)))))
