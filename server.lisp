(defpackage #:qlot/server
  (:use #:cl)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:export #:with-qlot-server))
(in-package #:qlot/server)

(defvar *handler*)

(defun qlot-fetch (url file &key follow-redirects quietly maximum-redirects)
  (declare (ignore follow-redirects quietly maximum-redirects))
  (let ((result (funcall *handler* url)))
    (etypecase result
      (pathname (uiop:copy-file result file))
      (null)))
  (values (make-instance (intern (string '#:header) '#:ql-http) :status 200)
          (probe-file file)))

(defun make-handler (destination)
  (lambda (url)
    (when (and (<= (length "qlot://localhost/")
                   (length url))
               (string= "qlot://localhost/" url :end2 (length "qlot://localhost/")))
      (let* ((path (subseq url (length "qlot://localhost/")))
             (file (merge-pathnames path destination)))
        (probe-file file)))))

(defmacro with-qlot-server (qlfile &body body)
  (let ((g-qlfile (gensym "QLFILE"))
        (fetch-scheme-functions (gensym "FETCH-SCHEME-FUNCTIONS"))
        (destination (gensym "DESTINATION")))
    `(let* ((,g-qlfile ,qlfile)
            (,fetch-scheme-functions (intern (string '#:*fetch-scheme-functions*) '#:ql-http))
            (,destination (tmp-directory)))
       ;; Run distify in another Lisp process
       (run-lisp (list
                   `(uiop:symbol-call :qlot/distify :distify-qlfile ,,g-qlfile ,,destination))
                 :systems '("qlot/distify")
                 :source-registry (asdf:system-source-directory :qlot))
       (progv (list ,fetch-scheme-functions '*handler*)
           (list (cons '("qlot" . qlot-fetch)
                       (symbol-value ,fetch-scheme-functions))
                 (make-handler ,destination))
         ,@body))))
