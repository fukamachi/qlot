(defpackage #:qlot/server
  (:use #:cl)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
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
    (when (and (stringp url)
               (<= (length "qlot://localhost/")
                   (length url))
               (string= "qlot://localhost/" url :end2 (length "qlot://localhost/")))
      (let* ((path (subseq url (length "qlot://localhost/")))
             (file (merge-pathnames path destination)))
        (when (uiop:file-exists-p file)
          file)))))

(defmacro with-qlot-server ((qlfile &optional qlhome) &body body)
  (let ((g-qlfile (gensym "QLFILE"))
        (g-qlhome (gensym "QLHOME"))
        (fetch-scheme-functions (gensym "FETCH-SCHEME-FUNCTIONS"))
        (destination (gensym "DESTINATION")))
    `(let ((,g-qlfile ,qlfile)
           (,g-qlhome ,qlhome)
           (,fetch-scheme-functions (intern (string '#:*fetch-scheme-functions*) '#:ql-http)))
       (with-tmp-directory (,destination)
         ;; Run distify in another Lisp process
         (run-lisp (list
                     `(when ,,g-qlhome
                        (load (merge-pathnames #P"setup.lisp" ,,g-qlhome))
                        (setf (symbol-value (intern (string '#:*quicklisp-home*) '#:ql)) ,,g-qlhome))
                     `(uiop:symbol-call :qlot/distify :distify-qlfile ,,g-qlfile ,,destination))
                   :systems '("qlot/distify")
                   :source-registry (asdf:system-source-directory :qlot))
         (progv (list ,fetch-scheme-functions '*handler*)
             (list (cons '("qlot" . qlot-fetch)
                         (symbol-value ,fetch-scheme-functions))
                   (make-handler ,destination))
           ,@body)))))
