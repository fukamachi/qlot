(defpackage #:qlot/server
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-version
                #:source-initargs
                #:source-frozen-slots
                #:defrost-source)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:export #:with-qlot-server
           #:run-distify-source-process))
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

(defvar *system-quicklisp-home*)

(defun run-distify-source-process (source destination &key quicklisp-home distinfo-only)
  (let (#+quicklisp (ql:*quicklisp-home* *system-quicklisp-home*))
    (run-lisp (append
                (when quicklisp-home
                  (list `(let ((*error-output* (make-broadcast-stream)))
                           (load (merge-pathnames #P"setup.lisp" ,quicklisp-home)))))
                (list `(uiop:symbol-call :qlot/distify :distify
                                         ;; Call defrost-source to set '%version' from 'source-version'.
                                         (defrost-source
                                           (make-instance ',(type-of source)
                                                          ,@(source-initargs source)
                                                          ,@(and (slot-boundp source 'qlot/source/base::version)
                                                                 `(:version ,(source-version source)))
                                                          ,@(source-frozen-slots source)))
                                         ,destination
                                         :distinfo-only ,distinfo-only)))
              :systems '("qlot/distify")
              :source-registry (asdf:system-source-directory :qlot))))

(defmacro with-qlot-server ((source &optional qlhome destination) &body body)
  (let ((g-source (gensym "SOURCE"))
        (g-qlhome (gensym "QLHOME"))
        (fetch-scheme-functions (gensym "FETCH-SCHEME-FUNCTIONS"))
        (destination (or destination (gensym "DESTINATION"))))
    `(let ((,g-source ,source)
           (,g-qlhome ,qlhome)
           (*system-quicklisp-home* #+quicklisp ql:*quicklisp-home*
                                    #-quicklisp nil)
           (,fetch-scheme-functions (intern (string '#:*fetch-scheme-functions*) '#:ql-http)))
       (with-tmp-directory (,destination)
         ;; Run distify in another Lisp process
         (run-distify-source-process ,g-source ,destination
                                     :quicklisp-home ,g-qlhome
                                     :distinfo-only t)
         (progv (list ,fetch-scheme-functions '*handler*)
             (list (cons '("qlot" . qlot-fetch)
                         (symbol-value ,fetch-scheme-functions))
                   (make-handler ,destination))
           (with-quicklisp-home ,g-qlhome
             ,@body))))))
