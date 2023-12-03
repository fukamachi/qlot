(defpackage #:qlot/source/ql
  (:nicknames #:qlot.source.ql)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/source/dist
                #:source-dist
                #:source-dist-project
                #:source-distribution)
  (:import-from #:qlot/source/git
                #:source-git
                #:source-git-remote-url)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:import-from #:qlot/utils/ql
                #:quicklisp-distinfo-url)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:export #:source-ql
           #:source-ql-all
           #:source-ql-upstream))
(in-package #:qlot/source/ql)

(defclass source-ql (source-dist-project)
  ())

(defclass source-ql-upstream (source-git)
  ())

(defmethod initialize-instance ((source source-ql) &rest initargs &key distribution)
  ;; Just to ignore :distribution
  (declare (ignore initargs distribution))
  (call-next-method))

(defmethod source-distribution ((source source-ql))
  (quicklisp-distinfo-url))

(defclass source-ql-all (source-dist)
  ()
  (:default-initargs
    :distribution (quicklisp-distinfo-url)))

;; For backward-compatibility
;; source-ql-dist will be replaced by source-dist
(defmethod initialize-instance :around ((source source-ql-all) &rest initargs)
  (let* ((source (apply #'call-next-method source initargs))
         (source (apply #'change-class source 'source-dist initargs)))
    (setf (slot-value source 'qlot/source/base::initargs) initargs)
    source))

(defmethod make-source ((source (eql :ql)) &rest args)
  (handler-case
      (destructuring-bind (project-name &rest initargs) args
        (check-type project-name (or string (eql :all)))
        ;; Assuming :latest if there's no arguments
        (let ((initargs (or initargs '(:latest))))
          (destructuring-bind (version &key distribution) initargs
            (check-type version (or string (member :latest :upstream)))
            (when (and (eq project-name :all)
                       (eq version :upstream))
              (error "Can't specify :upstream for ql :all."))

            (let ((distribution (or distribution
                                    (quicklisp-distinfo-url))))
              (cond
                ((eq project-name :all)
                 (make-instance 'source-dist
                                :project-name "quicklisp"
                                :distribution distribution
                                :%version version))
                ((eq version :upstream)
                 (make-instance 'source-ql-upstream
                                :project-name project-name))
                (t
                 (make-instance 'source-ql
                                :project-name project-name
                                :%version version)))))))
    (error (e)
      (error 'invalid-definition
             :source :ql
             :usage "ql <project name> [<version>]"
             :reason (princ-to-string e)))))

(defmethod freeze-source :before ((source source-ql-upstream))
  (unless (source-git-remote-url source)
    (setf (source-git-remote-url source)
          (run-lisp `((write-string
                       (uiop:symbol-call :qlot/utils/quickdocs :project-upstream-url
                                         ,(source-project-name source))))
                    :systems '("qlot/utils/quickdocs")
                    :source-registry (asdf:system-source-directory :qlot)
                    :output :string))))

(defmethod source-frozen-slots ((source source-ql-upstream))
  (append (call-next-method)
          (when (source-git-remote-url source)
            `(:remote-url ,(source-git-remote-url source)))))
