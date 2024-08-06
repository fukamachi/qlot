(defpackage #:qlot/utils/qlot
  (:use #:cl)
  (:import-from #:qlot/source
                #:source=
                #:source-local
                #:source-project-name
                #:source-local-path
                #:source-local-registry-directive
                #:freeze-source)
  (:import-from #:qlot/logger
                #:message
                #:warn-message)
  (:export #:dump-source-registry-conf
           #:dump-qlfile-lock))
(in-package #:qlot/utils/qlot)

(defvar *source-registry-exclusions*
  '(".qlot" ".bundle-libs"))

(defun dump-source-registry-conf (stream sources)
  (let ((*print-pretty* nil)
        (*print-case* :downcase))
    (format stream
            "~&(~{~S~^~% ~})~%"
            `(:source-registry
              :ignore-inherited-configuration
              ,@(loop for exclude in *source-registry-exclusions*
                      collect `(:also-exclude ,exclude))
              (:directory ,(asdf:system-source-directory :qlot))
              ,@(loop for source in sources
                      when (typep source 'source-local)
                      collect (progn
                                (message "Adding ~S located at '~A'."
                                         (source-project-name source)
                                         (source-local-path source))
                                (unless (uiop:directory-exists-p (source-local-path source))
                                  (warn-message "Directory '~A' does not exist."
                                                (source-local-path source)))
                                `(:tree ,(source-local-registry-directive source))))))))

(defun dump-qlfile-lock (file sources)
  (uiop:with-output-file (out file :if-exists :supersede)
    (let ((*print-pretty* nil)
          (*print-case* :downcase))
      (loop for source in sources
            for (project-name . contents) = (freeze-source source)
            do (format out "~&(~S .~% (~{~S ~S~^~%  ~}))~%" project-name contents)))))
