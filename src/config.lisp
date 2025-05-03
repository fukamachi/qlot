(defpackage #:qlot/config
  (:use #:cl)
  (:import-from #:qlot/utils/shell
                #:*qlot-source-directory*)
  (:export #:dump-qlot-config
           #:load-qlot-config
           #:make-config))
(in-package #:qlot/config)

(defun setup-file-path ()
  (cond
    ((find :quicklisp *features*)
     (uiop:symbol-call '#:ql-setup '#:qmerge #P"setup.lisp"))
    ((uiop:file-exists-p (merge-pathnames #P".bundle-libs/setup.lisp" *qlot-source-directory*)))
    ((uiop:file-exists-p (merge-pathnames #P".bundle-libs/bundle.lisp" *qlot-source-directory*)))
    (t nil)))

(defun make-config ()
  (let ((setup-file (setup-file-path)))
    `(:qlot-source-directory ,(uiop:native-namestring *qlot-source-directory*)
      :qlot-version ,(asdf:component-version (asdf:find-system '#:qlot))
      ,@(and setup-file
             `(:setup-file ,(uiop:native-namestring
                             (uiop:enough-pathname setup-file *qlot-source-directory*)))))))

(defun dump-qlot-config (&optional (stream *standard-output*))
  (let ((config (make-config)))
    (let ((*print-case* :downcase))
      (format stream
              "~&(~{~S ~S~^~% ~})~%"
              config))))

(defun load-qlot-config (directory)
  (let ((config-file (merge-pathnames #P"qlot.conf" directory)))
    (and (uiop:file-exists-p config-file)
         (uiop:read-file-form config-file))))
