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
    ((uiop:file-exists-p (merge-pathnames #P".bundle-libs/bundle.lisp" *qlot-source-directory*)))
    (t (error "Qlot isn't setup yet, and no Quicklisp found to setup"))))

(defun make-config ()
  `(:qlot-source-directory ,(uiop:native-namestring *qlot-source-directory*)
    :setup-file ,(uiop:native-namestring
                  (uiop:enough-pathname (setup-file-path) *qlot-source-directory*))))

(defun dump-qlot-config (&optional (stream *standard-output*))
  (let ((config (make-config)))
    (let ((*print-case* :downcase))
      (format stream
              "~&(~{~S ~S~^~% ~})~%"
              config))))

(defun load-qlot-config ()
  (when (find :quicklisp *features*)
    (let ((config-file (uiop:symbol-call '#:ql-setup '#:qmerge #P"qlot.conf")))
      (and (uiop:file-exists-p config-file)
           (uiop:read-file-form config-file)))))
