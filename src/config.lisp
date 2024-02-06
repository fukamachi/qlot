(defpackage #:qlot/config
  (:use #:cl)
  (:export #:dump-qlot-config))
(in-package #:qlot/config)

(defun make-config ()
  (let ((qlot-home (asdf:system-source-directory :qlot)))
    `(:qlot-home ,(uiop:native-namestring qlot-home)
      :setup-file ,(uiop:native-namestring
                    (uiop:enough-pathname
                     (cond
                       ((find :quicklisp *features*)
                        (uiop:symbol-call '#:ql '#:qmerge #P"setup.lisp"))
                       ((uiop:file-exists-p (asdf:system-relative-pathname '#:qlot #P".bundle-libs/bundle.lisp")))
                       (t (error "Qlot isn't setup yet")))
                     qlot-home)))))

(defun dump-qlot-config (&optional (stream *standard-output*))
  (let ((config (make-config)))
    (let ((*print-case* :downcase))
      (format stream
              "~&(~{~S ~S~^~% ~})~%"
              config))))
