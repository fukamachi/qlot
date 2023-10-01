(defpackage #:qlot/local-init/https
  (:use #:cl))
(in-package #:qlot/local-init/https)

(defvar *fetch-script*
  (uiop:native-namestring
   (asdf:system-relative-pathname :qlot #P"scripts/fetch.sh")))

(defun https-of (url)
  (if (and (stringp url)
           (<= 7 (length url))
           (search "http://" url :end2 7))
      (format nil "https://~A" (subseq url 7))
      url))

(defun run-fetch (url file)
  (uiop:run-program (list *fetch-script*
                          (https-of url)
                          (uiop:native-namestring
                           (uiop:ensure-absolute-pathname file *default-pathname-defaults*)))
                    :output :interactive
                    :error-output :interactive))

(defun add-to-fetch-scheme-functions ()
  (when (and (uiop:file-exists-p *fetch-script*)
             (uiop:file-exists-p
              (asdf:system-relative-pathname :qlot #P".qlot/setup.lisp")))
    (setf ql-http:*fetch-scheme-functions*
          (append '(("https" . run-fetch)
                    ("http" . run-fetch))
                  ql-http:*fetch-scheme-functions*))))

(add-to-fetch-scheme-functions)
