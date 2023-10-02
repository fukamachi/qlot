(defpackage #:qlot/local-init/https
  (:use #:cl))
(in-package #:qlot/local-init/https)

(defvar *fetch-script*
  (and (asdf:find-system :qlot nil)
       (uiop:native-namestring
        (asdf:system-relative-pathname :qlot #P"scripts/fetch.sh"))))

(defun https-of (url)
  (if (and (stringp url)
           (<= 7 (length url))
           (search "http://" url :end2 7))
      (format nil "https://~A" (subseq url 7))
      url))

(defmacro with-logging ((url file &key quietly) &body body)
  (let ((now (gensym "NOW")))
    `(let ((,now (get-internal-real-time)))
       (unless ,quietly
         (format t "~&; Fetching '~A'.~%" ,url))
       (prog1 (progn ,@body)
         (unless ,quietly
           (format t "~&; Done '~A' (~$KB) in ~A seconds.~%"
                   (file-namestring ,file)
                   (/ (ql-util:file-size ,file) 1024)
                   (coerce
                    (/ (- (get-internal-real-time) ,now)
                       internal-time-units-per-second)
                    'float)))))))

(defun run-fetch (url file &rest args &key quietly &allow-other-keys)
  (declare (ignore args))
  (let ((url (https-of url)))
    (uiop:run-program (list *fetch-script*
                            url
                            (uiop:native-namestring
                             (uiop:ensure-absolute-pathname file *default-pathname-defaults*)))
                      :output (and (not quietly)
                                   :interactive)
                      :error-output :interactive)))

(defun which (cmd)
  (handler-case
      (string-right-trim '(#\Newline)
                         (with-output-to-string (s)
                           (uiop:run-program `("which" ,cmd)
                                             :output s)))
    (uiop/run-program:subprocess-error ()
      nil)))

(defun curl-fetch (url file &rest args &key quietly &allow-other-keys)
  (declare (ignore args))
  (let ((url (https-of url)))
    (with-logging (url file :quietly quietly)
      (uiop:run-program (list "curl" "-sSL" url "-o" (uiop:native-namestring file))
                        :error-output :interactive))))

(defun wget-fetch (url file &rest args &key quietly &allow-other-keys)
  (declare (ignore args))
  (let ((url (https-of url)))
    (with-logging (url file :quietly quietly)
      (uiop:run-program (list "wget" url "-O" (uiop:native-namestring file))
                        :error-output :interactive))))

(defun add-to-fetch-scheme-functions ()
  (let* ((preference (uiop:getenv "QLOT_FETCH"))
         (fn (cond
               ((and (null preference)
                     *fetch-script*
                     (uiop:file-exists-p *fetch-script*)
                     (uiop:file-exists-p
                      (asdf:system-relative-pathname :qlot #P".qlot/setup.lisp")))
                'run-fetch)
               ((which "curl")
                'curl-fetch)
               ((which "wget")
                'wget-fetch))))
    (when fn
      (setf ql-http:*fetch-scheme-functions*
            (append `(("https" . ,fn)
                      ("http" . ,fn))
                    ql-http:*fetch-scheme-functions*)))))

(add-to-fetch-scheme-functions)
