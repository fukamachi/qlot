(defpackage #:qlot/server
  (:use #:cl)
  (:import-from #:qlot/source
                #:*dist-base-url*
                #:prepare
                #:source-prepared
                #:url-path-for
                #:project.txt
                #:distinfo.txt
                #:releases.txt
                #:systems.txt
                #:archive)
  (:import-from #:qlot/parser
                #:prepare-qlfile)
  (:import-from #:qlot/tmp
                #:*tmp-directory*)
  (:import-from #:qlot/util
                #:with-quicklisp-home)
  (:import-from #:clack
                #:clackup
                #:stop)
  (:import-from #:usocket)
  (:import-from #:alexandria
                #:when-let)
  (:export #:localhost
           #:start-server
           #:stop-server))
(in-package #:qlot/server)

(defvar *handler* nil)

(defvar *qlot-port* nil)

(defun localhost (&optional (path ""))
  ;; Use PATH If PATH is an URL, not an URL path.
  (when (and (< 0 (length path))
             (not (char= (aref path 0) #\/)))
    (return-from localhost path))

  (unless *qlot-port*
    (error "~S is not set." '*qlot-port*))
  (format nil "http://127.0.0.1:~D~A"
          *qlot-port*
          path))

(defun port-available-p (port)
  (handler-case (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))

(defun make-app (sources)
  (flet ((make-route (source action)
           (let ((action-name (symbol-name action)))
             (lambda ()
               (let* ((*dist-base-url* (localhost))
                      (res (funcall (symbol-function action) source)))
                 (list 200
                       (if (string-equal (subseq action-name (- (length action-name) 4))
                                         ".txt")
                           (list :content-type "text/plain")
                           '())
                       (if (stringp res)
                           (list res)
                           res)))))))
    (let ((route (make-hash-table :test 'equal))
          (tmp-directory *tmp-directory*))
      (dolist (source sources)
        (setf (gethash (url-path-for source 'project.txt) route)
              (lambda ()
                (let ((*tmp-directory* tmp-directory))
                  (prepare source))
                (dolist (action '(project.txt distinfo.txt releases.txt systems.txt archive))
                  (when-let (path (url-path-for source action))
                    (setf (gethash path route)
                          (make-route source action))))
                (funcall (make-route source 'project.txt)))))
      (let ((qlhome #+quicklisp ql:*quicklisp-home*))
        (lambda (env)
          (with-quicklisp-home qlhome
            (let ((fn (gethash (getf env :path-info) route))
                  (*tmp-directory* tmp-directory))
              (if fn
                  (funcall fn)
                  '(404 (:content-type "text/plain") ("Not Found"))))))))))

(defgeneric start-server (sources)
  (:method ((sources list))
    (when *handler*
      (stop-server))

    (let ((port (random-port)))
      (prog1
          (setf *handler*
                (let ((app (make-app sources)))
                  (clackup app :port port :silent t)))
        (setf *qlot-port* port)
        (sleep 0.5))))
  (:method ((qlfile pathname))
    (start-server (prepare-qlfile qlfile))))

(defun stop-server ()
  (when *handler*
    (stop *handler*)
    (setf *handler* nil
          *qlot-port* nil)))
