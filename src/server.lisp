(in-package :cl-user)
(defpackage qlot.server
  (:use :cl)
  (:import-from :qlot.source
                :*qlot-port*
                :initialize
                :url-path-for
                :project.txt
                :distinfo.txt
                :releases.txt
                :systems.txt
                :archive)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :clack.response
                :headers)
  (:import-from :ningle
                :<app>
                :route
                :not-found
                :*response*)
  (:import-from :alexandria
                :when-let)
  (:export :*qlot-port*
           :localhost
           :start-server
           :stop-server))
(in-package :qlot.server)

(defvar *handler* nil)

(defun localhost (path)
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
  (map nil #'initialize sources)
  (let ((app (make-instance '<app>)))
    (dolist (source sources)
      (dolist (action '(project.txt distinfo.txt releases.txt systems.txt archive))
        (when-let ((path (url-path-for source action)))
          (setf (route app path)
                (lambda (params)
                  (declare (ignore params))
                  (let ((action-name (symbol-name action)))
                    (when (string-equal (subseq action-name (- (length action-name) 4))
                                        ".txt")
                      (setf (headers *response* :content-type) "text/plain")))
                  (funcall (symbol-function action) source))))))
    app))

(defun start-server (sources)
  (when *handler*
    (stop-server))

  (let ((port (random-port)))
    (prog1
        (setf *handler*
              (let ((*standard-output* (make-broadcast-stream)))
                (clackup (make-app sources) :port port)))
      (setf *qlot-port* port))))

(defun stop-server ()
  (when *handler*
    (stop *handler*)
    (setf *handler* nil
          *qlot-port* nil)))
