(defpackage #:qlot/utils/http
  (:use #:cl
        #+sbcl #:sb-bsd-sockets)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:export #:http-fetch))
(in-package #:qlot/utils/http)

(define-condition invalid-url (error)
  ((url :initarg :url)))

(define-condition not-supported-url (invalid-url) ())

(define-condition http-parse-error (error) ())

(define-condition invalid-http-header (http-parse-error)
  ((reason :initarg :reason))
  (:report (lambda (condition stream)
             (format stream "Invalid HTTP Header: ~A" (slot-value condition 'reason)))))

(define-condition http-request-failed (error)
  ((status :initarg :status))
  (:report (lambda (condition stream)
             (format stream "HTTP request failed with the status ~A"
                     (slot-value condition 'status)))))

#+sbcl
(defun host-to-address (host)
  (check-type host string)
  (host-ent-address (get-host-by-name host)))

(defun parse-http-url (url)
  (check-type url string)
  (let ((len (length url))
        host port path)
    (unless (and (< (length "http://") len)
                 (search "http://" url :end2 7))
      (error 'not-supported-url :url url))
    (let ((/-pos (position #\/ url :start 8)))
      (if /-pos
          (setf host (subseq url 7 /-pos)
                path (subseq url /-pos (position #\# url :start /-pos)))
          (setf host (subseq url 7)
                path "/")))
    (let ((non-digit-pos (position-if (complement #'digit-char-p) host
                                      :from-end t)))
      (when (and non-digit-pos
                 (char= (aref host non-digit-pos) #\:)
                 (/= non-digit-pos (1- (length host))))
        (setf port (parse-integer (subseq host (1+ non-digit-pos)))
              host (subseq host 0 non-digit-pos))))
    (list host port path)))

#+sbcl
(defun connect-tcp (host port)
  (let ((address (host-to-address host))
        (socket (make-instance 'inet-socket
                               :protocol :tcp
                               :type :stream)))
    (socket-connect socket address port)
    (socket-make-stream socket
                        :element-type '(unsigned-byte 8)
                        :timeout 10
                        :input t
                        :output t)))

(defun string-octets (string &rest args)
  (map '(simple-array (unsigned-byte 8) (*))
       #'char-code
       (if args
           (apply #'format nil string args)
           string)))

(defun octets-string (octets &key (start 0) end)
  (map 'string #'code-char
       (subseq octets start end)))

(defvar +crlf+
  (string-octets "~C~C" #\Return #\Linefeed))
(defvar +header-delimiter+
  (string-octets "~C~C~:*~:*~C~C" #\Return #\Linefeed))

(defun write-crlf (stream)
  (write-sequence +crlf+ stream))

(defun parse-http-header-line (octets &key start end)
  (let ((colon-pos (position (char-code #\:) octets :start start :end end)))
    (unless colon-pos
      (error 'invalid-http-header
             :reason (octets-string octets :start start :end end)))
    (cons
     (string-trim '(#\Space #\Tab)
                  (octets-string octets :start start :end colon-pos))
     (string-trim '(#\Space #\Tab)
                  (octets-string octets :start (1+ colon-pos) :end end)))))

(defun parse-first-line (octets &key (start 0) end)
  (unless (search (string-octets "HTTP/1.1 ") octets
                  :start2 start
                  :end2 (+ 9 start))
    (error 'invalid-http-header
           :reason "Non HTTP/1.1 response"))
  (let* ((status-start (+ start 9))
         (status-end
           (position-if (lambda (octet)
                          (not (<= (char-code #\0) octet (char-code #\9))))
                        octets
                        :start status-start
                        :end end)))
    (when (or (null status-end)
              (= status-end status-start))
      (error 'invalid-http-header
             :reason "No status code found in the response"))
    (let ((status (parse-integer (octets-string octets
                                                :start status-start
                                                :end status-end))))
      ;; TODO: Allow 3xx
      (unless (<= 200 status 299)
        (error 'http-request-failed :status status))
      status)))

(defun parse-http-headers (octets)
  (let ((first-line-end (search +crlf+ octets)))
    (unless first-line-end
      (error "Invalid HTTP header"))
    (let ((status
            (parse-first-line octets :start 0 :end first-line-end)))
      (list :status status
            :headers
            (loop with start = (+ first-line-end 2)
                  for nl-pos = (search +crlf+ octets :start2 start)
                  while nl-pos
                  collect (parse-http-header-line octets :start start :end nl-pos)
                  do (setf start (+ nl-pos 2)))))))

(defun read-response (stream)
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (let* ((read-bytes (read-sequence buf stream))
           (delim-pos (search +header-delimiter+ buf :end2 read-bytes)))
      ;; TODO
      (unless delim-pos
        (error "Too long HTTP header"))
      (let ((http-headers (subseq buf 0 (+ 2 delim-pos))))
        (destructuring-bind (&key status headers)
            (parse-http-headers http-headers)
          (let ((content-length (cdr (assoc "Content-Length" headers
                                            :test #'string-equal))))
            ;; TODO: Transfer-Encoding: chunked
            (unless (and content-length
                         (every #'digit-char-p content-length))
              (error "No Content-Length is found"))
            (let ((content-length (parse-integer content-length))
                  (start (+ delim-pos 4)))
              (if (< content-length (- (length buf) start))
                  (subseq buf start (+ start content-length))
                  (let ((body (make-array content-length :element-type '(unsigned-byte 8))))
                    (replace body buf :start2 start)
                    (read-sequence body stream :start (- (length buf) start))
                    (values body status headers))))))))))

#+sbcl
(defun http-fetch (url destination)
  (destructuring-bind (host port path)
      (parse-http-url url)
    (let ((stream (if *proxy*
                      (destructuring-bind (proxy-host proxy-port path)
                          (parse-http-url *proxy*)
                        (declare (ignore path))
                        (connect-tcp proxy-host (or proxy-port 80)))
                      (connect-tcp host (or port 80)))))
      (flet ((send-line (str &rest args)
               (write-sequence (apply #'string-octets str args) stream)
               (write-crlf stream)))
        (unwind-protect
             (progn
               (send-line "GET ~A HTTP/1.1" path)
               (send-line "Host: ~A" host)
               (send-line "Connection: close")
               (write-crlf stream)
               (force-output stream)
               (let ((body (read-response stream)))
                 (with-open-file (out destination
                                      :element-type '(unsigned-byte 8)
                                      :direction :output)
                   (write-sequence body out)))
               destination)
          (close stream))))))

#-sbcl
(defun http-fetch (url destination)
  (progv (list (intern #.(string '#:*proxy-url*) '#:ql-http))
      (list *proxy*)
    (uiop:symbol-call '#:ql-http '#:http-fetch url destination)))
