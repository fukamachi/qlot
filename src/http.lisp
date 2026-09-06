(defpackage #:qlot/http
  (:use #:cl)
  (:shadow #:get)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/logger
                #:*debug*)
  (:import-from #:dexador)
  (:import-from #:quri)
  #-(or mswindows win32)
  (:import-from #:cl+ssl)
  (:export #:fetch
           #:get
           #:netrc-basic-auth))
(in-package #:qlot/http)

;;
;; Credentials for private dists
;;
;; Entries are collected from all available sources: the file named by $NETRC,
;; ~/.authinfo.gpg, and ~/.netrc; on a host match the earlier source wins. Both
;; formats use the same 'machine/login/password' tokens, so a single parser
;; covers them.

(defun whitespacep (char)
  (member char '(#\Space #\Tab)))

(defun tokenize-line (line)
  "Split LINE on whitespace. A token may be double-quoted, which is how a value
containing spaces is written."
  (let ((tokens '())
        (start 0)
        (end (length line)))
    (loop while (< start end)
          do (cond ((whitespacep (char line start))
                    (incf start))
                   ((char= #\" (char line start))
                    (let ((close (position #\" line :start (1+ start))))
                      (push (subseq line (1+ start) (or close end)) tokens)
                      (setf start (if close (1+ close) end))))
                   (t
                    (let ((next (or (position-if #'whitespacep line :start start)
                                    end)))
                      (push (subseq line start next) tokens)
                      (setf start next)))))
    (nreverse tokens)))

(defun netrc-tokens (text)
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
        unless (or (string= trimmed "")
                   (char= #\# (char trimmed 0)))
        append (tokenize-line trimmed)))

(defun parse-netrc (text)
  "Parse netrc/authinfo TEXT into a list of (MACHINE USERNAME . PASSWORD).
MACHINE is NIL for the 'default' entry, which matches any host."
  (let ((entries '())
        (machine nil)
        (username nil)
        (password nil)
        (in-entry nil))
    (labels ((flush ()
               (when (and in-entry username password)
                 (push (list* machine username password) entries))
               (setf machine nil username nil password nil in-entry nil)))
      (loop with tokens = (netrc-tokens text)
            while tokens
            for token = (pop tokens)
            do (cond ((string= token "machine")
                      (flush)
                      (setf machine (pop tokens)
                            in-entry t))
                     ((string= token "default")
                      (flush)
                      (setf in-entry t))
                     ((or (string= token "login")
                          (string= token "user"))
                      (setf username (pop tokens)))
                     ((string= token "password")
                      (setf password (pop tokens)))
                     ((or (string= token "account")
                          (string= token "port"))
                      (pop tokens))
                     ;; A macdef body runs to the next blank line, which is not
                     ;; recoverable once the text is tokenised. Stop rather than
                     ;; risk reading the body as credentials.
                     ((string= token "macdef")
                      (return))))
      (flush))
    (nreverse entries)))

(defun decrypt-file (file)
  (handler-case
      (with-output-to-string (out)
        (uiop:run-program (list "gpg" "--quiet" "--decrypt"
                                (uiop:native-namestring file))
                          :output out
                          :error-output :interactive))
    (error (e)
      (warn "Failed to decrypt ~A: ~A" file e)
      nil)))

(defun netrc-text ()
  (let ((netrc (uiop:getenvp "NETRC"))
        (authinfo.gpg (merge-pathnames ".authinfo.gpg" (user-homedir-pathname)))
        (netrc-file (merge-pathnames ".netrc" (user-homedir-pathname))))
    (with-output-to-string (out)
      (dolist (text (list (and netrc
                               (uiop:file-exists-p netrc)
                               (uiop:read-file-string netrc))
                          (and (uiop:file-exists-p authinfo.gpg)
                               (decrypt-file authinfo.gpg))
                          (and (uiop:file-exists-p netrc-file)
                               (uiop:read-file-string netrc-file))))
        (when text
          (write-string text out)
          (terpri out))))))

(defvar *netrc-entries* :unread
  "Cache of the parsed credential file. Decryption may prompt for a passphrase,
so it must happen at most once per image.")

(defun netrc-entries ()
  (when (eq *netrc-entries* :unread)
    (setf *netrc-entries*
          (let ((text (netrc-text)))
            (and text (parse-netrc text)))))
  *netrc-entries*)

(defun netrc-basic-auth (url)
  "Return (USERNAME . PASSWORD) for URL's host, or NIL if no entry matches.
As with curl, a 'default' entry matches every host."
  (let ((host (ignore-errors (quri:uri-host (quri:uri url)))))
    (when host
      (let ((entry (or (find host (netrc-entries) :key #'first :test #'equal)
                       (find nil (netrc-entries) :key #'first))))
        (cdr entry)))))

(defun call-with-retry (fn)
  (let ((retry-request (dex:retry-request 2 :interval 3))
        (retry-connect (dex:retry-request 1)))
    (handler-bind ((dex:http-request-failed
                     (lambda (e)
                       (when (<= 500 (dex:response-status e))
                         (funcall retry-request e))))
                   #-(or mswindows win32)
                   ((or usocket:host-down-error
                        usocket:host-unreachable-error)
                     retry-request)
                   (end-of-file retry-connect)
                   #+sbcl
                   ((or sb-bsd-sockets:interrupted-error
                        sb-bsd-sockets:operation-timeout-error)
                     retry-connect)
                   #-(or mswindows win32)
                   ((or usocket:connection-reset-error
                        usocket:timeout-error
                        cl+ssl::ssl-error)
                     retry-connect))
      (funcall fn))))

(defmacro with-retry (() &body body)
  `(call-with-retry (lambda () ,@body)))

(defun fetch (url file &key (basic-auth (netrc-basic-auth url)))
  "Download URL into FILE.
Credentials default to the netrc entry matching URL's host. Note that binding
QLOT/LOGGER:*DEBUG* makes Dexador log request headers, including this one."
  (with-retry ()
    (apply #'dex:fetch url file
           :if-exists :supersede
           :keep-alive nil
           :proxy *proxy*
           :verbose *debug*
           (and basic-auth
                (list :basic-auth basic-auth)))))

(defun get (url &rest args &key want-stream basic-auth force-binary)
  "Request URL.
Credentials default to the netrc entry matching URL's host. Note that binding
QLOT/LOGGER:*DEBUG* makes Dexador log request headers, including this one."
  (declare (ignore want-stream force-binary))
  (let ((netrc-auth (and (not basic-auth)
                         (netrc-basic-auth url))))
    (with-retry ()
      (apply #'dex:get url
             :keep-alive nil
             :proxy *proxy*
             :verbose *debug*
             :connect-timeout nil
             :read-timeout 30
             (if netrc-auth
                 (list* :basic-auth netrc-auth args)
                 args)))))
