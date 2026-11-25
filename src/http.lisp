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
;; Each source is parsed on its own and they are asked in this order: the file
;; named by $NETRC, ~/.netrc, and ~/.authinfo.gpg; the first source with an entry
;; for the host wins. The encrypted file comes last so that it is decrypted only
;; when the plaintext files have no answer. Both formats use the same
;; 'machine/login/password' tokens, so a single parser covers them.

(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Return)))

(defun tokenize-line (line)
  "Split LINE on whitespace into a list of (TOKEN . START), START being the
zero-based index of the token in LINE. A token may be double-quoted, which is
how a value containing spaces is written."
  (let ((tokens '())
        (start 0)
        (end (length line)))
    (loop while (< start end)
          do (cond ((whitespacep (char line start))
                    (incf start))
                   ((char= #\" (char line start))
                    (let ((close (position #\" line :start (1+ start))))
                      (push (cons (subseq line (1+ start) (or close end)) start)
                            tokens)
                      (setf start (if close (1+ close) end))))
                   (t
                    (let ((next (or (position-if #'whitespacep line :start start)
                                    end)))
                      (push (cons (subseq line start next) start) tokens)
                      (setf start next)))))
    (nreverse tokens)))

(defun parse-netrc (text &optional (source "netrc text"))
  "Parse netrc/authinfo TEXT into a list of (MACHINE USERNAME . PASSWORD).
SOURCE names the text in warnings.
MACHINE is NIL for the 'default' entry, which matches any host.
A token is a keyword only where a keyword is expected: the token after
'machine', 'login' or 'password' is always its value, whatever it spells, and
may stand on a later line. A 'macdef' and its body, which runs to the next
blank line, are skipped. A token that is neither a keyword nor a value is
skipped with a warning that gives its position."
  (let ((entries '())
        (machine nil)
        (username nil)
        (password nil)
        ;; True after 'default', or after 'machine' followed by a host.
        (in-entry nil)
        ;; The keyword whose value is the next token, if any.
        (expecting nil)
        (in-macro nil))
    (flet ((flush ()
             (when (and in-entry username password)
               (push (list* machine username password) entries))
             (setf machine nil username nil password nil in-entry nil)))
      (loop
        for line in (uiop:split-string text :separator '(#\Newline))
        for line-number from 1
        do
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Return) line))
               (blank (string= trimmed "")))
          (cond (in-macro
                 (when blank
                   (setf in-macro nil)))
                ((or blank (char= #\# (char trimmed 0))))
                (t
                 (loop
                   for (token . index) in (tokenize-line line)
                   do
                   (cond (expecting
                          (ecase expecting
                            (:machine (setf machine token
                                            in-entry t))
                            (:login (setf username token))
                            (:password (setf password token))
                            (:ignored))
                          (setf expecting nil))
                         ((string= token "machine")
                          (flush)
                          (setf expecting :machine))
                         ((string= token "default")
                          (flush)
                          (setf in-entry t))
                         ((or (string= token "login")
                              (string= token "user"))
                          (setf expecting :login))
                         ((string= token "password")
                          (setf expecting :password))
                         ((or (string= token "account")
                              (string= token "port"))
                          (setf expecting :ignored))
                         ;; The rest of the line is the macro's name.
                         ((string= token "macdef")
                          (setf in-macro t)
                          (return))
                         ;; The token itself is not shown: after a misspelt
                         ;; 'password' it is the secret.
                         (t
                          (warn "~A, line ~D, index ~D: unexpected token"
                                source line-number index))))))))
      (flush))
    (nreverse entries)))

(defun decrypt-file (file)
  (handler-case
      (with-output-to-string (out)
        (uiop:run-program (list "gpg" "--batch" "--quiet" "--decrypt"
                                (uiop:native-namestring file))
                          :output out
                          :error-output :interactive))
    (error (e)
      (warn "Failed to decrypt ~A: ~A" file e)
      nil)))

(defun read-netrc-file (file)
  "Entries of the plaintext FILE; NIL when it is absent or cannot be read, so
that a broken credential file does not fail downloads from public hosts."
  (and file
       (uiop:file-exists-p file)
       (handler-case (parse-netrc (uiop:read-file-string file) file)
         (error (e)
           (warn "Ignoring credential file ~A: ~A" file e)
           nil))))

(defvar *netrc-entries* :unread
  "Cache of the plaintext sources: one list of entries per file, in lookup
order.")

(defvar *authinfo-entries* :unread
  "Cache of the entries in ~/.authinfo.gpg. Decryption may prompt for a
passphrase, so it must happen at most once per image.")

(defun netrc-entries ()
  (when (eq *netrc-entries* :unread)
    (setf *netrc-entries*
          (list (read-netrc-file (uiop:getenvp "NETRC"))
                (read-netrc-file
                 (merge-pathnames ".netrc" (user-homedir-pathname))))))
  *netrc-entries*)

(defun authinfo-entries ()
  (when (eq *authinfo-entries* :unread)
    (setf *authinfo-entries*
          (let* ((file (merge-pathnames ".authinfo.gpg" (user-homedir-pathname)))
                 (text (and (uiop:file-exists-p file)
                            (decrypt-file file))))
            (and text (parse-netrc text file)))))
  *authinfo-entries*)

(defun find-entry (host entries)
  ;; EQUALP: host names are case-insensitive, and unlike STRING-EQUAL it does
  ;; not take the NIL machine of a 'default' entry for a host named "nil".
  (or (find host entries :key #'first :test #'equalp)
      (find nil entries :key #'first)))

(defun netrc-basic-auth (url)
  "Return (USERNAME . PASSWORD) for URL's host, or NIL if no entry matches.
Only an https URL gets credentials. As with curl, a 'default' entry matches
every host."
  (let* ((uri (ignore-errors (quri:uri url)))
         (host (and uri
                    (equalp (quri:uri-scheme uri) "https")
                    (quri:uri-host uri))))
    (when host
      (cdr (or (some (lambda (entries) (find-entry host entries))
                     (netrc-entries))
               (find-entry host (authinfo-entries)))))))

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

(defun request (url basic-auth args &key (requester #'dex:get))
  "Request URL with Dexador, authenticated with BASIC-AUTH if given and with
the netrc entry for URL's host otherwise.
Dexador repeats :basic-auth on every redirect, also to another host or to plain
http. Netrc credentials are therefore sent with redirects disabled, and each
redirect is followed here with the credentials of its own target."
  (if basic-auth
      (apply requester url :basic-auth basic-auth args)
      (loop repeat 6
            for auth = (netrc-basic-auth url)
            do (unless auth
                 (return (apply requester url args)))
               (multiple-value-bind (body status headers)
                   (apply requester url :basic-auth auth :max-redirects 0 args)
                 (let ((location (and (member status '(301 302 303 307 308))
                                      (gethash "location" headers))))
                   (unless location
                     (return (values body status headers)))
                   (when (streamp body)
                     (close body))
                   (setf url (quri:render-uri
                              (quri:merge-uris (quri:uri location)
                                               (quri:uri url))))))
            finally (error "Too many redirects, last to ~A" url))))

(defun fetch (url file &key basic-auth)
  "Download URL into FILE.
Credentials default to the netrc entry matching URL's host. Note that binding
QLOT/LOGGER:*DEBUG* makes Dexador log request headers, including this one."
  (with-retry ()
    (let ((body (request url basic-auth
                         (list :want-stream t
                               :force-binary t
                               :keep-alive nil
                               :proxy *proxy*
                               :verbose *debug*))))
      (unwind-protect
           (with-open-file (out file
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede
                                :if-does-not-exist :create)
             (uiop:copy-stream-to-stream body out
                                         :element-type '(unsigned-byte 8)))
        (close body)))))

(defun get (url &rest args &key want-stream basic-auth force-binary)
  "Request URL.
Credentials default to the netrc entry matching URL's host. Note that binding
QLOT/LOGGER:*DEBUG* makes Dexador log request headers, including this one."
  (declare (ignore want-stream force-binary))
  (with-retry ()
    (request url basic-auth
             (list* :keep-alive nil
                    :proxy *proxy*
                    :verbose *debug*
                    :connect-timeout nil
                    :read-timeout 30
                    (uiop:remove-plist-key :basic-auth args)))))
