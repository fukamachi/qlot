(defpackage #:qlot-tests/http
  (:use #:cl
        #:rove)
  (:import-from #:qlot/http
                #:netrc-basic-auth)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/http)

(defun parse (text)
  (qlot/http::parse-netrc text))

(defmacro with-entries ((plain &optional authinfo) &body body)
  "Run BODY with these credential sources instead of the user's own files."
  `(let ((qlot/http::*netrc-entries* (list ,plain))
         (qlot/http::*authinfo-entries* ,authinfo))
     ,@body))

(deftest parse-netrc-tests
  (testing "a single entry"
    (ok (equal (parse "machine example.com login alice password s3cret")
               '(("example.com" "alice" . "s3cret")))))

  (testing "tokens spread over several lines"
    (ok (equal (parse "machine example.com
  login alice
  password s3cret
")
               '(("example.com" "alice" . "s3cret")))))

  (testing "several entries"
    (ok (equal (parse "machine a.example.com login alice password one
machine b.example.com login bob password two")
               '(("a.example.com" "alice" . "one")
                 ("b.example.com" "bob" . "two")))))

  (testing "comments and blank lines are ignored"
    (ok (equal (parse "# a comment

machine example.com login alice password s3cret
  # indented comment
")
               '(("example.com" "alice" . "s3cret")))))

  (testing "quoted values"
    (ok (equal (parse "machine example.com login \"alice\" password \"s3 cret\"")
               '(("example.com" "alice" . "s3 cret")))))

  (testing "account and port are skipped, not read as credentials"
    (ok (equal (parse "machine example.com port 443 login alice password s3cret account x")
               '(("example.com" "alice" . "s3cret")))))

  (testing "the authinfo 'user' spelling"
    (ok (equal (parse "machine example.com user alice password s3cret")
               '(("example.com" "alice" . "s3cret")))))

  (testing "a default entry has no machine"
    (ok (equal (parse "machine example.com login alice password one
default login bob password two")
               '(("example.com" "alice" . "one")
                 (nil "bob" . "two")))))

  (testing "incomplete entries are dropped"
    (ok (null (parse "machine example.com login alice")))
    (ok (null (parse "machine example.com password s3cret"))))

  (testing "a macdef and its body are skipped"
    (ok (equal (parse "machine a.example.com login alice password one
macdef init
  login mallory password gotcha

machine b.example.com login bob password two")
               '(("a.example.com" "alice" . "one")
                 ("b.example.com" "bob" . "two"))))
    (ok (equal (parse "machine a.example.com login alice password one macdef init
  machine c.example.com login mallory password gotcha")
               '(("a.example.com" "alice" . "one")))))

  (testing "a keyword in value position is a value"
    (ok (equal (parse "machine example.com login macdef password default")
               '(("example.com" "macdef" . "default"))))
    (ok (equal (parse "machine example.com login alice password
  machine")
               '(("example.com" "alice" . "machine")))))

  (testing "a blank line outside a macro changes nothing"
    (ok (equal (parse "machine example.com

  login alice password s3cret")
               '(("example.com" "alice" . "s3cret")))))

  (testing "an unexpected token is skipped with a warning giving its position"
    (let ((messages '()))
      (ok (null (handler-bind ((warning (lambda (w)
                                          (push (princ-to-string w) messages)
                                          (muffle-warning w))))
                  (qlot/http::parse-netrc "# comment
  machine login alice password s3cret" "/tmp/netrc"))))
      (ok (equal messages
                 '("/tmp/netrc, line 2, index 16: unexpected token")))))

  (testing "a machine line without a host is not a default entry"
    (ok (null (parse "login alice password one machine"))))

  (testing "empty input"
    (ok (null (parse "")))))

(deftest netrc-basic-auth-tests
  (with-entries ((parse "machine nexus.example.com login alice password s3cret"))
    (testing "matching host"
      (ok (equal (netrc-basic-auth "https://nexus.example.com/repository/cl-dist/d.txt")
                 '("alice" . "s3cret"))))
    (testing "host names match case-insensitively"
      (ok (equal (netrc-basic-auth "https://Nexus.Example.COM/x")
                 '("alice" . "s3cret"))))
    (testing "plain http gets nothing"
      (ok (null (netrc-basic-auth "http://nexus.example.com/x"))))
    (testing "other hosts get nothing"
      (ok (null (netrc-basic-auth "https://beta.quicklisp.org/dist/quicklisp.txt")))
      (ok (null (netrc-basic-auth "qlot://localhost/example.txt")))))

  (testing "a default entry matches any host, as with curl"
    (with-entries ((parse "default login bob password two"))
      (ok (equal (netrc-basic-auth "https://anywhere.example.com/x")
                 '("bob" . "two")))
      (ok (equal (netrc-basic-auth "https://nil/x")
                 '("bob" . "two")))))

  (testing "a host named nil does not match without a default entry"
    (with-entries ((parse "machine example.com login alice password one"))
      (ok (null (netrc-basic-auth "https://nil/x")))))

  (testing "no credential file"
    (with-entries (nil)
      (ok (null (netrc-basic-auth "https://nexus.example.com/x")))))

  (testing "the encrypted file is asked only when the plaintext files have no entry"
    (let ((qlot/http::*netrc-entries*
            (list (parse "machine a.example.com login alice password one")))
          (qlot/http::*authinfo-entries* :unread))
      (ok (equal (netrc-basic-auth "https://a.example.com/x")
                 '("alice" . "one")))
      (ok (null (netrc-basic-auth "http://b.example.com/x")))
      (ok (eq qlot/http::*authinfo-entries* :unread)))
    (with-entries ((parse "machine a.example.com login alice password one")
                   (parse "machine a.example.com login eve password three
machine b.example.com login bob password two"))
      (ok (equal (netrc-basic-auth "https://a.example.com/x")
                 '("alice" . "one")))
      (ok (equal (netrc-basic-auth "https://b.example.com/x")
                 '("bob" . "two"))))))

(defun call-with-netrc-file (write-contents fn)
  (with-tmp-directory (tmp)
    (let ((file (merge-pathnames "netrc" tmp))
          (previous (or (uiop:getenvp "NETRC") "")))
      (funcall write-contents file)
      (setf (uiop:getenv "NETRC") (uiop:native-namestring file))
      (unwind-protect
           (let ((qlot/http::*netrc-entries* :unread)
                 (qlot/http::*authinfo-entries* nil))
             (funcall fn file))
        (setf (uiop:getenv "NETRC") previous)))))

(deftest netrc-file-tests
  (testing "$NETRC is read, and read only once"
    (call-with-netrc-file
     (lambda (file)
       (uiop:with-output-file (out file)
         (write-line "machine nexus.example.com login alice password s3cret" out)))
     (lambda (file)
       (ok (equal (netrc-basic-auth "https://nexus.example.com/x")
                  '("alice" . "s3cret")))
       (delete-file file)
       (ok (equal (netrc-basic-auth "https://nexus.example.com/x")
                  '("alice" . "s3cret"))))))

  (testing "an unreadable file is skipped with a warning"
    (call-with-netrc-file
     (lambda (file)
       (with-open-file (out file :direction :output
                                 :element-type '(unsigned-byte 8))
         ;; Not valid UTF-8.
         (write-sequence #(109 97 99 104 105 110 101 32 255 254 10) out)))
     (lambda (file)
       (declare (ignore file))
       (ok (null (handler-bind ((warning #'muffle-warning))
                   (netrc-basic-auth "https://nexus.example.com/x"))))))))

(deftest redirect-tests
  (with-entries ((parse "machine a.example.com login alice password one
machine b.example.com login bob password two"))
    (let* ((calls '())
           (requester
             (lambda (url &rest args)
               (push (list url (getf args :basic-auth)) calls)
               (let ((headers (make-hash-table :test 'equal))
                     (target (cond ((search "/to-b" url) "https://b.example.com/x")
                                   ((search "/to-http" url) "http://a.example.com/y")
                                   ((search "/relative" url) "/to-http"))))
                 (when (and target (eql 0 (getf args :max-redirects)))
                   (setf (gethash "location" headers) target))
                 (values "body" (if target 302 200) headers)))))
      (testing "each redirect target gets its own credentials"
        (qlot/http::request "https://a.example.com/to-b" nil '()
                            :requester requester)
        (ok (equal (reverse calls)
                   '(("https://a.example.com/to-b" ("alice" . "one"))
                     ("https://b.example.com/x" ("bob" . "two"))))))
      (testing "a redirect to plain http carries no credentials"
        (setf calls '())
        (qlot/http::request "https://a.example.com/relative" nil '()
                            :requester requester)
        (ok (equal (reverse calls)
                   '(("https://a.example.com/relative" ("alice" . "one"))
                     ("https://a.example.com/to-http" ("alice" . "one"))
                     ("http://a.example.com/y" nil))))))))
