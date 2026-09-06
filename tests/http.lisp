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

  (testing "parsing stops at macdef rather than reading its body"
    (ok (equal (parse "machine a.example.com login alice password one
macdef init
  login mallory password gotcha

machine b.example.com login bob password two")
               '(("a.example.com" "alice" . "one")))))

  (testing "empty input"
    (ok (null (parse "")))))

(deftest netrc-basic-auth-tests
  (let ((qlot/http::*netrc-entries*
          (parse "machine nexus.example.com login alice password s3cret")))
    (testing "matching host"
      (ok (equal (netrc-basic-auth "https://nexus.example.com/repository/cl-dist/d.txt")
                 '("alice" . "s3cret"))))
    (testing "other hosts get nothing"
      (ok (null (netrc-basic-auth "https://beta.quicklisp.org/dist/quicklisp.txt")))
      (ok (null (netrc-basic-auth "qlot://localhost/example.txt")))))

  (testing "a default entry matches any host, as with curl"
    (let ((qlot/http::*netrc-entries* (parse "default login bob password two")))
      (ok (equal (netrc-basic-auth "https://anywhere.example.com/x")
                 '("bob" . "two")))))

  (testing "no credential file"
    (let ((qlot/http::*netrc-entries* nil))
      (ok (null (netrc-basic-auth "https://nexus.example.com/x"))))))

(deftest netrc-file-tests
  (with-tmp-directory (tmp)
    (let ((file (merge-pathnames "netrc" tmp)))
      (uiop:with-output-file (out file)
        (write-line "machine nexus.example.com login alice password s3cret" out))
      (let ((qlot/http::*netrc-entries* :unread)
            (previous (or (uiop:getenvp "NETRC") "")))
        (setf (uiop:getenv "NETRC") (uiop:native-namestring file))
        (unwind-protect
             (testing "$NETRC is read, and read only once"
               (ok (equal (netrc-basic-auth "https://nexus.example.com/x")
                          '("alice" . "s3cret")))
               (delete-file file)
               (ok (equal (netrc-basic-auth "https://nexus.example.com/x")
                          '("alice" . "s3cret"))))
          (setf (uiop:getenv "NETRC") previous))))))
