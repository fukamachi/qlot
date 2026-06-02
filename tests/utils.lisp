(defpackage #:qlot-tests/utils
  (:use #:cl
        #:rove
        #:qlot/utils))
(in-package #:qlot-tests/utils)

(deftest make-keyword-tests
  (ok (eq (make-keyword "foo") :foo))
  (ok (eq (make-keyword "FOO") :foo))
  (ok (eq (make-keyword "Foo") :foo))
  (ok (eq (make-keyword :|foo|) :foo))
  (ok (eq (make-keyword '|foo|) :foo))
  (ok (signals (make-keyword 1))))

(deftest split-with-tests
  (ok (equal (split-with #\Space "a b c")
             '("a" "b" "c")))
  (ok (equal (split-with #\Space "  a   b c ")
             '("a" "b" "c")))
  (ok (equal (split-with #\Space "abc")
             '("abc")))
  (ok (equal (split-with #\Space "a b c " :limit 2)
             '("a" "b c "))))

(deftest scrub-url-credentials-tests
  ;; Credentialed forms — userinfo must be stripped
  (ok (equal (qlot/utils::scrub-url-credentials "https://x-access-token:TOKEN@github.com/foo/bar")
             "https://github.com/foo/bar")
      "token:password form stripped")
  (ok (equal (qlot/utils::scrub-url-credentials "https://ghp_secret@github.com/foo/bar")
             "https://github.com/foo/bar")
      "bare-token userinfo (no colon) stripped")
  (ok (equal (qlot/utils::scrub-url-credentials "https://user:pass@gitlab.com/a/b.git")
             "https://gitlab.com/a/b.git")
      "user:pass form stripped")
  ;; Non-credentialed inputs — must pass through unchanged
  (ok (equal (qlot/utils::scrub-url-credentials "https://github.com/foo/bar")
             "https://github.com/foo/bar")
      "URL without credentials unchanged")
  (ok (equal (qlot/utils::scrub-url-credentials "git@github.com:foo/bar")
             "git@github.com:foo/bar")
      "SCP form (no ://) unchanged")
  (ok (null (qlot/utils::scrub-url-credentials nil))
      "nil passes through as NIL")
  ;; @ outside authority must not be stripped
  (ok (equal (qlot/utils::scrub-url-credentials "https://github.com/foo/bar?e=a@b.com")
             "https://github.com/foo/bar?e=a@b.com")
      "@ in query string not stripped"))
