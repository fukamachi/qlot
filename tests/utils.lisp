(defpackage #:qlot/tests/utils
  (:use #:cl
        #:rove
        #:qlot/utils))
(in-package #:qlot/tests/utils)

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
