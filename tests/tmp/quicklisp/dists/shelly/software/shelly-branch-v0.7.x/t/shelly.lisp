(in-package :cl-user)
(defpackage shelly-test
  (:use :cl
        :cl-test-more)
  (:shadowing-import-from :shelly.core
                          :read)
  (:import-from :shelly.core
                :canonicalize-arg))
(in-package :shelly-test)

(plan nil)

(is (canonicalize-arg "t") t "boolean T")
(is (canonicalize-arg "nil") nil "boolean NIL")
(is (canonicalize-arg "1") 1 "integer")
(is (canonicalize-arg "-1") -1 "integer")
(is (canonicalize-arg "1.01") 1.01 "float")
(is (canonicalize-arg "(+ 1 2)")
    '(+ 1 2)
    "cons")
(is (canonicalize-arg "clack")
    "clack"
    "string")
(is (canonicalize-arg "http://www.hatena.com")
    "http://www.hatena.com"
    "string (URL)")
(is (canonicalize-arg "") "" "empty string")
(is (canonicalize-arg ":clack")
    :clack
    "keyword")
(is (canonicalize-arg "--prompt")
    :prompt
    "keyword")

(is (canonicalize-arg "'clack")
    ''cl-user::clack
    "quoted symbol")

(is-type (canonicalize-arg
          (princ-to-string (asdf:system-relative-pathname :shelly "t/")))
         'pathname
         "pathname")

(is (read '("+" "1" "2"))
    '(+ 1 2)
    "read")

(is (read '("asdf:run-shell-command" "git status"))
    '(asdf:run-shell-command "git status")
    "read")

(finalize)
