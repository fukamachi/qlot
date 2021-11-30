(defpackage #:qlot/tests/main
  (:use #:cl
        #:rove))
(in-package #:qlot/tests/main)

(defun starts-with (prefix value)
  (and (<= (length prefix) (length value))
       (string= prefix value :end2 (length prefix))))

(defvar *allowed-systems*
  '("asdf" "uiop" "asdf-package-system" "quicklisp" "roswell.extend.quicklisp"))

(defun sbcl-contrib-p (name)
  (starts-with "sb-" name))

(defun qlot-system-p (name)
  (or (string= name "qlot")
      (starts-with "qlot/" name)))

(deftest no-core-dependencies
  (let ((external-dependencies
          (read-from-string
            (with-output-to-string (out)
              (uiop:run-program
                `("ros" "--no-rc" "-s" "qlot"
                  "-l" ,(uiop:native-namestring (asdf:system-relative-pathname :qlot #P"tests/check-dependencies-script.lisp"))
                  "-e" "(qlot/tests/check-dependencies-script:main)")
                :output out)))))
    (ok (null external-dependencies)
        "depends on no external dependencies")))
