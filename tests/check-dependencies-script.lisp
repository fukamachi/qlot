(defpackage #:qlot/tests/check-dependencies-script
  (:use #:cl)
  (:export #:main))
(in-package #:qlot/tests/check-dependencies-script)

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

(defun main ()
  (print
    (remove-if (lambda (name)
                 (or (sbcl-contrib-p name)
                     (qlot-system-p name)
                     (find name *allowed-systems* :test 'equal)))
               (asdf:already-loaded-systems)))
  (fresh-line))
