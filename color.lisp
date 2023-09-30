(defpackage #:qlot/color
  (:use #:cl)
  (:export #:*enable-color*
           #:color-text))
(in-package #:qlot/color)

(defvar *enable-color* nil)

(defun color-code (color)
  (ecase color
    (:gray "38;5;8")
    (:red 31)
    (:green 32)
    (:yellow 33)))

(defun color-text (color-name control &rest arguments)
  (check-type color-name keyword)
  (if *enable-color*
      (format nil "~C[~Am~A~C[0m"
              #\Esc
              (color-code color-name)
              (apply #'format nil control arguments)
              #\Esc)
      (apply #'format nil control arguments)))
