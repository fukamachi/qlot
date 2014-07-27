(in-package :cl-user)
(defpackage qlot-asdf-asd
  (:use :cl :asdf))
(in-package :qlot-asdf-asd)

(defsystem qlot-asdf
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :components ((:file "src/asdf" :depends-on ("src/util"))
               (:file "src/util")))
