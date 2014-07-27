#|
  This file is a part of qlot project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage qlot-asd
  (:use :cl :asdf))
(in-package :qlot-asd)

(defsystem qlot
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:clack
               :ningle
               :drakma
               :archive
               :salza2
               :ironclad
               :flexi-streams
               :alexandria
               :cl-ppcre
               :external-program
               :usocket
               :split-sequence
               :iterate
               :function-cache)
  :components ((:module "src"
                :components
                ((:file "qlot" :depends-on ("parser" "server" "http" "shell" "source" "tmp" "util"))
                 (:file "parser" :depends-on ("source" "error"))
                 (:file "server" :depends-on ("source"))
                 (:file "http")
                 (:file "shell")
                 (:file "tmp")
                 (:file "archive")
                 (:file "source" :depends-on ("tmp"))
                 (:module "dist-sources"
                  :pathname "source"
                  :depends-on ("source" "http" "shell" "server" "archive" "tmp" "util")
                  :components
                  ((:file "ql")
                   (:file "git")))
                 (:file "error")
                 (:file "util"))))
  :in-order-to ((test-op (test-op qlot-test))))
