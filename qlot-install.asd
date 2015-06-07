(in-package :cl-user)
(defpackage qlot-install-asd
  (:use :cl :asdf))
(in-package :qlot-install-asd)

(defsystem qlot-install
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:clack
               :clack-handler-hunchentoot
               :drakma
               :archive
               :salza2
               :gzip-stream
               :ironclad
               :yason
               :cl-fad
               :alexandria
               :cl-ppcre
               :external-program
               :usocket
               :split-sequence
               :iterate
               :function-cache)
  :components ((:module "src"
                :components
                ((:file "parser" :depends-on ("source" "dist-sources" "error" "util"))
                 (:file "install" :depends-on ("parser" "server" "tmp" "source" "http" "shell" "asdf" "util"))
                 (:file "server" :depends-on ("source" "parser" "tmp"))
                 (:file "http")
                 (:file "shell")
                 (:file "tmp")
                 (:file "archive")
                 (:file "source" :depends-on ("tmp" "util"))
                 (:module "dist-sources"
                  :pathname "source"
                  :depends-on ("source" "http" "shell" "archive" "tmp" "util")
                  :serial t
                  :components
                  ((:file "ql")
                   (:file "git")
                   (:file "http")
                   (:file "github")))
                 (:file "error")
                 (:file "asdf")
                 (:file "util" :depends-on ("asdf")))))
  :in-order-to ((test-op (test-op qlot-test))))
