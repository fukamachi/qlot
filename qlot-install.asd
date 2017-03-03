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
               #:bordeaux-threads
               :dexador
               :archive
               :salza2
               :gzip-stream
               :ironclad
               :yason
               :alexandria
               :cl-ppcre
               :uiop
               :usocket
               :split-sequence
               :iterate
               :function-cache)
  :components ((:module "src"
                :components
                ((:file "parser" :depends-on ("source" "dist-sources" "error" "util"))
                 (:file "install" :depends-on ("parser" "server" "tmp" "source" "shell" "util" "proxy"))
                 (:file "server" :depends-on ("source" "parser" "tmp"))
                 (:file "shell")
                 (:file "tmp" :depends-on ("util"))
                 (:file "archive")
                 (:file "source" :depends-on ("tmp" "util"))
                 (:module "dist-sources"
                  :pathname "source"
                  :depends-on ("source" "shell" "archive" "tmp" "util" "proxy")
                  :serial t
                  :components
                  ((:file "ql")
                   (:file "git")
                   (:file "http")
                   (:file "github")))
                 (:file "error")
                 (:file "util")
                 (:file "proxy"))))
  :in-order-to ((test-op (test-op qlot-test))))
