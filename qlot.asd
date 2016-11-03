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
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :components ((:module "src"
                :components
                ((:file "qlot" :depends-on ("util"))
                 (:file "util"))))
  :in-order-to ((test-op (test-op qlot-test))))

#-qlot
(defmethod perform :after ((o load-op) (c (eql (find-system :qlot))))
  (declare (ignore o c))
  (pushnew :qlot *features*))
