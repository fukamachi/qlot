#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Database independent interface for Common Lisp

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi-asd
  (:use :cl :asdf))
(in-package :dbi-asd)

(defsystem dbi
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :split-sequence
               :closer-mop)
  :components ((:module "src"
                :components
                ((:file "dbi" :depends-on ("driver"))
                 (:file "driver" :depends-on ("error"))
                 (:file "error"))))
  :description "Database independent interface for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op dbi-test))))
