(in-package :cl-user)
(defpackage qlot.asdf
  (:use :cl)
  (:export :qlot-system
           :system-quicklisp-home))
(in-package :qlot.asdf)

(defclass qlot-system (asdf:system)
  ((quicklisp-home :initarg :quicklisp-home
                   :initform #P"quicklisp/")
   (qlhome-initialized :initform nil)))

(defgeneric system-quicklisp-home (system)
  (:method ((system asdf:system))
    (asdf:system-relative-pathname system #P"quicklisp/"))
  (:method ((system qlot-system))
    (asdf:system-relative-pathname system (slot-value system 'quicklisp-home))))

(import '(qlot-system) :asdf)
