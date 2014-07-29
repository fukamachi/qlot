#|
  This file is a part of qlot project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage qlot-test
  (:use :cl
        :qlot
        :cl-test-more)
  (:import-from :qlot.install
                :uninstall-all-dists))
(in-package :qlot-test)

(defparameter *tmp-directory* (asdf:system-relative-pathname :qlot #P"t/tmp/"))
(when (probe-file *tmp-directory*)
  (fad:delete-directory-and-files *tmp-directory*))
(ensure-directories-exist *tmp-directory*)

(plan 3)

(ok (install-quicklisp (merge-pathnames #P"quicklisp/" *tmp-directory*))
    "can install Quicklisp")

(uninstall-all-dists (merge-pathnames #P"quicklisp/" *tmp-directory*))

(is (fad:list-directory (merge-pathnames #P"quicklisp/dists/" *tmp-directory*))
    '()
    "can uninstall all dists")

(install (asdf:system-relative-pathname :qlot #P"t/data/qlfile")
         :quicklisp-home (merge-pathnames #P"quicklisp/" *tmp-directory*))

(is (mapcar (lambda (path)
              (car (last (pathname-directory path))))
            (fad:list-directory (merge-pathnames #P"quicklisp/dists/" *tmp-directory*)))
    '("cl-dbi"
      "clack"
      "datafly"
      "log4cl"
      "quicklisp"
      "shelly")
    :test #'equal
    "can install dists from qlfile")

(finalize)
