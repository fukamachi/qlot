(defpackage #:qlot/tests/install
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install
                #:install-qlfile)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:import-from #:alexandria
                #:starts-with-subseq
                #:set-equal))
(in-package #:qlot/tests/install)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*)
  (uiop:copy-file (asdf:system-relative-pathname :qlot #P"tests/data/qlfile")
                  (merge-pathnames #P"qlfile" *tmp-directory*)))

(defun directory-name (path)
  (car (last (pathname-directory path))))

(deftest install-qlfile-tests
  (install-qlfile (merge-pathnames #P"qlfile" *tmp-directory*)
                  :quicklisp-home *tmp-directory*)
  (ok (uiop:file-exists-p (merge-pathnames #P"qlfile.lock" *tmp-directory*)))
  (ok (set-equal (mapcar #'directory-name
                         (uiop:subdirectories (merge-pathnames #P"dists/" *tmp-directory*)))
                 '("cl-dbi"
                   "clack"
                   "log4cl"
                   "quicklisp"
                   "shelly")))
  ;; Check if Dexador, qlot/distify depends on, is not installed in the local Quicklisp
  (ng (find-if (lambda (name)
                 (starts-with-subseq "dexador-" name))
               (uiop:subdirectories (merge-pathnames #P"dists/quicklisp/software/"))
               :key #'directory-name)))
