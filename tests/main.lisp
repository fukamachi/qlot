(defpackage #:qlot/tests/main
  (:use #:cl
        #:qlot
        #:rove)
  (:import-from #:qlot/install
                #:uninstall-all-dists)
  (:import-from #:uiop
                #:file-exists-p
                #:delete-directory-tree
                #:subdirectories))
(in-package #:qlot/tests/main)

(defparameter *tmp-directory* (asdf:system-relative-pathname :qlot #P"tests/tmp/"))

(defun test-data (file)
  (merge-pathnames file
                   (asdf:system-relative-pathname :qlot #P"tests/data/")))

(setup
  (ensure-directories-exist *tmp-directory*))

(teardown
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (flet ((delete-if-exists (name)
           (let ((file (test-data name)))
             (when (probe-file file)
               (delete-file file)))))
    (delete-if-exists "qlfile.lock")
    (delete-if-exists "qlfile2.lock")
    (delete-if-exists "qlfile3.lock")))

(deftest uninstall-test
  (let ((res (install-quicklisp (merge-pathnames #P"quicklisp/" *tmp-directory*))))
    (ok (not (null res)) "can install Quicklisp"))

  (dolist (dir (uiop:subdirectories (merge-pathnames #P"quicklisp/dists/" *tmp-directory*)))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

  (ok (null (uiop:subdirectories (merge-pathnames #P"quicklisp/dists/" *tmp-directory*)))
      "can uninstall all dists"))

(deftest install-test
  (install (asdf:system-relative-pathname :qlot #P"tests/data/qlfile")
           :quicklisp-home (merge-pathnames #P"quicklisp/" *tmp-directory*))

  (ok (equal (mapcar (lambda (path)
                       (car (last (pathname-directory path))))
                     (uiop:subdirectories (merge-pathnames #P"quicklisp/dists/" *tmp-directory*)))
             '("cl-dbi"
               "clack"
               "datafly"
               "log4cl"
               "quicklisp"
               "shelly"))
      "can install dists from qlfile"))

(deftest update-test
  (update (asdf:system-relative-pathname :qlot #P"tests/data/qlfile2")
          :quicklisp-home (merge-pathnames #P"quicklisp/" *tmp-directory*))

  (ok (equal (mapcar (lambda (path)
                       (car (last (pathname-directory path))))
                     (uiop:subdirectories (merge-pathnames #P"quicklisp/dists/" *tmp-directory*)))
             '("datafly"
               "log4cl"
               "quicklisp"
               "shelly"))
      "can update dists from qlfile")

  (update (asdf:system-relative-pathname :qlot #P"tests/data/qlfile3")
          :quicklisp-home (merge-pathnames #P"quicklisp/" *tmp-directory*))

  (ok (equal (mapcar (lambda (path)
                       (car (last (pathname-directory path))))
                     (uiop:subdirectories (merge-pathnames #P"quicklisp/dists/" *tmp-directory*)))
             '("quicklisp"))
      "can install dists from qlfile")

  (ok
   (search "version: 2014-12-17"
           (alexandria:read-file-into-string (merge-pathnames #P"quicklisp/dists/quicklisp/distinfo.txt" *tmp-directory*)))
   "can install old Quicklisp dist"))
