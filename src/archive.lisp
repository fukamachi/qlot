(in-package :cl-user)
(defpackage qlot.archive
  (:use :cl)
  (:import-from :fad
                :pathname-parent-directory
                :walk-directory)
  (:import-from :archive
                :create-tar-file)
  (:import-from :salza2
                :gzip-file)
  (:export :create-tarball))
(in-package :qlot.archive)

(defun create-tarball (directory destination)
  (let ((filelist '())
        (ignore-len (length (pathname-directory (truename (fad:pathname-parent-directory directory)))))
        (*default-pathname-defaults* (truename (fad:pathname-parent-directory directory)))
        (tar-file (make-pathname
                   :directory (pathname-directory destination)
                   :name (pathname-name destination)))
        (tar-gz-file destination))

    (flet ((to-relative (path)
             (make-pathname
              :defaults path
              :directory (cons :relative
                               (nthcdr ignore-len (pathname-directory path)))))
           (git-dir-p (path)
             (find ".git"
                   (nthcdr ignore-len (pathname-directory path))
                   :test #'string=)))
      (fad:walk-directory directory
                          (lambda (file)
                            (push (to-relative file) filelist))
                          :test (complement #'git-dir-p)))

    (archive::create-tar-file tar-file filelist)
    (salza2:gzip-file tar-file tar-gz-file)
    (delete-file tar-file)
    tar-gz-file))
