(defpackage #:qlot/archive
  (:use #:cl)
  (:import-from #:uiop
                #:pathname-parent-directory-pathname
                #:directory-files
                #:subdirectories)
  (:import-from #:archive
                #:create-tar-file
                #:open-archive
                #:read-entry-from-archive
                #:extract-files-from-archive)
  (:import-from #:salza2
                #:gzip-file)
  (:import-from #:gzip-stream
                #:with-open-gzip-file)
  (:export #:create-tarball
           #:extract-tarball))
(in-package #:qlot/archive)

(defun create-tarball (directory destination)
  (let ((ignore-len (length (pathname-directory (truename (uiop:pathname-parent-directory-pathname directory)))))
        (*default-pathname-defaults* (truename (uiop:pathname-parent-directory-pathname directory)))
        (tar-file (make-pathname
                   :directory (pathname-directory destination)
                   :name (pathname-name destination)))
        (tar-gz-file destination))

    (flet ((to-relative (path)
             (make-pathname
              :defaults path
              :device nil
              :directory (cons :relative
                               (nthcdr ignore-len (pathname-directory path)))))
           (git-dir-p (path)
             (find ".git"
                   (nthcdr ignore-len (pathname-directory path))
                   :test #'string=)))
      (archive::create-tar-file
       tar-file
       (mapcar #'to-relative
               (remove-if #'git-dir-p
                          (nconc (uiop:subdirectories directory)
                                 (uiop:directory-files directory))))))

    (salza2:gzip-file tar-file tar-gz-file)
    (delete-file tar-file)
    tar-gz-file))

(defun extract-tarball (tarball &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (with-open-gzip-file (gzip tarball)
      (let ((archive (archive:open-archive 'archive:tar-archive gzip)))
        (prog1
            (merge-pathnames
             (archive:name (archive:read-entry-from-archive archive))
             *default-pathname-defaults*)
          (archive::extract-files-from-archive archive))))))
