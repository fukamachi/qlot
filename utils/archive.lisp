(defpackage #:qlot/utils/archive
  (:use #:cl)
  (:import-from #:gzip-stream
                #:with-open-gzip-file)
  (:import-from #:archive
                #:open-archive
                #:tar-archive
                #:name
                #:read-entry-from-archive
                #:extract-files-from-archive)
  (:export #:extract-tarball))
(in-package #:qlot/utils/archive)

;; For the purposes of this function (and tarball sources in general),
;; all tarballs are assumed to contain exactly one directory with a
;; source archive in it. The root of this directory must be the first
;; entry in the tarball. Other files and directories will be ignored,
;; although they will be extracted.
(defun extract-tarball (tarball &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (with-open-gzip-file (gzip tarball)
      (let* ((archive (archive:open-archive 'archive:tar-archive gzip))
             (first-entry (archive:read-entry-from-archive archive)))
        (archive:discard-entry archive first-entry)
        (prog1
            (merge-pathnames
             (archive:name first-entry)
             *default-pathname-defaults*)
          (archive::extract-files-from-archive archive))))))
