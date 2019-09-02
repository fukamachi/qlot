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

(defun extract-tarball (tarball &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (with-open-gzip-file (gzip tarball)
      (let ((archive (archive:open-archive 'archive:tar-archive gzip)))
        (prog1
            (merge-pathnames
             (archive:name (archive:read-entry-from-archive archive))
             *default-pathname-defaults*)
          (archive::extract-files-from-archive archive))))))
