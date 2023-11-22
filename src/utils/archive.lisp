(defpackage #:qlot/utils/archive
  (:use #:cl)
  (:import-from #:deflate
                #:inflate-gzip-stream)
  (:import-from #:archive
                #:name
                #:read-entry-from-archive
                #:extract-files-from-archive)
  (:export #:extract-tarball))
(in-package #:qlot/utils/archive)

(defun extract-tarball (tarball &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (with-open-file (in tarball
                        :element-type '(unsigned-byte 8))
      (uiop:with-temporary-file (:pathname tar-file
                                 :stream tar-stream
                                 :direction :output
                                 :element-type '(unsigned-byte 8))
        (deflate:inflate-gzip-stream in tar-stream)
        (archive:with-open-archive (archive tar-file)
          (prog1
              (merge-pathnames
                (archive:name (archive:read-entry-from-archive archive))
                *default-pathname-defaults*)
            (archive::extract-files-from-archive archive)))))))
