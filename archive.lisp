(defpackage #:qlot/archive
  (:use #:cl)
  (:import-from #:qlot/util
                #:with-package-functions)
  (:import-from #:uiop
                #:with-temporary-file)
  (:export #:extract-tarball))
(in-package #:qlot/archive)

(defun extract-tarball (tarball &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (with-package-functions :ql-minitar (unpack-tarball contents)
      (with-package-functions :ql-gunzipper (gunzip)
        (uiop:with-temporary-file (:pathname tarfile :type "tar")
          (unpack-tarball (gunzip tarball tarfile))
          (first (contents tarfile)))))))
