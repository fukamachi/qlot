(defpackage #:qlot/utils/archive
  (:use #:cl)
  (:import-from #:deflate
                #:inflate-gzip-stream)
  (:import-from #:archive
                #:name
                #:read-entry-from-archive
                #:extract-files-from-archive
                #:extract-entry
                #:discard-entry)
  (:export #:extract-tarball))
(in-package #:qlot/utils/archive)

(defun extract-tarball (tarball &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination)
        (first-entry-name nil))
    (with-open-file (in tarball
                        :element-type '(unsigned-byte 8))
      (uiop:with-temporary-file (:pathname tar-file
                                 :stream tar-stream
                                 :direction :output
                                 :element-type '(unsigned-byte 8))
        (deflate:inflate-gzip-stream in tar-stream)
        (finish-output tar-stream)
        (archive:with-open-archive (archive tar-file)
          ;; Read entries one by one and extract them, handling errors
          (loop
            (handler-case
                (let ((entry (archive:read-entry-from-archive archive)))
                  (unless entry
                    (return))
                  ;; Remember the first entry name
                  (unless first-entry-name
                    (setf first-entry-name (archive:name entry)))
                  ;; Try to extract the entry
                  (handler-case
                      (archive:extract-entry archive entry)
                    (archive:unhandled-extract-entry-error (e)
                      ;; Skip entries we can't extract (like symlinks)
                      (when (= (archive::typeflag e) archive::+tar-symbolic-link+)
                        (archive:discard-entry archive entry)))))
              (archive:unhandled-read-header-error ()
                ;; Skip entries with unreadable headers
                ;; This happens with POSIX extended headers
                nil)))
          (merge-pathnames (or first-entry-name "") *default-pathname-defaults*))))))
