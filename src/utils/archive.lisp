(defpackage #:qlot/utils/archive
  (:use #:cl)
  (:import-from #:deflate
                #:inflate-gzip-stream)
  (:import-from #:archive
                #:name
                #:read-entry-from-archive
                #:extract-entry
                #:discard-entry)
  ;; Unexported symbols used to patch tar entry reading.
  (:import-from #:archive
                #:null-block-p
                #:read-entry-block
                #:read-tar-entry-from-buffer
                #:read-data-block
                #:typeflag
                #:size
                #:%name
                #:linkname
                #:round-up-to-tar-block
                #:+gnutar-long-name+
                #:+gnutar-long-link-name+
                #:+tar-regular-file+
                #:+tar-regular-alternate-file+
                #:+tar-directory-file+
                #:+tar-symbolic-link+
                #:+tar-hard-link+
                #:+posix-global-header+
                #:+posix-extended-header+)
  (:export #:extract-tarball))
(in-package #:qlot/utils/archive)

(defclass qlot-tar-archive (archive:tar-archive)
  ()
  (:documentation "A wrapper class to override entry-reading of tar
  archives within Qlot, without affecting other readers. This is
  necessary to work around some shortcomings of archive, notably its
  inability to read symlink/hardlink entries (which prevents them from
  being skipped)."))

(defun read-tar-block (archive entry)
  (read-data-block archive (size entry) #'round-up-to-tar-block))

(defmethod archive:read-entry-from-archive ((archive qlot-tar-archive))
  ;; This duplicates the logic of ARCHIVE:READ-ENTRY-FROM-ARCHIVE,
  ;; only with support for skipping symlink/hardlink entries.
  (let ((entry-block (read-entry-block archive)))
    (if (null-block-p entry-block 0)
        nil
        (let* ((entry (read-tar-entry-from-buffer entry-block :start 0))
               (type (typeflag entry)))
          (cond
            ((or (= type +gnutar-long-name+)
                 (= type +gnutar-long-link-name+))
             (let ((real-name (read-tar-block archive entry))
                   (entry (read-entry-from-archive archive)))
               (ecase (typeflag entry)
                 (+gnutar-long-name+
                  (setf (%name entry) real-name))
                 (+gnutar-long-link-name+
                  (setf (linkname entry) real-name)))
               entry))
            ;; TODO: add support for +POSIX-EXTENDED-HEADER+. This
            ;; will require a utility to convert UTF-8.
            (t entry))))))

(defun extract-tarball (tarball &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (with-open-file (in tarball
                        :element-type '(unsigned-byte 8))
      (uiop:with-temporary-file (:pathname tar-file
                                 :stream tar-stream
                                 :direction :output
                                 :element-type '(unsigned-byte 8))
        (deflate:inflate-gzip-stream in tar-stream)
        (let ((archive (archive:open-archive 'archive:tar-archive in)))
          ;; Switch to the local class to use our read method override.
          (setf archive (change-class archive 'qlot-tar-archive))
          (prog1
              (merge-pathnames
               (archive:name (archive:read-entry-from-archive archive))
               *default-pathname-defaults*)

            (archive:do-archive-entries (entry archive)
              ;; Replicate ql-minitar's behavior: regular files and
              ;; directories get extracted, symlinks get ignored,
              ;; everything else gets ignored with a
              ;; warning. Extended-name entries like
              ;; +posix-extended-header+ and +gnutar-long-name+ are
              ;; handled by READ-ENTRY-FROM-ARCHIVE.
              (cond
                ((or (= (typeflag entry) +tar-regular-alternate-file+)
                     (= (typeflag entry) +tar-regular-file+)
                     (= (typeflag entry) +tar-directory-file+))
                 (extract-entry archive entry))
                ((or (= (typeflag entry) +tar-symbolic-link+)
                     (= (typeflag entry) +posix-global-header+))
                 (discard-entry archive entry))
                (t
                 (warn "Unknown tar entry type code ~D, skipping entry" (typeflag entry))
                 (discard-entry archive entry))))))))))
