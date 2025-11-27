(defpackage #:qlot/utils/archive
  (:use #:cl)
  (:import-from #:deflate
                #:inflate-gzip-stream)
  (:export #:extract-tarball))
(in-package #:qlot/utils/archive)

;; Simple tar extractor that handles ustar format correctly.
;; The archive library has bugs:
;; 1. It doesn't populate the %prefix slot from ustar headers
;; 2. It doesn't insert "/" between prefix and name fields

(defconstant +tar-block-size+ 512)
(defconstant +tar-name-offset+ 0)
(defconstant +tar-name-length+ 100)
(defconstant +tar-size-offset+ 124)
(defconstant +tar-size-length+ 12)
(defconstant +tar-typeflag-offset+ 156)
(defconstant +tar-prefix-offset+ 345)
(defconstant +tar-prefix-length+ 155)

(defconstant +tar-type-file+ (char-code #\0))
(defconstant +tar-type-file-alt+ 0)  ; NUL also means regular file
(defconstant +tar-type-directory+ (char-code #\5))
(defconstant +tar-type-pax-global+ (char-code #\g))
(defconstant +tar-type-pax-extended+ (char-code #\x))

(defun bytes-to-string (bytes &optional (start 0) (end (length bytes)))
  "Convert bytes to string, stopping at first NUL."
  (let ((nul-pos (position 0 bytes :start start :end end)))
    (map 'string #'code-char (subseq bytes start (or nul-pos end)))))

(defun parse-octal (bytes &optional (start 0) (end (length bytes)))
  "Parse octal number from tar header field."
  (let ((str (string-trim '(#\Space #\Nul)
                          (bytes-to-string bytes start end))))
    (if (string= str "")
        0
        (parse-integer str :radix 8))))

(defun tar-entry-name-from-header (header)
  "Extract full pathname from tar header, handling ustar prefix correctly."
  (let ((name (bytes-to-string header +tar-name-offset+
                               (+ +tar-name-offset+ +tar-name-length+)))
        (prefix (bytes-to-string header +tar-prefix-offset+
                                 (+ +tar-prefix-offset+ +tar-prefix-length+))))
    (cond
      ((zerop (length prefix)) name)
      ((or (uiop:string-suffix-p prefix "/")
           (uiop:string-prefix-p "/" name)
           (zerop (length name)))
       (concatenate 'string prefix name))
      (t
       (concatenate 'string prefix "/" name)))))

(defun read-tar-block (stream)
  "Read a 512-byte tar block. Returns NIL at end of archive."
  (let ((block (make-array +tar-block-size+ :element-type '(unsigned-byte 8))))
    (let ((bytes-read (read-sequence block stream)))
      (when (< bytes-read +tar-block-size+)
        (return-from read-tar-block nil))
      ;; Check for end-of-archive (two consecutive zero blocks)
      (when (every #'zerop block)
        (return-from read-tar-block nil))
      block)))

(defun skip-tar-data (stream size)
  "Skip SIZE bytes of data in tar archive, accounting for block padding."
  (let ((blocks (ceiling size +tar-block-size+)))
    (dotimes (i blocks)
      (let ((block (make-array +tar-block-size+ :element-type '(unsigned-byte 8))))
        (read-sequence block stream)))))

(defun extract-tar-file (stream header destination)
  "Extract a regular file from tar archive."
  (let* ((name (tar-entry-name-from-header header))
         (size (parse-octal header +tar-size-offset+
                           (+ +tar-size-offset+ +tar-size-length+)))
         (path (merge-pathnames name destination)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (let ((remaining size))
        (loop while (> remaining 0)
              for block = (make-array +tar-block-size+ :element-type '(unsigned-byte 8))
              do (read-sequence block stream)
                 (let ((to-write (min remaining +tar-block-size+)))
                   (write-sequence block out :end to-write)
                   (decf remaining +tar-block-size+)))))))

(defun extract-tar-directory (header destination)
  "Create a directory from tar archive entry."
  (let* ((name (tar-entry-name-from-header header))
         (path (merge-pathnames name destination)))
    (ensure-directories-exist path)))

(defun extract-tarball (tarball &optional (destination *default-pathname-defaults*))
  "Extract a gzipped tarball to DESTINATION.
Returns the path to the first extracted entry (usually the root directory)."
  (let ((destination (uiop:ensure-directory-pathname destination))
        (first-entry-name nil))
    (ensure-directories-exist destination)
    (with-open-file (in tarball :element-type '(unsigned-byte 8))
      (uiop:with-temporary-file (:pathname tar-file
                                 :stream tar-stream
                                 :direction :output
                                 :element-type '(unsigned-byte 8))
        (deflate:inflate-gzip-stream in tar-stream)
        (finish-output tar-stream)
        (with-open-file (tar tar-file :element-type '(unsigned-byte 8))
          (loop for header = (read-tar-block tar)
                while header
                for typeflag = (aref header +tar-typeflag-offset+)
                for size = (parse-octal header +tar-size-offset+
                                        (+ +tar-size-offset+ +tar-size-length+))
                do (cond
                     ;; Regular file
                     ((or (= typeflag +tar-type-file+)
                          (= typeflag +tar-type-file-alt+))
                      (unless first-entry-name
                        (setf first-entry-name (tar-entry-name-from-header header)))
                      (extract-tar-file tar header destination))
                     ;; Directory
                     ((= typeflag +tar-type-directory+)
                      (unless first-entry-name
                        (setf first-entry-name (tar-entry-name-from-header header)))
                      (extract-tar-directory header destination))
                     ;; PAX headers - skip data
                     ((or (= typeflag +tar-type-pax-global+)
                          (= typeflag +tar-type-pax-extended+))
                      (skip-tar-data tar size))
                     ;; Other types (symlinks, etc.) - skip
                     (t
                      (skip-tar-data tar size)))))))
    (merge-pathnames (or first-entry-name "") destination)))
