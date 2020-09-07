(defpackage #:qlot/utils/distify
  (:use #:cl)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:directory-system-files)
  (:import-from #:ironclad)
  (:export #:releases.txt
           #:systems.txt))
(in-package #:qlot/utils/distify)

(defun normalize-pathname (path)
  "Return the pathname PATH resolved to a normalized pathname in the filesystem.
Does not resolve symlinks, but PATH must actually exist in the filesystem."
  (when (wild-pathname-p path)
    (error "Wild pathnames cannot be normalized."))
  (first (uiop:directory* path)))

(defun releases.txt (project-name source-directory tarball-file)
  (let* ((source-directory (normalize-pathname source-directory))
         (prefix (car (last (pathname-directory source-directory)))))
    (multiple-value-bind (size file-md5 content-sha1)
        (with-open-file (in tarball-file :element-type '(unsigned-byte 8))
          (values (file-length in)
                  (ironclad:byte-array-to-hex-string
                    (ironclad:digest-file :md5 tarball-file))
                  (ironclad:byte-array-to-hex-string
                    (ironclad:digest-stream :sha1 in))))
      (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%~A ~A ~A ~A ~A ~A~{ ~A~}~%"
              project-name
              (format nil "qlot://localhost/archives/~A"
                      (file-namestring tarball-file))
              size
              file-md5
              content-sha1
              prefix
              (let (result)
                (dolist (system-file (directory-system-files source-directory))
                  (push (subseq (namestring system-file)
                                (length (namestring source-directory)))
                        result))
                result)))))

(defun systems.txt (project-name source-directory)
  (with-output-to-string (s)
    (format s "# project system-file system-name [dependency1..dependencyN]~%")
    (with-directory (system-file system-name dependencies) source-directory
      (format s "~A ~A ~A~{ ~A~}~%"
              project-name
              (pathname-name system-file)
              system-name
              dependencies))))
