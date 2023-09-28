(defpackage #:qlot/distify/http
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-http-url
                #:source-http-archive-md5
                #:source-version
                #:source-version-prefix)
  (:import-from #:qlot/logger
                #:progress)
  (:import-from #:qlot/utils/distify
                #:releases.txt
                #:systems.txt
                #:write-source-distinfo)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:import-from #:qlot/utils/http)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file)
  (:export #:distify-http))
(in-package #:qlot/distify/http)

(defun source-metadata-destination (source destination)
  (uiop:ensure-absolute-pathname
    (merge-pathnames
      (make-pathname :directory `(:relative ,(source-project-name source) ,(source-version source)))
      destination)))

(defun distify-http (source destination &key distinfo-only)
  (let ((distinfo.txt (merge-pathnames
                       (make-pathname :name (source-project-name source)
                                      :type "txt")
                       destination)))

    (when (and (not (ignore-errors (source-version source)))
               (uiop:file-exists-p distinfo.txt))
      (let ((distinfo (parse-distinfo-file distinfo.txt)))
        (setf (source-version source)
              (cdr (assoc "version" distinfo :test 'equal)))
        (setf (source-http-archive-md5 source)
              (subseq (source-version source)
                      (length (source-version-prefix source)))))))

  (unless (and (ignore-errors (source-version source))
               (uiop:file-exists-p (merge-pathnames "archive.tar.gz"
                                                    (source-metadata-destination source destination))))
    (uiop:with-temporary-file (:pathname tmp-archive :direction :io)
      (progress "Downloading ~S" (source-http-url source))
      (qdex:fetch (source-http-url source) tmp-archive)

      (progress "Calculating the MD5 of the archive.")
      (let ((archive-md5 (byte-array-to-hex-string
                          (digest-file :md5 tmp-archive))))
        (when (and (source-http-archive-md5 source)
                   (not (string= (source-http-archive-md5 source) archive-md5)))
          (cerror "Ignore and continue."
                  "File MD5 of ~S is different from ~S.~%The content seems to have changed."
                  (source-http-url source)
                  archive-md5))

        (setf (source-http-archive-md5 source) archive-md5)
        (setf (source-version source)
              (format nil "~A~A"
                      (source-version-prefix source)
                      archive-md5)))

      (let ((archive-file (merge-pathnames "archive.tar.gz"
                                           (source-metadata-destination source destination))))
        (ensure-directories-exist archive-file)
        (unless (uiop:file-exists-p archive-file)
          (rename-file tmp-archive archive-file))
        (progress "Downloaded ~S." archive-file))))

  (let* ((*default-pathname-defaults*
           (source-metadata-destination source destination))
         (archive-file (merge-pathnames "archive.tar.gz")))
    (ensure-directories-exist *default-pathname-defaults*)

    (progress "Writing the distinfo to ~S." destination)
    (write-source-distinfo source destination)

    (when distinfo-only
      (return-from distify-http))

    (unless (and (uiop:file-exists-p "systems.txt")
                 (uiop:file-exists-p "releases.txt"))
      (with-tmp-directory (softwares-dir)
        (let ((source-directory (extract-tarball archive-file softwares-dir)))
          (progress "Writing the systems.txt.")
          (uiop:with-output-file (out "systems.txt" :if-exists :supersede)
            (princ (systems.txt (source-project-name source)
                                source-directory)
                   out))
          (progress "Writing the releases.txt.")
          (uiop:with-output-file (out "releases.txt" :if-exists :supersede)
            (princ (releases.txt (source-project-name source)
                                 (source-version source)
                                 source-directory
                                 archive-file)
                   out)))))

    *default-pathname-defaults*))
