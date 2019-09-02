(defpackage #:qlot/distify/http
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-http-url
                #:source-http-archive-md5
                #:source-version
                #:write-distinfo)
  (:import-from #:qlot/utils/distify
                #:releases.txt
                #:systems.txt)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:dexador)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file)
  (:export #:distify-http))
(in-package #:qlot/distify/http)

(defun distify-http (source destination &key distinfo-only)
  (let* ((destination (truename destination))
         (archives-dir (merge-pathnames #P"archives/" destination))
         (softwares-dir (merge-pathnames #P"softwares/" destination)))
    (let ((archive (merge-pathnames (format nil "~A.tar.gz" (source-project-name source))
                                    archives-dir)))
      (unless (uiop:file-exists-p archive)
        (ensure-directories-exist archive)
        (dex:fetch (source-http-url source) archive
                   :if-exists :supersede))

      (let ((archive-md5 (byte-array-to-hex-string
                           (digest-file :md5 archive))))
        (when (and (source-http-archive-md5 source)
                   (not (string= (source-http-archive-md5 source) archive-md5)))
          (cerror "Ignore and continue."
                  "File MD5 of ~S is different from ~S.~%The content seems to have changed."
                  (source-http-url source)
                  archive-md5))

        (setf (source-http-archive-md5 source) archive-md5)
        (setf (source-version source)
              (format nil "http-~A" archive-md5)))

      (uiop:with-output-file (out (make-pathname :name (source-project-name source)
                                                 :type "txt"
                                                 :defaults destination)
                                  :if-exists :supersede)
        (write-distinfo source out))

      (when distinfo-only
        (return-from distify-http destination))

      (let ((metadata (merge-pathnames (format nil "~A/~A/"
                                               (source-project-name source)
                                               (source-version source))
                                       destination))
            (source-directory (extract-tarball archive softwares-dir)))
        (ensure-directories-exist metadata)

        (uiop:with-output-file (out (merge-pathnames "systems.txt" metadata)
                                    :if-exists :supersede)
          (princ (systems.txt (source-project-name source)
                              source-directory)
                 out))
        (uiop:with-output-file (out (merge-pathnames "releases.txt" metadata)
                                    :if-exists :supersede)
          (princ (releases.txt (source-project-name source)
                               source-directory
                               archive)
                 out))))

    destination))
