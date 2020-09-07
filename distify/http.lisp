(defpackage #:qlot/distify/http
  (:use #:cl #:qlot/distify-protocol)
  (:import-from #:qlot/source/http
                #:source-http)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-http-url
                #:source-http-archive-md5
                #:source-version
                #:source-version-prefix
                #:write-distinfo)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/distify
                #:write-standard-metadata)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:dexador)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file))
(in-package #:qlot/distify/http)

(defun archive-path (source prep-dir)
  (let* ((destination (truename prep-dir))
         (archives (merge-pathnames #P"archives/" destination)))
    (merge-pathnames (format nil "~A.tar.gz" (source-project-name source))
                     archives)))

(defmethod prepare-source-for-dist ((source source-http) destination)
  (let* ((archive (archive-path source destination)))
    (unless (uiop:file-exists-p archive)
      (ensure-directories-exist archive)
      (dex:fetch (source-http-url source) archive
                 :if-exists :supersede
                 :proxy *proxy*))
    (values)))

(defmethod lock-version ((source source-http) prep-dir)
  (let* ((archive (archive-path source prep-dir))
         (archive-md5 (byte-array-to-hex-string
                       (digest-file :md5 archive))))
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
                  archive-md5))))

(defun distify-http-source (source prep-dir)
  (check-type source source-http)
  (let* ((destination (truename prep-dir))
         (softwares-dir (merge-pathnames #P"softwares/" destination))
         (archive (archive-path source destination))
         (metadata (merge-pathnames (format nil "~A/~A/"
                                            (source-project-name source)
                                            (source-version source))
                                    destination))
         (source-directory (extract-tarball archive softwares-dir)))

    (ensure-directories-exist metadata)

    (run-func-process 'write-standard-metadata
                      (source-project-name source)
                      source-directory
                      archive
                      metadata)))

(defmethod distify-source ((source source-http) prep-dir &key distinfo-only)
  ;; Write distinfo.
  (let ((destination (truename prep-dir)))
    (uiop:with-output-file (out (make-pathname :name (source-project-name source)
                                               :type "txt"
                                               :defaults destination)
                                :if-exists :supersede)
      (write-distinfo source out))
    (when distinfo-only
      (return-from distify-source destination)))

  ;; If not distinfo-only, construct the rest of the dist.
  (distify-http-source source prep-dir)
  (values))
