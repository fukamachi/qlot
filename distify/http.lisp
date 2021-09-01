(defpackage #:qlot/distify/http
  (:use #:cl)
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
                #:releases.txt
                #:systems.txt)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:import-from #:dexador)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file)
  (:export #:distify-http))
(in-package #:qlot/distify/http)

(defun distify-http (source destination)
  (uiop:with-temporary-file (:pathname tmp-archive :direction :io)
    (dex:fetch (source-http-url source) tmp-archive
               :if-exists :supersede
               :proxy *proxy*)

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

    (let* ((*default-pathname-defaults*
             (uiop:ensure-absolute-pathname
               (merge-pathnames
                 (make-pathname :directory `(:relative ,(source-project-name source) ,(source-version source)))
                 destination)))
           (archive-file (merge-pathnames "archive.tar.gz")))
      (ensure-directories-exist *default-pathname-defaults*)
      (unless (uiop:file-exists-p archive-file)
        (uiop:copy-file tmp-archive archive-file))

      (uiop:with-output-file (out (merge-pathnames
                                    (make-pathname :name (source-project-name source)
                                                   :type "txt")
                                    destination)
                                  :if-exists :supersede)
        (write-distinfo source out))

      (unless (and (uiop:file-exists-p "systems.txt")
                   (uiop:file-exists-p "releases.txt"))
        (with-tmp-directory (softwares-dir)
          (let ((source-directory (extract-tarball archive-file softwares-dir)))

            (uiop:with-output-file (out "systems.txt" :if-exists :supersede)
              (princ (systems.txt (source-project-name source)
                                  source-directory)
                     out))
            (uiop:with-output-file (out "releases.txt" :if-exists :supersede)
              (princ (releases.txt (source-project-name source)
                                   (source-version source)
                                   source-directory
                                   archive-file)
                     out)))))

      *default-pathname-defaults*)))
