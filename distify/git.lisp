(defpackage #:qlot/distify/git
  (:use #:cl)
  (:import-from #:qlot/source/base
                #:source-project-name
                #:source-dist-name
                #:source-version
                #:source-version-prefix)
  (:import-from #:qlot/source/git
                #:source-git
                #:source-git-branch
                #:source-git-tag
                #:source-git-remote-access-url
                #:source-git-ref
                #:source-git-identifier)
  (:import-from #:qlot/utils/distify
                #:releases.txt
                #:systems.txt
                #:write-source-distinfo)
  (:import-from #:qlot/utils/git
                #:git-clone
                #:git-ref
                #:create-git-tarball)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:directory-system-files)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file
                #:digest-sequence)
  (:export #:distify-git))
(in-package #:qlot/distify/git)

(defun load-source-git-version (source)
  (unless (ignore-errors (source-git-ref source))
    (let ((ref (git-ref (source-git-remote-access-url source)
                        (or (source-git-tag source)
                            (source-git-branch source)
                            "HEAD"))))
      (setf (source-git-ref source) ref)))
  (unless (ignore-errors (source-version source))
    (setf (source-version source)
          (format nil "~A~A"
                  (source-version-prefix source)
                  (source-git-ref source)))))

(defun write-metadata-files (source destination source-directory tarball)
  (uiop:with-output-file (out (merge-pathnames "systems.txt" destination)
                              :if-exists :supersede)
    (princ (systems.txt (source-project-name source) source-directory) out))

  (uiop:with-output-file (out (merge-pathnames "releases.txt" destination)
                              :if-exists :supersede)
    (princ (releases.txt (source-project-name source) (source-version source)
                         source-directory tarball) out)))

(defun distify-git (source destination &key distinfo-only)
  (check-type source source-git)
  (load-source-git-version source)

  (let ((*default-pathname-defaults*
          (uiop:ensure-absolute-pathname
            (merge-pathnames
              (make-pathname :directory `(:relative ,(source-project-name source) ,(source-version source)))
              destination))))
    (ensure-directories-exist *default-pathname-defaults*)

    (write-source-distinfo source destination)

    (when distinfo-only
      (return-from distify-git))

    (let ((archive-file (merge-pathnames "archive.tar.gz")))
      (cond
        ((not (uiop:file-exists-p archive-file))
         (with-tmp-directory (source-directory)
           (git-clone (source-git-remote-access-url source)
                      source-directory
                      :checkout-to (or (source-git-branch source)
                                       (source-git-tag source))
                      :ref (source-git-ref source))

           (create-git-tarball source-directory
                               archive-file
                               (source-git-ref source))
           (unless (and (uiop:file-exists-p "systems.txt")
                        (uiop:file-exists-p "releases.txt"))
             (write-metadata-files source *default-pathname-defaults* source-directory archive-file))))
        ((not (and (uiop:file-exists-p "systems.txt")
                   (uiop:file-exists-p "releases.txt")))
         (with-tmp-directory (softwares-dir)
           (let ((source-directory (extract-tarball archive-file softwares-dir)))
             (write-metadata-files source *default-pathname-defaults* source-directory archive-file))))))

    *default-pathname-defaults*))
