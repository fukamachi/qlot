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
                #:source-git-remote-url
                #:source-git-remote-access-url
                #:source-git-ref
                #:source-git-identifier)
  (:import-from #:qlot/source/ql
                #:source-ql-upstream)
  (:import-from #:qlot/logger
                #:progress)
  (:import-from #:qlot/utils/distify
                #:releases.txt
                #:systems.txt
                #:write-source-distinfo)
  (:import-from #:qlot/utils/git
                #:git-clone
                #:git-ref
                #:create-git-tarball)
  (:import-from #:qlot/utils/quickdocs
                #:project-upstream-url)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
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

  (when (typep source 'source-ql-upstream)
    (unless (source-git-remote-url source)
      (progress "Determining the upstream URL.")
      (setf (source-git-remote-url source)
            (project-upstream-url (source-project-name source)))))

  (progress "Determining the project version.")
  (load-source-git-version source)

  (let ((*default-pathname-defaults*
          (uiop:ensure-absolute-pathname
            (merge-pathnames
              (make-pathname :directory `(:relative ,(source-project-name source) ,(source-version source)))
              destination))))
    (ensure-directories-exist *default-pathname-defaults*)

    (progress "Writing the distinfo.")
    (write-source-distinfo source destination)

    (when distinfo-only
      (return-from distify-git))

    (let ((archive-file (merge-pathnames "archive.tar.gz")))
      (cond
        ((not (uiop:file-exists-p archive-file))
         (with-tmp-directory (softwares-dir)
           (let ((source-directory (uiop:ensure-directory-pathname
                                     (merge-pathnames (format nil "~A-~A"
                                                              (source-project-name source)
                                                              (source-git-identifier source))
                                                      softwares-dir))))
             (progress "Running git clone.")
             (git-clone (source-git-remote-access-url source)
                        source-directory
                        :checkout-to (or (source-git-branch source)
                                         (source-git-tag source))
                        :ref (source-git-ref source))

             (progress "Creating a tarball.")
             (create-git-tarball source-directory
                                 archive-file
                                 (source-git-ref source))
             (unless (and (uiop:file-exists-p "systems.txt")
                          (uiop:file-exists-p "releases.txt"))
               (progress "Writing metadata files.")
               (write-metadata-files source *default-pathname-defaults* source-directory archive-file)))))
        ((not (and (uiop:file-exists-p "systems.txt")
                   (uiop:file-exists-p "releases.txt")))
         (with-tmp-directory (softwares-dir)
           (progress "Extracting a tarball.")
           (let ((source-directory (extract-tarball archive-file softwares-dir)))
             (progress "Writing metadata files.")
             (write-metadata-files source *default-pathname-defaults* source-directory archive-file))))))

    *default-pathname-defaults*))
