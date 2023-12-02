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
                #:write-source-distinfo
                #:load-version-from-distinfo)
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

;; TODO: Move to qlot/utils/distify
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

  (let ((distinfo.txt (merge-pathnames
                       (make-pathname :name (source-project-name source)
                                      :type "txt")
                       destination)))
    (progress "Determining the project version.")
    (cond
      ((uiop:file-exists-p distinfo.txt)
       (load-version-from-distinfo source distinfo.txt)
       (setf (source-git-ref source)
             (subseq (source-version source)
                     (length (source-version-prefix source)))))
      (t
       (load-source-git-version source)))

    (unless (uiop:file-exists-p distinfo.txt)
      (progress "Writing the distinfo.")
      (write-source-distinfo source destination))

    (when distinfo-only
      (return-from distify-git destination))

    (let ((workspace (uiop:ensure-absolute-pathname
                      (merge-pathnames
                       (make-pathname :directory `(:relative ,(source-project-name source) ,(source-version source)))
                       destination))))
      (ensure-directories-exist workspace)
      (with-tmp-directory (softwares-dir)
        (let ((archive-file (merge-pathnames "archive.tar.gz" workspace))
              (source-directory (uiop:ensure-directory-pathname
                                 (merge-pathnames (format nil "~A-~A"
                                                          (source-project-name source)
                                                          (source-git-identifier source))
                                                  softwares-dir))))
          (unless (uiop:file-exists-p archive-file)
            (progress "Running git clone.")
            (git-clone (source-git-remote-access-url source)
                       source-directory
                       :checkout-to (or (source-git-branch source)
                                        (source-git-tag source))
                       :ref (source-git-ref source))

            (progress "Creating a tarball.")
            (create-git-tarball source-directory
                                archive-file
                                (source-git-ref source)))

          (unless (and (uiop:file-exists-p (merge-pathnames "systems.txt" workspace))
                       (uiop:file-exists-p (merge-pathnames "releases.txt" workspace)))
            (progress "Writing metadata files.")
            (write-metadata-files source workspace source-directory archive-file))))
      workspace)))
