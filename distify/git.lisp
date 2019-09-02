(defpackage #:qlot/distify/git
  (:use #:cl)
  (:import-from #:qlot/source/base
                #:source-project-name
                #:source-dist-name
                #:source-version
                #:write-distinfo)
  (:import-from #:qlot/source/git
                #:source-git
                #:source-git-branch
                #:source-git-tag
                #:source-git-remote-url
                #:source-git-ref
                #:source-git-identifier)
  (:import-from #:qlot/utils/distify
                #:releases.txt
                #:systems.txt)
  (:import-from #:qlot/utils/git
                #:git-clone
                #:git-ref
                #:create-git-tarball)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:directory-system-files)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file
                #:digest-sequence)
  (:export #:distify-git))
(in-package #:qlot/distify/git)

(defun load-source-git-version (source)
  (unless (ignore-errors (source-git-ref source))
    (let ((ref (git-ref (source-git-remote-url source)
                        (or (source-git-tag source)
                            (source-git-branch source)
                            "HEAD"))))
      (setf (source-git-ref source) ref)))
  (unless (ignore-errors (source-version source))
    (setf (source-version source)
          (format nil "git-~A"
                  (source-git-ref source)))))

(defun distify-git (source destination &key distinfo-only)
  (check-type source source-git)
  (load-source-git-version source)

  (let ((*default-pathname-defaults* (truename destination)))
    (uiop:with-output-file (out (make-pathname :name (source-project-name source)
                                               :type "txt")
                                :if-exists :supersede)
      (write-distinfo source out))
    (when distinfo-only
      (return-from distify-git destination))

    (let ((softwares (merge-pathnames #P"softwares/"))
          (archives (merge-pathnames #P"archives/"))
          (metadata (merge-pathnames (format nil "~A/~A/"
                                             (source-project-name source)
                                             (source-version source)))))
      (ensure-directories-exist softwares)
      (ensure-directories-exist archives)
      (ensure-directories-exist metadata)
      (let ((source-directory (merge-pathnames (format nil "~A-~A/"
                                                       (source-project-name source)
                                                       (source-git-identifier source))
                                               softwares)))
        (git-clone (source-git-remote-url source)
                   source-directory
                   :checkout-to (or (source-git-branch source)
                                    (source-git-tag source)
                                    "master")
                   :ref (source-git-ref source))

        (let ((tarball (create-git-tarball source-directory
                                           archives
                                           (source-git-ref source))))
          (uiop:with-output-file (out (merge-pathnames "systems.txt" metadata))
            (princ (systems.txt (source-project-name source) source-directory) out))

          (uiop:with-output-file (out (merge-pathnames "releases.txt" metadata))
            (princ (releases.txt (source-project-name source) source-directory tarball) out)))))

    *default-pathname-defaults*))
