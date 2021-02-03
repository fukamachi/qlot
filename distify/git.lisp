(defpackage #:qlot/distify/git
  (:use #:cl #:qlot/distify-protocol)
  (:import-from #:qlot/source/base
                #:source-project-name
                #:source-dist-name
                #:source-version
                #:source-version-prefix
                #:source-locked-p
                #:write-distinfo)
  (:import-from #:qlot/source/git
                #:source-git
                #:source-git-branch
                #:source-git-tag
                #:source-git-remote-url
                #:source-git-ref
                #:source-git-identifier)
  (:import-from #:qlot/utils/distify
                #:write-standard-metadata)
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
                #:digest-sequence))
(in-package #:qlot/distify/git)

;; Version-locking drives the clone, rather than cloning and
;; determining the version afterwards, so prepare is a no-op.
(defmethod prepare-source-for-dist ((source source-git) destination)
  (declare (ignore source destination))
  (values))

(defmethod lock-version ((source source-git) prep-dir)
  (declare (ignore prep-dir))
  (unless (source-locked-p source)
    (let ((ref (git-ref (source-git-remote-url source)
                        (or (source-git-tag source)
                            (source-git-branch source)
                            "HEAD"))))
      (setf (source-version source)
            (format nil "~A~A"
                    (source-version-prefix source)
                    ref))))

  (setf (source-git-ref source)
        (subseq (source-version source) (length (source-version-prefix source))))

  (source-version source))

(defmethod finalize-dist ((source source-git) prep-dir)
  (let* ((*default-pathname-defaults* (truename prep-dir))
         (softwares (merge-pathnames #P"softwares/"))
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
        (run-func-process 'write-standard-metadata
                          (source-project-name source)
                          source-directory
                          tarball
                          metadata)))))
