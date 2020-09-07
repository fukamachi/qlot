(defpackage #:qlot/distify/github
  (:use #:cl #:qlot/distify-protocol)
  (:import-from #:qlot/source/github
                #:source-github)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-version
                #:source-version-prefix
                #:source-locked-p
                #:source-github-url
                #:source-github-repos
                #:source-github-ref
                #:source-github-branch
                #:source-github-tag
                #:source-github-identifier
                #:write-distinfo)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/distify
                #:write-standard-metadata)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:dexador)
  (:import-from #:yason))
(in-package #:qlot/distify/github)

(defun retrieve-from-github (repos action)
  (let ((github-token (uiop:getenv "GITHUB_TOKEN")))
    (yason:parse
     (apply #'dex:get
            (format nil "https://api.github.com/repos/~A/~A" repos action)
            :want-stream t
            :proxy *proxy*
            (if (and (stringp github-token)
                     (not (string= github-token "")))
                (list :basic-auth (cons github-token "x-oauth-basic"))
                '())))))

(defun retrieve-source-git-ref-from-github (source)
  (labels ((find-ref (results name)
             (let ((result (find-if (lambda (result)
                                      (string= (gethash "name" result) name))
                                    results)))
               (and result
                    (gethash "sha" (gethash "commit" result)))))
           (get-ref (action name)
             (find-ref (retrieve-from-github (source-github-repos source) action)
                       name)))
    (cond
      ((source-github-ref source))
      ((source-github-branch source)
       (get-ref "branches" (source-github-branch source)))
      ((source-github-tag source)
       (get-ref "tags" (source-github-tag source)))
      (t (get-ref "branches" "master")))))


;; Version-locking drives which archive we download later, rather than
;; fetching the archive and reading the version from it, so prepare is
;; a no-op.
(defmethod prepare-source-for-dist ((source source-github) destination)
  (declare (ignore source destination))
  (values))

(defmethod lock-version ((source source-github) prep-dir)
  (declare (ignore prep-dir))
  (unless (source-locked-p source)
    (let ((ref (retrieve-source-git-ref-from-github source)))
      (setf (source-version source)
            (format nil "~A~A"
                    (source-version-prefix source)
                    ref))))

  (setf (source-github-ref source)
        (subseq (source-version source) (length (source-version-prefix source))))

  (source-version source))

(defun distify-github-source (source prep-dir)
  (check-type source source-github)
  (let* ((destination (truename prep-dir))
         (softwares-dir (merge-pathnames #P"softwares/" destination))
         (archives-dir (merge-pathnames #P"archives/" destination))
         (metadata-dir (merge-pathnames (format nil "~A/~A/"
                                                (source-project-name source)
                                                (source-version source))
                                        destination))
         (archive (merge-pathnames (format nil "~A-~A.tar.gz"
                                           (source-project-name source)
                                           (source-github-identifier source))
                                   archives-dir)))
    (mapc #'ensure-directories-exist (list softwares-dir archives-dir metadata-dir))

    (dex:fetch (source-github-url source) archive
               :if-exists :supersede
               :proxy *proxy*)

    (let ((extracted-source-directory (extract-tarball archive softwares-dir))
          (source-directory (merge-pathnames (format nil "~A-~A/"
                                                     (source-project-name source)
                                                     (source-github-identifier source))
                                             softwares-dir)))
      (rename-file extracted-source-directory source-directory)
      (run-func-process 'write-standard-metadata
                        (source-project-name source)
                        source-directory
                        archive
                        metadata-dir))))

(defmethod distify-source ((source source-github) prep-dir &key distinfo-only)
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
  (distify-github-source source prep-dir)
  (values))
