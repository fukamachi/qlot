(defpackage #:qlot/distify/github
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-version
                #:source-version-prefix
                #:source-github-repos
                #:source-github-ref
                #:source-github-branch
                #:source-github-tag
                #:source-github-identifier
                #:source-github-url
                #:write-distinfo)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/distify
                #:releases.txt
                #:systems.txt)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:dexador)
  (:import-from #:yason)
  (:export #:distify-github))
(in-package #:qlot/distify/github)

(defun github-credentials ()
  (let ((github-token (uiop:getenv "GITHUB_TOKEN")))
    (when (and (stringp github-token)
               (not (string= github-token "")))
      (cons "x-access-token" github-token))))

(defun retrieve-from-github (repos action)
  (let ((cred (github-credentials)))
    (yason:parse
      (apply #'dex:get
             (format nil "https://api.github.com/repos/~A/~A" repos action)
             :want-stream t
             :proxy *proxy*
             (when cred
               `(:basic-auth ,cred))))))

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

(defun load-source-github-version (source)
  (setf (source-github-ref source)
        (retrieve-source-git-ref-from-github source))
  (setf (source-version source)
        (format nil "~A~A"
                (source-version-prefix source)
                (source-github-ref source))))

(defun distify-github (source destination &key distinfo-only)
  (load-source-github-version source)

  (let ((destination (truename destination)))
    (uiop:with-output-file (out (make-pathname :name (source-project-name source)
                                               :type "txt"
                                               :defaults destination)
                                :if-exists :supersede)
      (write-distinfo source out))

    (when distinfo-only
      (return-from distify-github destination))

    (let ((softwares-dir (merge-pathnames #P"softwares/" destination))
          (archives-dir (merge-pathnames #P"archives/" destination))
          (metadata-dir (merge-pathnames (format nil "~A/~A/"
                                                 (source-project-name source)
                                                 (source-version source))
                                         destination)))
      (mapc #'ensure-directories-exist (list softwares-dir archives-dir metadata-dir))

      (let ((archive (merge-pathnames
                       (format nil "~A-~A.tar.gz"
                               (source-project-name source)
                               (source-github-identifier source))
                       archives-dir))
            (cred (github-credentials)))
        (apply #'dex:fetch (source-github-url source) archive
               :proxy *proxy*
               :want-stream t
               :allow-other-keys t   ;; Old Dexador doesn't accept :basic-auth
               (when cred
                 `(:basic-auth ,cred)))

        (let ((extracted-source-directory (extract-tarball archive softwares-dir))
              (source-directory (merge-pathnames (format nil "~A-~A/"
                                                         (source-project-name source)
                                                         (source-github-identifier source))
                                                 softwares-dir)))
          (rename-file extracted-source-directory source-directory)
          (uiop:with-output-file (out (merge-pathnames "systems.txt" metadata-dir)
                                      :if-exists :supersede)
            (princ (systems.txt (source-project-name source)
                                source-directory)
                   out))
          (uiop:with-output-file (out (merge-pathnames "releases.txt" metadata-dir)
                                      :if-exists :supersede)
            (princ (releases.txt (source-project-name source)
                                 source-directory
                                 archive)
                   out)))))

    destination))
