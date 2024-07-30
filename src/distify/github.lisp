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
                #:source-github-url)
  (:import-from #:qlot/progress
                #:progress)
  (:import-from #:qlot/utils/distify
                #:releases.txt
                #:systems.txt
                #:write-source-distinfo)
  (:import-from #:qlot/utils/archive
                #:extract-tarball)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:import-from #:qlot/utils
                #:split-with)
  (:import-from #:qlot/http)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:import-from #:yason)
  (:export #:distify-github))
(in-package #:qlot/distify/github)

(defun github-credentials ()
  (let ((github-token (uiop:getenvp "GITHUB_TOKEN")))
    (when github-token
      (cons "x-access-token" github-token))))

(defun retrieve-from-github (repos &optional action)
  (let ((cred (github-credentials)))
    (handler-case
        (yason:parse
         (apply #'qlot/http:get
                (format nil "https://api.github.com/repos/~A~@[~A~]" repos action)
                :want-stream t
                (when cred
                  `(:basic-auth ,cred))))
      (dex:http-request-not-found ()
        (error 'qlot-simple-error
               :format-control "'~A' is not found in GitHub."
               :format-arguments (list repos))))))

(defun retrieve-default-branch (repos)
  (gethash "default_branch" (retrieve-from-github repos)))

(defun retrieve-source-git-ref-from-github (source)
  (labels ((get-tag-ref (name)
             (reduce #'gethash
                     '("sha" "object")
                     :initial-value
                     (retrieve-from-github (source-github-repos source)
                                           (format nil "/git/refs/tags/~A" name))
                     :from-end t))
           (get-branch-ref (name)
             (reduce #'gethash
                     '("sha" "commit")
                     :initial-value
                     (retrieve-from-github (source-github-repos source)
                                           (format nil "/branches/~A" name))
                     :from-end t)))
    (cond
      ((source-github-ref source))
      ((source-github-branch source)
       (get-branch-ref (source-github-branch source)))
      ((source-github-tag source)
       (get-tag-ref (source-github-tag source)))
      (t
       (get-branch-ref (retrieve-default-branch (source-github-repos source)))))))

(defun load-source-github-version (source)
  (unless (ignore-errors (source-github-ref source))
    (progress "Retrieving the git ref from GitHub.")
    (setf (source-github-ref source)
          (retrieve-source-git-ref-from-github source)))
  (unless (ignore-errors (source-version source))
    (setf (source-version source)
          (format nil "~A~A"
                  (source-version-prefix source)
                  (source-github-ref source)))))

(defun distify-github (source destination &key distinfo-only)
  (load-source-github-version source)

  (let ((*default-pathname-defaults*
          (uiop:ensure-absolute-pathname
           (merge-pathnames
            (make-pathname :directory
                           `(:relative ,@(append (split-with #\/ (source-project-name source))
                                                 (list (source-version source)))))
            destination))))
    (ensure-directories-exist *default-pathname-defaults*)

    (progress "Writing the distinfo to ~S." destination)
    (write-source-distinfo source destination)
    (progress "Wrote the distinfo to ~S." destination)

    (when distinfo-only
      (return-from distify-github))

    (with-tmp-directory (softwares-dir)
      (let ((archive-file (merge-pathnames "archive.tar.gz")))
        (unless (uiop:file-exists-p archive-file)
          (progress "Downloading ~S." (source-github-url source))
          (let ((cred (github-credentials)))
            (apply #'qlot/http:fetch (source-github-url source) archive-file
                   (when cred
                     `(:basic-auth ,cred))))
          (progress "Downloaded ~S." (source-github-url source)))

        (unless (and (uiop:file-exists-p "systems.txt")
                     (uiop:file-exists-p "releases.txt"))
          (progress "Extracting a tarball.")
          (let ((source-directory (extract-tarball archive-file softwares-dir)))
            (progress "Writing systems.txt.")
            (uiop:with-output-file (out "systems.txt" :if-exists :supersede)
              (princ (systems.txt (source-project-name source)
                                  source-directory)
                     out))
            (progress "Writing releases.txt.")
            (uiop:with-output-file (out "releases.txt" :if-exists :supersede)
              (princ (releases.txt (source-project-name source)
                                   (source-version source)
                                   source-directory
                                   archive-file)
                     out))))))

    *default-pathname-defaults*))
