(defpackage #:qlot/utils/quickdocs
  (:use #:cl)
  (:import-from #:qlot/utils/http)
  (:import-from #:quri)
  (:import-from #:yason)
  (:export #:project-upstream-url))
(in-package #:qlot/utils/quickdocs)

(defun git-url-p (url)
  ;; Currently supports GitHub and GitLab
  (find (quri:uri-host (quri:uri url))
        '("github.com"
          "gitlab.com"
          "gitlab.common-lisp.net")
        :test #'string=))

(defun project-upstream-url (project-name)
  (let* ((project-info
           (qdex:get (format nil "https://api.quickdocs.org/projects/~A"
                             (quri:url-encode project-name))))
         (upstream-url (gethash "upstream_url" (yason:parse project-info))))
    (unless (git-url-p upstream-url)
      (error "Not supported upstream URL: ~A" upstream-url))
    upstream-url))
