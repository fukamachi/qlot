(defpackage #:qlot/utils/quickdocs
  (:use #:cl)
  (:import-from #:qlot/http)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
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
           (handler-case
               (qlot/http:get (format nil "https://api.quickdocs.org/projects/~A"
                                      (quri:url-encode project-name)))
             (dex:http-request-not-found ()
               (error 'qlot-simple-error
                      :format-control "'~A' is not found."
                      :format-arguments (list project-name)))))
         (upstream-url (gethash "upstream_url" (yason:parse project-info))))
    (unless (git-url-p upstream-url)
      (error "Not supported upstream URL: ~A" upstream-url))
    upstream-url))
