(defpackage #:qlot/distify
  (:use #:cl
        #:qlot/distify/ql
        #:qlot/distify/git
        #:qlot/distify/http
        #:qlot/distify/github
        #:qlot/distify/dist)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-git
                #:source-github
                #:source-http
                #:source-dist
                #:source-dist-project)
  (:import-from #:qlot/logger
                #:progress)
  (:export #:distify))
(in-package #:qlot/distify)

(defun distify (source-or-sources destination &key distinfo-only)
  (check-type destination pathname)
  (dolist (source (if (listp source-or-sources)
                      source-or-sources
                      (list source-or-sources)))
    (funcall (etypecase source
               (source-dist #'distify-dist)
               (source-dist-project #'distify-ql)
               (source-git #'distify-git)
               (source-github #'distify-github)
               (source-http #'distify-http))
             source
             destination
             :distinfo-only distinfo-only))
  (progress "Generated dist files.")
  destination)
