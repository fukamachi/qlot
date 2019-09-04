(defpackage #:qlot/distify
  (:use #:cl
        #:qlot/distify/ql
        #:qlot/distify/git
        #:qlot/distify/http
        #:qlot/distify/github
        #:qlot/distify/dist)
  (:import-from #:qlot/source
                #:source-ql
                #:source-git
                #:source-http
                #:source-dist)
  (:export #:distify))
(in-package #:qlot/distify)

(defun distify (source-or-sources destination &key distinfo-only)
  (check-type destination pathname)
  (dolist (source (if (listp source-or-sources)
                      source-or-sources
                      (list source-or-sources)))
    (funcall (etypecase source
               (source-ql #'distify-ql)
               (source-git #'distify-git)
               (source-http #'distify-http)
               (source-dist #'distify-dist))
             source
             destination
             :distinfo-only distinfo-only))
  destination)
