(defpackage #:qlot/distify
  (:use #:cl
        #:qlot/distify/git
        #:qlot/distify/ql)
  (:import-from #:qlot/source
                #:source-ql
                #:source-ql-all
                #:source-git)
  (:import-from #:qlot/parser
                #:parse-qlfile
                #:parse-qlfile-lock)
  (:export #:distify
           #:distify-qlfile
           #:distify-qlfile-lock))
(in-package #:qlot/distify)

(defun distify (source-or-sources destination &key distinfo-only)
  (check-type destination pathname)
  (dolist (source (if (listp source-or-sources)
                      source-or-sources
                      (list source-or-sources)))
    (funcall (etypecase source
               (source-ql #'distify-ql)
               (source-ql-all #'distify-ql-all)
               (source-git #'distify-git))
             source
             destination
             :distinfo-only distinfo-only))
  destination)

(defun distify-qlfile (qlfile destination &key distinfo-only)
  (distify (parse-qlfile qlfile) destination
           :distinfo-only distinfo-only))

(defun distify-qlfile-lock (qlfile-lock destination &key distinfo-only)
  (distify (parse-qlfile-lock qlfile-lock) destination
           :distinfo-only distinfo-only))
