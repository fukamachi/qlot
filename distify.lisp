(defpackage #:qlot/distify
  (:use #:cl
        #:qlot/distify/git
        #:qlot/distify/ql)
  (:import-from #:qlot/source
                #:source-ql
                #:source-ql-all
                #:source-git)
  (:export #:distify))
(in-package #:qlot/distify)

(defun distify (source-or-sources destination)
  (check-type destination pathname)
  (dolist (source (if (listp source-or-sources)
                      source-or-sources
                      (list source-or-sources)))
    (funcall (etypecase source
               (source-ql #'distify-ql)
               (source-ql-all #'distify-ql-all)
               (source-git #'distify-git))
             source
             destination))
  destination)
