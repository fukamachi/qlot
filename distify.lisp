(defpackage #:qlot/distify
  (:use #:cl
        #:qlot/distify-protocol)
  ;; Require the various source distification modules.
  (:import-from #:qlot/distify/dist)
  (:import-from #:qlot/distify/ql)
  (:import-from #:qlot/distify/git)
  (:import-from #:qlot/distify/github)
  (:import-from #:qlot/distify/http)
  (:export #:distify))
(in-package #:qlot/distify)

(defun distify (source-or-sources destination &key distinfo-only)
  (check-type destination pathname)
  (dolist (source (if (listp source-or-sources)
                      source-or-sources
                      (list source-or-sources)))
    (prepare-source-for-dist source destination)
    (lock-version source destination)
    (distify-source source destination :distinfo-only distinfo-only))
  destination)
