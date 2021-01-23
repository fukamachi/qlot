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

;; :DISTINFO-ONLY is an old option formerly used for a performance
;; optimization now done more cleanly by using CREATE-VERSIONED-DIST
;; on its own. It's still here for backwards compatibility, but
;; there's not much reason to use it.
(defun distify (source-or-sources destination &key distinfo-only)
  (check-type destination pathname)
  (dolist (source (if (listp source-or-sources)
                      source-or-sources
                      (list source-or-sources)))
    (write-source-distinfo source destination)
    (unless distinfo-only
      (finalize-dist source destination)))
  destination)
