(defpackage #:qlot/distify/dist
  (:use #:cl #:qlot/distify-protocol)
  (:import-from #:qlot/source
                #:source-dist
                #:source-distribution
                #:source-distinfo-url
                #:source-project-name)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/ql
                #:get-distinfo-url)
  (:import-from #:dexador))
(in-package #:qlot/distify/dist)

;; Distification for whole-dist sources, like `ql :all` or `dist
;; ultralisp`.

(defmethod prepare-source-for-dist ((source source-dist) destination)
  (declare (ignore source destination))
  ;; No-op.
  (values))

(defmethod lock-version ((source source-dist) prep-dir)
  (declare (ignore prep-dir))
  (unless (source-distinfo-url source)
    (setf (source-distinfo-url source)
          (get-distinfo-url (source-distribution source)
                            (slot-value source 'qlot/source/dist::%version))))
  (source-distinfo-url source))

(defmethod write-source-distinfo ((source source-dist) prep-dir)
  (let ((destination (truename prep-dir)))
    (prepare-source-for-dist source destination)
    (lock-version source destination)
    (dex:fetch (source-distinfo-url source)
               (make-pathname :name (source-project-name source)
                              :type "txt"
                              :defaults destination)
               :if-exists :supersede
               :proxy *proxy*)
    destination))

(defmethod finalize-dist ((source source-dist) prep-dir)
  ;; We do not need to generate system or release data for a dist source.
  (values))
