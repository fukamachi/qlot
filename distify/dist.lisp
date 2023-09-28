(defpackage #:qlot/distify/dist
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-dist
                #:source-distribution
                #:source-distinfo-url
                #:source-project-name)
  (:import-from #:qlot/logger
                #:progress)
  (:import-from #:qlot/utils/distify
                #:get-distinfo-url)
  (:import-from #:qlot/utils/http)
  (:export #:distify-dist))
(in-package #:qlot/distify/dist)

(defun distify-dist (source destination &key distinfo-only)
  (declare (ignore distinfo-only))
  (check-type source source-dist)
  (progress "Determining the distinfo URL.")
  (unless (source-distinfo-url source)
    (setf (source-distinfo-url source)
          (get-distinfo-url (source-distribution source)
                            (slot-value source 'qlot/source/dist::%version))))
  (progress "Fetching the distinfo.")
  (let* ((destination (truename destination))
         (relative-path
           ;; distribution name may include slashes
           ;; and can't be used directly as a name
           ;; of a pathname.
           (uiop:parse-unix-namestring (source-project-name source)
                                       :type "txt"))
         (target-path (merge-pathnames
                       relative-path
                       destination)))
    (ensure-directories-exist target-path)
    (qdex:fetch (source-distinfo-url source) target-path)
    destination))
