(defpackage #:qlot/distify/ql
  (:use #:cl #:qlot/distify-protocol)
  (:import-from #:qlot/source/dist
                #:source-dist-project)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-version
                #:source-version-prefix
                #:source-distinfo-url
                #:source-distribution
                #:write-distinfo)
  (:import-from #:qlot/proxy
                #:*proxy*)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-stream
                #:parse-space-delimited-stream
                #:get-distinfo-url)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:import-from #:dexador))
(in-package #:qlot/distify/ql)

;;; Distification for single-project quicklisp sources, like `ql clack
;;; :latest`. The version here represents the version of the project
;;; in that quicklisp dist version.

;; Version locking drives which release we install later, so
;; preparation is a no-op.
(defmethod prepare-source-for-dist ((source source-dist-project) prep-dir)
  (declare (ignore source prep-dir))
  (values))

(defmethod lock-version ((source source-dist-project) prep-dir)
  (unless (source-distinfo-url source)
    (setf (source-distinfo-url source)
          (get-distinfo-url (source-distribution source)
                            (slot-value source 'qlot/source/dist::%version))))

  (let* ((body-stream (handler-case (dex:get (source-distinfo-url source)
                                             :want-stream t
                                             :proxy *proxy*)
                        (dex:http-request-failed (e)
                          (error 'qlot-simple-error
                                 :format-control "Not available dist: ~A (~A)"
                                 :format-arguments (list (source-distinfo-url source)
                                                         (type-of e))))))
         (distinfo (parse-distinfo-stream body-stream))
         (release-index-url (cdr (assoc "release-index-url" distinfo :test 'equal)))
         (version (cdr (assoc "version" distinfo :test 'equal))))
    (check-type release-index-url string)
    (check-type version string)
    ;; Check if the project is available
    (let ((stream (dex:get release-index-url
                           :want-stream t
                           :proxy *proxy*)))
      (block nil
        (parse-space-delimited-stream stream
                                      :test (lambda (data)
                                              (when (equal (first data) (source-project-name source))
                                                (return t))))
        (error 'qlot-simple-error
               :format-control "'~A' is not available in dist '~A'"
               :format-arguments (list
                                  (source-project-name source)
                                  (source-distinfo-url source)))))
    (setf (source-version source)
          (format nil "~A~A"
                  (source-version-prefix source)
                  version))))

(defmethod finalize-dist ((source source-dist-project) prep-dir)
  (let* ((destination (truename prep-dir))
         (metadata (merge-pathnames (format nil "~A/~A/"
                                            (source-project-name source)
                                            (source-version source))
                                    destination)))
    (ensure-directories-exist metadata)
    (let ((original-distinfo
           (parse-distinfo-stream (dex:get (source-distinfo-url source)
                                           :want-stream t
                                           :proxy *proxy*))))
      (dolist (metadata-pair `(("systems.txt" . ,(cdr (assoc "system-index-url" original-distinfo :test 'equal)))
                               ("releases.txt" . ,(cdr (assoc "release-index-url" original-distinfo :test 'equal)))))
        (destructuring-bind (file . url) metadata-pair
          (check-type url string)
          (let ((data (parse-space-delimited-stream (dex:get url :want-stream t :proxy *proxy*)
                                                    :test (lambda (data)
                                                            (equal (first data) (source-project-name source)))
                                                    :include-header t)))
            (uiop:with-output-file (out (merge-pathnames file metadata))
              (format out "~{~{~A~^ ~}~%~}" data)))))))
  (values))
