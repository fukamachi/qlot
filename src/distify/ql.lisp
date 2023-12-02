(defpackage #:qlot/distify/ql
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-version
                #:source-version-prefix
                #:source-distinfo-url
                #:source-distribution)
  (:import-from #:qlot/logger
                #:progress)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-stream
                #:parse-space-delimited-stream)
  (:import-from #:qlot/utils/distify
                #:get-distinfo-url
                #:write-source-distinfo
                #:load-version-from-distinfo)
  (:import-from #:qlot/utils
                #:take
                #:https-of)
  (:import-from #:qlot/errors
                #:qlot-simple-error)
  (:import-from #:qlot/http)
  (:import-from #:fuzzy-match
                #:fuzzy-match)
  (:export #:distify-ql))
(in-package #:qlot/distify/ql)

(defun load-source-ql-version (source)
  (progress "Getting the distinfo.")
  (let* ((body-stream (handler-case (qlot/http:get (source-distinfo-url source)
                                                   :want-stream t)
                        (dex:http-request-failed (e)
                          (error 'qlot-simple-error
                                 :format-control "Not available dist: ~A (~A)"
                                 :format-arguments (list (source-distinfo-url source)
                                                         (type-of e))))))
         (distinfo (parse-distinfo-stream body-stream))
         (release-index-url (cdr (assoc "release-index-url" distinfo :test 'equal)))
         (version
           (cdr (assoc "version" distinfo :test 'equal))))
    (check-type release-index-url string)
    (check-type version string)
    ;; Check if the project is available
    (progress "Getting the release metadata.")
    (let ((stream (qlot/http:get (https-of release-index-url) :want-stream t))
          (candidates '()))
      (block nil
        (parse-space-delimited-stream stream
                                      :test (lambda (data)
                                              (when (equal (first data) (source-project-name source))
                                                (return t))
                                              (push (first data) candidates)))
        (error 'qlot-simple-error
               :format-control "'~A' is not available in dist '~A'.~@[~%Did you mean:~%~{  ~A~^~%~}~]"
               :format-arguments (list
                                   (source-project-name source)
                                   (source-distinfo-url source)
                                   (take 4 (fuzzy-match (source-project-name source) candidates))))))
    (setf (source-version source)
          (format nil "~A~A"
                  (source-version-prefix source)
                  version))))

(defun distify-ql (source destination &key distinfo-only)
  (let ((distinfo.txt (merge-pathnames
                       (make-pathname :name (source-project-name source)
                                      :type "txt")
                       destination)))
    (cond
      ((uiop:file-exists-p distinfo.txt)
       (load-version-from-distinfo source distinfo.txt))
      (t
       (unless (source-distinfo-url source)
         (progress "Determining the distinfo URL.")
         (setf (source-distinfo-url source)
               (get-distinfo-url (source-distribution source)
                                 (slot-value source 'qlot/source/dist::%version))))
       (load-source-ql-version source)
       (progress "Writing the distinfo.")
       (write-source-distinfo source destination)))

    (when distinfo-only
      (return-from distify-ql destination))

    (let* ((workspace (uiop:ensure-absolute-pathname
                       (merge-pathnames
                        (make-pathname :directory `(:relative ,(source-project-name source) ,(source-version source)))
                        destination)))
           (systems.txt (merge-pathnames "systems.txt" workspace))
           (releases.txt (merge-pathnames "releases.txt" workspace)))
      (ensure-directories-exist workspace)
      (unless (and (uiop:file-exists-p systems.txt)
                   (uiop:file-exists-p releases.txt))
        (progress "Getting the metadata files.")
        (let ((original-distinfo
                (parse-distinfo-stream (qlot/http:get (source-distinfo-url source)
                                                      :want-stream t))))
          (dolist (metadata-pair `(("systems.txt" . ,(cdr (assoc "system-index-url" original-distinfo :test 'equal)))
                                   ("releases.txt" . ,(cdr (assoc "release-index-url" original-distinfo :test 'equal)))))
            (destructuring-bind (file . url) metadata-pair
              (check-type url string)
              (let ((data (parse-space-delimited-stream (qlot/http:get (https-of url) :want-stream t)
                                                        :test (lambda (data)
                                                                (equal (first data) (source-project-name source)))
                                                        :include-header t)))
                (uiop:with-output-file (out (merge-pathnames file workspace)
                                            :if-exists :supersede)
                  (format out "~{~{~A~^ ~}~%~}" data)))))))

      workspace)))
