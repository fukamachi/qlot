(defpackage #:qlot/distify/ql
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-version
                #:source-distribution
                #:write-distinfo)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-stream
                #:parse-space-delimited-stream)
  (:import-from #:dexador)
  (:export #:distify-ql-all
           #:distify-ql))
(in-package #:qlot/distify/ql)

(defun distify-ql-all (source destination)
  (let ((destination (truename destination)))
    (dex:fetch (source-distribution source)
               (make-pathname :name (source-project-name source)
                              :type "txt"
                              :defaults destination)
               :if-exists :supersede)
    destination))

(defun load-source-ql-version (source)
  (let* ((body-stream (dex:get (source-distribution source) :want-stream t))
         (version
           (cdr (assoc "version" (parse-distinfo-stream body-stream) :test 'equal))))
    (check-type version string)
    (setf (source-version source)
          (format nil "ql-~A" version))))

(defun distify-ql (source destination)
  (load-source-ql-version source)
  (let ((destination (truename destination)))
    (uiop:with-output-file (out (make-pathname :name (source-project-name source)
                                               :type "txt"
                                               :defaults destination))
      (write-distinfo source out))
    (let ((metadata (merge-pathnames (format nil "~A/~A/"
                                             (source-project-name source)
                                             (source-version source))
                                     destination)))
      (ensure-directories-exist metadata)
      (let ((original-distinfo
              (parse-distinfo-stream (dex:get (source-distribution source) :want-stream t))))
        (dolist (metadata-pair `(("systems.txt" . ,(cdr (assoc "system-index-url" original-distinfo :test 'equal)))
                                 ("releases.txt" . ,(cdr (assoc "release-index-url" original-distinfo :test 'equal)))))
          (destructuring-bind (file . url) metadata-pair
            (check-type url string)
            (let ((data (parse-space-delimited-stream (dex:get url :want-stream t)
                                                      :test (lambda (data)
                                                              (equal (first data) (source-project-name source)))
                                                      :include-header t)))
              (uiop:with-output-file (out (merge-pathnames file metadata))
                (format out "~{~{~A~^ ~}~%~}" data)))))))
    destination))
