(defpackage #:qlot/utils/distify
  (:use #:cl)
  (:import-from #:qlot/utils/asdf
                #:system-class-name
                #:system-pathname
                #:with-directory
                #:directory-system-files
                #:directory-lisp-files
                #:lisp-file-system-name
                #:lisp-file-dependencies)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-stream
                #:make-versioned-distinfo-url
                #:make-versioned-distinfo-url-with-template)
  (:import-from #:qlot/utils
                #:https-of
                #:split-with)
  (:import-from #:qlot/http)
  (:import-from #:qlot/source
                #:source-dist-name
                #:source-distinfo-url
                #:source-project-name
                #:source-version)
  (:import-from #:ironclad)
  (:export #:releases.txt
           #:systems.txt
           #:get-distinfo-url
           #:write-source-distinfo))
(in-package #:qlot/utils/distify)

(defun normalize-pathname (path)
  "Return the pathname PATH resolved to a normalized pathname in the filesystem.
Does not resolve symlinks, but PATH must actually exist in the filesystem."
  (when (wild-pathname-p path)
    (error "Wild pathnames cannot be normalized."))
  (first (uiop:directory* path)))

(defun releases.txt (project-name project-version source-directory tarball-file)
  (let* ((source-directory (normalize-pathname source-directory))
         (prefix (car (last (pathname-directory source-directory)))))
    (multiple-value-bind (size file-md5 content-sha1)
        (with-open-file (in tarball-file :element-type '(unsigned-byte 8))
          (values (file-length in)
                  (ironclad:byte-array-to-hex-string
                    (ironclad:digest-file :md5 tarball-file))
                  (ironclad:byte-array-to-hex-string
                    (ironclad:digest-stream :sha1 in))))
      (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%~A ~A ~A ~A ~A ~A~{ ~A~}~%"
              project-name
              (format nil "qlot://localhost/~A/~A/~A"
                      project-name
                      project-version
                      (file-namestring tarball-file))
              size
              file-md5
              content-sha1
              prefix
              (let (result)
                (dolist (system-file (directory-system-files source-directory))
                  (push (subseq (namestring system-file)
                                (length (namestring source-directory)))
                        result))
                result)))))

(defun systems.txt (project-name source-directory)
  (with-output-to-string (s)
    (format s "# project system-file system-name [dependency1..dependencyN]~%")
    (with-directory (system-file system-name dependencies) source-directory
      (format s "~A ~A ~A~{ ~A~}~%"
              project-name
              (pathname-name system-file)
              system-name
              dependencies)
      (let ((system-class-name (system-class-name system-name)))
        ;; XXX: This doesn't work if it's a class inherits package-inferred-system.
        (when (eq system-class-name :package-inferred-system)
          (let* ((system-pathname (system-pathname system-name))
                 (system-pathname (if system-pathname
                                      (uiop:ensure-directory-pathname
                                       (merge-pathnames system-pathname source-directory))
                                      source-directory)))
            (loop for file in (directory-lisp-files system-pathname)
                  for sub-system-name = (lisp-file-system-name file
                                                               system-pathname
                                                               system-name)
                  when sub-system-name
                  do (format s "~A ~A ~A~{ ~A~}~%"
                             project-name
                             (enough-namestring file source-directory)
                             sub-system-name
                             (delete-duplicates
                              (mapcar #'string-downcase
                                      (lisp-file-dependencies file))
                              :test 'equal
                              :from-end t)))))))))

(defun find-versioned-distinfo-url (available-versions-url version)
  (let ((stream
          (qlot/http:get (https-of available-versions-url)
                         :want-stream t)))
    (loop for line = (read-line stream nil nil)
          while line
          for (v distinfo-url) = (split-with #\Space line :limit 2)
          when (equal v version)
          do (return distinfo-url))))

(defun get-distinfo-url (distribution version)
  (let* ((distinfo-data
           (parse-distinfo-stream (qlot/http:get (https-of distribution)
                                                 :want-stream t)))
         (canonical-distinfo-url
           (cdr (assoc "canonical-distinfo-url" distinfo-data
                       :test #'string=)))
         (distinfo-template-url
           (cdr (assoc "distinfo-template-url" distinfo-data
                       :test #'string=))))
    (https-of
     (cond
       ((eq :latest version)
        (or canonical-distinfo-url
            (cdr (assoc "distinfo-subscription-url" distinfo-data
                        :test #'string=))
            distribution))
       (distinfo-template-url
        (make-versioned-distinfo-url-with-template
         distinfo-template-url
         version))
       (t
        (let ((available-versions-url
                (cdr (assoc "available-versions-url" distinfo-data
                            :test #'string=))))
          (cond
            (available-versions-url
             (find-versioned-distinfo-url available-versions-url version))
            (t
             (make-versioned-distinfo-url
              distribution
              version)))))))))

(defun write-distinfo (distinfo.txt key-values)
  (uiop:with-output-file (out distinfo.txt :if-exists :supersede)
    (format out "~{~(~A~): ~A~%~}"
            key-values)))

(defun write-source-distinfo (source destination &optional additional-values)
  (let ((distinfo.txt (merge-pathnames
                       (make-pathname :name (source-project-name source)
                                      :type "txt")
                       destination))
        (distinfo-key-values
          (append
           (list :name (source-dist-name source)
                 :version (source-version source)
                 :distinfo-subscription-url (format nil "qlot://localhost/~A.txt"
                                                    (source-project-name source))
                 :canonical-distinfo-url (format nil "qlot://localhost/~A.txt"
                                                 (source-project-name source))
                 :release-index-url (format nil "qlot://localhost/~A/~A/releases.txt"
                                            (source-project-name source)
                                            (source-version source))
                 :system-index-url (format nil "qlot://localhost/~A/~A/systems.txt"
                                           (source-project-name source)
                                           (source-version source)))
           additional-values)))
    (write-distinfo distinfo.txt distinfo-key-values)))
