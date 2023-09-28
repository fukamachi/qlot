(defpackage #:qlot/utils/distify
  (:use #:cl)
  (:import-from #:qlot/utils/asdf
                #:system-class-name
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
                #:https-of)
  (:import-from #:qlot/utils/http)
  (:import-from #:qlot/source
                #:source-distinfo-url
                #:source-project-name
                #:source-version
                #:write-distinfo)
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
          (loop for file in (directory-lisp-files source-directory)
                for sub-system-name = (lisp-file-system-name file
                                                             source-directory
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
                            :from-end t))))))))

(defun get-distinfo-url (distribution version)
  (let* ((distinfo-data
           (parse-distinfo-stream (qdex:get (https-of distribution)
                                            :want-stream t)))
         (distinfo-template-url (cdr (assoc "distinfo-template-url" distinfo-data
                                            :test #'string=)))
         (distinfo-url (or (cdr (assoc "canonical-distinfo-url" distinfo-data
                                       :test #'string=))
                           (cdr (assoc "distinfo-subscription-url" distinfo-data
                                       :test #'string=))
                           distribution)))
    (https-of
     (cond
       ((eq :latest version)
        distinfo-url)
       (distinfo-template-url
        (make-versioned-distinfo-url-with-template
         distinfo-template-url
         version))
       (t
        (make-versioned-distinfo-url
         distribution
         version))))))

(defun write-source-distinfo (source destination)
  (let ((distinfo.txt (merge-pathnames
                        (make-pathname :name (source-project-name source)
                                       :type "txt")
                        destination)))
    (uiop:with-output-file (out distinfo.txt :if-exists :supersede)
      (write-distinfo source out))))
