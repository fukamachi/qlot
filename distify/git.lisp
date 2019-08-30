(defpackage #:qlot/distify/git
  (:use #:cl)
  (:import-from #:qlot/source/base
                #:source-project-name
                #:source-dist-name
                #:source-version
                #:write-distinfo)
  (:import-from #:qlot/source/git
                #:source-git
                #:source-git-branch
                #:source-git-tag
                #:source-git-remote-url
                #:source-git-ref
                #:source-git-identifier)
  (:import-from #:qlot/utils/git
                #:git-clone
                #:git-ref
                #:create-git-tarball)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:system-file-systems
                #:system-dependencies)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file
                #:digest-sequence)
  (:export #:distify-git))
(in-package #:qlot/distify/git)

(defun load-source-git-version (source)
  (unless (ignore-errors (source-git-ref source))
    (let ((ref (git-ref (source-git-remote-url source)
                        (or (source-git-tag source)
                            (source-git-branch source)
                            "HEAD"))))
      (setf (source-git-ref source) ref)))
  (unless (ignore-errors (source-version source))
    (setf (source-version source)
          (format nil "git-~A"
                  (source-git-ref source)))))

(defun normalize-pathname (path)
  "Return the pathname PATH resolved to a normalized pathname in the filesystem.
Does not resolve symlinks, but PATH must actually exist in the filesystem."
  (when (wild-pathname-p path)
    (error "Wild pathnames cannot be normalized."))
  (first (uiop:directory* path)))

(defun releases.txt (project-name source-directory tarball-file)
  (let* ((source-directory (normalize-pathname source-directory))
         (prefix (car (last (pathname-directory source-directory)))))
    (multiple-value-bind (size file-md5 content-sha1)
        (with-open-file (in tarball-file :element-type '(unsigned-byte 8))
          (values (file-length in)
                  (ironclad:byte-array-to-hex-string
                    (ironclad:digest-file :md5 tarball-file))
                  (ironclad:byte-array-to-hex-string
                    (ironclad:digest-sequence :sha1
                                              (let ((out (make-array (file-length in) :element-type '(unsigned-byte 8))))
                                                (read-sequence out in)
                                                out)))))
      (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%~A ~A ~A ~A ~A ~A~{ ~A~}~%"
              project-name
              (format nil "qlot://localhost/archives/~A"
                      (file-namestring tarball-file))
              size
              file-md5
              content-sha1
              prefix
              (let (result)
                (with-directory (file source-directory)
                  (push (subseq (namestring file) (length (namestring source-directory)))
                        result))
                result)))))

(defun systems.txt (project-name source-directory)
  (with-output-to-string (s)
    (format s "# project system-file system-name [dependency1..dependencyN]~%")
    (with-directory (system-file source-directory)
      (dolist (system (system-file-systems system-file))
        (format s "~A ~A ~A~{ ~A~}~%"
                project-name
                (pathname-name system-file)
                (asdf:component-name system)
                (system-dependencies system))
        (asdf:clear-system system)))))

(defun distify-git (source destination)
  (check-type source source-git)
  (load-source-git-version source)

  (let ((*default-pathname-defaults* (truename destination)))
    (uiop:with-output-file (out (make-pathname :name (source-project-name source)
                                               :type "txt")
                                :if-exists :supersede)
      (write-distinfo source out))

    (let ((softwares (merge-pathnames #P"softwares/"))
          (archives (merge-pathnames #P"archives/"))
          (metadata (merge-pathnames (format nil "~A/~A/"
                                             (source-project-name source)
                                             (source-version source)))))
      (ensure-directories-exist softwares)
      (ensure-directories-exist archives)
      (ensure-directories-exist metadata)
      (let ((source-directory (merge-pathnames (format nil "~A-~A/"
                                                       (source-project-name source)
                                                       (source-git-identifier source))
                                               softwares)))
        (git-clone (source-git-remote-url source)
                   source-directory
                   :checkout-to (or (source-git-branch source)
                                    (source-git-tag source)
                                    "master")
                   :ref (source-git-ref source))

        (let ((tarball (create-git-tarball source-directory
                                           archives
                                           (source-git-ref source))))
          (uiop:with-output-file (out (merge-pathnames "systems.txt" metadata))
            (princ (systems.txt (source-project-name source) source-directory) out))

          (uiop:with-output-file (out (merge-pathnames "releases.txt" metadata))
            (princ (releases.txt (source-project-name source) source-directory tarball) out)))))

    *default-pathname-defaults*))
