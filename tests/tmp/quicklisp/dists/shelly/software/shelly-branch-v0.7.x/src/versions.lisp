(in-package :cl-user)
(defpackage shelly.versions
  (:use :cl))
(in-package :shelly.versions)

(cl-annot:enable-annot-syntax)

(defun qlrequire (packages)
  (dolist (package packages)
    (unless (find-package package)
      (let ((*standard-output* (make-broadcast-stream)))
        #+quicklisp (ql:quickload package :verbose nil)
        #-quicklisp (asdf:load-system package :verbose nil)))))

(defmacro i (symbol &optional (args nil p))
  (let* ((pos (position #\# (symbol-name symbol)))
         (intern `(intern ,(subseq (symbol-name symbol) (1+ pos))
                          (find-package ,(subseq (symbol-name symbol) 0 pos)))))
    (if p
        `(funcall ,intern ,@args)
        intern)))

(let (releases)
  (defun retrieve-releases ()
    (qlrequire '(:yason :drakma :flexi-streams))
    (labels ((retrieve-from-api ()
               (let ((res
                       (i #:yason#parse
                          ((i #:flex#octets-to-string
                              ((i #:drakma#http-request ("https://api.github.com/repos/fukamachi/shelly/tags"))))))))
                 res)))
      (or releases
          (setf releases (retrieve-from-api))))))

@export
(defun release-versions ()
  (let ((releases (retrieve-releases)))
    (mapcar #'(lambda (release) (gethash "name" release)) releases)))

@export
(defun find-version (version)
  (when (string= version "latest")
    (return-from find-version (find-version :latest)))

  (case version
    (:latest (find-version (car (sort (release-versions) #'string>))))
    ('nil nil)
    (t (car
        (member-if #'(lambda (release)
                       (string= (gethash "name" release)
                                version))
                   (retrieve-releases))))))

(defun version-tarball-url (version)
  (let ((version (find-version version)))
    (if version
        (gethash "tarball_url" version)
        nil)))

@export
(defun download-version (version &optional (destination *default-pathname-defaults*))
  (let ((tarball-url (version-tarball-url version)))
    (unless tarball-url
      (error "Version ~A is not found." version))
    (qlrequire '(:drakma :archive :chipz))

    (multiple-value-bind (tarball-stream status)
        (i #:drakma#http-request (tarball-url :want-stream t))
      (unless (= status 200)
        (error "Failed to download a tarball from GitHub (~A / status=~D)."
               tarball-url status))

      (unwind-protect
           (extract-tarball tarball-stream destination)
        (close tarball-stream)))))

(defun extract-tarball (tarball-stream &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (qlrequire '(:archive :chipz))
    (let ((archive (i #:archive#open-archive
                      ((i #:archive#tar-archive)
                       (i #:chipz#make-decompressing-stream ((i #:chipz#gzip) tarball-stream))
                       :direction :input))))
      (prog1
          (merge-pathnames
           (i #:archive#name ((i #:archive#read-entry-from-archive (archive))))
           *default-pathname-defaults*)
        (i #:archive#extract-files-from-archive (archive))))))
