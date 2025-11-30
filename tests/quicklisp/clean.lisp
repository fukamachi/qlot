(defpackage #:qlot-tests/quicklisp/clean
  (:use #:cl
        #:rove)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory))
(in-package #:qlot-tests/quicklisp/clean)

(defclass test-release (ql-dist:release)
  ((base-path
    :initarg :base-path
    :reader test-base-path)
   (archive-path
    :initarg :archive-path
    :reader test-archive-path)))

(defmethod ql-dist:base-directory ((release test-release))
  (test-base-path release))

(defmethod ql-dist:local-archive-file ((release test-release))
  (test-archive-path release))

(defun make-symlink (target link)
  #+sbcl
  (sb-posix:symlink (namestring target) (namestring link))
  #-sbcl
  (uiop:run-program (list "ln" "-s"
                          (uiop:native-namestring target)
                          (uiop:native-namestring link))))

(deftest clean-preserves-software-symlink
  (with-tmp-directory (tmp-dir)
    (let* ((archives-dir (merge-pathnames #P"archives/" tmp-dir))
           (archive-path (merge-pathnames #P"archives/demo-1.tgz" tmp-dir))
           (software-dir (merge-pathnames #P"software/" tmp-dir))
           (cache-target (uiop:ensure-directory-pathname
                          (merge-pathnames #P"cache/demo-1/" tmp-dir)))
           (symlink-file (merge-pathnames #P"software/demo-1" tmp-dir))
           (symlink-path (uiop:ensure-directory-pathname symlink-file))
           (dist (make-instance 'ql-dist:dist
                                :base-directory tmp-dir
                                :name "test-dist"
                                :version "0"
                                :provided-releases nil
                                :provided-systems nil))
           (release (make-instance 'test-release
                                   :dist dist
                                   :project-name "demo"
                                   :archive-path archive-path
                                   :base-path symlink-path
                                   :provided-systems nil
                                   :archive-url "https://example.invalid/demo-1.tgz"
                                   :archive-size 0
                                   :archive-md5 ""
                                   :archive-content-sha1 ""
                                   :prefix "demo-1"
                                   :system-files nil)))
      (setf (ql-dist:provided-releases dist) (list release))
      (ensure-directories-exist archives-dir)
      (with-open-file (out archive-path
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
        (write-line "dummy" out))
      (ensure-directories-exist cache-target)
      (ensure-directories-exist software-dir)
      (make-symlink cache-target symlink-file)
      (ok (uiop:directory-exists-p symlink-path) "symlink exists before clean")
      (let* ((present-directories (ql-dist::directory-entries
                                   (ql-dist:relative-to dist "software/")))
             (known-directories (mapcar #'ql-dist:base-directory
                                        (ql-dist:provided-releases dist)))
             (garbage-directories
               (set-difference present-directories known-directories
                               :test #'equalp
                               :key #'namestring)))
        (ok (null garbage-directories)
            (format nil "unexpected garbage detected: ~S"
                    (mapcar #'namestring garbage-directories))))
      (ql-dist:clean dist)
      (ok (uiop:directory-exists-p symlink-path) "clean should keep dist directory symlink")
      (ok (uiop:file-exists-p archive-path) "archive file is left intact"))))
