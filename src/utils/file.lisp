(defpackage #:qlot/utils/file
  (:use #:cl)
  (:export #:copy-directory))
(in-package #:qlot/utils/file)

(defun copy-directory (dir destination &key exclude)
  (let ((files
          (remove-if (or exclude (constantly nil)) (uiop:directory-files dir))))
    (when files
      (ensure-directories-exist destination)
      (dolist (file files)
        (uiop:copy-file file
                        (merge-pathnames (file-namestring file) destination)))))
  (dolist (subdir (uiop:subdirectories dir))
    (copy-directory subdir
                    (merge-pathnames (enough-namestring subdir dir) destination)
                    :exclude exclude)))
