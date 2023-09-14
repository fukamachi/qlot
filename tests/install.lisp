(defpackage #:qlot/tests/install
  (:use #:cl
        #:rove)
  (:import-from #:qlot/install
                #:install-qlfile)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file)
  (:import-from #:alexandria
                #:starts-with-subseq
                #:set-equal)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:qlot/tests/install)

(defun copy-qlfile (tmp-dir)
  (uiop:copy-file (asdf:system-relative-pathname :qlot #P"tests/data/qlfile5")
                  (merge-pathnames #P"qlfile" tmp-dir))
  (uiop:copy-file (asdf:system-relative-pathname :qlot #P"tests/data/qlfile5.lock")
                  (merge-pathnames #P"qlfile.lock" tmp-dir)))

(defun directory-name (path)
  (car (last (pathname-directory path))))

(deftest install-qlfile-tests
  (with-tmp-directory (tmp-dir)
    (copy-qlfile tmp-dir)

    (let ((qlhome (merge-pathnames #P".qlot/" tmp-dir)))
      (install-qlfile (merge-pathnames #P"qlfile" tmp-dir)
                      :quicklisp-home qlhome)

      (ok (set-equal (mapcar #'directory-name
                             (uiop:subdirectories (merge-pathnames #P"dists/" qlhome)))
                     '("quicklisp"
                       "ultralisp"
                       "ironclad"
                       "cl-ppcre"
                       "lsx"
                       "fukamachi-lack"
                       "mito")
                     :test 'string=)
          "dists are installed")

      (let ((data (parse-distinfo-file (merge-pathnames (format nil "dists/quicklisp/distinfo.txt") qlhome))))
        (ok (equal (aget data "version") "2018-02-28")))
      (let ((data (parse-distinfo-file (merge-pathnames (format nil "dists/ultralisp/distinfo.txt") qlhome))))
        (ok (equal (aget data "version") "20190904101505")))
      (let ((data (parse-distinfo-file (merge-pathnames (format nil "dists/ironclad/distinfo.txt") qlhome))))
        (ok (equal (aget data "version") "git-66ddf32d8afc6581315c72422bf2343eab65009e")))
      (let ((data (parse-distinfo-file (merge-pathnames (format nil "dists/cl-ppcre/distinfo.txt") qlhome))))
        (ok (equal (aget data "version") "ql-2018-08-31")))
      (let ((data (parse-distinfo-file (merge-pathnames (format nil "dists/lsx/distinfo.txt") qlhome))))
        (ok (equal (aget data "version") "github-546032449c010e4153501accf1cac521")))
      (let ((data (parse-distinfo-file (merge-pathnames (format nil "dists/fukamachi-lack/distinfo.txt") qlhome))))
        (ok (equal (aget data "version") "ultralisp-20190904101505")))
      (let ((data (parse-distinfo-file (merge-pathnames (format nil "dists/mito/distinfo.txt") qlhome))))
        (ok (equal (aget data "version") "ql-upstream-8c795b7b4de7dc635f1d2442ef1faf8f23d283e6")))

      ;; Check if Dexador, qlot/distify depends on, is not installed in the local Quicklisp
      (ng (find-if (lambda (name)
                     (starts-with-subseq "dexador-" name))
                   (uiop:subdirectories (merge-pathnames #P"dists/quicklisp/software/" qlhome))
                   :key #'directory-name))

      (dolist (dist-name '("ironclad"
                           "cl-ppcre"
                           "lsx"
                           "fukamachi-lack"
                           "mito"))
        (ok (uiop:directory-exists-p
              (merge-pathnames (format nil "dists/~A/software/" dist-name)
                               qlhome)))))))
