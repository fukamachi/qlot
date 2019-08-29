(defpackage #:qlot/tests/distify/ql
  (:use #:cl
        #:rove
        #:qlot/distify/ql)
  (:import-from #:qlot/source
                #:make-source
                #:source-project-name
                #:source-version)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file
                #:parse-space-delimited-file)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:qlot/tests/distify/ql)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-ql-tests
  (let ((source (make-source :ql "log4cl" "2014-03-17")))
    (distify-ql source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "log4cl"
                                       :type "txt"
                                       :defaults *tmp-directory*))
          (systems.txt (merge-pathnames (format nil "~A/~A/systems.txt"
                                                (source-project-name source)
                                                (source-version source))
                                        *tmp-directory*))
          (releases.txt (merge-pathnames (format nil "~A/~A/releases.txt"
                                                 (source-project-name source)
                                                 (source-version source))
                                         *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (ok (uiop:file-exists-p systems.txt))
      (ok (uiop:file-exists-p releases.txt))
      (let ((systems (parse-space-delimited-file systems.txt)))
        (ok (< 0 (length systems)))
        (ok (every (lambda (row)
                     (equal (first row) "log4cl"))
                   systems)))
      (let ((releases (parse-space-delimited-file releases.txt)))
        (ok (< 0 (length releases)))
        (ok (every (lambda (row)
                     (equal (first row) "log4cl"))
                   releases)))))

  (let ((source (make-source :ql :all :latest)))
    (distify-ql-all source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "quicklisp"
                                       :type "txt"
                                       :defaults *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (let ((data (parse-distinfo-file distinfo.txt)))
        (ok (equal (aget data "name") "quicklisp")))))

  (let ((source (make-source :ql :all "2019-08-13")))
    (distify-ql-all source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "quicklisp"
                                       :type "txt"
                                       :defaults *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (let ((data (parse-distinfo-file distinfo.txt)))
        (ok (equal (aget data "name") "quicklisp"))
        (ok (equal (aget data "version") "2019-08-13"))))))
