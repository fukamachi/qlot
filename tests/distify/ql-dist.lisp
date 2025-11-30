(defpackage #:qlot-tests/distify/ql-dist
  (:use #:cl
        #:rove
        #:qlot/distify/ql)
  (:import-from #:qlot/source
                #:make-source
                #:source-project-name
                #:source-version
                #:source-ql-dist
                #:source-ql-dist-dist-name
                #:source-distribution)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file
                #:parse-space-delimited-file
                #:quicklisp-distinfo-url)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:import-from #:qlot/errors
                #:qlot-error))
(in-package #:qlot-tests/distify/ql-dist)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-ql-dist-tests
  ;; Test ql-dist with quicklisp distribution (for testing purposes)
  (let ((source (make-source :ql-dist "quicklisp" "log4cl" "2014-03-17")))
    ;; Set the distribution URL (normally done by resolve-ql-dist-sources)
    (setf (source-distribution source) (quicklisp-distinfo-url))

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

  ;; Test error when project not found
  (let ((source (make-source :ql-dist "quicklisp" "not-found-project" "2014-03-17")))
    (setf (source-distribution source) (quicklisp-distinfo-url))
    (ok (signals (distify-ql source *tmp-directory*)
                 'qlot-error))
    (ng (uiop:file-exists-p (make-pathname :name "not-found-project"
                                           :type "txt"
                                           :defaults *tmp-directory*)))))
