(defpackage #:qlot/tests/distify/git
  (:use #:cl
        #:rove
        #:qlot/distify/git)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file
                #:parse-space-delimited-file)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:qlot/tests/distify/git)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-git-tests
  (let ((source (make-source :git
                             "cl-dbi"
                             "https://github.com/fukamachi/cl-dbi"
                             :tag "0.9.0"))
        (*default-pathname-defaults* *tmp-directory*))
    (distify-git source *tmp-directory*)

    (let ((distinfo.txt (merge-pathnames #P"cl-dbi.txt"))
          (systems.txt (merge-pathnames #P"cl-dbi/git-64941e1848354767e08e57aca90d7c40350bb6b3/systems.txt"))
          (releases.txt (merge-pathnames #P"cl-dbi/git-64941e1848354767e08e57aca90d7c40350bb6b3/releases.txt"))
          (tarball (merge-pathnames #P"cl-dbi/git-64941e1848354767e08e57aca90d7c40350bb6b3/archive.tar.gz")))
      (ok (uiop:file-exists-p tarball))

      (testing "distinfo.txt"
        (ok (uiop:file-exists-p distinfo.txt))
        (let ((metadata (parse-distinfo-file distinfo.txt)))
          (ok (equal (aget metadata "name") "cl-dbi"))
          (ok (equal (aget metadata "version") "git-64941e1848354767e08e57aca90d7c40350bb6b3"))
          (ok (equal (aget metadata "distinfo-subscription-url") "qlot://localhost/cl-dbi.txt"))
          (ok (equal (aget metadata "release-index-url") "qlot://localhost/cl-dbi/git-64941e1848354767e08e57aca90d7c40350bb6b3/releases.txt"))
          (ok (equal (aget metadata "system-index-url") "qlot://localhost/cl-dbi/git-64941e1848354767e08e57aca90d7c40350bb6b3/systems.txt"))))

      (testing "systems.txt"
        (ok (uiop:file-exists-p systems.txt))
        (let ((data (parse-space-delimited-file systems.txt)))
          (ok (equal data
                     '(("cl-dbi" "cl-dbi" "cl-dbi" "dbi")
                       ("cl-dbi" "dbd-mysql" "dbd-mysql" "cl-mysql" "cl-syntax" "cl-syntax-annot" "dbi")
                       ("cl-dbi" "dbd-postgres" "dbd-postgres" "cl-postgres" "cl-syntax" "cl-syntax-annot" "dbi" "trivial-garbage")
                       ("cl-dbi" "dbd-sqlite3" "dbd-sqlite3" "cl-syntax" "cl-syntax-annot" "dbi" "sqlite" "trivial-garbage" "uiop")
                       ("cl-dbi" "dbi-test" "dbi-test" "cl-syntax" "cl-syntax-annot" "closer-mop" "dbi" "prove" "trivial-types")
                       ("cl-dbi" "dbi" "dbi" "bordeaux-threads" "cl-syntax" "cl-syntax-annot" "closer-mop" "split-sequence"))))))

      (testing "releases.txt"
        (ok (uiop:file-exists-p releases.txt))
        (let ((data (parse-space-delimited-file releases.txt)))
          (equal data
                 '(("cl-dbi" "qlot://localhost/cl-dbi/git-64941e1848354767e08e57aca90d7c40350bb6b3/archive.tar.gz"
                    "14465" "9bf32509e09b89333630f6ac39a054b8" "e163fc7a0f899f06c20b2f0aab141e76a37be832"
                    "cl-dbi-ref-64941e1848354767e08e57aca90d7c40350bb6b3"
                    "dbi.asd" "dbi-test.asd" "dbd-sqlite3.asd" "dbd-postgres.asd" "dbd-mysql.asd" "cl-dbi.asd"))))))))
