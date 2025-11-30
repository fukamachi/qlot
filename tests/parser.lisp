(defpackage #:qlot-tests/parser
  (:use #:cl
        #:rove
        #:qlot/parser)
  (:import-from #:qlot/parser
                #:parse-qlfile-line)
  (:import-from #:qlot/source
                #:make-source
                #:source-project-name
                #:source-version
                #:source-identifier
                #:source-defrost-args
                #:source=
                #:defrost-source
                #:source-git
                #:source-git-remote-url
                #:source-ql
                #:source-ql-upstream
                #:source-dist
                #:source-distribution
                #:source-distinfo-url
                #:source-ql-dist
                #:source-ql-dist-dist-name
                #:freeze-source
                #:resolve-ql-dist-sources)
  (:import-from #:qlot/errors
                #:qlfile-parse-failed
                #:qlot-error
                #:unknown-source
                #:invalid-definition
                #:invalid-project-name))
(in-package #:qlot-tests/parser)

(defun test-qlfile (name)
  (merge-pathnames name (asdf:system-relative-pathname :qlot #P"tests/data/")))

(deftest parse-qlfile-line-test
  (testing "git source"
    (let ((source (parse-qlfile-line "git lsx https://github.com/fukamachi/lsx")))
      (ok (typep source 'source-git))
      (ok (equal (source-project-name source) "lsx")))
    (ok (typep (parse-qlfile-line "git myapp http://myapp.com/\\#/myapp.git")
               'source-git)
        "can escape a sharp"))

  (testing "ql source"
    (let ((source (parse-qlfile-line "ql :all :latest")))
      (ok (typep source 'source-dist))
      (ok (equal (source-project-name source) "quicklisp"))
      (ok (equal (source-distribution source) "https://beta.quicklisp.org/dist/quicklisp.txt")))
    (let ((source (parse-qlfile-line "ql log4cl 2014-03-17")))
      (ok (typep source 'source-ql))
      (ok (equal (source-project-name source) "log4cl"))
      (ok (equal (source-distribution source) "https://beta.quicklisp.org/dist/quicklisp.txt")))
    (let ((source (parse-qlfile-line "ql mito :upstream")))
      (ok (typep source 'source-ql-upstream))
      (ok (equal (source-project-name source) "mito"))
      (ok (null (source-git-remote-url source)))))

  (testing "ql-dist source"
    (let ((source (parse-qlfile-line "ql-dist shirakumo trial")))
      (ok (typep source 'source-ql-dist))
      (ok (equal (source-ql-dist-dist-name source) "shirakumo"))
      (ok (equal (source-project-name source) "trial"))
      (ok (null (source-distribution source)))
      "distribution URL is not set until resolution")
    (let ((source (parse-qlfile-line "ql-dist shirakumo trial 2024-01-01")))
      (ok (typep source 'source-ql-dist))
      (ok (equal (source-ql-dist-dist-name source) "shirakumo"))
      (ok (equal (source-project-name source) "trial"))))

  (ok (signals
        (parse-qlfile-line "source")
        'unknown-source)
      "invalid source")

  (ok (signals
       (parse-qlfile-line "git yaclml/test https://github.com/sharplispers/yaclml.git")
       'invalid-project-name)
      "Project name can't contain a slash")

  (ok (equal (parse-qlfile-line "# This is a comment.") nil)
      "# comment")
  (ok (equal (parse-qlfile-line " # This is a comment.") nil)
      " # comment")
  (ok (equal (parse-qlfile-line "; This is a comment.") nil)
      "; comment")
  (ok (equal (parse-qlfile-line " ; This is a comment.") nil)
      " ; comment")
  (ok (equal (parse-qlfile-line ";; This is a comment.") nil)
      ";; comment")
  (ok (equal (parse-qlfile-line " ;; This is a comment.") nil)
      " ;; comment"))

(deftest parse-qlfile-tests
  (let ((parsed (parse-qlfile (test-qlfile #P"qlfile"))))
    (ok (equal (length parsed) 5)))

  (ok (signals (parse-qlfile (test-qlfile #P"qlfile.error"))
               'qlfile-parse-failed))

  ;; https://github.com/fukamachi/qlot/issues/18
  (testing "CRLF"
    (let ((parsed (parse-qlfile (test-qlfile #P"qlfile-crlf"))))
      (ok (equal (length parsed) 4)))))

(deftest parse-qlfile-lock-tests
  (let ((parsed (parse-qlfile-lock (test-qlfile #P"qlfile.lock"))))
    (ok (equal (length parsed) 6)))

  (ok (signals (parse-qlfile-lock (test-qlfile #P"qlfile.error.lock"))
               'qlfile-parse-failed)))

(deftest read-qlfile-for-install-tests
  (let ((sources (read-qlfile-for-install (test-qlfile #P"qlfile") :ignore-lock t)))
    (ok (typep (first sources) 'source-dist))
    (ng (slot-boundp (first sources) 'qlot/source/base::version))
    (ok (= (length sources) 6)))
  (let ((sources (read-qlfile-for-install (test-qlfile #P"qlfile"))))
    (ok (typep (first sources) 'source-dist))
    (ok (slot-boundp (first sources) 'qlot/source/base::version))
    (ok (string= (source-version (first sources)) "2019-08-13"))
    (ok (= (length sources) 6))))

(deftest ql-upstream
  (let ((source (parse-qlfile-line "ql mito :upstream")))
    (setf (source-version source)
          "ql-upstream-8c795b7b4de7dc635f1d2442ef1faf8f23d283e6")
    (ok (equal (getf (cdr (freeze-source source)) :remote-url)
               "https://github.com/fukamachi/mito.git"))))

;;; ql-dist comprehensive tests

(deftest ql-dist-source-identifier-tests
  (testing "source-identifier includes dist-name for uniqueness"
    (let ((source (make-source :ql-dist "shirakumo" "trial")))
      (ok (equal (source-identifier source) "shirakumo/trial"))))

  (testing "different dists with same project have different identifiers"
    (let ((source1 (make-source :ql-dist "shirakumo" "log4cl"))
          (source2 (make-source :ql-dist "quicklisp" "log4cl")))
      (ng (equal (source-identifier source1) (source-identifier source2))))))

(deftest ql-dist-resolve-tests
  (testing "resolve-ql-dist-sources resolves URL from dist source with explicit name"
    (let* ((dist-source (make-source :dist "shirakumo" "http://dist.shirakumo.org/shirakumo.txt"))
           (ql-dist-source (make-source :ql-dist "shirakumo" "trial"))
           (sources (list dist-source ql-dist-source)))
      ;; dist source has explicit name "shirakumo"
      (ok (equal (source-project-name dist-source) "shirakumo"))
      (resolve-ql-dist-sources sources)
      (ok (equal (source-distribution ql-dist-source)
                 "https://dist.shirakumo.org/shirakumo.txt"))))

  (testing "resolve-ql-dist-sources works with dist source without explicit name"
    ;; When dist is declared as: dist http://dist.shirakumo.org/shirakumo.txt
    ;; The name is derived from distinfo (via prepare-source)
    (let* ((dist-source (make-source :dist "http://dist.shirakumo.org/shirakumo.txt"))
           (ql-dist-source (make-source :ql-dist "shirakumo" "trial"))
           (sources (list dist-source ql-dist-source)))
      ;; Verify dist source has no name initially
      (ok (null (source-project-name dist-source)))
      ;; Simulate what prepare-source does (fetches distinfo and sets name)
      ;; In real usage, resolve-ql-dist-sources calls prepare-source automatically
      (setf (source-project-name dist-source) "shirakumo")
      (resolve-ql-dist-sources sources)
      (ok (equal (source-distribution ql-dist-source)
                 "https://dist.shirakumo.org/shirakumo.txt"))))

  (testing "resolve-ql-dist-sources works regardless of order"
    (let* ((ql-dist-source (make-source :ql-dist "shirakumo" "trial"))
           (dist-source (make-source :dist "http://dist.shirakumo.org/shirakumo.txt"))
           (sources (list ql-dist-source dist-source)))
      (setf (source-project-name dist-source) "shirakumo")
      (resolve-ql-dist-sources sources)
      (ok (equal (source-distribution ql-dist-source)
                 "https://dist.shirakumo.org/shirakumo.txt"))))

  (testing "resolve-ql-dist-sources skips already-resolved sources"
    (let* ((dist-source (make-source :dist "http://dist.shirakumo.org/shirakumo.txt"))
           (ql-dist-source (make-source :ql-dist "shirakumo" "trial"))
           (sources (list dist-source ql-dist-source)))
      (setf (source-project-name dist-source) "shirakumo")
      ;; Pre-set distribution (simulating defrost from lock file)
      (setf (source-distribution ql-dist-source) "http://original-url.example.com/dist.txt")
      (resolve-ql-dist-sources sources)
      ;; Should NOT be overwritten
      (ok (equal (source-distribution ql-dist-source)
                 "http://original-url.example.com/dist.txt"))))

  (testing "resolve-ql-dist-sources errors on unknown dist"
    (let* ((ql-dist-source (make-source :ql-dist "nonexistent" "trial"))
           (sources (list ql-dist-source)))
      (ok (signals (resolve-ql-dist-sources sources) 'qlot-error)))))

(deftest ql-dist-source=-tests
  (testing "source= compares dist-name, project-name, and version"
    (let ((source1 (make-source :ql-dist "shirakumo" "trial" :latest))
          (source2 (make-source :ql-dist "shirakumo" "trial" :latest)))
      (ok (source= source1 source2))))

  (testing "source= returns nil for different dist-name"
    (let ((source1 (make-source :ql-dist "shirakumo" "trial"))
          (source2 (make-source :ql-dist "ultralisp" "trial")))
      (ng (source= source1 source2))))

  (testing "source= returns nil for different project-name"
    (let ((source1 (make-source :ql-dist "shirakumo" "trial"))
          (source2 (make-source :ql-dist "shirakumo" "kandria")))
      (ng (source= source1 source2))))

  (testing "source= returns nil for different version"
    (let ((source1 (make-source :ql-dist "shirakumo" "trial" :latest))
          (source2 (make-source :ql-dist "shirakumo" "trial" "2024-01-01")))
      (ng (source= source1 source2))))

  (testing "source= works between fresh and defrosted sources"
    (let ((fresh-source (make-source :ql-dist "shirakumo" "trial" :latest))
          (defrosted-source (make-instance 'source-ql-dist
                                           :dist-name "shirakumo"
                                           :project-name "trial"
                                           :%version :latest
                                           :distribution "http://dist.shirakumo.org/shirakumo.txt")))
      ;; Fresh source has no distribution, defrosted has one
      (ok (null (source-distribution fresh-source)))
      (ok (source-distribution defrosted-source))
      ;; But source= should still return t
      (ok (source= fresh-source defrosted-source)))))

(deftest ql-dist-freeze-defrost-tests
  (testing "freeze-source produces correct lock file format"
    (let ((source (make-source :ql-dist "shirakumo" "trial" "2024-01-01")))
      (setf (source-distribution source) "http://dist.shirakumo.org/shirakumo.txt")
      (setf (source-distinfo-url source) "http://dist.shirakumo.org/shirakumo/2024-01-01/distinfo.txt")
      (setf (source-version source) "ql-dist-2024-01-01")
      (let ((frozen (freeze-source source)))
        (ok (equal (car frozen) "trial"))
        (ok (equal (getf (cdr frozen) :class) 'qlot/source/ql-dist:source-ql-dist))
        (ok (equal (getf (cdr frozen) :dist-name) "shirakumo"))
        (ok (equal (getf (cdr frozen) :distribution) "http://dist.shirakumo.org/shirakumo.txt"))
        (ok (equal (getf (cdr frozen) :distinfo) "http://dist.shirakumo.org/shirakumo/2024-01-01/distinfo.txt")))))

  (testing "parse-qlfile-lock correctly reads ql-dist entries"
    (let ((sources (parse-qlfile-lock (test-qlfile #P"qlfile-ql-dist.lock"))))
      (ok (= (length sources) 4))
      ;; Find the ql-dist sources by project name (lock file uses project name as key)
      (let ((trial (find "trial" sources :key #'source-project-name :test #'equal))
            (kandria (find "kandria" sources :key #'source-project-name :test #'equal)))
        (ok trial "trial source found")
        (ok kandria "kandria source found")
        (when trial
          (ok (typep trial 'source-ql-dist))
          (ok (equal (source-ql-dist-dist-name trial) "shirakumo"))
          (ok (equal (source-project-name trial) "trial"))
          ;; Verify identifier includes dist-name
          (ok (equal (source-identifier trial) "shirakumo/trial")))
        (when kandria
          (ok (typep kandria 'source-ql-dist))
          (ok (equal (source-ql-dist-dist-name kandria) "shirakumo"))
          (ok (equal (source-project-name kandria) "kandria"))))))

  (testing "defrost-source correctly restores ql-dist source"
    (let ((source (make-instance 'source-ql-dist
                                 :dist-name "shirakumo"
                                 :project-name "trial"
                                 :%version :latest)))
      (setf (source-defrost-args source)
            '(:version "ql-dist-2024-01-01"
              :dist-name "shirakumo"
              :distinfo "http://dist.shirakumo.org/shirakumo/2024-01-01/distinfo.txt"
              :distribution "http://dist.shirakumo.org/shirakumo.txt"))
      (defrost-source source)
      (ok (equal (source-version source) "ql-dist-2024-01-01"))
      (ok (equal (source-distribution source) "http://dist.shirakumo.org/shirakumo.txt"))
      ;; Note: distinfo URL is converted to HTTPS by parent's defrost-source :after method
      (ok (equal (source-distinfo-url source) "https://dist.shirakumo.org/shirakumo/2024-01-01/distinfo.txt")))))

(deftest ql-dist-invalid-input-tests
  (testing "make-source errors on non-string dist-name"
    (ok (signals (make-source :ql-dist :invalid "trial") 'invalid-definition)))

  (testing "make-source errors on non-string project-name"
    (ok (signals (make-source :ql-dist "shirakumo" :invalid) 'invalid-definition)))

  (testing "make-source errors on invalid version type"
    (ok (signals (make-source :ql-dist "shirakumo" "trial" 123) 'invalid-definition))))
