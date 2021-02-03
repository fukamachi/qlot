(defpackage #:qlot/tests/distify/github
  (:use #:cl
        #:rove
        #:qlot/distify)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory))
(in-package #:qlot/tests/distify/github)

(deftest distify-github-tests
  (let ((source (make-source :github
                             "quri"
                             "fukamachi/quri"
                             :ref "adb6d04f1b9ea99fa7f18044df4c86b6c68023af")))
    (with-qlot-server (source nil *default-pathname-defaults*)
      (distify source *default-pathname-defaults*)

      (let ((distinfo.txt (merge-pathnames #P"quri.txt"))
            (software (merge-pathnames #P"softwares/quri-adb6d04f1b9ea99fa7f18044df4c86b6c68023af"))
            (archive (merge-pathnames #P"archives/quri-adb6d04f1b9ea99fa7f18044df4c86b6c68023af.tar.gz"))
            (releases.txt (merge-pathnames #P"quri/github-adb6d04f1b9ea99fa7f18044df4c86b6c68023af/releases.txt"))
            (systems.txt (merge-pathnames #P"quri/github-adb6d04f1b9ea99fa7f18044df4c86b6c68023af/systems.txt")))
        (ok (uiop:file-exists-p distinfo.txt))
        (ok (uiop:directory-exists-p software))
        (ok (uiop:file-exists-p archive))
        (ok (uiop:file-exists-p releases.txt))
        (ok (uiop:file-exists-p systems.txt))

        (ok (equal (uiop:read-file-string distinfo.txt)
                   (format nil "name: quri~%version: github-adb6d04f1b9ea99fa7f18044df4c86b6c68023af~%distinfo-subscription-url: qlot://localhost/quri.txt~%release-index-url: qlot://localhost/quri/github-adb6d04f1b9ea99fa7f18044df4c86b6c68023af/releases.txt~%system-index-url: qlot://localhost/quri/github-adb6d04f1b9ea99fa7f18044df4c86b6c68023af/systems.txt~%")))
        (ok (equal (uiop:read-file-string releases.txt)
                   (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%quri qlot://localhost/archives/quri-adb6d04f1b9ea99fa7f18044df4c86b6c68023af.tar.gz 70532 fcfcb5d96ee6e34a37548be23984382f 2fbd8df6afd6fa4ac585da3ee17128ddd411061f quri-adb6d04f1b9ea99fa7f18044df4c86b6c68023af quri.asd quri-test.asd~%")))
        (ok (equal (uiop:read-file-string systems.txt)
                   (format nil "# project system-file system-name [dependency1..dependencyN]~%quri quri-test quri-test prove prove-asdf quri~%quri quri quri alexandria babel cl-utilities split-sequence~%")))))))
