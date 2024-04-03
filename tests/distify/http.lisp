(defpackage #:qlot-tests/distify/http
  (:use #:cl
        #:rove
        #:qlot/distify/http)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory))
(in-package #:qlot-tests/distify/http)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-http-tests
  (let ((source (make-source :http
                             "zs3"
                             "https://www.xach.com/lisp/zs3.tgz")))
    (distify-http source *tmp-directory*)

    (let ((distinfo.txt (merge-pathnames #P"zs3.txt" *tmp-directory*))
          (archive (merge-pathnames #P"zs3/http-5ea13aa7a490758882e245c3f8bb063e/archive.tar.gz"
                                    *tmp-directory*))
          (releases.txt (merge-pathnames #P"zs3/http-5ea13aa7a490758882e245c3f8bb063e/releases.txt"
                                         *tmp-directory*))
          (systems.txt (merge-pathnames #P"zs3/http-5ea13aa7a490758882e245c3f8bb063e/systems.txt"
                                        *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (ok (uiop:file-exists-p archive))
      (ok (uiop:file-exists-p releases.txt))
      (ok (uiop:file-exists-p systems.txt))

      (ok (equal (uiop:read-file-string releases.txt)
                 (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%zs3 qlot://localhost/zs3/http-5ea13aa7a490758882e245c3f8bb063e/archive.tar.gz 57149 5ea13aa7a490758882e245c3f8bb063e df893e45c552fd50e02bb7c08601f47db9ca19ac zs3-1.3.3 zs3.asd~%")))
      (ok (equal (uiop:read-file-string systems.txt)
                 (format nil "# project system-file system-name [dependency1..dependencyN]~%zs3 zs3 zs3 alexandria cl-base64 cxml drakma ironclad puri~%"))))))
