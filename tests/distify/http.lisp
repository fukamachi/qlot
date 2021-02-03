(defpackage #:qlot/tests/distify/http
  (:use #:cl
        #:rove
        #:qlot/distify)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/source
                #:make-source))
(in-package #:qlot/tests/distify/http)

(deftest distify-http-tests
  (let ((source (make-source :http
                             "lisp-magick"
                             "http://www.nil.at/download/lisp-magick.tar.gz")))
    (with-qlot-server (source nil *default-pathname-defaults*)
      (distify source *default-pathname-defaults*)

      (let ((distinfo.txt (merge-pathnames #P"lisp-magick.txt"))
            (software (merge-pathnames #P"softwares/lisp-magick-0.71/"))
            (archive (merge-pathnames #P"archives/lisp-magick.tar.gz"))
            (releases.txt (merge-pathnames #P"lisp-magick/http-25e5075be456f8d2cc3d6ae238f12051/releases.txt"))
            (systems.txt (merge-pathnames #P"lisp-magick/http-25e5075be456f8d2cc3d6ae238f12051/systems.txt")))
        (ok (uiop:file-exists-p distinfo.txt))
        (ok (uiop:directory-exists-p software))
        (ok (uiop:file-exists-p archive))
        (ok (uiop:file-exists-p releases.txt))
        (ok (uiop:file-exists-p systems.txt))

        (ok (equal (uiop:read-file-string releases.txt)
                   (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%lisp-magick qlot://localhost/archives/lisp-magick.tar.gz 13884 25e5075be456f8d2cc3d6ae238f12051 05c9387e118fd9924c48523c5f70ee086e6eb53b lisp-magick-0.71 lisp-magick.asd~%")))
        (ok (equal (uiop:read-file-string systems.txt)
                   (format nil "# project system-file system-name [dependency1..dependencyN]~%lisp-magick lisp-magick lisp-magick cffi~%")))))))
