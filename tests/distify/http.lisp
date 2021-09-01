(defpackage #:qlot/tests/distify/http
  (:use #:cl
        #:rove
        #:qlot/distify/http)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory))
(in-package #:qlot/tests/distify/http)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-http-tests
  (let ((source (make-source :http
                             "lisp-magick"
                             "http://www.nil.at/download/lisp-magick.tar.gz")))
    (distify-http source *tmp-directory*)

    (let ((distinfo.txt (merge-pathnames #P"lisp-magick.txt" *tmp-directory*))
          (archive (merge-pathnames #P"lisp-magick/http-25e5075be456f8d2cc3d6ae238f12051/archive.tar.gz"
                                    *tmp-directory*))
          (releases.txt (merge-pathnames #P"lisp-magick/http-25e5075be456f8d2cc3d6ae238f12051/releases.txt"
                                         *tmp-directory*))
          (systems.txt (merge-pathnames #P"lisp-magick/http-25e5075be456f8d2cc3d6ae238f12051/systems.txt"
                                        *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (ok (uiop:file-exists-p archive))
      (ok (uiop:file-exists-p releases.txt))
      (ok (uiop:file-exists-p systems.txt))

      (ok (equal (uiop:read-file-string releases.txt)
                 (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%lisp-magick qlot://localhost/lisp-magick/http-25e5075be456f8d2cc3d6ae238f12051/archive.tar.gz 13884 25e5075be456f8d2cc3d6ae238f12051 05c9387e118fd9924c48523c5f70ee086e6eb53b lisp-magick-0.71 lisp-magick.asd~%")))
      (ok (equal (uiop:read-file-string systems.txt)
                 (format nil "# project system-file system-name [dependency1..dependencyN]~%lisp-magick lisp-magick lisp-magick cffi~%"))))))
