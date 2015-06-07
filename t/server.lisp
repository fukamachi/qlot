(in-package :cl-user)
(defpackage qlot-test.server
  (:use :cl
        :qlot.server
        :prove)
  (:import-from :qlot.parser
                :parse-qlfile)
  (:import-from :qlot.tmp
                :*tmp-directory*)
  (:import-from :drakma
                :http-request)
  (:import-from :fad
                :file-exists-p
                :pathname-as-directory
                :generate-random-string
                :delete-directory-and-files))
(in-package :qlot-test.server)

(plan 6)

(let ((lock (asdf:system-relative-pathname :qlot #P"t/data/qlfile.lock")))
  (when (fad:file-exists-p lock)
    (delete-file lock)))

#+thread-support
(let ((qlfile (asdf:system-relative-pathname :qlot #P"t/data/qlfile"))
      (*tmp-directory* (fad:pathname-as-directory
                        (merge-pathnames (fad::generate-random-string)
                                         (asdf:system-relative-pathname :qlot #P"t/tmp/qlot/")))))
  (ensure-directories-exist *tmp-directory*)
  (diag "starting a server..")
  (start-server qlfile)

  (is (nth-value 1 (http-request (localhost "/quicklisp.txt"))) 404)
  (is (nth-value 1 (http-request (localhost "/clack.txt"))) 200)
  (is (nth-value 1 (http-request (localhost "/shelly.txt"))) 200)
  (is (nth-value 1 (http-request (localhost "/cl-dbi.txt"))) 200)
  (is (nth-value 1 (http-request (localhost "/datafly.txt"))) 200)
  (is (nth-value 1 (http-request (localhost "/log4cl.txt"))) 200)

  (fad:delete-directory-and-files *tmp-directory*)

  (diag "stopping the server..")
  (stop-server))
#-thread-support
(skip 6 "because your Lisp doesn't support threads")

(finalize)
