(in-package :cl-user)
(defpackage qlot-test.server
  (:use :cl
        :qlot.server
        :prove)
  (:import-from :qlot.parser
                :parse-qlfile)
  (:import-from :qlot.tmp
                :*tmp-directory*)
  (:import-from :qlot.util
                :generate-random-string)
  (:import-from :drakma
                :http-request)
  (:import-from :uiop
                :file-exists-p
                :ensure-directory-pathname
                :delete-directory-tree))
(in-package :qlot-test.server)

(plan 6)

(let ((lock (asdf:system-relative-pathname :qlot #P"t/data/qlfile.lock")))
  (when (uiop:file-exists-p lock)
    (delete-file lock)))

#+thread-support
(let ((qlfile (asdf:system-relative-pathname :qlot #P"t/data/qlfile"))
      (*tmp-directory* (uiop:ensure-directory-pathname
                        (merge-pathnames (generate-random-string)
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

  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)

  (diag "stopping the server..")
  (stop-server))
#-thread-support
(skip 6 "because your Lisp doesn't support threads")

(finalize)
