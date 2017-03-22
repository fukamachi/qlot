(defpackage #:qlot/tests/server
  (:use #:cl
        #:qlot/server
        #:prove)
  (:import-from #:qlot/parser
                #:parse-qlfile)
  (:import-from #:qlot/tmp
                #:*tmp-directory*)
  (:import-from #:qlot/util
                #:generate-random-string)
  (:import-from #:uiop
                #:file-exists-p
                #:ensure-directory-pathname
                #:delete-directory-tree))
(in-package #:qlot/tests/server)

(plan 6)

(let ((lock (asdf:system-relative-pathname :qlot #P"tests/data/qlfile.lock")))
  (when (uiop:file-exists-p lock)
    (delete-file lock)))

#+thread-support
(let ((qlfile (asdf:system-relative-pathname :qlot #P"tests/data/qlfile"))
      (*tmp-directory* (uiop:ensure-directory-pathname
                        (merge-pathnames (generate-random-string)
                                         (asdf:system-relative-pathname :qlot #P"tests/tmp/qlot/")))))
  (ensure-directories-exist *tmp-directory*)
  (diag "starting a server..")
  (start-server qlfile)

  (handler-case
      (dex:get (localhost "/quicklisp.txt"))
    (dex:http-request-not-found () (pass "/quicklisp.txt is 404")))
  (is (nth-value 1 (dex:get (localhost "/clack.txt"))) 200)
  (is (nth-value 1 (dex:get (localhost "/shelly.txt"))) 200)
  (is (nth-value 1 (dex:get (localhost "/cl-dbi.txt"))) 200)
  (is (nth-value 1 (dex:get (localhost "/datafly.txt"))) 200)
  (is (nth-value 1 (dex:get (localhost "/log4cl.txt"))) 200)

  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)

  (diag "stopping the server..")
  (stop-server))
#-thread-support
(skip 6 "because your Lisp doesn't support threads")

(finalize)
