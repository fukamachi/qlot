(defpackage #:qlot/tests/server
  (:use #:cl
        #:rove)
  (:import-from #:qlot/server
                #:*handler*
                #:make-handler
                #:qlot-fetch
                #:with-qlot-server)
  (:import-from #:qlot/source
                #:make-source)
  (:import-from #:ironclad
                #:digest-file))
(in-package #:qlot/tests/server)

(deftest make-handler-tests
  (let ((handler (make-handler (asdf:system-source-directory :qlot))))
    (ok (typep handler 'function))
    (ok (equal (funcall handler "qlot://localhost/server.lisp")
               (asdf:system-relative-pathname :qlot #P"server.lisp")))
    (ok (equal (funcall handler "qlot://localhost/utils/git.lisp")
               (asdf:system-relative-pathname :qlot #P"utils/git.lisp")))
    (ok (null (funcall handler "qlot://localhost/not-found-file")))
    (ok (null (funcall handler "https://github.com/fukamachi/lsx")))
    (ok (null (funcall handler "qlot://local")))
    (ok (null (funcall handler "qlot://localhost/source")))
    (ok (null (funcall handler "qlot://localhost/source/")))
    (ok (null (funcall handler nil)))))

(deftest qlot-fetch-tests
  (let ((*handler* (make-handler (asdf:system-source-directory :qlot))))
    (uiop:with-temporary-file (:pathname file)
      (qlot-fetch "qlot://localhost/server.lisp" file)
      (ok (equalp (ironclad:digest-file :md5 file)
                  (ironclad:digest-file :md5 (asdf:system-relative-pathname :qlot #P"server.lisp")))))
    (uiop:with-temporary-file (:pathname file)
      (qlot-fetch "qlot://localhost/not-found-file" file)
      (ok (equal (uiop:read-file-string file)
                 "")))))

(deftest with-qlot-server-tests
  (with-qlot-server ((make-source :git "lsx" "https://github.com/fukamachi/lsx"))
    (uiop:with-temporary-file (:pathname file)
      (ql-http:fetch "qlot://localhost/lsx.txt" file)
      (ok (not (equal (uiop:read-file-string file)
                      ""))))
    (uiop:with-temporary-file (:pathname file)
      (ql-http:fetch "qlot://localhost/not-found-file" file)
      (ok (equal (uiop:read-file-string file)
                 "")))))
