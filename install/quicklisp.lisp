(defpackage #:qlot/install/quicklisp
  (:use #:cl)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/utils
                #:generate-random-string)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:import-from #:qlot/utils/tmp
                #:with-tmp-directory)
  (:export #:install-quicklisp))
(in-package #:qlot/install/quicklisp)

(defun fetch-installer (to)
  (let ((quicklisp-file (if (uiop:directory-pathname-p to)
                            (merge-pathnames (format nil "quicklisp-~A.lisp"
                                                     (generate-random-string))
                                             to)
                            to)))
    (uiop:symbol-call '#:ql-http '#:http-fetch
                      "http://beta.quicklisp.org/quicklisp.lisp"
                      quicklisp-file)
    quicklisp-file))

(defun install-quicklisp (path)
  (message "Installing Quicklisp to ~A ..." path)
  (with-tmp-directory (tmp-dir)
    (let ((quicklisp-file (fetch-installer tmp-dir)))
      (run-lisp (list
                  `(load ,quicklisp-file)
                  (format nil "(quicklisp-quickstart:install :path #P\"~A\")"
                          path))
                :without-quicklisp t)
      t)))
