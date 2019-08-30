(defpackage #:qlot/install
  (:use #:cl)
  (:import-from #:qlot/install/quicklisp
                #:install-quicklisp)
  (:import-from #:qlot/source
                #:source-dist-name
                #:source-distinfo-url)
  (:import-from #:qlot/parser
                #:parse-qlfile)
  (:import-from #:qlot/server
                #:with-qlot-server)
  (:import-from #:qlot/utils
                #:with-package-functions)
  (:import-from #:qlot/utils/ql
                #:with-quicklisp-home))
(in-package #:qlot/install)

(defvar *qlot-directory* #P".qlot/")

(defun install-qlfile (qlfile &key (quicklisp-home *qlot-directory*))
  (unless (uiop:file-exists-p qlfile)
    (error "File does not exist: ~A" qlfile))

  (let ((qlhome (merge-pathnames quicklisp-home)))
    (unless (and (uiop:directory-exists-p qlhome)
                 (uiop:file-exists-p (merge-pathnames "setup.lisp" qlhome)))
      (ensure-directories-exist qlhome)
      (install-quicklisp qlhome))

    (unless (find-package :ql)
      (load (merge-pathnames #P"setup.lisp" qlhome)))

    (apply-qlfile-to-qlhome qlfile qlhome)

    (format t "~&Successfully installed.~%")))

(defun install-source (source)
  (format t "~&Installing dist ~S.~%"
          (source-dist-name source))
  (uiop:symbol-call '#:ql-dist '#:install-dist
                    (source-distinfo-url source)
                    :prompt nil
                    :replace nil))

(defun apply-qlfile-to-qlhome (qlfile qlhome)
  (with-quicklisp-home qlhome
    (with-qlot-server qlfile
      (let ((sources (parse-qlfile qlfile))
            (preference (get-universal-time)))
        (dolist (source sources)
          (install-source source)
          (with-package-functions #:ql-dist (dist (setf preference))
            (setf (preference (dist (source-dist-name source)))
                  (incf preference))))))))
