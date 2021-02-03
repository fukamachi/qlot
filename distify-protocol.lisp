(defpackage #:qlot/distify-protocol
  (:use #:cl)
  (:import-from #:qlot/source
                #:source
                #:source-project-name
                #:source-version
                #:source-frozen-slots
                #:source-initargs
                #:defrost-source
                #:write-distinfo)
  (:import-from #:qlot/utils/ql
                #:*system-quicklisp-home*)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:import-from #:uiop
                #:with-output-file)
  (:export #:prepare-source-for-dist
           #:lock-version)
  (:export #:write-source-distinfo
           #:finalize-dist)
  (:export #:run-func-process))
(in-package #:qlot/distify-protocol)

(defgeneric prepare-source-for-dist (source destination)
  (:documentation "Prepare SOURCE for distification (e.g. fetching
sources), storing any intermediate artifacts in
DESTINATION. Preparation should be sufficient to determine a locked
version for the source."))

(defgeneric lock-version (source prep-dir)
  (:documentation "Set version-locking information for SOURCE, given
the prepared materials in PREP-DIR. PREPARE-SOURCE-FOR-DIST may not
have been called on this instance, but preparation will have been
performed before LOCK-VERSION is called (possibly on a copy of SOURCE
in a sub-process).

This method must set the information necessary for
version-locking (typically SOURCE-VERSION), and any additional fields
that will frozen with the object, in order for the frozen object in
the lockfile to be stable.

Returns the version that has been set by locking."))

(defgeneric write-dist-metadata (source destination)
  (:method ((source source) destination)
    (let ((destinatioon (truename destination)))
      (uiop:with-output-file (out (make-pathname :name (source-project-name source)
                                                 :type "txt"
                                                 :defaults destination)
                                  :if-exists :supersede)
        (write-distinfo source out)))))

(defun run-func-process (operation &rest args)
  "Apply the function named by OPERATION to ARGS in an isolated
sub-process."
  (check-type operation symbol)
  (let (#+quicklisp (ql:*quicklisp-home* *system-quicklisp-home*))
    (run-lisp (append
               (list `(uiop:symbol-call ,(package-name (symbol-package operation))
                                        ,(symbol-name operation)
                                        ,@args)))
              :systems (list (package-name (symbol-package operation)))
              :source-registry (asdf:system-source-directory :qlot))))

(defgeneric write-source-distinfo (source prep-dir)
  (:documentation "Write a distinfo file for SOURCE into
PREP-DIR. This involves determining the current version of the source,
which may in turn require dist data or code to be downloaded. This
must be called before FINALIZE-DIST in order to produce a complete
dist for SOURCE in PREP-DIR.")
  (:method ((source source) prep-dir)
    (prepare-source-for-dist source prep-dir)
    (lock-version source prep-dir)
    (write-dist-metadata source prep-dir)))

(defgeneric finalize-dist (source prep-dir)
  (:documentation "Finish constructing a complete installable dist for
SOURCE at PREP-DIR, after the distinfo metadata has been written by
calling WRITE-DISTINFO.

If the source version can be fetched remotely, and specific versions
of the source archive can be fetched, nearly all of the work of
producing a dist can be done here. This has performance benefits by
allowing a source to be checked for updates without WRITE-DISTINFO
having to construct a full dist.

For sources where this is not possible, like the HTTP source, this
method must perform any work not already done by WRITE-DISTINFO; as
much work as possible should be deferred to this method."))
