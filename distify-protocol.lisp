(defpackage #:qlot/distify-protocol
  (:use #:cl)
  (:import-from #:qlot/source
                #:source-project-name
                #:source-version
                #:source-frozen-slots
                #:source-initargs
                #:defrost-source)
  (:import-from #:qlot/utils/ql
                #:*system-quicklisp-home*)
  (:import-from #:qlot/utils/shell
                #:run-lisp)
  (:export #:prepare-source-for-dist
           #:lock-version
           #:distify-source)
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

(defgeneric distify-source (source prep-dir &key distinfo-only)
  (:documentation "Convert SOURCE, prepared into PREP-DIR, into an
ql-installable dist. SOURCE will have had LOCK-VERSION called on it
before this method is called, although this method may be called on a
defrosted copy of the source, and not the original object."))

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
