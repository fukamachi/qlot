(defpackage #:qlot-tests/distify/ql
  (:use #:cl
        #:rove
        #:qlot/distify/ql
        #:qlot/distify/dist)
  (:import-from #:qlot/source
                #:make-source
                #:source-project-name
                #:source-version)
  (:import-from #:qlot/source/dist
                #:source-distinfo-url)
  (:import-from #:qlot/modes
                #:*locked*)
  (:import-from #:qlot/utils/ql
                #:parse-distinfo-file
                #:parse-space-delimited-file)
  (:import-from #:qlot/utils/tmp
                #:tmp-directory)
  (:import-from #:qlot/errors
                #:qlot-error)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:qlot-tests/distify/ql)

(defparameter *tmp-directory* (tmp-directory))

(setup
  (uiop:delete-directory-tree *tmp-directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *tmp-directory*))

(deftest distify-ql-tests
  (let ((source (make-source :ql "log4cl" "2014-03-17")))
    (distify-ql source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "log4cl"
                                       :type "txt"
                                       :defaults *tmp-directory*))
          (systems.txt (merge-pathnames (format nil "~A/~A/systems.txt"
                                                (source-project-name source)
                                                (source-version source))
                                        *tmp-directory*))
          (releases.txt (merge-pathnames (format nil "~A/~A/releases.txt"
                                                 (source-project-name source)
                                                 (source-version source))
                                         *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (ok (uiop:file-exists-p systems.txt))
      (ok (uiop:file-exists-p releases.txt))
      (let ((systems (parse-space-delimited-file systems.txt)))
        (ok (< 0 (length systems)))
        (ok (every (lambda (row)
                     (equal (first row) "log4cl"))
                   systems)))
      (let ((releases (parse-space-delimited-file releases.txt)))
        (ok (< 0 (length releases)))
        (ok (every (lambda (row)
                     (equal (first row) "log4cl"))
                   releases)))))

  (let ((source (make-source :ql "not-found-project" "2014-03-17")))
    (ok (signals (distify-ql source *tmp-directory*)
                 'qlot-error))

    (ng (uiop:file-exists-p (make-pathname :name "not-found-project"
                                           :type "txt"
                                           :defaults *tmp-directory*))))

  (let ((source (make-source :ql "hunchentoot" "2000-01-01")))
    (ok (signals (distify-ql source *tmp-directory*)
                 'qlot-error))

    (ng (uiop:file-exists-p (make-pathname :name "hunchentoot"
                                           :type "txt"
                                           :defaults *tmp-directory*))))

  (let ((source (make-source :ql :all :latest)))
    (distify-dist source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "quicklisp"
                                       :type "txt"
                                       :defaults *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (let ((data (parse-distinfo-file distinfo.txt)))
        (ok (equal (aget data "name") "quicklisp")))))

  (let ((source (make-source :ql :all "2019-08-13")))
    (distify-dist source *tmp-directory*)

    (let ((distinfo.txt (make-pathname :name "quicklisp"
                                       :type "txt"
                                       :defaults *tmp-directory*)))
      (ok (uiop:file-exists-p distinfo.txt))
      (let ((data (parse-distinfo-file distinfo.txt)))
        (ok (equal (aget data "name") "quicklisp"))
        (ok (equal (aget data "version") "2019-08-13"))))))

;;; Minimal canned data returned by the qlot/http:get stub used in the three
;;; load-source-ql-version locked-mode tests.
;;;
;;; Call 1 (distinfo): parse-distinfo-stream needs "version" and "release-index-url".
;;; Call 2 (release-index): parse-space-delimited-stream needs a #-comment header
;;;   then a row whose first field matches (source-project-name source).
;;;   The second field is the archive URL; get-version-from-archive-url extracts
;;;   the date component "2014-03-17" from it and uses that as the resolved version.

(defun make-canned-distinfo-stream ()
  (make-string-input-stream
   "version: 2014-03-17
release-index-url: https://beta.quicklisp.org/dist/quicklisp/2014-03-17/releases.txt
"))

(defun make-canned-release-index-stream ()
  (make-string-input-stream
   "# project url size file-md5 content-sha1 prefix
log4cl https://beta.quicklisp.org/archive/log4cl/2014-03-17/log4cl-20140317-git.tgz 1234 abc def log4cl-20140317-git
"))

(defmacro with-http-get-stub ((call-count-var) &body body)
  "Replace qlot/http:get with a counting stub that returns canned streams.
  CALL-COUNT-VAR is bound to a place holding the number of calls made."
  (let ((saved (gensym "SAVED")))
    `(let ((,call-count-var 0)
           (,saved (fdefinition 'qlot/http:get)))
       (setf (fdefinition 'qlot/http:get)
             (lambda (url &rest args)
               (declare (ignore url args))
               (incf ,call-count-var)
               (case ,call-count-var
                 (1 (make-canned-distinfo-stream))
                 (2 (make-canned-release-index-stream))
                 (otherwise
                  (error "qlot/http:get stub: unexpected call ~A" ,call-count-var)))))
       (unwind-protect
            (progn ,@body)
         (setf (fdefinition 'qlot/http:get) ,saved)))))

(defun make-pinned-source ()
  "Return a source-ql for 'log4cl' with the version slot already bound,
  simulating a source that was defrosted from qlfile.lock.
  source-distinfo-url is set to a non-nil dummy so load-source-ql-version
  does not call get-distinfo-url before its first qlot/http:get.
  The sentinel version is deliberately distinct from the canned network-resolved
  value (ql-2014-03-17) so tests can distinguish 'version preserved from lock'
  from 'version re-resolved from network'."
  (let ((source (make-source :ql "log4cl" "2014-03-17")))
    (setf (source-version source) "ql-pinned-from-lock")
    (setf (source-distinfo-url source)
          "https://beta.quicklisp.org/dist/quicklisp/2014-03-17/distinfo.txt")
    source))

(defun make-unpinned-source ()
  "Return a source-ql for 'log4cl' whose version slot is unbound,
  simulating a source read from qlfile (never defrosted).
  source-distinfo-url is set to a non-nil dummy for the same reason."
  (let ((source (make-source :ql "log4cl" "2014-03-17")))
    (setf (source-distinfo-url source)
          "https://beta.quicklisp.org/dist/quicklisp/2014-03-17/distinfo.txt")
    source))

;;; *locked* t + pinned version → qlot/http:get NOT called.
;;; load-source-ql-version must return early without touching the network
;;; when *locked* is true and the source already carries a bound version slot
;;; (i.e. it was defrosted from qlfile.lock).
(deftest locked-pin-respect-skips-network
  (testing "*locked* t + pinned version: qlot/http:get must not be called"
    (let ((source (make-pinned-source))
          (http-called-p nil)
          (saved (fdefinition 'qlot/http:get)))
      (setf (fdefinition 'qlot/http:get)
            (lambda (url &rest args)
              (declare (ignore url args))
              (setf http-called-p t)
              (error "network trap: qlot/http:get called despite *locked* t + pinned version")))
      (unwind-protect
           (progn
             (let ((*locked* t))
               (ignore-errors
                 (qlot/distify/ql::load-source-ql-version source)))
             (ng http-called-p
                 "qlot/http:get must NOT be called when *locked* t and version slot is bound")
             (ok (equal (source-version source) "ql-pinned-from-lock")
                 "pinned version must be preserved unchanged: early return must not alter the version slot"))
        (setf (fdefinition 'qlot/http:get) saved)))))

;;; *locked* nil + pinned version → qlot/http:get IS called.
;;; The early return must not fire when *locked* is nil, even if the version
;;; slot is already bound.  The stub verifies the two expected HTTP calls are made
;;; AND that the response was actually processed (source-version is updated from
;;; the canned data, overwriting the distinct sentinel set in make-pinned-source).
(deftest unlocked-pinned-source-still-resolves
  (testing "*locked* nil + pinned version: qlot/http:get must be called"
    (let ((source (make-pinned-source)))
      (with-http-get-stub (n)
        (let ((*locked* nil))
          (qlot/distify/ql::load-source-ql-version source))
        (ok (>= n 2)
            "qlot/http:get must be invoked twice: distinfo call then release-index call")
        (ok (equal "ql-2014-03-17" (source-version source))
            "source-version must be updated to the network-resolved value, not the sentinel pin")))))

;;; *locked* t + version slot UNBOUND → qlot/http:get IS called.
;;; The early return applies only when the version is already pinned.
;;; When the source has no version (undefrosted), the network must still be consulted
;;; AND the response must be processed: version slot must be bound and set to the
;;; network-resolved value after the call.
(deftest locked-unpinned-source-still-resolves
  (testing "*locked* t + version slot unbound: qlot/http:get must be called"
    (let ((source (make-unpinned-source)))
      (with-http-get-stub (n)
        (let ((*locked* t))
          (qlot/distify/ql::load-source-ql-version source))
        (ok (>= n 2)
            "qlot/http:get must be invoked twice: distinfo call then release-index call")
        (ok (and (slot-boundp source 'qlot/source/base::version)
                 (equal "ql-2014-03-17" (source-version source)))
            "source-version must be bound and set to the network-resolved value")))))
