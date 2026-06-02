(defpackage #:qlot-tests/modes
  (:use #:cl #:rove)
  (:import-from #:qlot/modes
                #:*offline*
                #:*locked*
                #:initialize-modes)
  (:import-from #:qlot/errors
                #:offline-cache-conflict))
(in-package #:qlot-tests/modes)

;;; Save/restore env vars around a test body.
;;; A nil value is written as "" so uiop:getenvp treats it as unset.
(defmacro with-env ((&rest pairs) &body body)
  (let ((saved (gensym "SAVED")))
    `(let ((,saved
             (list ,@(mapcar (lambda (p)
                               `(cons ,(first p) (uiop:getenv ,(first p))))
                             pairs))))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (p)
                          `(setf (uiop:getenv ,(first p))
                                 ,(or (second p) "")))
                        pairs)
              ,@body)
         ;; Teardown limitation: a var that was originally unset is restored to
         ;; "" rather than truly unset (portable CL has no unsetenv). This is
         ;; safe here because every reader of QLOT_OFFLINE / QLOT_LOCKED /
         ;; QLOT_NO_CACHE goes through initialize-modes -> uiop:getenvp, which
         ;; treats "" as unset; no code reads these vars with raw uiop:getenv.
         (loop for (name . old) in ,saved
               do (setf (uiop:getenv name) (or old "")))))))

;;; --- Gate 1: QLOT_OFFLINE -> *offline* ---

(deftest offline-mode-reads-from-env
  (testing "QLOT_OFFLINE=1 makes *offline* true after initialize-modes"
    (with-env (("QLOT_OFFLINE" "1") ("QLOT_LOCKED" nil) ("QLOT_NO_CACHE" nil))
      (let ((*offline* nil) (*locked* nil))
        (initialize-modes)
        (ok *offline*
            "*offline* must be true when QLOT_OFFLINE=1"))))
  (testing "unset QLOT_OFFLINE makes *offline* nil after initialize-modes"
    (with-env (("QLOT_OFFLINE" nil) ("QLOT_LOCKED" nil) ("QLOT_NO_CACHE" nil))
      (let ((*offline* t) (*locked* nil))
        (initialize-modes)
        (ng *offline*
            "*offline* must be nil when QLOT_OFFLINE is unset")))))

;;; --- Gate 2: QLOT_LOCKED -> *locked* ---

(deftest locked-mode-reads-from-env
  (testing "QLOT_LOCKED=1 makes *locked* true after initialize-modes"
    (with-env (("QLOT_OFFLINE" nil) ("QLOT_LOCKED" "1") ("QLOT_NO_CACHE" nil))
      (let ((*offline* nil) (*locked* nil))
        (initialize-modes)
        (ok *locked*
            "*locked* must be true when QLOT_LOCKED=1"))))
  (testing "unset QLOT_LOCKED makes *locked* nil after initialize-modes"
    (with-env (("QLOT_OFFLINE" nil) ("QLOT_LOCKED" nil) ("QLOT_NO_CACHE" nil))
      (let ((*offline* nil) (*locked* t))
        (initialize-modes)
        (ng *locked*
            "*locked* must be nil when QLOT_LOCKED is unset")))))

;;; --- Gate 3: QLOT_OFFLINE + QLOT_NO_CACHE conflict ---

(deftest offline-cache-conflict-detection
  (testing "QLOT_OFFLINE + QLOT_NO_CACHE signals offline-cache-conflict"
    (with-env (("QLOT_OFFLINE" "1") ("QLOT_NO_CACHE" "1") ("QLOT_LOCKED" nil))
      (let ((*offline* nil) (*locked* nil))
        (ok (signals (initialize-modes) 'offline-cache-conflict)
            "initialize-modes must signal offline-cache-conflict when both QLOT_OFFLINE and QLOT_NO_CACHE are set"))))
  (testing "QLOT_OFFLINE alone does not signal and *offline* becomes true"
    (with-env (("QLOT_OFFLINE" "1") ("QLOT_NO_CACHE" nil) ("QLOT_LOCKED" nil))
      (let ((*offline* nil) (*locked* nil))
        (initialize-modes)
        (ok *offline*
            "*offline* must be true when QLOT_OFFLINE is set without QLOT_NO_CACHE")))))

;;; --- Gate 4: qlot-command-install flag->env bridge ---
;;;
;;; qlot/cli is gated behind :ros.installing in qlot.asd, so the test system
;;; does not depend on it at compile time.  We load it at runtime and call
;;; apply-install-mode-flags via uiop:symbol-call to keep the package reference
;;; out of read/compile time, mirroring the pattern in tests/cli.lisp.

(deftest install-flag-env-bridge
  (asdf:load-system :qlot/cli)
  (testing "--frozen (t t) sets both QLOT_OFFLINE and QLOT_LOCKED"
    (with-env (("QLOT_OFFLINE" nil) ("QLOT_LOCKED" nil) ("QLOT_NO_CACHE" nil))
      (uiop:symbol-call '#:qlot/cli '#:apply-install-mode-flags t t)
      (ok (uiop:getenvp "QLOT_OFFLINE") "--frozen: QLOT_OFFLINE must be set")
      (ok (uiop:getenvp "QLOT_LOCKED")  "--frozen: QLOT_LOCKED must be set")))
  (testing "--offline (t nil) sets only QLOT_OFFLINE"
    (with-env (("QLOT_OFFLINE" nil) ("QLOT_LOCKED" nil) ("QLOT_NO_CACHE" nil))
      (uiop:symbol-call '#:qlot/cli '#:apply-install-mode-flags t nil)
      (ok (uiop:getenvp "QLOT_OFFLINE")  "--offline: QLOT_OFFLINE must be set")
      (ng (uiop:getenvp "QLOT_LOCKED")   "--offline: QLOT_LOCKED must NOT be set")))
  (testing "--locked (nil t) sets only QLOT_LOCKED"
    (with-env (("QLOT_OFFLINE" nil) ("QLOT_LOCKED" nil) ("QLOT_NO_CACHE" nil))
      (uiop:symbol-call '#:qlot/cli '#:apply-install-mode-flags nil t)
      (ng (uiop:getenvp "QLOT_OFFLINE")  "--locked: QLOT_OFFLINE must NOT be set")
      (ok (uiop:getenvp "QLOT_LOCKED")   "--locked: QLOT_LOCKED must be set"))))

;;; --- Gate 5: README documents all three install modes and both env vars ---

(deftest readme-documents-modes
  (let* ((readme (asdf:system-relative-pathname :qlot #P"README.markdown"))
         (content (uiop:read-file-string readme)))
    (ok (search "--offline" content :test #'char-equal)
        "README.markdown documents --offline flag")
    (ok (search "--locked" content :test #'char-equal)
        "README.markdown documents --locked flag")
    (ok (search "--frozen" content :test #'char-equal)
        "README.markdown documents --frozen flag")
    (ok (search "QLOT_OFFLINE" content)
        "README.markdown documents QLOT_OFFLINE env var")
    (ok (search "QLOT_LOCKED" content)
        "README.markdown documents QLOT_LOCKED env var")))
