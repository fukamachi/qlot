(defpackage #:qlot-tests/utils/asdf
  (:use #:cl
        #:rove)
  (:import-from #:qlot/utils/asdf
                #:with-directory
                #:with-autoload-on-missing)
  (:import-from #:qlot/modes
                #:*offline*))
(in-package #:qlot-tests/utils/asdf)

(deftest with-directory-tests
  (let ((system-hash (make-hash-table :test 'equal)))
    (with-directory (system-file system-name dependencies)
        (asdf:system-source-directory :qlot)
      (declare (ignore dependencies))
      (push system-name (gethash system-file system-hash)))
    (ok (null (set-difference (gethash (asdf:system-source-file :qlot) system-hash)
                              '("qlot" "qlot/subcommands" "qlot/command" "qlot/tests")
                              :test 'equal)))
    (ok (null (set-difference (gethash (asdf:system-source-file :qlot-tests) system-hash)
                              '("qlot-tests" "qlot-tests/patched")
                              :test 'equal)))))

;;; with-autoload-on-missing autoloads missing systems via ql-dist:ensure-installed
;;; (a network fetch) when online, but must skip that in offline mode -- the macro
;;; runs in the dependency-resolution child process, which has no release cache.
;;; ensure-installed is trapped; asdf:load-system of an absent system drives the
;;; asdf:missing-component handler the macro establishes.
(deftest with-autoload-on-missing-offline-guard
  (let ((ensure-sym (and (find-package '#:ql-dist)
                         (find-symbol (string '#:ensure-installed) '#:ql-dist))))
    (ok ensure-sym "ql-dist:ensure-installed must be available for this test")
    (when ensure-sym
      (let ((original (symbol-function ensure-sym)))
        (unwind-protect
             (let (called)
               (setf (symbol-function ensure-sym)
                     (lambda (&rest args) (declare (ignore args)) (setf called t)))
               ;; Offline: the handler short-circuits, so ensure-installed is
               ;; never reached and the missing-component error propagates.
               (setf called nil)
               (let ((*offline* t))
                 (ignore-errors
                   (with-autoload-on-missing
                     (asdf:load-system "qlot-nonexistent-offline-test-xyz"))))
               (ng called
                   "ensure-installed is not called when *offline* is t")
               ;; Online: the handler attempts the autoload (network fetch).
               (setf called nil)
               (let ((*offline* nil))
                 (ignore-errors
                   (with-autoload-on-missing
                     (asdf:load-system "qlot-nonexistent-offline-test-xyz"))))
               (ok called
                   "ensure-installed is attempted when *offline* is nil"))
          (setf (symbol-function ensure-sym) original))))))
