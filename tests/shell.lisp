(defpackage #:qlot-tests/shell
  (:use #:cl
        #:rove)
  (:import-from #:qlot/utils/shell
                #:safety-shell-command
                #:shell-command-error)
  (:import-from #:qlot/logger
                #:*debug*
                #:*logger-debug-stream*))
(in-package #:qlot-tests/shell)

(deftest safety-shell-command-scrubs-token-from-error
  (let ((err (handler-case
                 (safety-shell-command "git" '("ls-remote"
                                               "https://x-access-token:SECRETTOKEN@nonexistent.invalid/foo/bar"))
               (shell-command-error (e) e))))
    (ok (typep err 'shell-command-error)
        "shell-command-error is signaled")
    (ng (search "SECRETTOKEN" (princ-to-string err))
        "token must not appear in error message")
    (ok (search "nonexistent.invalid" (princ-to-string err))
        "host must appear in error message")))

(deftest safety-shell-command-scrubs-token-from-debug-log
  (let ((log-stream (make-string-output-stream)))
    (let ((*debug* t)
          (*logger-debug-stream* log-stream))
      (handler-case
          (safety-shell-command "git" '("ls-remote"
                                        "https://x-access-token:SECRETTOKEN@nonexistent.invalid/foo/bar"))
        (shell-command-error () nil)))
    (ng (search "SECRETTOKEN" (get-output-stream-string log-stream))
        "token must not appear in debug log")))
