(defpackage #:qlot-tests/http
  (:use #:cl
        #:rove)
  (:import-from #:qlot/http)
  (:import-from #:qlot/errors)
  (:import-from #:qlot/modes
                #:*offline*)
  (:import-from #:usocket))
(in-package #:qlot-tests/http)

;; call-with-retry must re-invoke the thunk at least once when the first call
;; signals usocket:ns-try-again-error (transient EAI_AGAIN / DNS retry).
(deftest retry-on-ns-try-again-error
  (let ((call-count 0))
    (qlot/http::call-with-retry
     (lambda ()
       (incf call-count)
       (when (= call-count 1)
         (error (make-condition 'usocket:ns-try-again-error
                                :host-or-ip "test.example")))))
    (ok (>= call-count 2)
        "thunk is re-invoked at least once after ns-try-again-error")))

;; A persistent usocket:ns-host-not-found-error must be re-signaled as
;; qlot/errors:network-unreachable rather than propagating the raw usocket error.
(deftest dns-failure-signals-network-unreachable
  (let* ((caught (nth-value 1
                   (ignore-errors
                     (qlot/http::call-with-retry
                      (lambda ()
                        (error (make-condition 'usocket:ns-host-not-found-error
                                               :host-or-ip "missing.example")))))))
         (nru (find-symbol "NETWORK-UNREACHABLE" :qlot/errors)))
    (ok (and nru
             caught
             (ignore-errors (typep caught nru)))
        "ns-host-not-found-error is re-signaled as qlot/errors:network-unreachable")))

;; network-unreachable must be exported from :qlot/errors so callers can handle
;; it specifically, and its report must name the host/URL involved and suggest
;; checking connectivity.
(deftest network-unreachable-report-content
  (multiple-value-bind (nru status)
      (find-symbol "NETWORK-UNREACHABLE" :qlot/errors)
    (ok nru "NETWORK-UNREACHABLE symbol exists in :qlot/errors")
    (ok (eq :external status)
        "NETWORK-UNREACHABLE is exported from :qlot/errors")
    (let* ((condition (when nru
                        (ignore-errors
                          (make-condition nru :url "http://example.com/"))))
           (report (when condition
                     (with-output-to-string (s) (princ condition s)))))
      (ok (and report (search "example.com" report))
          "condition report names the failing host or URL")
      (ok (and report
               (some (lambda (w) (search w report))
                     '("network" "connectivity" "offline" "Network" "Connectivity")))
          "condition report suggests checking connectivity or --offline"))))

;; With *offline* true, fetch must signal offline-network-access immediately
;; without performing any HTTP request.
(deftest offline-fetch-blocks-network
  (let ((ona-sym (find-symbol "OFFLINE-NETWORK-ACCESS" :qlot/errors)))
    (ok ona-sym
        "qlot/errors:offline-network-access must be defined")
    (when ona-sym
      (let* ((caught (nth-value 1
                       (ignore-errors
                         (let ((*offline* t))
                           (qlot/http:fetch "http://unreachable.invalid/file"
                                            "/dev/null"))))))
        (ok (and caught (typep caught ona-sym))
            "fetch signals offline-network-access when *offline* is true")))))

;; With *offline* true, get must signal offline-network-access immediately
;; without performing any HTTP request.
(deftest offline-get-blocks-network
  (let ((ona-sym (find-symbol "OFFLINE-NETWORK-ACCESS" :qlot/errors)))
    (ok ona-sym
        "qlot/errors:offline-network-access must be defined")
    (when ona-sym
      (let* ((caught (nth-value 1
                       (ignore-errors
                         (let ((*offline* t))
                           (qlot/http:get "http://unreachable.invalid/"))))))
        (ok (and caught (typep caught ona-sym))
            "get signals offline-network-access when *offline* is true")))))
