(defsystem "qlot-tests"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "qlot"
               "qlot-tests/main"
               "qlot-tests/parser"
               "qlot-tests/distify/ql"
               "qlot-tests/distify/git"
               "qlot-tests/distify/http"
               "qlot-tests/distify/github"
               "qlot-tests/distify/dist"
               "qlot-tests/distify/ultralisp"
               "qlot-tests/distify"
               "qlot-tests/server"
               "qlot-tests/install/quicklisp"
               "qlot-tests/install/cache"
               "qlot-tests/install"
               "qlot-tests/cache"
               "qlot-tests/cache-releases"
               "qlot-tests/utils"
               "qlot-tests/utils/ql"
               "qlot-tests/utils/asdf")
  :perform (test-op (op c) (symbol-call :rove :run c)))

;; Tests that require patched quicklisp-client (with *install-release-hook*).
;; These tests verify symlink-based caching which only works with the patched client.
;; Run separately with: ros +Q run -- --load .qlot/setup.lisp --eval "(rove:run :qlot-tests/patched)"
(defsystem "qlot-tests/patched"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "qlot"
               "qlot-tests/cache-integration"
               "qlot-tests/quicklisp/clean")
  :perform (test-op (op c) (symbol-call :rove :run c)))
