(defpackage #:qlot/tests/util
  (:use #:cl
        #:qlot
        #:rove)
  (:import-from #:qlot/util
                #:extend-source-registry)
  (:import-from #:uiop
                #:file-exists-p
                #:delete-directory-tree
                #:subdirectories))
(in-package #:qlot/tests/util)


(deftest extend-registry-test
  (rove:ok (string-equal
            (extend-source-registry "(:source-registry (:tree (:home \"common-lisp\")) :inherit-configuration)"
                                    "foo/bar")
            "(:SOURCE-REGISTRY (:DIRECTORY (\"foo/bar\")) (:TREE (:HOME \"common-lisp\")) :INHERIT-CONFIGURATION)")))
