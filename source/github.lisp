(defpackage #:qlot/source/github
  (:nicknames #:qlot.source.github)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:export #:source-github
           #:source-github-repos
           #:source-github-ref
           #:source-github-branch
           #:source-github-tag
           #:source-github-identifier
           #:source-github-url))
(in-package #:qlot/source/github)

(defclass source-github (source)
  ((repos :initarg :repos
          :accessor source-github-repos)
   (ref :initarg :ref
        :initform nil
        :accessor source-github-ref)
   (branch :initarg :branch
           :initform nil
           :accessor source-github-branch)
   (tag :initarg :tag
        :initform nil
        :accessor source-github-tag)))

(defun source-github-identifier (source)
  (or (source-github-ref source)
      (source-github-branch source)
      (source-github-tag source)
      "master"))

(defun source-github-url (source)
  (format nil "https://github.com/~A/archive/~A.tar.gz"
          (source-github-repos source)
          (if (source-locked-p source)
              ;; If locked, get a ref out of the version.
              (subseq (source-version source)
                      (length (source-version-prefix source)))
              ;; Otherwise use a ref name from the args.
              (source-github-identifier source))))

(defmethod make-source ((source (eql :github)) &rest initargs)
  (handler-case
      (destructuring-bind (project-name repos &key ref branch tag) initargs
        (make-instance 'source-github
                       :project-name project-name
                       :repos repos
                       :ref ref
                       :branch branch
                       :tag tag))
    (error ()
      (error 'invalid-definition
             :source :github
             :usage "github <project name> <user/repository> [:ref <commit sha1>] [:branch <branch name>] [:tag <tag name>]"))))

(defmethod source= ((source1 source-github) (source2 source-github))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (source-github-repos source1)
                (source-github-repos source2))
       (equal (source-github-ref source1)
              (source-github-ref source2))
       (equal (source-github-branch source1)
              (source-github-branch source2))
       (equal (source-github-tag source1)
              (source-github-tag source2))))
