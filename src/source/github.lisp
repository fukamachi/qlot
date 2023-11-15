(defpackage #:qlot/source/github
  (:nicknames #:qlot.source.github)
  (:use #:cl
        #:qlot/source/base
        #:qlot/source/http)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:import-from #:qlot/utils
                #:split-with)
  (:export #:source-github
           #:source-github-repos
           #:source-github-ref
           #:source-github-branch
           #:source-github-tag
           #:source-github-identifier
           #:source-github-url))
(in-package #:qlot/source/github)

(defclass source-github (source-http)
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
      ;; When reached here it should be a bug
      (error "Undetermined GitHub identifier")))

(defun source-github-url (source)
  (format nil "https://github.com/~A/archive/~A.tar.gz"
          (source-github-repos source)
          (source-github-identifier source)))

(defmethod make-source ((source (eql :github)) &rest initargs)
  ;; Assume project-name by the repository identifier
  (when (or (= 1 (length initargs))
            (eql 1 (position-if #'keywordp initargs)))
    (let ((repos (first initargs)))
      (destructuring-bind (username repository)
          (split-with #\/ repos)
        (declare (ignore username))
        (setf initargs
              (cons repository initargs)))))
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
             :usage "github <user/repository> [:ref <commit sha1>] [:branch <branch name>] [:tag <tag name>]"))))

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
