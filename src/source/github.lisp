(defpackage #:qlot/source/github
  (:nicknames #:qlot.source.github)
  (:use #:cl
        #:qlot/source/base
        #:qlot/source/http)
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

(defmethod usage-of-source ((source (eql :github)))
  "github <user/repository> [:ref <commit sha1>] [:branch <branch name>] [:tag <tag name>]")

(defmethod make-source ((source (eql :github)) &rest initargs)
  (let (project-name)
    ;; Assume project-name by the repository identifier
    (unless (or (= 1 (length initargs))
                (eql 1 (position-if #'keywordp initargs)))
      (setf project-name (pop initargs)))
    (destructuring-bind (repos &key ref branch tag) initargs
      (unless project-name
        (destructuring-bind (username repository)
            (split-with #\/ repos)
          (declare (ignore username))
          (setf project-name repository)))
      (make-instance 'source-github
                     :repos repos
                     :ref ref
                     :branch branch
                     :tag tag
                     :project-name project-name))))

(defmethod defrost-source :after ((source source-github))
  (when (slot-boundp source 'qlot/source/base::version)
    (setf (source-github-ref source)
          (subseq (source-version source)
                  (length (source-version-prefix source))))))

(defmethod source-identifier ((source source-github))
  (source-github-repos source))

(defmethod source= ((source1 source-github) (source2 source-github))
  (and (string= (source-github-repos source1)
                (source-github-repos source2))
       (equal (source-github-ref source1)
              (source-github-ref source2))
       (equal (source-github-branch source1)
              (source-github-branch source2))
       (equal (source-github-tag source1)
              (source-github-tag source2))))

(defmethod print-object ((source source-github) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "~A ~A"
            (source-github-repos source)
            (or (ignore-errors (source-github-identifier source))
                "<undetermined>"))))
