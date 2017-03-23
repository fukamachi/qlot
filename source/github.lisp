(defpackage #:qlot.source.github
  (:nicknames #:qlot/source/github)
  (:use #:cl
        #:qlot/source)
  (:import-from #:qlot/source/http
                #:source-http
                #:source-http-url
                #:url)
  (:import-from #:qlot/proxy
                #:get-proxy)
  (:import-from #:dexador)
  (:import-from #:yason
                #:parse)
  (:export #:source-github
           #:source-github-repos
           #:source-github-ref
           #:source-github-branch
           #:source-github-tag))
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

(defun source-github-url (source)
  (format nil "https://github.com/~A/archive/~A.tar.gz"
          (source-github-repos source)
          (or (source-github-ref source)
              (source-github-branch source)
              (source-github-tag source)
              "master")))

(defmethod initialize-instance :after ((source source-github) &key)
  (unless (slot-boundp source 'url)
    (setf (source-http-url source)
          (source-github-url source))))

(defmethod make-source ((source (eql 'source-github)) &rest args)
  (destructuring-bind (project-name repos &key ref branch tag) args
    (make-instance 'source-github
                   :project-name project-name
                   :repos repos
                   :ref ref
                   :branch branch
                   :tag tag)))

(defmethod freeze-source-slots ((source source-github))
  `(:repos ,(source-github-repos source)
    :url ,(source-github-url source)
    :ref ,(source-github-ref source)))

(defun retrieve-from-github (repos action)
  (let ((github-access-token (uiop:getenv "GITHUB_ACCESS_TOKEN")))
    (yason:parse
     (apply #'dex:get
            (format nil "https://api.github.com/repos/~A/~A" repos action)
            :want-stream t
            :proxy (get-proxy)
            :insecure t
            (if github-access-token
                (list :basic-auth (cons github-access-token "x-oauth-basic"))
                '())))))

(defun retrieve-source-git-ref-from-github (source)
  (labels ((find-ref (results name)
             (let ((result (find-if (lambda (result)
                                      (string= (gethash "name" result) name))
                                    results)))
               (and result
                    (gethash "sha" (gethash "commit" result)))))
           (get-ref (action name)
             (find-ref (retrieve-from-github (source-github-repos source) action)
                       name)))
    (cond
      ((source-github-ref source))
      ((source-github-branch source)
       (get-ref "branches" (source-github-branch source)))
      ((source-github-tag source)
       (get-ref "tags" (source-github-tag source)))
      (T (get-ref "branches" "master")))))

(defmethod prepare :after ((source source-github))
  (setf (source-github-ref source)
        (retrieve-source-git-ref-from-github source))
  (setf (source-version source)
        (format nil "github-~A" (source-github-ref source))))

(defmethod update-available-p ((source source-github) current-version)
  (setf (source-github-ref source)
        (retrieve-source-git-ref-from-github source))
  (setf (source-version source)
        (format nil "github-~A" (source-github-ref source)))
  (not (string= (source-version source) current-version)))

(defmethod source-equal ((source1 source-github) (source2 source-github))
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
