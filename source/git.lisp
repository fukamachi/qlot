(defpackage #:qlot.source.git
  (:nicknames #:qlot/source/git)
  (:use #:cl
        #:qlot/source)
  (:import-from #:qlot/shell
                #:safety-shell-command
                #:shell-command-error)
  (:import-from #:qlot/util
                #:with-in-directory)
  (:import-from #:uiop
                #:delete-directory-tree)
  (:export #:source-git
           #:retry-git-clone))
(in-package #:qlot/source/git)

(defclass source-git (source-has-directory)
  ((remote-url :initarg :remote-url
               :accessor source-git-remote-url)
   (ref :initarg :ref
        :initform nil
        :accessor source-git-ref)
   (branch :initarg :branch
           :initform nil
           :accessor source-git-branch)
   (tag :initarg :tag
        :initform nil
        :accessor source-git-tag)

   (git-cloned-p :initform nil
                 :accessor git-cloned-p)))

(defmethod make-source ((source (eql 'source-git)) &rest initargs)
  (destructuring-bind (project-name remote-url &rest args) initargs
    (apply #'make-instance 'source-git
           :project-name project-name
           :remote-url remote-url
           args)))

(defmethod freeze-source-slots ((source source-git))
  `(:remote-url ,(source-git-remote-url source)
    :ref ,(source-git-ref source)))

(defmethod prepare ((source source-git))
  (setf (source-directory source)
        (pathname
         (format nil "~A~:[~;~:*-~A~]/"
                 (source-project-name source)
                 (source-git-identifier source))))
  (setf (source-git-ref source)
        (retrieve-source-git-ref source))
  (setf (source-version source)
        (format nil "git-~A" (source-git-ref source))))

(defmethod archive ((source source-git))
  (unless (git-cloned-p source)
    (git-clone source (source-directory source))
    (let ((prefix (car (last (pathname-directory (source-directory source))))))
      (setf (source-archive source)
            (pathname
             (format nil "~A.tar.gz" prefix)))
      (with-in-directory (source-directory source)
        (safety-shell-command "git"
                              `("archive" "--format=tar.gz" ,(format nil "--prefix=~A/" prefix)
                                          ,(source-git-ref source)
                                          "-o" ,(source-archive source))))))

  (source-archive source))

(defmethod source-equal ((source1 source-git) (source2 source-git))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (source-git-remote-url source1)
                (source-git-remote-url source2))
       (equal (source-git-ref source1)
              (source-git-ref source2))
       (equal (source-git-branch source1)
              (source-git-branch source2))
       (equal (source-git-tag source1)
              (source-git-tag source2))))

(defmethod print-object ((source source-git) stream)
  (format stream "#<~S ~A ~A~:[~;~:* ~A~]>"
          (type-of source)
          (source-project-name source)
          (source-git-remote-url source)
          (source-git-identifier source)))

(defun source-git-identifier (source)
  (cond
    ((source-git-ref source)
     (concatenate 'string "ref-" (source-git-ref source)))
    ((source-git-branch source)
     (concatenate 'string "branch-" (source-git-branch source)))
    ((source-git-tag source)
     (concatenate 'string "tag-" (source-git-tag source)))))

(defun retrieve-source-git-ref (source)
  (check-type source source-git)
  (flet ((show-ref (pattern)
           (handler-case
               (let ((*standard-output* (make-broadcast-stream)))
                 (ppcre:scan-to-strings "^\\S+"
                                        (safety-shell-command "git"
                                                              (list "ls-remote"
                                                                    (source-git-remote-url source)
                                                                    pattern))))
             (shell-command-error ()
               (error "No git references named '~A'." pattern)))))
    (or (source-git-ref source)
        (show-ref (or (source-git-tag source)
                      (source-git-branch source)
                      "HEAD")))))

(defun git-clone (source destination)
  (check-type source source-git)
  (when (git-cloned-p source)
    (return-from git-clone))
  (setf (git-cloned-p source) t)

  (let ((checkout-to (or (source-git-branch source)
                         (source-git-tag source)
                         "master")))
    (tagbody git-cloning
       (restart-case
           (safety-shell-command "git"
                                 `("clone"
                                   "--branch" ,checkout-to
                                   "--depth" "1"
                                   "--recursive"
                                   "--config" "core.eol=lf"
                                   "--config" "core.autocrlf=input"
                                   ,(source-git-remote-url source)
                                   ,destination))
         (retry-git-clone ()
           :report "Retry to git clone the repository."
           (uiop:delete-directory-tree destination :validate t :if-does-not-exist :ignore)
           (go git-cloning)))))

  (when (source-git-ref source)
    (let ((*error-output* (make-broadcast-stream)))
      (with-in-directory destination
        (safety-shell-command "git" '("fetch" "--unshallow"))
        (safety-shell-command "git"
                              (list "checkout"
                                    (source-git-ref source))))))

  (values))
