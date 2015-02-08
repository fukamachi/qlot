(in-package :cl-user)
(defpackage qlot.source.git
  (:use :cl
        :qlot.source)
  (:import-from :qlot.tmp
                :tmp-path)
  (:import-from :qlot.archive
                :create-tarball)
  (:import-from :qlot.shell
                :safety-shell-command
                :shell-command-error)
  (:import-from :fad
                :directory-exists-p
                :delete-directory-and-files)
  (:export :source-git
           :retry-git-clone))
(in-package :qlot.source.git)

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
        :accessor source-git-tag)))

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
  (git-clone source (source-directory source))
  (setf (source-git-ref source)
        (retrieve-source-git-ref source))
  (setf (source-version source)
        (format nil "git-~A" (source-git-ref source)))
  (setf (source-archive source)
        (pathname
         (format nil "~A-~A.tar.gz"
                 (source-project-name source)
                 (source-version source)))))

(defmethod package-source ((source source-git))
  (create-tarball (source-directory source)
                  (source-archive source)))

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
  (labels ((show-ref (pattern)
             (handler-case
                 (let ((*standard-output* (make-broadcast-stream)))
                   (ppcre:scan-to-strings "^\\S+"
                                          (safety-shell-command "git"
                                                                (list "--git-dir"
                                                                      (format nil "~A.git"
                                                                              (source-directory source))
                                                                      "show-ref"
                                                                      pattern))))
               (shell-command-error ()
                 (error "No git references named '~A'." pattern))))
           (get-ref (source)
             (cond
               ((source-git-ref source))
               ((source-git-branch source)
                (show-ref (format nil "refs/heads/~A" (source-git-branch source))))
               ((source-git-tag source)
                (show-ref (format nil "refs/tags/~A" (source-git-tag source))))
               (T (show-ref "HEAD")))))
    (get-ref source)))

(defun git-clone (source destination)
  (check-type source source-git)
  (let ((checkout-to (or (source-git-ref source)
                         (source-git-branch source)
                         (source-git-tag source))))
    (tagbody git-cloning
       (restart-case
           (safety-shell-command "git"
                                 (list "clone"
                                       "--recursive"
                                       (source-git-remote-url source)
                                       destination))
         (retry-git-clone ()
           :report "Retry to git clone the repository."
           (when (fad:directory-exists-p destination)
             (fad:delete-directory-and-files destination))
           (go git-cloning))))
    (when checkout-to
      (let ((*error-output* (make-broadcast-stream)))
        (safety-shell-command "git"
                              (list "--git-dir"
                                    (format nil "~A.git" destination)
                                    "--work-tree"
                                    destination
                                    "checkout"
                                    checkout-to))))))
