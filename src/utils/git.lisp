(defpackage #:qlot/utils/git
  (:use #:cl)
  (:import-from #:qlot/utils/shell
                #:safety-shell-command
                #:shell-command-error
                #:shell-command-error-output)
  (:import-from #:qlot/utils
                #:split-with
                #:starts-with)
  (:export #:git-switch-tag
           #:git-clone
           #:create-git-tarball
           #:git-ref))
(in-package #:qlot/utils/git)

(defun git-fetch (directory &rest args)
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "fetch" "--quiet" ,@args))
  (values))

(defun git-checkout (directory ref)
  (safety-shell-command "git" `("-C" ,(uiop:native-namestring directory)
                                "checkout" ,ref "--quiet"))
  (values))

(defun git-switch-tag (directory tag)
  (git-fetch directory "origin" (format nil "refs/tags/~A:refs/tags/~:*~A" tag) "--no-tags")
  (git-checkout directory tag)
  (values))

(defun git-clone (remote-url destination &key checkout-to ref (recursive t))
  (let ((shallow (not ref)))
    (tagbody git-cloning
      (when (uiop:directory-exists-p destination)
        #+(or mswindows win32)
        (uiop:run-program (list "attrib"
                                "-r" "-h"
                                (format nil "~A*.*" (uiop:native-namestring destination))
                                "/s" "/d")
                          :error-output *error-output*
                          :ignore-error-status t)
        (uiop:delete-directory-tree destination :validate t))
      (restart-case
          (handler-bind ((shell-command-error
                           (lambda (e)
                             (when (and shallow
                                        (starts-with "fatal: dumb http transport does not support shallow capabilities"
                                                     (shell-command-error-output e)))
                               (setf shallow nil)
                               (go git-cloning)))))
            (safety-shell-command "git"
                                  `("clone"
                                    ,@(and checkout-to
                                           `("--branch" ,checkout-to))
                                    ,@(and shallow
                                           '("--depth" "1"))
                                    ,@(and recursive
                                           '("--recursive"))
                                    "--quiet"
                                    "--config" "core.eol=lf"
                                    "--config" "core.autocrlf=input"
                                    ,remote-url
                                    ,(uiop:native-namestring destination))))
        (retry-git-clone ()
          :report "Retry to git clone the repository."
          (uiop:delete-directory-tree destination :validate t :if-does-not-exist :ignore)
          (go git-cloning)))))

  (when ref
    (git-fetch destination)
    (git-checkout destination ref)))

(defun create-git-tarball (project-directory destination ref)
  (check-type project-directory pathname)
  (check-type destination pathname)
  (check-type ref string)
  (let ((prefix (car (last (pathname-directory project-directory)))))
    (safety-shell-command "git"
                          `("-C" ,(uiop:native-namestring project-directory)
                            "archive" "--format=tar.gz" ,(format nil "--prefix=~A/" prefix)
                            ,ref
                            "-o" ,(uiop:native-namestring destination)))
    destination))

(defun git-ref (remote-url &optional (ref-identifier "HEAD"))
  (handler-case
      (let ((*standard-output* (make-broadcast-stream)))
        (first
          (split-with #\Tab
                      (safety-shell-command "git"
                                            (list "ls-remote"
                                                  remote-url
                                                  ref-identifier))
                      :limit 2)))
    (shell-command-error (e)
      (warn (princ-to-string e))
      (error "No git references named '~A'." ref-identifier))))
