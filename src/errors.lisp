(defpackage #:qlot/errors
  (:use #:cl)
  (:export #:qlot-error
           #:qlot-simple-error
           #:unknown-source
           #:invalid-definition
           #:invalid-project-name
           #:duplicate-project
           #:qlfile-parse-failed
           #:missing-projects
           #:unnecessary-projects
           #:outdated-projects
           #:qlfile-not-found
           #:qlfile-lock-not-found
           #:qlot-directory-not-found
           #:qlot-directory-invalid
           #:network-unreachable
           #:offline-cache-conflict
           #:offline-cache-miss
           #:offline-cache-miss-project-name
           #:offline-cache-miss-requested-version
           #:offline-network-access
           #:offline-network-access-target
           #:locked-operation-rejected
           #:locked-operation-rejected-operation
           #:github-ratelimit-error
           #:ros-command-error
           #:command-not-found
           #:qlot-warning
           #:qlot-simple-warning
           #:ros-command-warn))
(in-package #:qlot/errors)

(define-condition qlot-error (error) ())

(define-condition qlot-simple-error (qlot-error simple-error) ())

(define-condition qlot-syntax-error (qlot-error) ())

(define-condition unknown-source (qlot-syntax-error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Unknown source: ~A"
                     (slot-value condition 'name)))))

(define-condition invalid-definition (qlot-syntax-error)
  ((source :initarg :source)
   (usage :initarg :usage
          :initform nil)
   (reason :initarg :reason
           :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid definition of '~(~A~)'.~@[~%[usage] ~A~]"
                     (slot-value condition 'source)
                     (slot-value condition 'usage)))))

(define-condition invalid-project-name (qlot-syntax-error)
  ((name :initarg :name)
   (reason :initarg :reason
           :initform nil))
  (:report (lambda (condition stream)
             (with-slots (name reason) condition
               (format stream "~A: ~A" reason name)))))

(define-condition duplicate-project (qlot-syntax-error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Duplicate project: ~A"
                     (slot-value condition 'name)))))

(define-condition qlfile-parse-failed (qlot-error)
  ((file :initarg :file)
   (lineno :initarg :lineno)
   (line :initarg :line)
   (error :initarg :error))
  (:report (lambda (condition stream)
             (with-slots (file lineno line error) condition
               (format stream "Error raised while parsing '~A' at line ~A:~2%  ~A~2%~A"
                       file lineno line error)))))

(define-condition missing-projects (qlot-error)
  ((projects :initarg :projects))
  (:report (lambda (condition stream)
             (with-slots (projects) condition
               (format stream "The following libraries are missing:~%~{ * ~A~^~%~}"
                       projects)))))

(define-condition unnecessary-projects (qlot-error)
  ((projects :initarg :projects))
  (:report (lambda (condition stream)
             (with-slots (projects) condition
               (format stream "The following libraries need to be removed:~%~{ * ~A~^~%~}"
                       projects)))))

(define-condition outdated-projects (qlot-error)
  ((projects :initarg :projects))
  (:report (lambda (condition stream)
             (with-slots (projects) condition
               (format stream "New updates found for the following libraries:~%~{ * ~A~^~%~}"
                       projects)))))

(define-condition file-not-found (qlot-error)
  ((path :initarg :path))
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "File not found: ~A" path)))))

(define-condition qlfile-not-found (file-not-found)
  ()
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "qlfile not found: ~A" path)))))

(define-condition qlfile-lock-not-found (file-not-found)
  ()
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "qlfile.lock not found: ~A" path)))))

(define-condition qlot-directory-not-found (file-not-found)
  ()
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "Directory '~A' does not exist." path)))))

(define-condition qlot-directory-invalid (file-not-found)
  ()
  (:report (lambda (condition stream)
             (with-slots (path) condition
               (format stream "Directory '~A' is not valid." path)))))

(define-condition network-unreachable (qlot-error)
  ((url :initarg :url
        :initform nil)
   (condition :initarg :condition
              :initform nil))
  (:report (lambda (condition stream)
             (with-slots (url) condition
               (format stream "Network unreachable~@[: ~A~]. Check network connectivity or use --offline with a warm cache."
                       url)))))

(define-condition offline-cache-conflict (qlot-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Offline mode cannot be used with cache disabled."))))

(define-condition offline-cache-miss (qlot-error)
  ((project-name :initarg :project-name
                 :reader offline-cache-miss-project-name)
   (requested-version :initarg :requested-version
                      :initarg :version
                      :reader offline-cache-miss-requested-version))
  (:report (lambda (condition stream)
             (with-slots (project-name requested-version) condition
               (format stream "Offline cache miss for ~S version ~S. Populate the cache online before installing with --offline."
                       project-name
                       requested-version)))))

(define-condition offline-network-access (qlot-error)
  ((target :initarg :target
           :initform nil
           :reader offline-network-access-target))
  (:report (lambda (condition stream)
             (with-slots (target) condition
               (format stream "Offline mode blocked network access~@[: ~A~]."
                       target)))))

(define-condition locked-operation-rejected (qlot-error)
  ((operation :initarg :operation
              :initform nil
              :reader locked-operation-rejected-operation))
  (:report (lambda (condition stream)
             (with-slots (operation) condition
               (if operation
                   (format stream "~A is incompatible with --locked; use qlot install to install pinned versions."
                           operation)
                   (format stream "This operation is incompatible with --locked; use qlot install to install pinned versions."))))))

(define-condition github-ratelimit-error (qlot-error)
  ((repos :type string
          :initarg :repos
          :documentation "Repository identifier (ex. fukamachi/dexador)")
   (retry-after :type (or integer null)
                :initarg :retry-after
                :documentation "Time that has to wait in seconds.")
   (token-used :type boolean
               :initarg :token-used
               :initform nil
               :documentation "GITHUB_TOKEN is set or not")
   (http-error :initarg :http-error
               :documentation "A raw error object returned from Dexador"))
  (:report (lambda (condition stream)
             (with-slots (repos retry-after token-used)
                 condition
               (format stream "Exceeded the rate limit of GitHub API during access to '~A'.~@[ Retry after ~A seconds.~]~@[~%TIPS: Using a personal access token will increase the limit. Set it to an environment variable 'GITHUB_TOKEN'.~]"
                       repos
                       retry-after
                       (not token-used))))))

(define-condition ros-command-error (qlot-error)
  ((message :initarg :message))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'message) stream))))

(defun ros-command-error (format-control &rest format-arguments)
  (error 'ros-command-error
         :message (apply #'format nil format-control format-arguments)))

(define-condition command-not-found (ros-command-error)
  ((command :initarg :command))
  (:report (lambda (condition stream)
             (format stream "Command not found: ~A" (slot-value condition 'command)))))

(define-condition qlot-warning (warning) ())

(define-condition qlot-simple-warning (qlot-warning simple-warning) ())

(defun ros-command-warn (format-control &rest format-arguments)
  (restart-case
      (error 'qlot-simple-warning
             :format-control format-control
             :format-arguments format-arguments)
    (continue ())))
