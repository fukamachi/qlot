(defpackage #:qlot/add
  (:use #:cl)
  (:import-from #:qlot/parser
                #:parse-qlfile-line)
  (:import-from #:qlot/source
                #:source-project-name)
  (:import-from #:qlot/logger
                #:message)
  (:import-from #:qlot/errors
                #:qlfile-parse-failed
                #:missing-projects)
  (:export #:add-project
           #:remove-project))
(in-package #:qlot/add)

(defun add-project (new-definition qlfile)
  (check-type new-definition cons)
  (let* ((new-line (format nil "~{~A~^ ~}" new-definition))
         (new-source (handler-bind ((error
                                      (lambda (e)
                                        (error 'qlfile-parse-failed
                                               :file qlfile
                                               :lineno nil
                                               :line new-line
                                               :error e))))
                       (parse-qlfile-line new-line)))
         (new-project-name (source-project-name new-source))
         (lines (uiop:read-file-lines qlfile)))
    (uiop:with-output-file (out qlfile :if-exists :supersede :if-does-not-exist :create)
      (let ((replaced nil))
        (loop for line in lines
              for source = (ignore-errors (parse-qlfile-line line))
              do (format out "~A~%"
                         (if (and source
                                  (equal new-project-name (source-project-name source)))
                             (progn
                               (setf replaced t)
                               (message "Update '~A' to '~A' in '~A'."
                                        new-project-name
                                        new-line
                                        qlfile)
                               new-line)
                             line)))
        (unless replaced
          (message "Add '~A' to '~A'." new-line qlfile)
          (format out "~A~%" new-line))))))

(defun remove-project (targets qlfile)
  (check-type targets cons)
  (assert (every #'stringp targets))
  (let* ((lines (uiop:read-file-lines qlfile)))
    (uiop:with-temporary-file (:stream out
                               :pathname tmpfile
                               :direction :output)
      (prog1
          (loop for line in lines
                for source = (ignore-errors (parse-qlfile-line line))
                if (and source
                        (find (source-project-name source) targets
                              :test 'equal))
                collect
                   (progn
                     (message "Remove '~A'." (source-project-name source))
                     (source-project-name source))
                into removed-projects
                else
                do (format out "~A~%" line)
                finally
                   (let ((not-found (set-difference targets removed-projects
                                                    :test 'string=)))
                     (when not-found
                       (error 'missing-projects
                              :projects not-found))
                     (return removed-projects)))
        (force-output out)
        (uiop:copy-file tmpfile qlfile)))))
