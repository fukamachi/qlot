(defpackage #:qlot/add
  (:use #:cl)
  (:import-from #:qlot/parser
                #:parse-qlfile-line)
  (:import-from #:qlot/source
                #:source-project-name)
  (:import-from #:qlot/errors
                #:qlfile-parse-failed)
  (:export #:add-project))
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
                               new-line)
                             line)))
        (unless replaced
          (format out "~A~%" new-line))))))
