(defpackage #:qlot/utils
  (:use #:cl)
  (:export #:with-in-directory
           #:make-keyword
           #:split-with
           #:generate-random-string))
(in-package #:qlot/utils)

(defmacro with-in-directory (dir &body body)
  (let ((cwd (gensym "CWD")))
    `(let ((,cwd (uiop:getcwd)))
       (uiop:chdir ,dir)
       (unwind-protect
            (progn ,@body)
         (uiop:chdir ,cwd)))))

(defun make-keyword (text)
  "This function differ from alexandria:make-keyword
   because it upcases text before making it a keyword."
  (intern (string-upcase text) :keyword))

(defun split-with (delimiter value &key limit)
  (check-type delimiter character)
  (check-type value string)
  (check-type limit (or null (integer 1)))
  (let ((results '())
        (pos 0)
        (count 0))
    (block nil
      (flet ((keep (i)
               (unless (= pos i)
                 (push (subseq value pos i) results)
                 (incf count))))
        (do ((i 0 (1+ i)))
            ((= i (length value))
             (keep i))
          (when (and limit
                     (= (1+ count) limit))
            (push (subseq value i (length value)) results)
            (return))
          (when (char= (aref value i) delimiter)
            (keep i)
            (setf pos (1+ i))))))
    (nreverse results)))

(defun generate-random-string ()
  (let ((*random-state* (make-random-state t)))
    (format nil "~36R" (random (expt 36 #-gcl 8 #+gcl 5)))))
