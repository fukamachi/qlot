(defpackage #:qlot/parser/version
  (:use #:cl)
  (:import-from #:qlot/errors
                #:qlot-error)
  (:export #:parse-version-range))
(in-package #:qlot/parser/version)

;; TODO: Add report functions

(define-condition version-range-parse-error (qlot-error) ())

(define-condition invalid-version-operator (version-range-parse-error) ())

(define-condition invalid-version (version-range-parse-error) ())

(define-condition empty-version (version-range-parse-error) ())

(defun check-version-string (version)
  (or (uiop:parse-version version)
      (error 'invalid-version)))

(defun parse-version-range-list (version-range)
  (check-type version-range cons)
  (labels ((valid-version-specifier (object)
             (unless (and (consp object)
                          (member (first object)
                                  '(= < <= > >= ~>)
                                  :test 'eq))
               (error 'invalid-version-operator))
             (check-version-string (second object))
             (unless (null (cddr object))
               (error 'invalid-version))
             t))
    (case (car version-range)
      ((and or)
       (when (null (cdr version-range))
         (error 'empty-version))
       (mapc #'valid-version-specifier (cdr version-range)))
      (otherwise
       (valid-version-specifier version-range)))
    version-range))

(defun parse-version-range-string (version-range)
  (check-type version-range string)
  (when (string= version-range "")
    (error 'empty-version))

  (case (aref version-range 0)
    (#\~
     (unless (and (<= 2 (length version-range))
                  (char= (aref version-range 1) #\>))
       (error 'invalid-version-operator))
     (let* ((version (subseq version-range 2))
            (version-parts
              (or (uiop:parse-version version)
                  (error 'invalid-version))))
       `(and (<= ,version)
             (> ,(format nil "~{~A~^.~}" (butlast version-parts))))))
    ((#\< #\>)
     (cond
       ((char= (aref version-range 1) #\=)
        (let ((version (subseq version-range 2)))
          (check-version-string version)
          `(,(if (char= (aref version-range 0) #\<)
                 '<=
                 '>=)
            ,version)))
       (t
        (let ((version (subseq version-range 1)))
          (check-version-string version)
          `(,(if (char= (aref version-range 0) #\<)
                 '<
                 '>)
            ,version)))))
    (#\=
     (let ((version (subseq version-range 1)))
       (check-version-string version)
       `(= ,version)))
    (otherwise
     (check-version-string version-range)
     `(= ,version-range))))

(defun parse-version-range (version-range)
  (etypecase version-range
    (string (parse-version-range-string version-range))
    (list (parse-version-range-list version-range))))
