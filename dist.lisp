(defpackage #:qlot/dist
  (:use #:cl)
  (:export
   ;; #:make-dists-storage
   ;; #:add-dist
   ;; #:search-dist
   #:register-distribution))
(in-package #:qlot/dist)


;; (defclass dist ()
;;   ((name :initarg :name
;;          :reader dist-name)
;;    (url :initarg :url
;;         :reader dist-url)))


;; (defun make-dists-storage ()
;;   (make-hash-table))


;; (defun add-dist (dists name url)
;;   (check-type dists hash-table)
;;   (check-type name keyword)
;;   (check-type url string)
  
;;   (setf (gethash name dists)
;;         url))


;; (defun search-dist (dists name)
;;   (check-type dists hash-table)
;;   (check-type name keyword)
;;   (gethash name dists))


(defun register-distribution (name url)
  (defmethod qlot/source:make-source ((source (eql (qlot/util:make-keyword name)))
                                      &rest args)
    (apply #'qlot/source:make-source
           :ql
           :distribution url
           args)))
