(in-package #:cl-user)

(include "quicklisp")
(include "quicklisp+")

(defmacro hash (&rest kvp)
  "Pseudo-syntax for literla hash tables"
  (let ((kvp-l (alexandria:plist-alist kvp))
        (ht-sym (gensym "HASH-TABLE")))
    `(let ((,ht-sym (make-hash-table :test #'equal)))
       (setf ,@(mapcan (lambda (kvp)
                         (destructuring-bind (k . v) kvp
                           (list `(gethash (quote ,k) ,ht-sym) v)))
                       kvp-l))
       ,ht-sym)))

(defmacro alist (&rest kvp)
  "Pseudo-syntax for literla alist"
  `(list ,@(mapcar (lambda (kvp)
                     (destructuring-bind (k . v) kvp
                       `(cons (quote ,k) ,v)))
                   (alexandria:plist-alist kvp))))

(defmacro plist (&rest kvp)
  "Pseudo-syntax for literla plist"
  `(list ,@(mapcan (lambda (kvp)
                     (destructuring-bind (k . v) kvp
                       (list `(quote ,k) v)))
                   (alexandria:plist-alist kvp))))

(include "alert")
(include "apropos+")
(include "apropos-value")
(include "cl-dot+")
(include "fizzbuzz")
(include "make-project")
(include "uiop+")
(include "user")
(include "place")
(include "print-hash-table")
