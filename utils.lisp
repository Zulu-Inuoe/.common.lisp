(in-package #:cl-user)

(:include "quicklisp")
(:include "quicklisp+")

(defmacro :hash (&rest kvp)
  "Pseudo-syntax for literla hash tables"
  (let* ((kvp-l (alexandria:plist-alist kvp))
         (ht-sym (gensym "HASH-TABLE"))
         (keys (mapcar #'car kvp-l))
         (test (cond
                 ((every #'symbolp keys) 'eq)
                 ((or (every #'numberp keys)
                      (every #'characterp keys))
                  'eql)
                 (t 'equal))))
    `(let ((,ht-sym (make-hash-table :test #',test)))
       (setf ,@(mapcan (lambda (kvp)
                         (destructuring-bind (k . v) kvp
                           (list `(gethash (quote ,k) ,ht-sym) v)))
                       kvp-l))
       ,ht-sym)))

(defmacro :alist (&rest kvp)
  "Pseudo-syntax for literla alist"
  `(list ,@(mapcar (lambda (kvp)
                     (destructuring-bind (k . v) kvp
                       `(cons (quote ,k) ,v)))
                   (alexandria:plist-alist kvp))))

(defmacro :plist (&rest kvp)
  "Pseudo-syntax for literla plist"
  `(list ,@(mapcan (lambda (kvp)
                     (destructuring-bind (k . v) kvp
                       (list `(quote ,k) v)))
                   (alexandria:plist-alist kvp))))

(:include "alert")
(:include "apropos+")
(:include "apropos-value")
(:include "cl-dot+")
(:include "fizzbuzz")
(:include "make-project")
(:include "uiop+")
(:include "user")
(:include "package+")
(:include "place")
(:include "print-hash-table")
