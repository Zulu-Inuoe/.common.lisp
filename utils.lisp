(in-package #:cl-user)

(:include "quicklisp")
(:include "quicklisp+")

(defun :hash (&rest kvp)
  "Pseudo-syntax for literla hash tables"
  (declare (dynamic-extent kvp))
  (let ((test (cond
                ((alexandria:doplist (k v kvp t)
                   (when (not (symbolp k))
                     (return nil)))
                 'eq)
                ((alexandria:doplist (k v kvp t)
                   (unless (or (numberp k)
                               (characterp k))
                     (return nil)))
                 'eql)
                (t 'equal))))
    (alexandria:plist-hash-table kvp :size (/ (length kvp) 2) :test test)))

(define-compiler-macro :hash (&whole whole &rest kvp)
  (if (every #'constantp kvp)
      (let* ((kvp (mapcar #'eval kvp))
             (kvp-l (alexandria:plist-alist kvp))
             (ht-sym (gensym "HASH-TABLE"))
             (keys (mapcar #'car kvp-l))
             (test (cond
                     ((every #'symbolp keys) 'eq)
                     ((or (every #'numberp keys)
                          (every #'characterp keys))
                      'eql)
                     (t 'equal))))
        `(let ((,ht-sym (make-hash-table :size ,(length keys) :test #',test)))
           (setf ,@(mapcan (lambda (kvp)
                             (destructuring-bind (k . v) kvp
                               (list `(gethash (quote ,k) ,ht-sym) v)))
                           kvp-l))
           ,ht-sym))
      whole))

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

(declaim (inline :uint->int))
(defun :uint->int (size value)
  "unsigned integer -> signed integer conversion"
  (declare (type unsigned-byte size value))
  (logior value (- (mask-field (byte 1 (1- size)) value))))

(declaim (inline :u8->int))
(defun :u8->int (value)
  (declare (type (unsigned-byte 8) value))
  (the (signed-byte 8) (:uint->int 8 value)))

(declaim (inline :u16->int))
(defun :u16->int (value)
  (declare (type (unsigned-byte 16) value))
  (the (signed-byte 16) (:uint->int 16 value)))

(declaim (inline :u32->int))
(defun :u32->int (value)
  (declare (type (unsigned-byte 32) value))
  (the (signed-byte 32) (:uint->int 32 value)))

(declaim (inline :u64->int))
(defun :u64->int (value)
  (declare (type (unsigned-byte 64) value))
  (the (signed-byte 64) (:uint->int 64 value)))

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
