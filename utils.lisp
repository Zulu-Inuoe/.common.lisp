(in-package #:cl-user)

(:include "quicklisp")
(:include "quicklisp+")

(:import+ #:named-readtables)

(use-package '#:named-readtables)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %kvp->table-forms (kvp type)
    (let ((table-sym (gensym (format nil "~A-VALUE" type))))
      `(let ((,table-sym ,(ecase type
                            (:hash `(make-hash-table :test 'equal))
                            ((:alist :plist) `(list)))))
         ,@(loop
             :with setter-generator := (ecase type
                                         (:hash (lambda (k v) `(setf (gethash ,k ,table-sym) ,v)))
                                         (:alist (lambda (k v) `(setf (alexandria:assoc-value ,table-sym ,k) ,v)))
                                         (:plist (lambda (k v) `(setf (getf ,table-sym ,k) ,v))))
             :with cell := kvp
             :with key-sym := (gensym "KEY")
             :with value-sym := (gensym "VALUE")
             :collect
             (labels ((merge-hash (table-form)
                        `(maphash (lambda (,key-sym ,value-sym)
                                    ,(funcall setter-generator key-sym value-sym))
                                  ,table-form))
                      (merge-alist (alist-form)
                        `(loop
                           :for (,key-sym . ,value-sym) :in ,alist-form
                           :do ,(funcall setter-generator key-sym value-sym)))
                      (merge-plist (plist-form)
                        `(loop
                           :for (,key-sym ,value-sym) :on ,plist-form :by #'cddr
                           :do ,(funcall setter-generator key-sym value-sym)))
                      (merge-vector (vector-form)
                        `(loop
                           :for ,key-sym :from 0
                           :for ,value-sym :across ,vector-form
                           :do ,(funcall setter-generator key-sym value-sym)))
                      (merge-list (list-form)
                        `(loop
                           :for ,key-sym :from 0
                           :for ,value-sym :in ,list-form
                           :do ,(funcall setter-generator key-sym value-sym)))
                      (merge-sequence (sequence-form)
                        `(map nil (let ((,key-sym 0))
                                    (lambda (,value-sym)
                                      ,(funcall setter-generator key-sym value-sym)
                                      (incf ,key-sym)))
                              ,sequence-form))
                      (merge-on (type form)
                        (ecase type
                          (:hash (merge-hash form))
                          (:alist (merge-alist form))
                          (:plist (merge-plist form))
                          (:vector (merge-vector form))
                          (:list (merge-list form))
                          (:sequence (merge-sequence form)))))
               (cond
                 ((and (vectorp (car cell)) (not (stringp (car cell))))
                  (prog1 (merge-on type (aref (car cell) 0))
                    (setf cell (cdr cell))))
                 ((consp (car cell))
                  (prog1 (destructuring-bind (type form) (car cell)
                           (merge-on type form))
                    (setf cell (cdr cell))))
                 ((cdr cell)
                  (destructuring-bind (k v &rest next) cell
                    (setf cell next)
                    (funcall setter-generator `(quote ,k) v)))
                 (t
                  (error "Bad number of arguments in ~A" kvp))))
             :when (null cell)
               :do (loop-finish))
         ,table-sym))))

(defmacro :hash (&rest kvp)
  "Pseudo-syntax for literla hash tables"
  (if kvp
      (%kvp->table-forms kvp :hash)
      `(make-hash-table :test 'equal)))

(define-setf-expander :hash (table &rest keys)
  (let ((table-sym (gensym "TABLE"))
        (val-syms (mapcar (lambda (k) (declare (ignore k)) (gensym "VALUE")) keys)))
    (values `(,table-sym)
            `(,table)
            `(,@val-syms)
            `(setf (values ,@(mapcar (lambda (k) `(gethash ',k ,table-sym)) keys))
                   (values ,@val-syms))
            `(values ,@(mapcar (lambda (k) `(gethash ,k ',table-sym)) keys)))))

(defmacro :alist (&rest kvp)
  "Pseudo-syntax for literal alists"
  (if kvp
      `(nreverse ,(%kvp->table-forms kvp :alist))
      `(list)))

(defmacro :plist (&rest kvp)
  "Pseudo-syntax for literal plists"
  (if kvp
      (%kvp->table-forms kvp :plist)
      `(list)))

(defmacro :vector (&rest values)
  (if values
      (let ((vector-sym (gensym "VECTOR"))
            (length-sym (gensym "LENGTH")))
        `(let ((,vector-sym ())
               (,length-sym 0))
           ,@(loop
               :for v :in values
               :collect
               (cond
                 ((and (vectorp v) (not (stringp v)))
                  `(map nil (lambda (elt)
                              (push elt ,vector-sym)
                              (incf ,length-sym))
                        ,(aref v 0)))
                 (t
                  `(progn
                     (push ,v ,vector-sym)
                     (incf ,length-sym)))))
           (make-array ,length-sym :initial-contents (nreverse ,vector-sym))))
      `(vector)))

(defun :alist-remove (alist keys &key key test)
  "Remove the given `keys' from `alist'"
  (remove-if (lambda (cell) (member (car cell) keys :key key :test test))
             alist))

(:include "alert")
(:include "apropos+")
(:include "apropos-value")
(:include "cl-dot+")
(:include "fizzbuzz")
(:include "id")
(:include "indent")
(:include "int-uint")
(:include "guid")
(:include "make-project")
(:include "uiop+")
(:include "user")
(:include "package+")
(:include "place")
(:include "print-hash-table")

(defreadtable zulu
  (:merge :standard)
  (:macro-char #\{ #':hash-table-reader)
  (:macro-char #\} (lambda (stream char)
                     (declare (ignore stream char))
                     (error "unmatched close bracket"))))

(in-readtable zulu)
