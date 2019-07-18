(in-package #:cl-user)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-always
  (require "asdf")
  (require "uiop"))

(defmacro unless-arg (arg &body body)
  `(eval-always
     (unless (member ,arg (uiop:command-line-arguments) :test #'string=)
       ,@body)))

(defmacro when-arg (arg &body body)
  (destructuring-bind (arg-name &optional arg-value-sym)
      (uiop:ensure-list arg)
    (let ((tmp-sym (gensym "TMP")))
      `(eval-always
         (let ((,tmp-sym (member ,arg-name (uiop:command-line-arguments) :test #'string=)))
           (when ,tmp-sym
             ,@(if arg-value-sym
                   `((let ((,arg-value-sym (cadr ,tmp-sym)))
                       ,@body))
                   body)))))))

(defun arg= (arg value)
  (let* ((arg-cons (member arg (uiop:command-line-arguments) :test #'string=))
         (arg-val (second arg-cons)))
    (and arg-val (string= arg-val value))))

(defmacro include (file)
  (let ((file (macroexpand file)))
    (assert (constantp file) (file))
    (setf file (eval file))
    (check-type file (or string pathname))
    `(progn
       (eval-when (:compile-toplevel)
         (compile-file (merge-pathnames ',file *compile-file-truename*)))
       (eval-when (:load-toplevel :execute)
         (load (merge-pathnames ',file *load-truename*))))))

(unless-arg "--no-quicklisp"
  (include "quicklisp"))

(unless-arg "--no-utils"
  (include "utils"))
