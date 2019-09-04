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

(defun argp (arg)
  (and (member arg (uiop:command-line-arguments) :test #'string=)
       t))

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
         (compile-file (merge-pathnames ',file (or *compile-file-truename* *default-pathname-defaults*))))
       (eval-when (:load-toplevel :execute)
         (load (merge-pathnames ',file (or *load-truename* *default-pathname-defaults*)))))))

(unless-arg "--no-quicklisp"
  (include "quicklisp"))

(defmacro import+ (&rest systems)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@ (mapcar (lambda (system)
                  (let ((system-sym (gensym "SYSTEM")))
                    `(let ((,system-sym (asdf:find-system ',system)))
                       (unless (and ,system-sym (asdf:component-loaded-p ,system-sym))
                         #+quicklisp
                         (ql:quickload '(,system) :verbose nil :silent t)
                         #-quicklisp
                         (asdf:load-op ',system)))))
                systems)))

(unless-arg "--no-utils"
  (include "utils"))

(when-arg ("--compile" thing)
  (flet ((lose (fmt &rest args)
           (format *error-output* "error: ")
           (apply #'format *error-output* fmt args)
           (terpri *error-output*)
           (uiop:quit 1)))
    (unless thing
      (lose "Missing argument to --compile"))
    (let ((truename (probe-file thing)))
      (cond
        ((null truename)
         (lose "Cannot find file '~A'" thing))
        ((uiop:directory-pathname-p truename)
         (lose "Pathname is a directory: '~A'" truename))
        ((and (pathname-type truename) (string-equal (pathname-type truename) "asd"))
         (let ((success nil))
           (unwind-protect
                (progn
                  (let ((*debugger-hook*
                          (lambda (c h)
                            (declare (ignore h))
                            (lose "~A" c))))
                    (asdf:load-asd truename)
                    (asdf:load-system (pathname-name truename) :verbose (argp "--verbose") :force (argp "--force"))))
             (uiop:quit (if success 0 1)))))
        (t
         (let ((success nil))
           (unwind-protect
                (handler-case
                    (let ((*compile-verbose*  (argp "--verbose")))
                      (setf success (not (nth-value 2 (compile-file truename)))))
                  (error (e)
                    (lose "~A" e)))
             (uiop:quit (if success 0 1)))))))))
