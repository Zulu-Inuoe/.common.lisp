(in-package #:cl-user)

#+sbcl
(progn
  (restrict-compiler-policy 'debug 3 3)
  (restrict-compiler-policy 'safety 3 3))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "asdf")
  (require "uiop"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member "--no-quicklisp" (uiop:command-line-arguments) :test #'string=)
    (load (merge-pathnames (make-pathname :name "quicklisp" :type "lisp") #.(or *compile-file-truename* *load-truename*)))))

;; Load up flexi-streams in case we're using Sly
#+lispworks
(ql:quickload '#:flexi-streams :silent t)

#-lispworks
(progn
  (defmacro :eval-always (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@body))

  (defun :arg (arg)
    (cadr (member arg (uiop:command-line-arguments) :test #'string=)))

  (defun :argp (arg)
    (and (member arg (uiop:command-line-arguments) :test #'string=)
         t))

  (defmacro :unless-arg (arg &body body)
    `(:eval-always
       (unless (:argp ,arg)
         ,@body)))

  (defmacro :when-arg (arg &body body)
    (destructuring-bind (arg-name &optional arg-value-sym)
        (uiop:ensure-list arg)
      (let ((tmp-sym (gensym "TMP")))
        `(:eval-always
           (let ((,tmp-sym (:arg ,arg-name)))
             (when ,tmp-sym
               ,@(if arg-value-sym
                     `((let ((,arg-value-sym ,tmp-sym))
                         ,@body))
                     body)))))))

  (defun :arg= (arg value)
    (let ((arg-val (:arg arg)))
      (and arg-val (string= arg-val value))))

  (defmacro :include (file)
    (let ((file (macroexpand file)))
      (assert (constantp file) (file))
      (setf file (eval file))
      (check-type file (or string pathname))
      `(progn
         (eval-when (:compile-toplevel)
           (compile-file (merge-pathnames ',file (or *compile-file-truename* *default-pathname-defaults*))))
         (eval-when (:load-toplevel :execute)
           (load (merge-pathnames ',file (or *load-truename* *default-pathname-defaults*)))))))

  (:unless-arg "--no-quicklisp"
    (:include "quicklisp"))

  (defmacro :import+ (&rest systems)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@ (mapcar (lambda (system-designator)
                    `(handler-bind ((asdf:bad-system-name #'muffle-warning))
                       (let ((system (asdf:find-system ',system-designator nil)))
                         (unless (and system (asdf:component-loaded-p system))
                           (let* ((ql (find-package "QUICKLISP"))
                                  (quickload (and ql (find-symbol "QUICKLOAD" ql))))
                             (if quickload
                                 (funcall quickload '(,system-designator) :verbose nil :silent t)
                                 (asdf:load-system ',system-designator)))))))
                  systems)))

  (:unless-arg "--no-utils"
    (:include "utils"))

  (:when-arg ("--compile" thing)
    (flet ((lose (fmt &rest args)
             (format *error-output* "error: ")
             (apply #'format *error-output* fmt args)
             (terpri *error-output*)
             (uiop:quit 1)))
      (unless thing
        (lose "Missing argument to --compile"))
      (let* ((truename (probe-file thing))
             (out-name (or (:arg "-o")
                           (:arg "--output")
                           (unless (and truename (string= (pathname-type truename) "asd"))
                             (merge-pathnames (make-pathname :type "fasl")
                                              truename))))
             (out-type
               (cond
                 ((null out-name) nil)
                 ((string= (pathname-type out-name) "fasl") :fasl)
                 ((string= (pathname-type out-name) "exe") :exe)))
             (entry-point (string-upcase (or (:arg "-e")
                                             (:arg "--entry")
                                             "CL-USER::MAIN"))))
        (cond
          ((null truename)
           (lose "Cannot find file '~A'" thing))
          ((uiop:directory-pathname-p truename)
           (lose "Pathname is a directory: '~A'" truename))
          ((and (pathname-type truename) (string-equal (pathname-type truename) "asd"))
           (cond
             ((eq out-type nil)
              (let ((success nil))
                (unwind-protect
                     (progn
                       (let ((*debugger-hook*
                               (lambda (c h)
                                 (declare (ignore h))
                                 (lose "~A" c))))
                         (asdf:load-asd truename)
                         (asdf:load-system (pathname-name truename) :verbose (:argp "--verbose") :force (:argp "--force"))))
                  (uiop:quit (if success 0 1)))))
             (t
              (lose "unsupported out-type '~A'" out-type))))
          (t
           (cond
             ((eq out-type :fasl)
              (let ((success nil))
                (unwind-protect
                     (handler-case
                         (let ((*compile-verbose*  (:argp "--verbose")))
                           (setf success (not (nth-value 2 (compile-file truename :output-file out-name)))))
                       (error (e)
                         (lose "~A" e)))
                  (uiop:quit (if success 0 1)))))
             ((eq out-type :exe)
              (let ((success nil)
                    (file nil))
                (unwind-protect
                     (handler-case
                         (let ((*compile-verbose*  (:argp "--verbose")))
                           (multiple-value-bind (out-file warnings-p failure-p) (compile-file truename)
                             (declare (ignore warnings-p))
                             (when (not failure-p)
                               (load out-file :verbose (:argp "--verbose"))
                               #+sbcl
                               (sb-ext:save-lisp-and-die out-name :toplevel (read-from-string entry-point) :executable t)
                               #-sbcl
                               (lose "executable dumping unsupported"))
                             (setf success (not failure-p)
                                   file out-file)))
                       (error (e)
                         (lose "~A" e)))
                  (uiop:quit (if success 0 1)))))
             (t
              (lose "unsupported out-type: '~A'" out-type)))))))))
