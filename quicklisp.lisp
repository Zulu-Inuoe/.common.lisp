(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find :quicklisp *features*)
    (let ((#1=#:quicklisp-init (merge-pathnames (make-pathname
                                                 :directory '(:relative "quicklisp")
                                                 :name "setup"
                                                 :type "lisp")
                                                (user-homedir-pathname))))
      (when (probe-file #1#)
        (load #1#)))))
