(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find :quicklisp *features*)
    (let ((quicklisp-init (merge-pathnames (make-pathname
                                            :directory '(:relative "quicklisp")
                                            :name "setup"
                                            :type "lisp")
                                           (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))))
