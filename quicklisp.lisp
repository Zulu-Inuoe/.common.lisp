;;Put initialization code here, to be loaded for any common lisp initialization
;;initialize quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find :quicklisp *features*)
    (let ((quicklisp-init (merge-pathnames (make-pathname
                                            :directory '(:relative :up "quicklisp")
                                            :name "setup"
                                            :type "lisp")
                                           #.(or *compile-file-truename* *load-truename*))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))))
