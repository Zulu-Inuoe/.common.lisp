(in-package #:cl-user)

(import+ #:uiop)

(defun open-in-default-application (pathname)
  (cond
    ((uiop:os-windows-p)
     (if (uiop:directory-pathname-p pathname)
          (uiop:launch-program (list "explorer" (uiop:native-namestring pathname)))
          (uiop:launch-program (uiop:native-namestring pathname))))
    ((uiop:os-macosx-p)
     (uiop:launch-program (list "open" (uiop:native-namestring pathname))))
    ((uiop:os-unix-p)
     (uiop:launch-program (list "xdg-open" (uiop:native-namestring pathname))))
    (t
     (error "Don't know how to open in default application"))))

(defun executable-find (command)
  "Attempt to find the executable corresponding to `command'."
  (loop
    :with commands := (cons command
                            (cond
                              ((uiop:os-windows-p)
                               (when (null (pathname-type command))
                                 (mapcar (lambda (p)
                                           (make-pathname :type (subseq (pathname-name p) 1)
                                                          :defaults command))
                                         (uiop:getenv-pathnames "PATHEXT"))))
                              (t nil)))
    :for dir :in (uiop:getenv-absolute-directories "PATH")
    :if (some (lambda (c) (probe-file (uiop:merge-pathnames* c dir))) commands)
      :return :it))

