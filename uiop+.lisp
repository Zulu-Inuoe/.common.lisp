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
