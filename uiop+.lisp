(in-package #:cl-user)

(import+ #:uiop)

(defun open-in-default-application (pathname)
  (if (uiop:os-windows-p)
      (if (uiop:directory-pathname-p pathname)
          (uiop:launch-program (list "explorer" (uiop:native-namestring pathname)))
          (uiop:launch-program (uiop:native-namestring pathname)))
      (uiop:launch-program (list "xdg-open" (uiop:native-namestring pathname)))))
