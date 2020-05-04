(in-package #:cl-user)

(:import+ #:uiop)

(defun :open-in-default-application (pathname)
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

(defun :executable-find (command)
  "Attempt to find the executable corresponding to `command'."
  (multiple-value-bind (outstring errstring exit-code)
      (uiop:run-program (list (cond
                                ((uiop:os-windows-p) "where")
                                ((or (uiop:os-unix-p)
                                     (uiop:os-macosx-p)) "which"))
                              command) :force-shell t :output '(:string :stripped t) :ignore-error-status t)
    (declare (ignore errstring))
    (when (zerop exit-code) (uiop:parse-native-namestring
                             (if (uiop:os-windows-p)
                                 (subseq outstring 0 (position #\Return outstring))
                                 outstring)))))
