(in-package #:cl-user)

(include "uiop+")
(include "user")

(defun %write-system-template (stream
                               &key
                                 (name (alexandria:required-argument :name))
                                 (version "0.0.0")
                                 (description "")
                                 (author (format nil "~A~@[ <~A>~]" user-full-name user-mail-address))
                                 (license "MIT")
                                 static-files
                                 files
                                 depends-on)
  (format stream "(defsystem #:~A
  :version \"~A\"
  :description \"~A\"
  :author \"~A\"
  :license \"~A\"
  :serial t
"
          name
          version
          description
          author
          license)

  (format stream "  :components~%  (")
  (let ((first t))
    (dolist (static-file static-files)
      (if first
          (setq first nil)
          (format stream "~%   "))
      (format stream "(:static-file \"~A\")" static-file))
    (dolist (file files)
      (if first
          (setq first nil)
          (format stream "~%   "))
      (format stream "(:file \"~A\")" file)))
  (format stream ")~%")

  (format stream "  :depends-on~%  (")
  (let ((first t))
    (dolist (dep depends-on)
      (if first
          (setq first nil)
          (format stream  "~%  "))
      (format stream "#:~A" dep)))
  (format stream "))~%"))

(defun %executable-system-template (path)
  (let ((name (pathname-name path))
        (dir (uiop:pathname-directory-pathname path)))
    ;;Make the asd file
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (%write-system-template stream :name name
                                    :static-files '("qlfile")
                                    :files '("package" "main")
                                    :depends-on '("alexandria")))
    ;;Make the qlfile
    (with-open-file (stream (uiop:merge-pathnames* "qlfile" dir) :direction :output :if-exists :supersede)
      (format stream "ql alexandria :latest~%"))

    ;;Make the package.lisp
    (with-open-file (stream (uiop:merge-pathnames* "package.lisp" dir) :direction :output :if-exists :supersede)
      (format stream "(defpackage #:~A
  (:use #:alexandria #:cl)
  (:export
    #:main))~%"
              name))

    ;;Make the main .lisp
    (let ((main-file (uiop:merge-pathnames* "main.lisp" dir)))
      (with-open-file (stream main-file :direction :output :if-exists :supersede)
        (format stream "(in-package #:~A)~%~%" name)
        (format stream "(defun main (&rest args)
  0)~%"))
      main-file)))

(defun %library-system-template (path)
  (let ((name (pathname-name path))
        (dir (uiop:pathname-directory-pathname path)))
    ;;Make the asd file
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (%write-system-template stream :name name
                                    :static-files '("qlfile")
                                    :files (list "package" name)
                                    :depends-on '("alexandria")))
    ;;Make the qlfile
    (with-open-file (stream (uiop:merge-pathnames* "qlfile" dir) :direction :output :if-exists :supersede)
      (format stream "ql alexandria :latest~%"))

    ;;Make the package.lisp
    (with-open-file (stream (uiop:merge-pathnames* "package.lisp" dir) :direction :output :if-exists :supersede)
      (format stream "(defpackage #:~A
  (:use #:alexandria #:cl)
  (:export))~%"
              name))

    ;;Make the main .lisp
    (let ((main-file (uiop:merge-pathnames* (make-pathname :name name :type "lisp") dir)))
      (with-open-file (stream main-file :direction :output :if-exists :supersede)
        (format stream "(in-package #:~A)~%~%" name))
      main-file)))

(defun make-project (name &key
                            (dir *default-pathname-defaults*)
                            (type :library)
                     &aux
                       (dir (uiop:ensure-directory-pathname dir))
                       (path (uiop:merge-pathnames* (make-pathname :name name :type "asd") dir)))
  (open-in-default-application
   (ecase type
     (:library (%library-system-template path))
     (:executable (%executable-system-template path)))))
