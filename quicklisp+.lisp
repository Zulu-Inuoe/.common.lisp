(in-package #:cl-user)

(include "quicklisp")
(include "asdf+")

(defun ql-dependencies (system)
  "Get the dependencies of system which are in Quicklisp"
  (let* ((dependent-system-names (system-dependencies system))
         (ql-system-names (mapcar #'ql-dist:name (ql:system-list))))
    (nintersection dependent-system-names ql-system-names :test #'string=)))

(defun make-ql-bundle (systems
                       &optional
                         (to (uiop:merge-pathnames*
                              (make-pathname
                               :directory
                               (list :relative (asdf:primary-system-name (first systems))))))
                         (overwrite t))
  "Create a ql bundle as per `ql:bundle-systems', but selectively includes non-ql dependencies
  into `ql:*local-project-directories*'."
  (let* ((all-deps (delete-duplicates (mapcan #'system-dependencies systems) :test #'string=))
         ;; Exclude asdf
         (all-deps (delete "asdf" all-deps :test #'string=))
         ;; Exclude uiop
         (all-deps (delete "uiop" all-deps :test #'string=))
         (ql-deps (nintersection (mapcar #'ql-dist:name (ql:system-list)) all-deps :test #'string=))
         (local-deps (nset-difference all-deps ql-deps :test #'string=))
         (ql:*local-project-directories*
          (delete-duplicates
           (mapcar (lambda (s) (truename (uiop:pathname-directory-pathname (asdf:system-source-file s))))
                   local-deps)
           :test #'uiop:pathname-equal))
         ;;On SBCL, get rid of the contrib systems
         #+sbcl
         (ql:*local-project-directories*
          (delete (truename (uiop:merge-pathnames* "contrib/" (sb-int:sbcl-homedir-pathname)))
                  ql:*local-project-directories*
                  :test #'uiop:pathname-equal)))
    (ql:bundle-systems ql-deps
                       :include-local-projects t
                       :to to
                       :overwrite overwrite)))
