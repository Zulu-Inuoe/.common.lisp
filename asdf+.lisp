(in-package #:cl-user)

(:import+ #:alexandria #:cl-ppcre)

(defun :normalize-system-dependency (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))
    (list
     (let ((dep-type (first name))
           (supported-dep-types '(:version :feature :require)))
       (unless (member dep-type
                       supported-dep-types)
         (error "This component \"~A\" should have first element from this list: ~A."
                name
                supported-dep-types))
       (:normalize-system-dependency
        (case dep-type
          (:version (second name))
          (:feature (third name))
          (:require (second name))))))))

(defun :normalized-dependencies (system)
  (mapcar #':normalize-system-dependency (asdf:system-depends-on system)))

(defun :system-dependencies (system)
  "Get a list of all system dependencies (direct and transient) of `system'
Each dependency is identified by its name (a string)."
  (loop
    :with processed := (list (:normalize-system-dependency system))
    :with tbd := processed
    :for current-name := (pop tbd)
    :while current-name
    :do
       (loop
         ;; Sometimes system can't be found because itself depends on some feature,
         ;; for example, you can specify dependency as a list:
         ;; (:FEATURE :SBCL (:REQUIRE :SB-INTROSPECT))
         ;; and it will be loaded only on SBCL.
         ;; When we are collecting dependencies on another implementation,
         ;; we don't want to fail with an error because ASDF is unable to find
         ;; such dependencies
         :with current-system := (ignore-errors (asdf:find-system current-name))
         :for dep :in (and current-system (asdf:component-sideway-dependencies current-system))
         :for normalized-dep := (:normalize-system-dependency dep)
         :unless (position normalized-dep processed :test #'string=)
           :do
              (push normalized-dep tbd)
              (push normalized-dep processed))
    :finally (return (nreverse processed))))

(defun :component-equal (component other)
  (string-equal (asdf:component-name component)
                (asdf:component-name other)))

(defun :asdf-inferred-system-deps (system &optional (visited nil))
  ;; A "proper-dep" is a dependency with the same primary system name (i.e., not an external library)
  (flet ((proper-dep-p (dep) (string-equal (asdf:primary-system-name dep) (asdf:primary-system-name system)))
         (recurse (prev dep) (:asdf-inferred-system-deps dep prev)))
    (let* ((proper-deps (remove-if-not #'proper-dep-p (mapcar #'asdf:find-system (asdf:system-depends-on system))))
           (unvisited-deps (set-difference proper-deps visited :test #':component-equal)))
      (reduce #'recurse unvisited-deps :initial-value (append unvisited-deps visited)))))

(defun :asdf-inferred-system-files (system ending)
  (let ((system-pathname (asdf:component-pathname system)))
    (flet ((dep-pathname (dep)
             (let ((name (asdf:component-name dep)))
               (parse-namestring (concatenate
                'string
                (namestring system-pathname )
                (subseq name (1+ (position #\/ name)))
                ending)))))
      (mapcar #'dep-pathname (:asdf-inferred-system-deps system)))))

(defun :asdf-inferred-system-files (system ending)
  (let ((system-pathname (asdf:component-pathname system)))
    (flet ((dep-pathname (dep)
             (let ((name (asdf:component-name dep)))
               (parse-namestring (concatenate
                'string
                (namestring system-pathname )
                (subseq name (1+ (position #\/ name)))
                ending)))))
      (mapcar #'dep-pathname (:asdf-inferred-system-deps system)))))

(defun :asdf-component-source-files (component)
  (if (and #+asdf3 (typep component 'asdf:package-inferred-system))
      (:asdf-inferred-system-files component ".lisp")
      (let ((files ()))
        (labels ((f (x)
                   (typecase x
                     (asdf:source-file
                      (let ((truename (uiop:truenamize (asdf:component-pathname x))))
                        (push truename files)))
                     (asdf:module
                      (map nil #'f (asdf:module-components x))))))
          (f component))
        files)))

(defun :pathname-system (pathname)
  "Find an `asdf:system' to which `pathname' belongs to, or nil if no such system exists."
  (let ((pathname (uiop:ensure-absolute-pathname pathname *default-pathname-defaults*)))
    (block nil
      (asdf:map-systems
       (lambda (system)
         (when (find pathname (:asdf-component-source-files system) :test #'uiop:pathname-equal)
           (return system)))))))
