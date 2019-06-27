(in-package #:cl-user)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:uiop) :verbose nil :silent t))

(defun open-in-default-application (pathname)
  (if (uiop:os-windows-p)
      (uiop:launch-program (uiop:native-namestring pathname))
      (uiop:launch-program (list "xdg-open" (uiop:native-namestring pathname)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :cl-ppcre) :verbose nil :silent t))

(defun system-dependencies (system)
  "Get a list of all system dependencies (direct and transient) of `system'
Each dependency is identified by its name (a string)."
  (labels ((normalize (name)
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
                  (normalize
                   (case dep-type
                     (:version (second name))
                     (:feature (third name))
                     (:require (second name)))))))))
    (loop
      :with processed := (list (normalize system))
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
           :for normalized-dep := (normalize dep)
           :unless (position normalized-dep processed :test #'string=)
             :do
                (push normalized-dep tbd)
                (push normalized-dep processed))
      :finally (return (nreverse processed)))))

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

(defun symbol-macro-p (sym &optional env)
  "Returns true if `sym' is a symbol macro in `env'"
  (and (symbolp sym)
       (nth-value 1 (macroexpand-1 sym env))))

(defun apropos-symbol-print (sym)
  (format t "~S~:[~; (bound)~]~:[~; (symbol-macro)~]~:[~; (fbound)~]~%" sym (boundp sym) (symbol-macro-p sym) (fboundp sym)))

(defun %apropos-iterator (filter &optional package external-only)
  "Iterates over symbols according to `package' and `external-only', testing them with `filter'.
  If `filter' returns true, prints the symbols as per `apropos-symbol-print'"
  (let ((seen (make-hash-table :test 'eq)))
    (flet ((apply-filter (sym)
             (when (and (not (gethash sym seen)) (funcall filter sym))
               (setf (gethash sym seen) t)
               (apropos-symbol-print sym))))
      (cond
        ((and package external-only)
         (do-external-symbols (sym package)
           (apply-filter sym)))
        (package
         (do-symbols (sym package)
           (apply-filter sym)))
        (t
         (do-all-symbols (sym)
           (apply-filter sym))))))
  (values))

(defun apropos-var (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `boundp' or `symbol-macro-p' and not keywords."
  (let ((str (string string-designator)))
    (%apropos-iterator
     (lambda (sym)
       (and (not (keywordp sym))
            (or (boundp sym) (symbol-macro-p sym))
            (search str (symbol-name sym) :test #'char-equal)))
     package external-only))
  (values))

(defun apropos-fn (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `fboundp'."
  (let ((str (string string-designator)))
    (%apropos-iterator
     (lambda (sym)
       (and (fboundp sym)
            (search str (symbol-name sym) :test #'char-equal)))
     package external-only))
  (values))

(defun apropos-bound (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `boundp' (and not keywords), `symbol-macro-p', or `fboundp'"
  (let ((str (string string-designator)))
    (%apropos-iterator
     (lambda (sym)
       (and (not (keywordp sym))
            (or (boundp sym) (symbol-macro-p sym) (fboundp sym))
            (search str (symbol-name sym) :test #'char-equal)))
     package external-only))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :cl-ppcre) :verbose nil :silent t))

(defun apropos-value (pattern &optional do-all)
  "Search for symbols who's value matches `pattern'.
    `pattern' may be a single regex or a list of regexes to match against.
  When `do-all' is true, additionally match against symbol properties, as well as function contents.
  Keyword symbols are excluded in all cases."
  (let ((scanners (mapcar (lambda (regex) (cl-ppcre:create-scanner regex :case-insensitive-mode t))
                          (alexandria:ensure-list pattern)))
        (excluded-symbols
                  ;; Skip SLYNK::*EVENT-HISTORY* since it's probably not something we want..
          '(("SLYNK" . "*EVENT-HISTORY*")
            ;; This has the last inputed expression if at the REPL, so likewise
            ("SB-IMPL" . "*EVAL-SOURCE-CONTEXT*")))
        (seen (make-hash-table :test 'eq)))
    (labels ((is-match-p (string)
               (loop :for scanner :in scanners
                     :always (cl-ppcre:scan scanner string)))
             (stringify (value)
               (if (stringp value)
                   value
                   ;; HACK: SBCL has some internals that fail real bad to print
                   (ignore-errors (princ-to-string value))))
             (value-match (sym)
               (cond
                 ((and (not (keywordp sym)) (boundp sym))
                  (alexandria:when-let* ((string (stringify (symbol-value sym))))
                    (values (is-match-p string)
                            (symbol-value sym)
                            string)))
                 ((symbol-macro-p sym)
                  (let* ((expansion (macroexpand-1 sym))
                         (string (stringify expansion)))
                    (when string
                      (values (is-match-p string)
                              expansion
                              string))))))
             (function-match (sym)
               (and do-all
                    (fboundp sym)
                    (alexandria:when-let* ((lambda-exp (ignore-errors (function-lambda-expression (symbol-function sym))))
                                           (string (stringify lambda-exp)))
                      (values (is-match-p string)
                              lambda-exp
                              string))))
             (plist-match (sym)
               (and do-all
                    (loop :for (key value . rest) :on (symbol-plist sym) :by #'cddr
                          :for string := (ignore-errors (format nil "~S ~A" key value))
                          ;; HACK: SBCL has some internals that fail real bad to print
                          :if (and string (is-match-p string))
                            :return (values t (list key value) string))))
             (symbol-match (sym)
               (loop
                 :for fn :in (list #'value-match #'function-match #'plist-match)
                 :do (multiple-value-bind (match-p value)
                         (funcall fn sym)
                       (when match-p
                         (return (values match-p value))))))
             (symbol-is (sym name package)
               (or (string= (symbol-name sym) name)
                   (and package (let ((package (find-package package)))
                                  (when package
                                    (eq sym (find-symbol name package)))))))
             (exclude-symbol-p (sym)
               (loop :for (package . name) :in excluded-symbols
                       :thereis (symbol-is sym name package))))
      (with-package-iterator (iter (list-all-packages) :internal :external :inherited)
        (loop
          (tagbody
             (multiple-value-bind (more? sym accessibility) (iter)
               (unless more?
                 (return))
               (unless (or do-all (eq accessibility :external))
                 (go :continue))
               (when (exclude-symbol-p sym)
                 (go :continue))

               ;; Skip symbols that have no properties to match against
               (unless (or (boundp sym) (symbol-macro-p sym)
                           (and do-all
                                (or (fboundp sym) (symbol-plist sym))))
                 (go :continue))
               ;; Skip symbols we've seen before
               (when (gethash sym seen)
                 (go :continue))
               (setf (gethash sym seen) t)

               (multiple-value-bind (match-p value)
                   (symbol-match sym)
                 (when match-p
                   (apropos-symbol-print sym)
                   ;; Also print the value that matched
                   (format t "~0,3T~S~2%" value))))
           :continue)))))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '#:cl-dot :verbose nil :silent t))

(defun visualize-graph (graph objects &optional attributes)
  "Generate a graph via `cl-dot:generate-graph' to a temporary file and open it in the default image viewer application."
  (let ((dgraph (cl-dot:generate-graph-from-roots graph objects attributes)))
    (uiop:with-temporary-file (:pathname pathname :keep t :type "png")
      (cl-dot:dot-graph dgraph pathname :format :png)
      (open-in-default-application pathname)
      pathname)))
