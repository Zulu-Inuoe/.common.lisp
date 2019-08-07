(in-package #:cl-user)

(defmacro ql-import (&rest systems)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@ (mapcar (lambda (system)
                  (let ((system-sym (gensym "SYSTEM")))
                    `(let ((,system-sym (asdf:find-system ',system)))
                       (unless (and ,system-sym (asdf:component-loaded-p ,system-sym))
                         (ql:quickload '(,system) :verbose nil :silent t)))))
                systems)))

(ql-import #:uiop)

(defun open-in-default-application (pathname)
  (if (uiop:os-windows-p)
      (if (uiop:directory-pathname-p pathname)
          (uiop:launch-program (list "explorer" (uiop:native-namestring pathname)))
          (uiop:launch-program (uiop:native-namestring pathname)))
      (uiop:launch-program (list "xdg-open" (uiop:native-namestring pathname)))))

(ql-import #:alexandria #:cl-ppcre)

(defun normalize-system-dependency (name)
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
       (normalize-system-dependency
        (case dep-type
          (:version (second name))
          (:feature (third name))
          (:require (second name))))))))

(defun normalized-dependencies (system)
  (mapcar #'normalize-system-dependency (asdf:system-depends-on system)))

(defun system-dependencies (system)
  "Get a list of all system dependencies (direct and transient) of `system'
Each dependency is identified by its name (a string)."
  (loop
    :with processed := (list (normalize-system-dependency system))
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
         :for normalized-dep := (normalize-system-dependency dep)
         :unless (position normalized-dep processed :test #'string=)
           :do
              (push normalized-dep tbd)
              (push normalized-dep processed))
    :finally (return (nreverse processed))))

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


(defun component-equal (component other)
  (string-equal (asdf:component-name component)
                (asdf:component-name other)))

(defun asdf-inferred-system-deps (system &optional (visited nil))
  ;; A "proper-dep" is a dependency with the same primary system name (i.e., not an external library)
  (flet ((proper-dep-p (dep) (string-equal (asdf:primary-system-name dep) (asdf:primary-system-name system)))
         (recurse (prev dep) (asdf-inferred-system-deps dep prev)))
    (let* ((proper-deps (remove-if-not #'proper-dep-p (mapcar #'asdf:find-system (asdf:system-depends-on system))))
           (unvisited-deps (set-difference proper-deps visited :test #'component-equal)))
      (reduce #'recurse unvisited-deps :initial-value (append unvisited-deps visited)))))

(defun asdf-inferred-system-files (system ending)
  (let ((system-pathname (asdf:component-pathname system)))
    (flet ((dep-pathname (dep)
             (let ((name (asdf:component-name dep)))
               (parse-namestring (concatenate
                'string
                (namestring system-pathname )
                (subseq name (1+ (position #\/ name)))
                ending)))))
      (mapcar #'dep-pathname (asdf-inferred-system-deps system)))))

(defun asdf-inferred-system-files (system ending)
  (let ((system-pathname (asdf:component-pathname system)))
    (flet ((dep-pathname (dep)
             (let ((name (asdf:component-name dep)))
               (parse-namestring (concatenate
                'string
                (namestring system-pathname )
                (subseq name (1+ (position #\/ name)))
                ending)))))
      (mapcar #'dep-pathname (asdf-inferred-system-deps system)))))

(defun asdf-component-source-files (component)
  (if (and #+asdf3 (typep component 'asdf:package-inferred-system))
      (asdf-inferred-system-files component ".lisp")
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

(defun pathname-system (pathname)
  "Find an `asdf:system' to which `pathname' belongs to, or nil if no such system exists."
  (let ((pathname (uiop:ensure-absolute-pathname pathname *default-pathname-defaults*)))
    (block nil
      (asdf:map-systems
       (lambda (system)
         (when (find pathname (asdf-component-source-files system) :test #'uiop:pathname-equal)
           (return system)))))))

(defun symbol-macro-p (sym &optional env)
  "Returns true if `sym' is a symbol macro in `env'"
  (and (symbolp sym)
       (nth-value 1 (macroexpand-1 sym env))))

(defun apropos-symbol-print (sym)
  (format t "~S~:[~; (bound)~]~:[~; (symbol-macro)~]~:[~; (fbound)~]~%" sym (boundp sym) (symbol-macro-p sym) (fboundp sym)))

(defun %apropos-iterator (str filter selector &optional package external-only)
  "Iterates over symbols according to `package' and `external-only', testing them with `filter'.
  If `filter' returns true, prints the symbols as per `apropos-symbol-print'"
  (let ((seen (make-hash-table :test 'eq)))
    (flet ((apply-filter (sym)
             (when (and (not (gethash sym seen))
                        (search str (symbol-name sym) :test #'char-equal)
                        (funcall filter sym))
               (setf (gethash sym seen) t)
               (funcall selector sym))))
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

(defun %apropos-printing (string-designator filter &optional package external-only)
  (let ((str (string string-designator)))
    (%apropos-iterator str filter #'apropos-symbol-print
                       package external-only))
  (values))

(defun %apropos-collecting (string-designator filter &optional package external-only)
  (let ((str (string string-designator))
        (ret ()))
    (%apropos-iterator str filter (lambda (sym) (push sym ret))
                       package external-only)
    ret))

(defun symbol-var-p (sym)
  (and (not (keywordp sym))
       (or (boundp sym) (symbol-macro-p sym))))

(defun apropos-var (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `boundp' or `symbol-macro-p' and not keywords."
  (%apropos-printing string-designator #'symbol-var-p package external-only))

(defun apropos-var-list (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `boundp' or `symbol-macro-p' and not keywords."
  (%apropos-collecting string-designator #'symbol-var-p package external-only))

(defun apropos-fn (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `fboundp'."
  (%apropos-printing string-designator #'fboundp package external-only))

(defun apropos-fn-list (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `fboundp'."
  (%apropos-collecting string-designator #'fboundp package external-only))

(defun symbol-bound-p (sym)
  (and (not (keywordp sym))
       (or (boundp sym) (symbol-macro-p sym) (fboundp sym))))

(defun apropos-bound (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `boundp' (and not keywords), `symbol-macro-p', or `fboundp'"
  (%apropos-printing string-designator #'symbol-bound-p package external-only))

(defun apropos-bound-list (string-designator &optional package external-only)
  "As `apropos', but only examines symbols that are `boundp' (and not keywords), `symbol-macro-p', or `fboundp'"
  (%apropos-collecting string-designator #'symbol-bound-p package external-only))

(defun external-symbols (package)
  (let ((ret ()))
    (do-external-symbols (sym package ret)
      (push sym ret))))

(defun unexport-all (package)
  (dolist (sym (external-symbols package))
    (unexport sym package)))

(ql-import #:alexandria #:cl-ppcre)

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

(ql-import #:cl-dot)

(defun visualize-graph (graph objects &optional attributes)
  "Generate a graph via `cl-dot:generate-graph' to a temporary file and open it in the default image viewer application."
  (let ((dgraph (cl-dot:generate-graph-from-roots graph objects attributes)))
    (uiop:with-temporary-file (:pathname pathname :keep t :type "png")
      (cl-dot:dot-graph dgraph pathname :format :png)
      (open-in-default-application pathname)
      pathname)))

(ql-import #:bordeaux-threads)
(ql-import #:cffi)

(cffi:define-foreign-library kernel32
  (:win32 "Kernel32.dll"))

(cffi:use-foreign-library kernel32)

(cffi:defcfun ("Beep" kernel32-beep :library kernel32 :convention :stdcall) :bool
  (freq :uint32)
  (duration :uint32))

(defun alert ()
  (loop
    :repeat 2
    :do
       (kernel32-beep 1000 800)
       (kernel32-beep 800 800)
       (kernel32-beep 1000 400)
       (kernel32-beep 800 400)
       (sleep .7)))

(defun alert-timer (time &key (units :minutes))
  (let ((multiplier
          (ecase units
            (:seconds 1)
            (:minutes 60)
            (:hours (* 60 60)))))
    (bordeaux-threads:make-thread
     (lambda ()
       (sleep (* multiplier time))
       (alert)))))

(include "place")
(include "fizzbuzz")
