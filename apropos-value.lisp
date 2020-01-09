(in-package #:cl-user)

(:import+ #:alexandria #:cl-ppcre)

(:include "apropos+")

(defun :apropos-value (pattern &optional do-all)
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
                 ((:symbol-macro-p sym)
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
               (unless (or (boundp sym) (:symbol-macro-p sym)
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
                   (:apropos-symbol-print sym)
                   ;; Also print the value that matched
                   (format t "~0,3T~S~2%" value))))
           :continue)))))
  (values))
