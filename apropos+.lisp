(in-package #:cl-user)

(defun symbol-macro-p (sym &optional env)
  "Returns true if `sym' is a symbol macro in `env'"
  (and (symbolp sym)
       (nth-value 1 (macroexpand-1 sym env))))

(defun apropos-symbol-print (sym)
  (format t "~S~:[~; (bound)~]~:[~; (symbol-macro)~]~:[~; (fbound)~]~%" sym (boundp sym) (symbol-macro-p sym) (fboundp sym)))

(defun %apropos-iterator (str filter selector &optional package external-only)
  "Iterates over symbols according to `package' and `external-only', testing them with `filter'.
  If `filter' returns true, calls `selector'."
  (let ((str (string str))
        (seen (make-hash-table :test 'eq)))
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
  (%apropos-iterator string-designator filter #'apropos-symbol-print
                     package external-only))

(defun %apropos-collecting (string-designator filter &optional package external-only)
  (let ((ret ()))
    (%apropos-iterator string-designator filter (lambda (sym) (push sym ret))
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
