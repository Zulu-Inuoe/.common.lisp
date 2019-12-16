(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hash-table-reader (stream char)
    (declare (ignore char))
    `(hash ,@(read-delimited-list #\} stream  t))))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (set-macro-character #\{ 'hash-table-reader)
;;   (set-macro-character #\} (lambda (stream char)
;;                              (declare (ignore stream char))
;;                              (error "unmatched close bracket"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pprint-hash-table (stream object)
    (check-type object hash-table)
    (cond
      (*print-readably*
       (pprint-logical-block (stream nil :prefix "#.(" :suffix ")")
         (write 'hash :stream stream)
         (maphash
          (lambda (k v)
            (write-char #\Space stream)
            (write k :stream stream)
            (write-char #\Space stream)
            (write v :stream stream))
          object)))
      (t
       (pprint-logical-block (stream nil :prefix "{")
         (if (> (hash-table-count object) 1)
             (maphash
              (lambda (k v)
                (pprint-newline :mandatory stream)
                (write k :stream stream)
                (write-char #\Space stream)
                (write v :stream stream))
              object)
             (maphash
              (lambda (k v)
                (write-char #\Space stream)
                (write k :stream stream)
                (write-char #\Space stream)
                (write v :stream stream))
              object)))
       (case (hash-table-count object)
         ((0 1) (write-char #\Space stream))
         (t (pprint-newline :mandatory stream)))
       (write-char #\} stream)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-pprint-dispatch 'hash-table 'pprint-hash-table))

