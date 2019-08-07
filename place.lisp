(in-package #:cl-user)

(defstruct (place (:constructor make-place (reader writer)))
  (reader nil :type function :read-only t)
  (writer nil :type function :read-only t))

(defmacro p& (place &environment env)
  (multiple-value-bind (temp-vars value-forms store-vars store-form access-form)
      (get-setf-expansion place env)
    (let ((ret
            `(make-place
              (lambda () ,access-form)
              (lambda (,@store-vars) ,store-form))))
      (when temp-vars
        (setf ret
              `(let (,@(mapcar #'list temp-vars value-forms))
                 ,ret)))
      ret)))

(defun p* (ptr &optional (num-vals 1))
  (declare (type place ptr))
  (declare (ignore num-vals))
  (funcall (place-reader ptr)))

(define-setf-expander p* (ptr &optional (num-vals 1))
  (let ((ptr-var (gensym))
        (new-value-vars (loop :repeat num-vals :collect (gensym))))
    (values `(,ptr-var)
            `(,ptr)
            new-value-vars
            `(funcall (place-writer (the place ,ptr-var)) ,@new-value-vars)
            `(funcall (place-reader (the place ,ptr-var))))))
