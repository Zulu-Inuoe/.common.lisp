(in-package #:cl-user)

(defun :make-id ()
  "Create a 16-character 'id' "
  (loop
    :with bag := "abcdefghjkmnpqrstuvwxyzABCDEFGHIJKLMNPQRSTUVWXYZ23456789"
    :with id := (make-string 16)
      :initially
         (loop
           :for c := (char bag (random (length bag)))
           :when (not (digit-char-p c)) :do (loop-finish)
             :finally (setf (char id 0) c))
    :for i :from 1 :below 16
    :do (setf (char id i) (char bag (random (length bag))))
    :finally (return id)))
