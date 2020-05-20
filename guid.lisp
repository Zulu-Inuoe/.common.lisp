(in-package #:cl-user)

(defun :make-guid ()
  "Create a randomly-generated 128-bit GUID in the form of a {} wrapped, 32-digit, hyphen-separated, hexadecimal string.
  Example: {A3C78807-EA6D-A4AE-1CCA-797A6BF88C31}"
  (format nil "{~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X}"
          (random (ash 1 32))
          (random (ash 1 16))
          (random (ash 1 16))
          (random (ash 1 16))
          (random (ash 1 48))))
