(in-package #:cl-user)

(defun external-symbols (package)
  (let ((ret ()))
    (do-external-symbols (sym package ret)
      (push sym ret))))

(defun unexport-all (package)
  (dolist (sym (external-symbols package))
    (unexport sym package)))
