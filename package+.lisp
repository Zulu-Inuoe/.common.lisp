(in-package #:cl-user)

(defun :external-symbols (package)
  (let ((ret ()))
    (do-external-symbols (sym package ret)
      (push sym ret))))

(defun :unexport-all (package)
  (dolist (sym (external-symbols package))
    (unexport sym package)))

(defun :delete-package* (package-designator)
  "As `delete-package', but also removes the package from the package-use-list of dependent ones."
  (dolist (dep (package-used-by-list package-designator))
    (unuse-package package-designator dep))
  (delete-package package-designator))
