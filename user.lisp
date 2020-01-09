(in-package #:cl-user)

(defvar *user-full-name* nil)

(defun :user-full-name ()
  (or *user-full-name*
      (ignore-errors (string-trim '(#\Newline) (uiop:run-program (list "git" "config" "user.name") :output :string)))
      (uiop:getenv "USER")
      (uiop:getenv "USERNAME")))

(defun (setf :user-full-name) (value)
  (setf *user-full-name* value))

(define-symbol-macro user-full-name (:user-full-name))

(defvar *user-mail-address* nil)

(defun :user-mail-address ()
  (or *user-mail-address*
      (ignore-errors (string-trim '(#\Newline) (uiop:run-program (list "git" "config" "user.email") :output :string)))))

(defun (setf :user-mail-address) (value)
  (setf *user-mail-address* value))

(define-symbol-macro user-mail-address (:user-mail-address))
