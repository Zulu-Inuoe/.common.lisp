(in-package #:cl-user)

(:import+ #:alexandria #:trivial-indent)

(trivial-indent:define-indentation alexandria:unwind-protect-case (4 2 &body))
