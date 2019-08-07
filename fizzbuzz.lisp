(in-package #:cl-user)

(import+ #:drakma #:jonathan)

(defun fizzbuzz (n)
  "Return \"Fizz\", \"Buzz\", \"FizzBuzz\", or \"`n'\" depending on the divisibility of `n'"
  (cadr
   (assoc "numbers"
          (jonathan:parse
           (drakma:http-request (format nil "https://itwont.work/fizzbuzz/api?start=~A&end=~:*~A" n))
           :as :alist)
          :test #'string=)))