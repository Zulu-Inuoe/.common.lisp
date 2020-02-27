(in-package #:cl-user)

(:import+ #:drakma #:jsown)

(defun :fizzbuzz (n)
  "Return \"Fizz\", \"Buzz\", \"FizzBuzz\", or \"`n'\" depending on the divisibility of `n'"
  (jsown:val-safe (jsown:parse
                   (drakma:http-request (format nil "https://itwont.work/fizzbuzz/api?start=~A&end=~:*~A" n)))
                  "numbers"))
