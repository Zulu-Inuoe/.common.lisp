(in-package #:cl-user)

(:import+ #:bordeaux-threads)
(:import+ #:cffi)

(cffi:define-foreign-library kernel32
  (:win32 "Kernel32.dll"))

(cffi:use-foreign-library kernel32)

(cffi:defcfun ("Beep" kernel32-beep :library kernel32 :convention :stdcall) :bool
  (freq :uint32)
  (duration :uint32))

(defun :alert ()
  (loop
    :repeat 2
    :do
       (kernel32-beep 1000 800)
       (kernel32-beep 800 800)
       (kernel32-beep 1000 400)
       (kernel32-beep 800 400)
       (sleep .7)))

(defun :alert-timer (time &key (units :minutes))
  (let ((multiplier
          (ecase units
            (:seconds 1)
            (:minutes 60)
            (:hours (* 60 60)))))
    (bordeaux-threads:make-thread
     (lambda ()
       (sleep (* multiplier time))
       (:alert)))))
