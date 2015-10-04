
(defpackage :things.cffi
  (:documentation "CFFI Experiments.")
  (:use #:cl #:cffi)
  (:export #:main))

(in-package :things.cffi)

;; Public

(defun main ()
  (print "Hello, World!"))

;; Private

(define-foreign-library sfml-system
  (:unix "libsfml-system.so" "libsfml-system-2.so")
  (T (:default "sfml-system-2")))

(use-foreign-library sfml-system)

