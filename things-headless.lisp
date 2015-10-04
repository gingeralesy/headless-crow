
(defpackage :things-headless
  (:documentation "Main things of experimentations without X")
  (:use #:cl #:things.utils #:things.queens #:things.twist #:things.cffi)
  (:export #:main))

(in-package :things-headless)

