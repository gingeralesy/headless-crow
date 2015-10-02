
(defpackage :things.twist
  (:documentation "Mersenne Twister")
  (:use #:cl)
  (:export #:twist))

(in-package :things.twist)

;; Public

(defun twist ()
  (when (not *initialized*)
    (initialize))
  (* (ash (genrand-int) -11) (/ 1.0 9007199254740992.0)))

;; Private

(defparameter *NN* 312)
(defparameter *MM* 156)
(defparameter *MATRIX-A* 0xB5026F5AA96619E9)
(defparameter *UM* 0xFFFFFFFF80000000) ;; Most signifact 33 bits
(defparameter *LM* 0x7FFFFFFF) ;; Least significant 31 bits

(defvar *initialized* NIL)
(defvar *MT* (make-array *NN*
                         :initial-element 0
                         :element-type '(unsigned-byte 64)))
(defvar *MTI* 0)

(defun initialize (seed))

(defun genrand-int ()
  (dotimes (i *NN*)
    (let ((y (+ (logand (elt *MT* i) *UM*)
                (logand (elt *MT* (mod (1+ i) *NN*)) *LM*))))
      (setf (elt *MT* i) ))))

