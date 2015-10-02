
(defpackage :things.twist
  (:documentation "Mersenne Twister")
  (:use #:cl)
  (:export #:twist))

(in-package :things.twist)

;; Public

(defun twist ()
  (when (not *initialized*)
    (initialize (get-universal-time)))
  (* (ash (genrand-int) -11) (/ 1.0 9007199254740992.0)))

;; Private

(defparameter *word-size* 64)
(defparameter *recurrence-degree* 312)
(defparameter *initialization-multiplier* 6364136223846793005)
(defparameter *middle-word* 156)
(defparameter *matrix-a* #xB5026F5AA96619E9)
(defparameter *upper-mask* #xFFFFFFFF80000000) ;; Most signifact 33 bits
(defparameter *lower-mask* #x7FFFFFFF)         ;; Least significant 31 bits

(defvar *initialized* NIL)
(defvar *twister-state* (make-array *recurrence-degree*
                         :initial-element 0
                         :element-type '(unsigned-byte 64))) ;; *word-size*
(defvar *twister-index* *recurrence-degree*)

(defun to-64-bits (value)
  (logand value #xFFFFFFFFFFFFFFFF))

(defun initialize (seed)
  (setf *twister-index* *recurrence-degree*)
  (setf (elt *twister-state* 0) seed)
  (dotimes (i (1- *recurrence-degree*)) ;; eh, find a better way to start from 1
    (setf (elt *twister-state* (1+ i))
          (to-64-bits
           (+ (1+ i)
              (* *initialization-multiplier*
                 (logxor (elt *twister-state* i)
                         (ash (elt *twister-state* i) -30)))))))
  (setf *initialized* T))

(defun do-the-twist ()
  (dotimes (i *recurrence-degree*)
    (let* ((y (to-64-bits
               (+ (logand (elt *twister-state* i) *upper-mask*)
                  (logand (elt *twister-state*
                               (mod (1+ i) *recurrence-degree*))
                          *lower-mask*))))
           (yA (ash y -1)))
      (when (not (= (mod y 2) 0))
        (setf yA (logxor yA #xB5026F5AA96619E9)))
      (setf (elt *twister-state* i)
            (logxor (elt *twister-state*
                         (mod (+ i *middle-word*) *recurrence-degree*))
                    yA))))
  (setf *twister-index* 0))

(defun genrand-int ()
  (when (>= *twister-index* *recurrence-degree*)
    (do-the-twist)) ;; yeaaah
  (let ((y (elt *twister-state* *twister-index*)))
    (setf y (logxor y (logand (ash y -29) #x5555555555555555)))
    (setf y (logxor y (logand (ash y 17) #x71D67FFFEDA60000)))
    (setf y (logxor y (logand (ash y 37) #xFFF7EEE000000000)))
    (setf y (logxor y (ash y -43)))
    (incf *twister-index*)
    (to-64-bits y)))

