
(defpackage :things.twist
  (:documentation "Mersenne Twister")
  (:use #:cl)
  (:export #:random-double #:random-int))

(in-package :things.twist)

;; Public

(defun random-double ()
  "Generates a random double between [0,1["
  (when (not *initialized*)
    (initialize (get-universal-time)))
  (* (ash (random-int) -11) (/ 1.0 9007199254740992.0)))

(defun random-int ()
  "Generates a random 64-bit integer."
  (when (>= *twister-index* *recurrence-degree*)
    (do-the-twist)) ;; yeaaah
  (let ((y (elt *twister-state* *twister-index*)))
    (setf y (logxor y (logand (ash y -29) #x5555555555555555)))
    (setf y (logxor y (logand (ash y 17) #x71D67FFFEDA60000)))
    (setf y (logxor y (logand (ash y 37) #xFFF7EEE000000000)))
    (setf y (logxor y (ash y -43)))
    (incf *twister-index*)
    (to-64-bits y)))

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
  "Ensures that the value does not exceed 64 bits."
  (logand value #xFFFFFFFFFFFFFFFF))

(defun initialize (seed)
  "Initializes the Mersenne Twister state."
  (setf *twister-index* *recurrence-degree*)
  (setf (elt *twister-state* 0) seed)
  (dotimes (prev-index (1- *recurrence-degree*))
    (let ((index (1+ prev-index))) ;; Must be a better way to get index
      (setf (elt *twister-state* index)
            (to-64-bits
             (+ index
                (* *initialization-multiplier*
                   (logxor (elt *twister-state* prev-index)
                           (ash (elt *twister-state* prev-index) -30))))))))
  (setf *initialized* T))

(defun do-the-twist ()
  "Fills the Mersenne Twister state with new values."
  (dotimes (i *recurrence-degree*)
    (let* ((y (to-64-bits
               (+ (logand (elt *twister-state* i) *upper-mask*)
                  (logand (elt *twister-state*
                               (mod (1+ i) *recurrence-degree*))
                          *lower-mask*))))
           (yA (ash y -1)))
      (when (not (= (mod y 2) 0))
        (setf yA (logxor yA *matrix-a*)))
      (setf (elt *twister-state* i)
            (logxor (elt *twister-state*
                         (mod (+ i *middle-word*) *recurrence-degree*))
                    yA))))
  (setf *twister-index* 0))
