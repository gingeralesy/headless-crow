
(defpackage :things.queens
  (:documentation "N-Queens Problem.")
  (:use #:cl)
  (:export #:queens))

(in-package :things.queens)

;; Public

(defun queens (n)
  "Finds and prints out a possible positioning of N queens on an N by N board."
  (format t "Finding ~a queens.~%" n)
  (let ((positions (find-positions n)))
    (format t "Positions:~%")
    (if positions
        (loop for p being the elements of positions
              do
              (dotimes (i p) (format t " *"))
              (format t " Q")
              (dotimes (i (- n p)) (format t " *"))
              (format t "~%"))
        (format t "No available positions.~%"))))

(defun valid-p (k positions)
  "Checks if this is a valid position for the next queen."
  (dotimes (i (1- k))
    (let ((qi (elt positions i)))
      (when (or (= qi k)
              (= (abs (- qi (elt positions k)))
                 (abs (- i k))))
        (return-from valid-p nil))))
  t)

(defun find-positions (n)
  "Finds the valid positions."
  (let ((positions (make-array n :initial-element 0)))
    (loop for k from 0
          while (< k n)
          do
          (loop while (and (< k n) (not (valid-p k positions)))
                do
                (setf (elt positions k) (1+ (elt positions k))))
          (when (and (= k (1- n)) (< (elt positions k) n))
            (return-from find-positions positions))
          (if (and (< k (1- n)) (< (elt positions k) n))
              (incf k)
              (progn
                (decf k)
                (if (< k 0)
                    (return-from find-positions nil)
                    (setf (elt positions k) (1+ (elt positions k))))))))
  nil)
