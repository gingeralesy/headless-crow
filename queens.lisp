
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
        (loop for p across positions
              do (format t "~v@{ *~} Q~v@{ *~}~%" p (- n (1+ p)) NIL))
        (format t "No available positions.~%"))))

(defun valid-p (k positions)
  "Checks if this is a valid position for the next queen."
  (let ((qk (elt positions k)))
    (dotimes (i k T)
      (let ((qi (elt positions i)))
        (when (or (= qi qk)
                  (= (abs (- qi qk))
                     (abs (- i k))))
          (return NIL))))))

(defun find-positions (n)
  "Finds the valid positions."
  (let ((positions (make-array n :initial-element 0)) (k 0))
    (loop while (< k n)
          do (loop while (and (< k n) (not (valid-p k positions)))
                   ;; Move rightwards until a good position is found
                   do (incf (elt positions k)))
             (cond ((and (= k (1- n)) (< (elt positions k) n))
                    (return positions))
                   ((and (< k (1- n)) (< (elt positions k) n))
                    (incf k))
                   ((< k 1)
                    ;; Welp, no solutions
                    (return NIL))
                   (T
                    ;; Go back a bit
                    (setf (elt positions k) 0)
                    (decf k)
                    (incf (elt positions k)))))))
