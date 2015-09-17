
(defpackage :things.queens
  (:documentation "N-Queens Problem.")
  (:use #:cl)
  (:export #:queens))

(in-package :things.queens)

(defun queens (n)
  (format t "Finding ~a queens.~%" n)
  (let ((positions ()))
    (dotimes (i n)
      (push (find-position positions n) positions))
    (if (= (length positions) n)
        (format t "Positions: ~a~%" positions)
        (format t "No available positions."))))

(defun collision-p (xy1 xy2)
  (let ((x1 (first xy1)) (y1 (second xy1))
        (x2 (first xy2)) (y2 (second xy2)))
    (or (= x1 x2) (= y1 y2) (= (- y2 y1) (- x2 x1)))))

(defun find-position (positions n)
  (if (> (length positions) 0)
      (dotimes (x n)
        (dotimes (y n)
          (let ((collided nil))
            (block find-collides
              (loop for pos in positions
                    do
                    (setf collided (collision-p (list x y) pos))
                    (when collided (return-from find-collides))))
            (when (not collided)
              (return-from find-position (list x y))))))
      (return-from find-position (list 0 0))))
