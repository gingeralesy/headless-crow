
(defpackage :things.utils
  (:documentation "Various useful and useless utils.")
  (:use #:cl)
  (:export #:queue-push #:queue-pop #:queue-front #:queue-back
           #:queue-make #:queue-length #:queue-empty-p))

(in-package :things.utils)

(defun queue-make ()
  (cons nil nil))

(defun queue-push (queue item)
  "Pushes a value into a queue."
  (let ((new-item (cons item nil)))
    (setf (cdr queue)
          (if (car queue)
              (setf (cddr queue) new-item)
              (setf (car queue) new-item))))
  queue)

(defun queue-pop (queue)
  "Pops a value from a queue."
  (pop (car queue)))

(defun queue-front (queue)
  "Returns the value at the head of the queue without popping it."
  (caar queue))

(defun (setf queue-front) (new-value queue)
  "Replaces the value in the front of the queue with a new one."
  (setf (caar queue) new-value))

(defun queue-back (queue)
  "Returns the value at the tail of the queue."
  (cadr queue))

(defun (setf queue-back) (new-value queue)
  "Replaces the value in the back of the queue with a new one."
  (setf (cadr queue) new-value))

(defun queue-length (queue)
  "Returns the size of the queue."
  (length (car queue)))

(defun queue-empty-p (queue)
  "Checks if the queue is empty of content."
  (null (car queue)))

