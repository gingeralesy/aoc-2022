#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day1-puzzle1 ()
  (with-open-file (stream (input 1) :if-does-not-exist :error)
    (loop with calories = 0
          with max = 0
          for line = (clean (read-line stream NIL))
          while line
          do (cond
               ((< 0 (length line))
                (incf calories (parse-integer line)))
               (T
                (when (< max calories) (setf max calories))
                (setf calories 0)))
          finally (return max))))

;; 69289

(defun day1-puzzle2 ()
  (with-open-file (stream (input 1) :if-does-not-exist :error)
    (loop with calories = 0
          with elves = NIL
          for line = (clean (read-line stream NIL))
          while line
          do (cond
               ((< 0 (length line))
                (incf calories (parse-integer line)))
               (T
                (push calories elves)
                (setf calories 0)))
          finally (progn
                    (setf elves (sort elves #'>))
                    (return (+ (first elves) (second elves) (third elves)))))))

;; 205615
