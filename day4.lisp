#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day4-section-re* (cl-ppcre:create-scanner "^(\\d+)-(\\d+),(\\d+)-(\\d+)$"))

(defun day4-ranges (line)
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings *day4-section-re* line)
    (unless match (error "Invalid string: ~a" line))
    (let ((min-a (parse-integer (aref groups 0)))
          (max-a (parse-integer (aref groups 1)))
          (min-b (parse-integer (aref groups 2)))
          (max-b (parse-integer (aref groups 3))))
      (unless (and min-a max-a min-b max-b)
        (error "Didn't get every value: ~a" line))
      (list min-a max-a min-b max-b))))

(defun day4-puzzle1 ()
  (with-open-file (stream (input 4) :if-does-not-exist :error)
    (loop with count = 0
          for line = (clean (read-line stream NIL))
          while line
          for (min-a max-a min-b max-b) = (day4-ranges line)
          when (or (and (<= min-a min-b) (<= max-b max-a))
                   (and (<= min-b min-a) (<= max-a max-b)))
          do (incf count)
          finally (return count))))

;; 466

(defun day4-puzzle2 ()
  (with-open-file (stream (input 4) :if-does-not-exist :error)
    (loop with count = 0
          for line = (clean (read-line stream NIL))
          while line
          for (min-a max-a min-b max-b) = (day4-ranges line)
          when (or (and (<= min-a min-b) (<= min-b max-a))
                   (and (<= min-a max-b) (<= max-b max-a))
                   (and (<= min-b min-a) (<= min-a max-b))
                   (and (<= min-b max-a) (<= max-a max-b)))
          do (incf count)
          finally (return count))))

;; 865
