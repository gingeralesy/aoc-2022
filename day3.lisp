#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day3-priorities* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun priority (char)
  (loop for ch across *day3-priorities*
        counting ch into value
        until (char= char ch)
        finally (return value)))

(defun day3-puzzle1 ()
  (with-open-file (stream (input 3) :if-does-not-exist :error)
    (loop with sum = 0
          for line = (clean (read-line stream NIL))
          while line
          for count = (length line)
          for left = (subseq line 0 (/ count 2))
          for right = (subseq line (/ count 2))
          do (loop for ch across left
                   for match = (find ch right)
                   do (when match (incf sum (priority ch)))
                   until match)
          finally (return sum))))

;; 7795

(defun day3-puzzle2 ()
  (with-open-file (stream (input 3) :if-does-not-exist :error)
    (loop with sum = 0
          for (elf1 elf2 elf3) = (loop repeat 3 collect (clean (read-line stream NIL)))
          while elf1
          unless elf3 do (error "Not enough elves!")
          do (loop for ch across elf1
                   for badge = (and (find ch elf2) (find ch elf3))
                   until badge
                   finally (incf sum (priority badge)))
          finally (return sum))))

;; 2703
