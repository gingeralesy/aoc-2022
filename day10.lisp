#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day10-command-re* (cl-ppcre:create-scanner "^(noop|addx)( (-?\\d+))?$"))

(defun day10-parse-command (line)
  (when line
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings *day10-command-re* line)
      (unless match (error "Invalid command: ~a" line))
      (let ((command (intern (format NIL "~:@(~a~)" (aref groups 0)) :keyword))
            (argument (aref groups 2)))
        (list command
              (when argument (parse-integer argument))
              (ecase command
                (:noop 1)
                (:addx 2)))))))

(defun day10-puzzle1 ()
  (with-open-file (stream (input 10) :if-does-not-exist :error)
    (loop with x = 1
          with signal = 0
          with next = 20
          with cycle = 0
          for (command argument time) = (day10-parse-command (clean (read-line stream NIL)))
          while command
          do (incf cycle time)
          do (when (<= next cycle)
               (incf signal (* next x))
               (incf next 40))
          do (ecase command
               (:noop) ;; Do nothing.
               (:addx (incf x argument)))
          finally (return signal))))

;; 14060

(defun day10-puzzle2 ()
  (with-open-file (stream (input 10) :if-does-not-exist :error)
    (let ((stack))
      (loop for (command argument time) = (day10-parse-command (clean (read-line stream NIL)))
            while command
            do (dotimes (i (1- time))
                 (push (cons :noop NIL) stack))
            do (push (cons command argument) stack))
      (loop with x = 1
            for cycle from 0
            for (command . argument) in (nreverse stack)
            when (and (< 0 cycle) (= 0 (mod cycle 40)))
            do (format T "~%")
            do (format T "~c" (if (and (<= x (1+ (mod cycle 40)))
                                       (<= (- (1+ (mod cycle 40)) 2) x))
                                  #\# #\.))
            do (ecase command
                 (:noop)
                 (:addx (incf x argument)))))))

;; PAPKFKEJ
;;
;; ###...##..###..#..#.####.#..#.####...##.
;; #..#.#..#.#..#.#.#..#....#.#..#.......#.
;; #..#.#..#.#..#.##...###..##...###.....#.
;; ###..####.###..#.#..#....#.#..#.......#.
;; #....#..#.#....#.#..#....#.#..#....#..#.
;; #....#..#.#....#..#.#....#..#.####..##..
