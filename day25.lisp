#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day25-snafu->decimal (snafu)
  (loop with value = 0
        for ch across snafu
        do (setf value (* 5 value))
        do (ecase ch
             (#\2 (incf value 2))
             (#\1 (incf value 1))
             (#\0)
             (#\- (decf value 1))
             (#\= (decf value 2)))
        finally (return value)))

(defun day25-decimal->snafu (decimal)
  (let ((snafu NIL)
        (value decimal))
    (loop for remainder = (mod value 5)
          do (cond
               ((< 2 remainder)
                (incf value remainder) ;; Important!
                (push (ecase remainder (3 #\=) (4 #\-)) snafu))
               (T (push (digit-char remainder) snafu)))
          do (setf value (floor value 5))
          while (< 0 value)
          finally (return (format NIL "~{~a~}" snafu)))))

(defun day25-test ()
  (with-open-file (stream (input 25) :if-does-not-exist :error)
    (loop for line = (clean (read-line stream NIL))
          while line
          for decimal = (day25-snafu->decimal line)
          for snafu = (day25-decimal->snafu decimal)
          do (format T "TEST: ~a => ~a => ~a~%" line decimal snafu)
          unless (string= line snafu)
          do (error "SNAFU mismatch: ~a /= ~a" line snafu))))

(defun day25-parse-input ()
  (with-open-file (stream (input 25) :if-does-not-exist :error)
    (loop for line = (clean (read-line stream NIL))
          while line
          collect (day25-snafu->decimal line))))

(defun day25-puzzle1 ()
  (loop for value in (day25-parse-input)
        summing value into sum
        finally (return (day25-decimal->snafu sum))))

;; 2-==10--=-0101==1201
