#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day6-puzzle1 ()
  (with-open-file (stream (input 6) :if-does-not-exist :error)
    (let ((buffer (make-array 4 :element-type 'character :initial-element #\Nul)))
      (loop for i from 0
            do (setf (aref buffer (mod i 4)) (read-char stream))
            while (or (< i 4)
                      (loop for match = 0
                            for ch across buffer
                            do (loop for ch2 across buffer when (char= ch ch2) do (incf match))
                            until (< 1 match)
                            finally (return (< 1 match))))
            finally (return (1+ i))))))

;; 1235

(defun day6-puzzle2 ()
  (with-open-file (stream (input 6) :if-does-not-exist :error)
    (let ((buffer (make-array 14 :element-type 'character :initial-element #\Nul)))
      (loop for i from 0
            do (setf (aref buffer (mod i 14)) (read-char stream))
            while (or (< i 14)
                      (loop for match = 0
                            for ch across buffer
                            do (loop for ch2 across buffer when (char= ch ch2) do (incf match))
                            until (< 1 match)
                            finally (return (< 1 match))))
            finally (return (1+ i))))))

;; 3051
