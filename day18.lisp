#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day18-cube-re* (cl-ppcre:create-scanner "^\\s*(\\d+),(\\d+),(\\d+)\\s*$"))

(defun day18-parse-input ()
  (with-open-file (stream (input 18) :if-does-not-exist :error)
    (loop for line = (read-line stream NIL)
          while line
          for (match groups) = (multiple-value-list
                                (cl-ppcre:scan-to-strings *day18-cube-re* line))
          for cube = (when match
                       (list (parse-integer (aref groups 0))
                             (parse-integer (aref groups 1))
                             (parse-integer (aref groups 2))))
          when cube collect cube into cubes
          maximizing (first cube) into width
          maximizing (second cube) into height
          maximizing (third cube) into depth
          finally (return (values cubes (1+ width) (1+ height) (1+ depth))))))

(defun day18-puzzle1 ()
  (multiple-value-bind (cubes width height depth)
      (day18-parse-input)
    (let ((map (make-array (list width height depth)
                           :element-type 'boolean :initial-element NIL)))
      (loop with count = 0
            for (x y z) in cubes
            do (setf (aref map x y z) T)
            do (incf count 6)
            do (loop for (xd yd zd) in '((-1 0 0) (0 -1 0) (0 0 -1)
                                         ( 1 0 0) (0  1 0) (0 0  1))
                     for x0 = (+ x xd)
                     for y0 = (+ y yd)
                     for z0 = (+ z zd)
                     when (and (<= 0 x0) (<= 0 y0) (<= 0 z0)
                               (< x0 width) (< y0 height) (< z0 depth)
                               (aref map x0 y0 z0))
                     do (decf count 2))
            finally (return count)))))

;; 3636
