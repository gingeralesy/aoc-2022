#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day15-sensor-re*
  (cl-ppcre:create-scanner "^Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)\\s*$"))

(defun day15-parse-sensors (&optional beacon-position)
  (with-open-file (stream (input 15) :if-does-not-exist :error)
    (loop with min = NIL
          with max = NIL
          for line = (read-line stream NIL)
          while line
          for (match groups) = (multiple-value-list
                                (cl-ppcre:scan-to-strings *day15-sensor-re* line))
          when match
          collect (let* ((sensor-x (parse-integer (aref groups 0)))
                         (sensor-y (parse-integer (aref groups 1)))
                         (beacon-x (parse-integer (aref groups 2)))
                         (beacon-y (parse-integer (aref groups 3)))
                         (distance (+ (abs (- beacon-x sensor-x))
                                      (abs (- beacon-y sensor-y))))
                         (min-x (- sensor-x distance))
                         (max-x (+ sensor-x distance))
                         (min-y (- sensor-y distance))
                         (max-y (+ sensor-y distance)))
                    (when (null min)
                      (setf min (cons min-x min-y)))
                    (when (and min (< min-x (car min)))
                      (setf (car min) min-x))
                    (when (and min (< min-y (cdr min)))
                      (setf (cdr min) min-y))
                    (when (null max)
                      (setf max (cons max-x max-y)))
                    (when (and max (< max-x (car max)))
                      (setf (car max) max-x))
                    (when (and max (< max-y (cdr max)))
                      (setf (cdr max) max-y))
                    (cons (cons sensor-x sensor-y)
                          (if beacon-position
                              (cons beacon-x beacon-y)
                              distance)))
          into sensors
          finally (return (values sensors min max)))))

(defun day15-puzzle1 (&optional (row 2000000))
  (flet ((intersects-p (range-a range-b)
           (when (and range-a range-b)
             (or (and (<= (car range-b) (cdr range-a)) (<= (cdr range-a) (cdr range-b)))
                 (and (<= (car range-a) (cdr range-b)) (<= (cdr range-b) (cdr range-a))))))
         (unionise (&rest ranges)
           (loop for range in ranges
                 minimize (car range) into min
                 maximize (cdr range) into max
                 finally (return (cons min max)))))
    (loop with unavailable = NIL
          for sensor in (day15-parse-sensors)
          for ((x . y) . distance) = sensor
          for xd = (- distance (abs (- row y)))
          for range = (cons (- x xd) (+ x xd))
          when (<= 0 xd)
          do (loop with others = NIL
                   with current = range
                   for other in unavailable
                   do (if (intersects-p current other)
                          (setf current (unionise current other))
                          (push other others))
                   finally (progn
                             (push current others)
                             (setf unavailable others)))
          finally (return (loop for (start . end) in unavailable
                                summing (- end start))))))

;; 4873353
