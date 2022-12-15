#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day15-sensor-re*
  (cl-ppcre:create-scanner "^Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)\\s*$"))

(defun day15-distance (x1 y1 x2 y2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defun day15-intersects-p (range-a range-b)
  (when (and range-a range-b)
    (or (<= (car range-b) (cdr range-a) (cdr range-b))
        (<= (car range-a) (cdr range-b) (cdr range-a)))))

(defun day15-adjacent-p (range-a range-b)
  (when (and range-a range-b)
    (or (= (1- (car range-a)) (cdr range-b))
        (= (1+ (cdr range-a)) (car range-b)))))

(defun day15-unionise (&rest ranges)
  (loop for range in ranges
        minimize (car range) into min
        maximize (cdr range) into max
        finally (return (cons min max))))

(defun day15-intersect (&rest ranges)
  (loop for range in ranges
        maximize (car range) into min
        minimize (cdr range) into max
        finally (return (when (<= min max) (cons min max)))))

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
                         (distance (day15-distance sensor-x sensor-y beacon-x beacon-y))
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
  (loop with unavailable = NIL
        for sensor in (day15-parse-sensors)
        for ((x . y) . distance) = sensor
        for xd = (- distance (abs (- row y)))
        for range = (cons (- x xd) (+ x xd))
        when (<= 0 xd)
        do (loop with others = NIL
                 with current = range
                 for other in unavailable
                 do (if (or (day15-intersects-p current other) (day15-adjacent-p current other))
                        (setf current (day15-unionise current other))
                        (push other others))
                 finally (setf unavailable (nconc (list current) others)))
        finally (return
                  (values
                   (loop for (start . end) in unavailable
                         summing (- end start))
                   unavailable))))

;; 4873353

(defun day15-puzzle2 (&optional (max 4000000))
  ;; We find all the intersections between the 45° squares, then we find the spots next to those
  ;; intersections that are outside those two squares, and then we iterate through those spots to
  ;; see which is not within any beacon range square. It works!
  ;; TODO: The *correct* way to calculate intersections is to first take the intersecting edges as
  ;;       lines, convert them into the ax + by + c = 0 format, and then use the Cramer's formula to
  ;;       find the intersection. I took a bit of a shortcut here with the maths. Doing it properly
  ;;       would remove the need of calling INTERSECTIONS twice and the filtering duplicates.
  ;;       The formula is x = (/ (- (* b2 c2) (* b2 c1)) (- (* a1 b2) (* a2 b1))),
  ;;                      y = (/ (- (* c1 a2) (* c2 a1)) (- (* a1 b2) (* a2 b1))).
  ;;       Value a is always 1 or -1, the b is 1, and the c is y0 - ax0.
  (flet ((intersections (sensor other)
           (let* ((sensor-x (caar sensor))
                  (sensor-y (cdar sensor))
                  (sensor-range (cdr sensor))
                  (other-x (caar other))
                  (other-y (cdar other))
                  (other-range (cdr other))
                  (distance (day15-distance sensor-x sensor-y other-x other-y))
                  (ranges (+ sensor-range other-range))
                  (depth (- ranges distance)))
             (when (and (< 0 distance)
                        (<= distance ranges)
                        (< (max sensor-range other-range)
                           (+ distance (min sensor-range other-range)))
                        (= 0 (mod depth 2)))
               (let* ((intersections)
                      (left (- sensor-x sensor-range))
                      (left-distance (day15-distance left sensor-y other-x other-y))
                      (right (+ sensor-x sensor-range))
                      (right-distance (day15-distance right sensor-y other-x other-y))
                      (top (- sensor-y sensor-range))
                      (top-distance (day15-distance sensor-x top other-x other-y))
                      (bottom (+ sensor-y sensor-range))
                      (bottom-distance (day15-distance sensor-x bottom other-x other-y))
                      (half-depth (/ depth 2)))
                 (when (= left-distance
                          (min left-distance right-distance top-distance bottom-distance))
                   (push (cons (+ left half-depth) (if (< sensor-y other-y)
                                                       (- sensor-y half-depth)
                                                       (+ sensor-y half-depth)))
                         intersections))
                 (when (= right-distance
                          (min left-distance right-distance top-distance bottom-distance))
                   (push (cons (- right half-depth) (if (<= sensor-y other-y)
                                                        (- sensor-y half-depth)
                                                        (+ sensor-y half-depth)))
                         intersections))
                 (when (= top-distance
                          (min left-distance right-distance top-distance bottom-distance))
                   (push (cons (if (< sensor-x other-x)
                                   (- sensor-x half-depth)
                                   (+ sensor-x half-depth))
                               (+ top half-depth))
                         intersections))
                 (when (= bottom-distance
                          (min left-distance right-distance top-distance bottom-distance))
                   (push (cons (if (<= sensor-x other-x)
                                   (- sensor-x half-depth)
                                   (+ sensor-x half-depth))
                               (- bottom half-depth))
                         intersections))
                 intersections)))))
    (loop with spots = NIL
          for all-sensors = (day15-parse-sensors)
          for sensors = all-sensors then (cdr sensors)
          for sensor = (car sensors)
          while sensors
          do (loop for other in (rest sensors)
                   append (intersections sensor other) into intersections
                   append (intersections other sensor) into intersections
                   finally (loop for (x . y) in intersections
                                 when (and (<= 0 x) (<= x max) (<= 0 y) (<= y max)
                                           (< (caar sensor) x) (< (caar other) x)
                                           (null (find (cons (1+ x) y) spots :test #'equal)))
                                 do (push (cons (1+ x) y) spots)
                                 when (and (<= 0 x) (<= x max) (<= 0 y) (<= y max)
                                           (< x (caar sensor)) (< x (caar other))
                                           (null (find (cons (1- x) y) spots :test #'equal)))
                                 do (push (cons (1- x) y) spots)
                                 when (and (<= 0 x) (<= x max) (<= 0 y) (<= y max)
                                           (< (cdar sensor) y) (< (cdar other) y)
                                           (null (find (cons x (1+ y)) spots :test #'equal)))
                                 do (push (cons x (1+ y)) spots)
                                 when (and (<= 0 x) (<= x max) (<= 0 y) (<= y max)
                                           (< y (cdar sensor)) (< y (cdar other))
                                           (null (find (cons x (1- y)) spots :test #'equal)))
                                 do (push (cons x (1- y)) spots)))
          finally (loop for (x1 . y1) in spots
                        for valid = T
                        do (loop for ((x2 . y2) . distance) in all-sensors
                                 when (< (day15-distance x1 y1 x2 y2) distance)
                                 do (setf valid NIL)
                                 while valid)
                        when valid do (return-from day15-puzzle2
                                        (values
                                         (+ (* x1 4000000) y1)
                                         (cons x1 y1)))))))

;; 11600823139120
