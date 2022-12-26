#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defclass day24-moment ()
  ((blizzards :initarg :blizzards :accessor blizzards)
   (f-score :initform NIL)
   (g-score :initform NIL)
   (came-from :initform NIL)
   (field :initform NIL :accessor field)))

(defmethod initialize-instance :after ((moment day24-moment) &key blizzards width height)
  (let ((dimensions (list height width)))
    (setf (slot-value moment 'g-score) (make-array dimensions :element-type '(integer 0 *)
                                                              :initial-element #xFFFFFFFFFFFFFFFF))
    (setf (slot-value moment 'f-score) (make-array dimensions :element-type '(integer 0 *)
                                                              :initial-element #xFFFFFFFFFFFFFFFF))
    (setf (slot-value moment 'came-from) (make-array dimensions :initial-element NIL)))
  (setf (field moment) (day24-build-map blizzards width height)))

(defun day24-parse-input ()
  (with-open-file (stream (input 24) :if-does-not-exist :error)
    (loop with width = 0
          with count = 0
          for height from 0
          for line = (clean (read-line stream NIL))
          while line
          append (loop for x from 0
                       for ch across line
                       for blizzard = (case ch (#\> :east) (#\< :west) (#\^ :north) (#\v :south))
                       when (<= width x) do (setf width (1+ x))
                       when blizzard do (incf count)
                       when blizzard collect (cons blizzard (cons x height)))
          into blizzards
          finally (return (values blizzards count width height)))))

(defun day24-build-map (blizzards width height)
  (let ((map (make-array (list height width) :element-type 'boolean :initial-element T)))
    (loop for y from 0 below height
          do (setf (aref map y 0) NIL)
          do (setf (aref map y (1- width)) NIL))
    (loop for x from 0 below width
          when (/= x 1) do (setf (aref map 0 x) NIL)
          when (/= x (- width 2)) do (setf (aref map (1- height) x) NIL))
    (loop for blizzard in blizzards
          do (setf (aref map (cddr blizzard) (cadr blizzard)) NIL))
    map))

(defun day24-move-blizzards (blizzards width height)
  (loop for (direction . (x . y)) in blizzards
        for next-x = (let ((value (case direction (:east (1+ x)) (:west (1- x)) (T x))))
                       (cond
                         ((< value 1) (+ value (- width 2)))
                         ((<= (1- width) value) (- value (- width 2)))
                         (T value)))
        for next-y = (let ((value (case direction (:south (1+ y)) (:north (1- y)) (T y))))
                       (cond
                         ((< value 1) (+ value (- height 2)))
                         ((<= (1- height) value) (- value (- height 2)))
                         (T value)))
        collect (cons direction (cons next-x next-y))))

(defun day24-distance (start-x start-y end-x end-y)
  (+ (abs (- end-x start-x)) (abs (- end-y start-y))))

(defun day24-neighbours (map x y width height)
  (let ((current (aref map y x))
        (east (when (< x (1- width)) (aref map y (1+ x))))
        (south (when (< y (1- height)) (aref map (1+ y) x)))
        (west (when (< 0 x) (aref map y (1- x))))
        (north (when (< 0 y) (aref map (1- y) x))))
    (nconc (when current (list (cons x y)))
           (when east (list (cons (1+ x) y)))
           (when south (list (cons x (1+ y))))
           (when west (list (cons (1- x) y)))
           (when north (list (cons x (1- y)))))))

(defun day24-astar (start end width height start-blizzards)
  (let ((start-x (car start))
        (start-y (cdr start))
        (end-x (car end))
        (end-y (cdr end))
        (moments (make-array 16 :adjustable T :fill-pointer 0))
        (available (list (cons 0 start))))
    (labels ((add-moment ()
               (let* ((blizzards (day24-move-blizzards (if (< 0 (length moments))
                                                           (blizzards (aref moments (1- (length moments))))
                                                           start-blizzards)
                                                       width height))
                      (moment (make-instance 'day24-moment :blizzards blizzards :width width :height height)))
                 (vector-push-extend moment moments)
                 moment))
             (g-score (time x y)
               (when (and (<= 0 x) (< x width) (<= 0 y) (< y height))
                 (with-slots (g-score) (aref moments time)
                   (aref g-score y x))))
             (set-g-score (time x y value)
               (when (and (<= 0 x) (< x width) (<= 0 y) (< y height))
                 (with-slots (g-score) (aref moments time)
                   (setf (aref g-score y x) value))))
             (f-score (time x y)
               (when (and (<= 0 x) (< x width) (<= 0 y) (< y height))
                 (with-slots (f-score) (aref moments time)
                   (aref f-score y x))))
             (set-f-score (time x y value)
               (when (and (<= 0 x) (< x width) (<= 0 y) (< y height))
                 (with-slots (f-score) (aref moments time)
                   (setf (aref f-score y x) value))))
             (remove-available (time x y)
               (loop for prev = NIL then cur
                     for cur = available then (cdr cur)
                     for (cur-time . cur-pos) = (car cur)
                     for (cur-x . cur-y) = cur-pos
                     until (or (null cur) (and (= time cur-time) (= x cur-x) (= y cur-y)))
                     finally (when cur
                               (if prev
                                   (setf (cdr prev) (cdr cur))
                                   (setf available (cdr cur))))))
             (add-available (time x y)
               (remove-available time x y)
               (loop with node-f = (f-score time x y)
                     with new = (list (cons time (cons x y)))
                     for prev = NIL then cur
                     for cur = available then (cdr cur)
                     for (cur-time . cur-pos) = (car cur)
                     for (cur-x . cur-y) = cur-pos
                     for cur-f = (when cur-time (f-score cur-time cur-x cur-y))
                     while (and cur-f (< cur-f node-f))
                     finally (progn
                               (setf (cdr new) cur)
                               (if prev
                                   (setf (cdr prev) new)
                                   (setf available new)))))
             (came-from (time x y)
               (with-slots (came-from) (aref moments time)
                 (or (aref came-from y x) (error "Lost at ~a min ~a,~a" time x y))))
             (set-came-from (time x y from-x from-y)
               (with-slots (came-from) (aref moments time)
                 (setf (aref came-from y x) (cons from-x from-y))))
             (find-path (time)
               (loop with path = NIL
                     for current = end then (came-from minute x y)
                     for (x . y) = current
                     for minute = time then (1- minute)
                     do (push current path)
                     while (< 0 minute)
                     finally (return path))))
      (add-moment) ;; Add beginning.
      (set-g-score 0 start-x start-y 0)
      (set-f-score 0 start-x start-y (day24-distance start-x start-y end-x end-y)) ;; g-score + heuristic
      (loop for (cur-time . (cur-x . cur-y)) = (pop available) ;; The lowest is always first
            until (or (null cur-time) (and (= cur-x end-x) (= cur-y end-y)))
            for cur-g = (g-score cur-time cur-x cur-y)
            for tentative-g = (when cur-g (1+ cur-g))
            for next-time = (1+ cur-time)
            for next-moment = (if (< next-time (length moments)) (aref moments next-time) (add-moment))
            for next-map = (field next-moment)
            for neighbours = (day24-neighbours next-map cur-x cur-y width height)
            ;; do (format T "current: ~a, next: ~{~a~^, ~}~%" (cons cur-time (cons cur-x cur-y)) available)
            ;; do (format T "~t~tneighbours: ~{~a~^, ~}~%" neighbours)
            ;; do (dotimes (y height)
            ;;      (dotimes (x width)
            ;;        (format T "~c"
            ;;                (cond
            ;;                  ((and (= x cur-x) (= y cur-y)) #\@)
            ;;                  ((and (= x start-x) (= y start-y)) #\S)
            ;;                  ((and (= x end-x) (= y end-y)) #\E)
            ;;                  ((aref next-map y x) #\.)
            ;;                  (T #\#))))
            ;;      (format T "~%"))
            ;; do (format T "~%")
            do (loop for (neighbour-x . neighbour-y) in neighbours
                     do (when (< tentative-g (g-score next-time neighbour-x neighbour-y))
                          (set-came-from next-time neighbour-x neighbour-y cur-x cur-y)
                          (set-g-score next-time neighbour-x neighbour-y tentative-g)
                          (set-f-score next-time neighbour-x neighbour-y
                                       (+ tentative-g (day24-distance neighbour-x neighbour-y end-x end-y)))
                          (add-available next-time neighbour-x neighbour-y)))
            finally (return (when cur-time (find-path cur-time)))))))

(defun day24-puzzle1 ()
  (multiple-value-bind (blizzards count width height)
      (day24-parse-input)
    (unless (< 0 count) (error "No blizzards found."))
    (day24-astar (cons 1 0) (cons (- width 2) (1- height)) width height blizzards)))

;; 253
