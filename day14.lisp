#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day14-path-re* (cl-ppcre:create-scanner "\\b(\\d+),(\\d+)\\b"))

(defun day14-build-map (coordinates)
  (let ((min (cons 999 0))
        (max (cons 0 0)))
    ;; FIXME: This is a bit silly but works.
    (loop for path in coordinates
          do (loop for (x . y) in path
                   when (< x (car min)) do (setf (car min) x)
                   when (< (car max) x) do (setf (car max) x)
                   when (< y (cdr min)) do (setf (cdr min) y)
                   when (< (cdr max) y) do (setf (cdr max) y)))
    (let* ((width (1+ (- (car max) (car min))))
           (height (1+ (- (cdr max) (cdr min))))
           (map (make-array (list height width) :element-type 'boolean :initial-element NIL)))
      (loop for path in coordinates
            do (loop for cur = path then (cdr cur)
                     for start = (car cur)
                     for end = (cadr cur)
                     while end
                     for horizontal-p = (/= (car start) (car end))
                     for from = (if horizontal-p (car start) (cdr start))
                     for to = (if horizontal-p (car end) (cdr end))
                     when (< to from) do (let ((tmp to))
                                           (setf to from)
                                           (setf from tmp))
                     do (loop for x = (if horizontal-p (- from (car min)) (- (car start) (car min)))
                              then (if horizontal-p (1+ x) x)
                              for y = (if horizontal-p (- (cdr start) (cdr min)) (- from (cdr min)))
                              then (if horizontal-p y (1+ y))
                              while (if horizontal-p (<= x (- to (car min))) (<= y (- to (cdr min))))
                              do (setf (aref map y x) T))))
      (values map width height (car min) (cdr min)))))

(defun day14-parse-map ()
  (with-open-file (stream (input 14) :if-does-not-exist :error)
    (loop for line = (clean (read-line stream NIL))
          while line
          collect (loop for coord in (cl-ppcre:all-matches-as-strings *day14-path-re* line)
                        for (match groups) = (multiple-value-list
                                              (cl-ppcre:scan-to-strings *day14-path-re* coord))
                        when match collect (cons (parse-integer (aref groups 0))
                                                 (parse-integer (aref groups 1))))
          into coords
          finally (return (day14-build-map coords)))))

(defun day14-puzzle1 ()
  (multiple-value-bind (map width height offset-x)
      (day14-parse-map)
    (flet ((free-p (x y)
             (or (< x 0) (<= width x) (< y 0) (<= height y) (not (aref map y x)))))
      (loop for x = (- 500 offset-x)
            for y = 0
            for i from 0
            do (loop do (cond
                          ((free-p x (1+ y))
                           (incf y))
                          ((free-p (1- x) (1+ y))
                           (incf y)
                           (decf x))
                          ((free-p (1+ x) (1+ y))
                           (incf y)
                           (incf x))
                          (T (setf (aref map y x) T)))
                     unless (and (< y height) (< x width) (<= 0 x))
                     do (return-from day14-puzzle1 (values i map))
                     until (aref map y x))))))

;; 696
