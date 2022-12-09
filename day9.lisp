#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day9-puzzle1 ()
  (with-open-file (stream (input 9) :if-does-not-exist :error)
    (flet ((distance (from to)
             (max (abs (- (car from) (car to)))
                  (abs (- (cdr from) (cdr to))))))
      (loop with route = (list (cons 0 0))
            with head = (cons 0 0)
            with tail = (cons 0 0)
            for line = (clean (read-line stream NIL))
            while line
            for direction = (char line 0)
            for travel = (parse-integer (subseq line 2))
            do (loop repeat travel
                     for prev = (cons (car head) (cdr head))
                     do (ecase direction
                          (#\L (decf (car head)))
                          (#\R (incf (car head)))
                          (#\U (decf (cdr head)))
                          (#\D (incf (cdr head))))
                     do (when (< 1 (distance head tail))
                          (setf (car tail) (car prev))
                          (setf (cdr tail) (cdr prev))
                          (unless (find prev route :test #'(lambda (a b)
                                                             (and (= (car a) (car b))
                                                                  (= (cdr a) (cdr b)))))
                            (push prev route))))
            finally (return (length route))))))

;; 5683

(defun day9-puzzle2 ()
  (with-open-file (stream (input 9) :if-does-not-exist :error)
    (labels ((distance (from to)
               (max (abs (- (car from) (car to)))
                    (abs (- (cdr from) (cdr to)))))
             (move-head (x y rope)
               (let* ((head (car rope))
                      (next (cadr rope)))
                 (setf (car head) x)
                 (setf (cdr head) y)
                 (when (and next (< 1 (distance head next)))
                   (let* ((xd (min 1 (max -1 (- x (car next)))))
                          (yd (min 1 (max -1 (- y (cdr next))))))
                     (move-head (+ (car next) xd) (+ (cdr next) yd) (rest rope)))))))
      (loop with route = (list (cons 0 0))
            with rope = (loop repeat 10 collect (cons 0 0))
            with head = (car rope)
            with tail = (car (last rope))
            for line = (clean (read-line stream NIL))
            while line
            for direction = (char line 0)
            for travel = (parse-integer (subseq line 2))
            do (loop repeat travel
                     for prev = (loop for knot in rope collect (cons (car knot) (cdr knot)))
                     do (ecase direction
                          (#\L (move-head (1- (car head)) (cdr head) rope))
                          (#\R (move-head (1+ (car head)) (cdr head) rope))
                          (#\U (move-head (car head) (1+ (cdr head)) rope))
                          (#\D (move-head (car head) (1- (cdr head)) rope)))
                     do (unless (find tail route :test #'(lambda (a b)
                                                           (and (= (car a) (car b))
                                                                (= (cdr a) (cdr b)))))
                          (push (cons (car tail) (cdr tail)) route)))
            finally (return (length route))))))

;; 2372
