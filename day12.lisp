#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day12-parse-heightmap ()
  (with-open-file (stream (input 12) :if-does-not-exist :error)
    (let ((start)
          (end)
          (heights "abcdefghijklmnopqrstuvwxyz"))
      (loop for y from 0
            for line = (clean (read-line stream NIL))
            while line
            collect (loop for x from 0
                          for node across line
                          collect (case node
                                    (#\S
                                     (when start
                                       (error "Multiple starting points ~a,~a and ~a,~a"
                                              (car start) (cdr start) x y))
                                     (setf start (cons x y))
                                     0)
                                    (#\E
                                     (when end
                                       (error "Multiple ending points ~a,~a and ~a,~a"
                                              (car end) (cdr end) x y))
                                     (setf end (cons x y))
                                     27)
                                    (T
                                     (let ((pos (position node heights)))
                                       (unless pos (error "Invalid height: ~a" node))
                                       (1+ pos)))))
            into height-map
            finally (return
                      (let ((height (length height-map))
                            (width (length (first height-map))))
                        (values (make-array (list height width)
                                            :element-type '(integer 0 32)
                                            :initial-contents height-map)
                                start end width height)))))))

(defun day12-astar (map start end width height)
  (let ((g-score (make-array (array-dimensions map)
                             :element-type '(integer 0 *)
                             :initial-element #xFFFFFFFFFFFFFFFF))
        (f-score (make-array (array-dimensions map)
                             :element-type '(integer 0 *)
                             :initial-element #xFFFFFFFFFFFFFFFF))
        (came-from (make-array (array-dimensions map)
                               :initial-element NIL))
        (available (list start)))
    (labels ((heuristic (to from)
               (+ (abs (- (car to) (car from)))
                  (abs (- (cdr to) (cdr from)))))
             (height (node)
               (when (and node
                          (<= 0 (car node))
                          (<= 0 (cdr node))
                          (< (car node) width)
                          (< (cdr node) height))
                 (aref map (cdr node) (car node))))
             (g-score (node)
               (when (and node
                          (<= 0 (car node))
                          (<= 0 (cdr node))
                          (< (car node) width)
                          (< (cdr node) height))
                 (aref g-score (cdr node) (car node))))
             (f-score (node)
               (when (and node
                          (<= 0 (car node))
                          (<= 0 (cdr node))
                          (< (car node) width)
                          (< (cdr node) height))
                 (aref f-score (cdr node) (car node))))
             (remove-available (node)
               (loop for prev = NIL then cur
                     for cur = available then (cdr cur)
                     until (or (null cur)
                               (and (= (car node) (caar cur)) (= (cdr node) (cdar cur))))
                     finally (return (when cur
                                       (if prev
                                           (setf (cdr prev) (cdr cur))
                                           (setf available (cdr cur)))
                                       (car cur)))))
             (add-available (node)
               (remove-available node)
               (loop with node-f = (f-score node)
                     with new = (cons node NIL)
                     for prev = NIL then cur
                     for cur = available then (cdr cur)
                     for cur-f = (f-score (car cur))
                     while (and cur-f (< cur-f node-f))
                     finally (progn
                               (setf (cdr new) cur)
                               (if prev
                                   (setf (cdr prev) new)
                                   (setf available new)))))
             (neighbours (cur)
               (let ((cur-height (height cur)))
                 (when cur-height
                   (loop for (xd . yd) in '((0 . -1) (-1 . 0) (1 . 0) (0 . 1))
                         for node = (cons (+ xd (car cur)) (+ yd (cdr cur)))
                         for node-height = (height node)
                         when (and node-height (<= (1- node-height) cur-height))
                         collect node))))
             (find-path ()
               (loop for current = end then (aref came-from (cdr current) (car current))
                     collect current
                     until (or (null current) (and (= (car start) (car current))
                                                   (= (cdr start) (cdr current)))))))
      (setf (aref g-score (cdr start) (car start)) 0)
      (setf (aref f-score (cdr start) (car start)) (heuristic end start)) ;; g-score + heuristic
      (loop for current = (pop available) ;; The lowest is always first
            until (or (null current) (and (= (car current) (car end))
                                          (= (cdr current) (cdr end))))
            for tentative-g = (+ (g-score current) 1)
            do (loop for neighbour in (neighbours current)
                     do (when (< tentative-g (g-score neighbour))
                          (setf (aref came-from (cdr neighbour) (car neighbour)) current)
                          (setf (aref g-score (cdr neighbour) (car neighbour)) tentative-g)
                          (setf (aref f-score (cdr neighbour) (car neighbour))
                                (+ tentative-g (heuristic end neighbour)))
                          (add-available neighbour)))
            finally (return (when current (find-path)))))))

(defun day12-puzzle1 ()
  (multiple-value-bind (map start end width height)
      (day12-parse-heightmap)
    (1- (length (day12-astar map start end width height)))))

;; 394

(defun day12-puzzle2 ()
  (multiple-value-bind (map start end width height)
      (day12-parse-heightmap)
    (setf (aref map (cdr start) (car start)) 1)
    (let ((starts (loop for y from 0 below height
                        append (loop for x from 0 below width
                                     when (= 1 (aref map y x))
                                     collect (cons x y)))))
      (loop for start in starts
            for path = (day12-astar map start end width height)
            for steps = (when path (1- (length path)))
            when steps
            minimizing steps))))

;; 388
