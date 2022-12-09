#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day8-char->int (ch)
  (ecase ch (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)))

(defun day8-tree-grid ()
  (let ((data)
        (width)
        (height))
    (with-open-file (stream (input 8) :if-does-not-exist :error)
      (loop for line = (clean (read-line stream NIL))
            while line
            collect line into lines
            count line into rows
            unless width do (setf width (length line))
            finally (progn
                      (setf height rows)
                      (setf data lines))))
    (loop with grid = (make-array (list height width)
                                  :element-type '(integer 0 9)
                                  :initial-element 0)
          for y from 0
          for line in data
          do (loop for x from 0
                   for ch across line
                   do (setf (aref grid y x) (day8-char->int ch)))
          finally (return grid))))

(defun day8-puzzle1 ()
  (let* ((tree-grid (day8-tree-grid))
         (dimensions (array-dimensions tree-grid))
         (height (first dimensions))
         (width (second dimensions))
         (grid (make-array dimensions :element-type 'boolean :initial-element NIL)))
    (loop for row from 0 below height ;; From sides.
          do (loop for col-left from 0 below width
                   for col-right from (1- width) downto 0
                   for prev-left = NIL then (cons cur-left prev-left)
                   for cur-left = (aref tree-grid row col-left)
                   for prev-right = NIL then (cons cur-right prev-right)
                   for cur-right = (aref tree-grid row col-right)
                   for left = (not (and prev-left (find cur-left prev-left :test #'<=)))
                   for right = (not (and prev-right (find cur-right prev-right :test #'<=)))
                   do (setf (aref grid row col-left) (or (aref grid row col-left) left))
                   do (setf (aref grid row col-right) (or (aref grid row col-right) right))))
    (loop for col from 0 below width ;; From top and bottom.
          do (loop for row-top from 0 below width
                   for row-bottom from (1- width) downto 0
                   for prev-top = NIL then (cons cur-top prev-top)
                   for cur-top = (aref tree-grid row-top col)
                   for prev-bottom = NIL then (cons cur-bottom prev-bottom)
                   for cur-bottom = (aref tree-grid row-bottom col)
                   for top = (not (and prev-top (find cur-top prev-top :test #'<=)))
                   for bottom = (not (and prev-bottom (find cur-bottom prev-bottom :test #'<=)))
                   do (setf (aref grid row-top col) (or (aref grid row-top col) top))
                   do (setf (aref grid row-bottom col) (or (aref grid row-bottom col) bottom))))
    (loop for row from 0 below height
          sum (loop with count = 0
                    for col from 0 below width
                    when (aref grid row col)
                    do (incf count)
                    finally (return count)))))

;; 1733

(defun day8-puzzle2 ()
  (let* ((tree-grid (day8-tree-grid))
         (dimensions (array-dimensions tree-grid))
         (height (first dimensions))
         (width (second dimensions)))
    (flet ((scenic-score (x y)
             (let ((score 1)
                   (base (aref tree-grid y x)))
               (loop with points = 0
                     for col from (1- x) downto 0 ;; Left
                     for tree = (aref tree-grid y col)
                     do (incf points)
                     until (<= base tree)
                     finally (setf score (* points score)))
               (loop with points = 0
                     for col from (1+ x) below width ;; Right
                     for tree = (aref tree-grid y col)
                     do (incf points)
                     until (<= base tree)
                     finally (setf score (* points score)))
               (loop with points = 0
                     for row from (1- y) downto 0 ;; Up
                     for tree = (aref tree-grid row x)
                     do (incf points)
                     until (<= base tree)
                     finally (setf score (* points score)))
               (loop with points = 0
                     for row from (1+ y) below height ;; Down
                     for tree = (aref tree-grid row x)
                     do (incf points)
                     until (<= base tree)
                     finally (setf score (* points score)))
               score)))
      (loop with max = 0
            for row from 0 below height
            do (loop for col from 0 below width
                     for score = (scenic-score col row)
                     when (< max score) do (setf max score))
            finally (return max)))))

;; 284648
