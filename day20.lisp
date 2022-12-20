#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day20-parse-input ()
  (with-open-file (stream (input 20) :if-does-not-exist :error)
    (loop for line = (clean (read-line stream NIL))
          while line
          collect (parse-integer line) into numbers
          finally (return (make-array (length numbers)
                                      :element-type 'integer
                                      :initial-contents numbers)))))

(defun day20-index (index length)
  (cond
    ((< index 0)
     (loop for ix = index then (+ ix length)
           while (< ix 0)
           finally (return (mod ix length))))
    ((<= length index) (mod index length))
    (T index)))

(defun day20-mix (&optional (key 1) (rounds 1))
  (let* ((numbers (day20-parse-input))
         (length (array-dimension numbers 0))
         (indices (make-array length :element-type 'integer
                                     :initial-element 0)))
    (dotimes (i length)
      (setf (aref numbers i) (* key (aref numbers i))))
    (dotimes (i length)
      (setf (aref indices i) i))
    (labels ((swap (from to)
               (let* ((from (day20-index from length))
                      (to (day20-index to length))
                      (tmp (aref numbers from))
                      (tmp-pos (aref indices from)))
                 (setf (aref numbers from) (aref numbers to))
                 (setf (aref indices from) (aref indices to))
                 (setf (aref numbers to) tmp)
                 (setf (aref indices to) tmp-pos)))
             (move (from amount)
               ;; NOTE: (1- length) moves returns to original state
               (dotimes (i (mod (abs amount) (1- length)))
                 (let* ((ix1 (if (< 0 amount) (+ from i) (- from i)))
                        (ix2 (if (< 0 amount) (1+ ix1) (1- ix1))))
                   (swap ix1 ix2)))))
      (dotimes (round rounds)
        (dotimes (i length)
          (let* ((position (position i indices))
                 (value (aref numbers position)))
            (move position value))))
      (let* ((zero-ix (position 0 numbers))
             (1k-ix (day20-index (+ zero-ix 1000) length))
             (2k-ix (day20-index (+ zero-ix 2000) length))
             (3k-ix (day20-index (+ zero-ix 3000) length))
             (1k (aref numbers 1k-ix))
             (2k (aref numbers 2k-ix))
             (3k (aref numbers 3k-ix)))
        (list 1k 2k 3k)))))

(defun day20-puzzle1 ()
  (destructuring-bind (x y z)
      (day20-mix)
    (format T "~d,~d,~d~%" x y z)
    (+ x y z)))

;; 7228

(defun day20-puzzle2 ()
  (destructuring-bind (x y z)
      (day20-mix 811589153 10)
    (format T "~d,~d,~d~%" x y z)
    (+ x y z)))

;; 4526232706281
