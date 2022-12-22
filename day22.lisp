#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day22-map-re* (cl-ppcre:create-scanner "^[ .#]+"))
(defparameter *day22-path-re* (cl-ppcre:create-scanner "([0-9]+|[RL])"))

(defun day22-parse-input ()
  (let ((map (make-array '(200 150) :element-type '(or null keyword) :initial-element NIL)))
    (with-open-file (stream (input 22) :if-does-not-exist :error)
      (loop with width = 0
            with height = 0
            with path = NIL
            with start = (cons NIL NIL)
            for line = (clean (read-line stream NIL))
            while line
            for row from 0
            for map-row = (cl-ppcre:scan-to-strings *day22-map-re* line)
            for steps = (unless map-row (cl-ppcre:all-matches-as-strings *day22-path-re* line))
            do (cond
                 (map-row
                  (loop for col from 0
                        for ch across map-row
                        do (ecase ch
                             (#\  (setf (aref map row col) NIL))
                             (#\.
                              (setf (aref map row col) :open)
                              (unless (car start)
                                (setf (car start) col)
                                (setf (cdr start) row)))
                             (#\# (setf (aref map row col) :wall)))
                        finally (when (< width col) (setf width col))))
                 (steps
                  (loop for step in steps
                        collect (if (digit-char-p (char step 0))
                                    (parse-integer step)
                                    (ecase (char step 0) (#\L :ccw) (#\R :cw)))
                        into pathway
                        finally (setf path pathway)))
                 (T (setf height row)))
            until path
            finally (return (values map path width height start))))))

(defun day22-turn (direction current)
  (let ((new-facing (ecase direction (:cw (1+ current)) (:ccw (1- current)))))
    (mod (+ 4 new-facing) 4)))

(defun day22-next-board-tile (facing position map width height)
  (let ((pos (cons (car position) (cdr position))))
    (ecase facing
      (0 (incf (car pos)))
      (1 (incf (cdr pos)))
      (2 (decf (car pos)))
      (3 (decf (cdr pos))))
    (cond
      ((and (<= 0 (car pos)) (< (car pos) width)
            (<= 0 (cdr pos)) (< (cdr pos) height)
            (aref map (cdr pos) (car pos)))
       (ecase (aref map (cdr pos) (car pos))
         (:open pos)
         (:wall (cons (car position) (cdr position)))))
      (T (loop for cur = position then next
               for next = (ecase facing
                            (0 (cons (1- (car cur)) (cdr cur)))
                            (1 (cons (car cur) (1- (cdr cur))))
                            (2 (cons (1+ (car cur)) (cdr cur)))
                            (3 (cons (car cur) (1+ (cdr cur)))))
               while (and (<= 0 (car next)) (< (car next) width)
                          (<= 0 (cdr next)) (< (cdr next) height)
                          (aref map (cdr next) (car next)))
               finally (return
                         (if (eql :wall (aref map (cdr cur) (car cur)))
                             position
                             cur)))))))

(defun day22-puzzle1 ()
  (multiple-value-bind (map path width height start)
      (day22-parse-input)
    (let ((position (cons (car start) (cdr start)))
          (facing 0))
      (loop for step in path
            do (cond
                 ((numberp step)
                  (loop for i from 0 below step
                        for next = (day22-next-board-tile facing position map width height)
                        until (and (= (car position) (car next)) (= (cdr position) (cdr next)))
                        do (setf position next)))
                 ((keywordp step) (setf facing (day22-turn step facing)))
                 (T (error "Invalid step input: ~a" step))))
      (+ (* 1000 (1+ (cdr position))) (* 4 (1+ (car position))) facing))))

;; 3590
