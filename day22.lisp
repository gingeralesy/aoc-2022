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

;; FIXME: I hurt my brain trying to automate the below values.

;; 0 0 1 0
;; 2 3 4 0
;; 0 0 5 6
;; 0 0 0 0

(defparameter *day22-sample-connections*
  '((1 ((0 :to 6 :new 2 :mirror T)
        (1 :to 4 :new 1 :mirror NIL)
        (2 :to 3 :new 1 :mirror NIL)
        (3 :to 2 :new 2 :mirror T)))
    (2 ((0 :to 3 :new 0 :mirror NIL)
        (1 :to 5 :new 3 :mirror T)
        (2 :to 6 :new 3 :mirror T)
        (3 :to 1 :new 1 :mirror T)))
    (3 ((0 :to 4 :new 0 :mirror NIL)
        (1 :to 5 :new 0 :mirror T)
        (2 :to 2 :new 2 :mirror NIL)
        (3 :to 1 :new 0 :mirror NIL)))
    (4 ((0 :to 6 :new 1 :mirror T)
        (1 :to 5 :new 1 :mirror NIL)
        (2 :to 3 :new 2 :mirror NIL)
        (3 :to 1 :new 3 :mirror NIL)))
    (5 ((0 :to 6 :new 0 :mirror NIL)
        (1 :to 2 :new 3 :mirror T)
        (2 :to 3 :new 3 :mirror T)
        (3 :to 4 :new 3 :mirror NIL)))
    (6 ((0 :to 1 :new 2 :mirror T)
        (1 :to 2 :new 2 :mirror NIL)
        (2 :to 5 :new 2 :mirror NIL)
        (3 :to 4 :new 2 :mirror T)))))

;; 0 1 2 0
;; 0 3 0 0
;; 4 5 0 0
;; 6 0 0 0

(defparameter *day22-input-connections*
  '((1 ((0 :to 2 :new 0 :mirror NIL)
        (1 :to 3 :new 1 :mirror NIL)
        (2 :to 4 :new 0 :mirror T)
        (3 :to 6 :new 0 :mirror NIL)))
    (2 ((0 :to 5 :new 2 :mirror T)
        (1 :to 3 :new 2 :mirror NIL)
        (2 :to 1 :new 2 :mirror NIL)
        (3 :to 6 :new 3 :mirror NIL)))
    (3 ((0 :to 2 :new 3 :mirror NIL)
        (1 :to 5 :new 1 :mirror NIL)
        (2 :to 4 :new 1 :mirror NIL)
        (3 :to 1 :new 3 :mirror NIL)))
    (4 ((0 :to 5 :new 0 :mirror NIL)
        (1 :to 6 :new 1 :mirror NIL)
        (2 :to 1 :new 0 :mirror T)
        (3 :to 3 :new 0 :mirror NIL)))
    (5 ((0 :to 2 :new 2 :mirror T)
        (1 :to 6 :new 2 :mirror NIL)
        (2 :to 4 :new 2 :mirror NIL)
        (3 :to 3 :new 3 :mirror NIL)))
    (6 ((0 :to 5 :new 3 :mirror NIL)
        (1 :to 2 :new 1 :mirror NIL)
        (2 :to 1 :new 1 :mirror NIL)
        (3 :to 4 :new 3 :mirror NIL)))))

(defun day22-analyze-cube (map width height)
  (let* ((sample-p (< width 100))
         (connections (if sample-p *day22-sample-connections* *day22-input-connections*))
         (side (/ (max width height) 4))
         (cube-width (/ width side))
         (cube-height (/ height side))
         (cube (make-array '(4 4) :element-type '(integer 0 6) :initial-element 0))
         (sides)
         (count 0))
    (dotimes (y cube-height)
      (dotimes (x cube-width)
        (when (aref map (* y side) (* x side))
          (let ((count (incf count))
                (info (list count :position (cons x y)
                                  :connections (cadr (assoc count connections)))))
            (if sides (setf (cdr (last sides)) (list info)) (push info sides))
            (setf (aref cube y x) count)))))
    (values cube side sides)))

(defun day22-next-cube-tile (facing position cube sides map width height side)
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
         (:open (values pos facing))
         (:wall (values (cons (car position) (cdr position)) facing))))
      (T ;; TODO: Clean this up. It's horrible.
       (let* ((cube-pos (cons (floor (car position) side) (floor (cdr position) side)))
              (cube-face (aref cube (cdr cube-pos) (car cube-pos)))
              (offset (ecase facing
                        ((0 2) (- (cdr position) (* (cdr cube-pos) side)))
                        ((1 3) (- (car position) (* (car cube-pos) side)))))
              (cube-info (cdr (assoc cube-face sides)))
              (connection (cdr (assoc facing (getf cube-info :connections))))
              (new-cube-pos (getf (cdr (assoc (getf connection :to) sides)) :position))
              (new-offset (if (getf connection :mirror) (- side offset 1) offset))
              (new-pos (case (getf connection :new)
                         (0 (cons 0 new-offset))
                         (1 (cons new-offset 0))
                         (2 (cons (1- side) new-offset))
                         (3 (cons new-offset (1- side))))))
         (incf (car new-pos) (* (car new-cube-pos) side))
         (incf (cdr new-pos) (* (cdr new-cube-pos) side))
         (ecase (aref map (cdr new-pos) (car new-pos))
           (:open (values new-pos (getf connection :new)))
           (:wall (values (cons (car position) (cdr position)) facing))))))))

(defun day22-puzzle2 ()
  (multiple-value-bind (map path width height start)
      (day22-parse-input)
    (multiple-value-bind (cube side sides)
        (day22-analyze-cube map width height)
      (let ((position (cons (car start) (cdr start)))
            (facing 0)
            (path-taken (list )))
        (loop for step in path
              do (cond
                   ((numberp step)
                    (loop for i from 0 below step
                          for (next new-facing) = (multiple-value-list
                                                   (day22-next-cube-tile
                                                    facing position cube sides
                                                    map width height side))
                          until (and (= (car position) (car next)) (= (cdr position) (cdr next)))
                          do (setf position next)
                          do (setf facing new-facing)
                          do (push next path-taken)))
                   ((keywordp step) (setf facing (day22-turn step facing)))
                   (T (error "Invalid step input: ~a" step))))
        (+ (* 1000 (1+ (cdr position))) (* 4 (1+ (car position))) facing)))))

;; 86382
