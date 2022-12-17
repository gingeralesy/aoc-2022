#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day17-rocks*
  (list (make-array '(1 4) :element-type 'boolean :initial-contents '((T T T T)))
        (make-array '(3 3) :element-type 'boolean :initial-contents '((NIL T NIL)
                                                                      ( T  T  T )
                                                                      (NIL T NIL)))
        (make-array '(3 3) :element-type 'boolean :initial-contents '((NIL NIL  T )
                                                                      (NIL NIL  T )
                                                                      ( T   T   T )))
        (make-array '(4 1) :element-type 'boolean :initial-contents '((T) (T) (T) (T)))
        (make-array '(2 2) :element-type 'boolean :initial-contents '((T T) (T T)))))

(defun day17-print-rock (rock)
  (dotimes (row (array-dimension rock 0))
    (dotimes (col (array-dimension rock 1))
      (format T "~c" (if (aref rock row col) #\# #\.)))
    (format T "~%")))

(defun day17-print-field (field height &optional rock rock-x rock-y)
  (loop with rock-height = (if rock (array-dimension rock 0) 0)
        with rock-width = (if rock (array-dimension rock 1) 0)
        for row from (- (array-dimension field 0) (1+ height)) below (array-dimension field 0)
        for rock-row = (and rock (- row rock-y))
        do (format T "|")
        do (dotimes (col (array-dimension field 1))
             (let ((rock-col (and rock (- col rock-x))))
               (format T "~c"
                       (cond
                         ((and rock
                               (<= 0 rock-row) (< rock-row rock-height)
                               (<= 0 rock-col) (< rock-col rock-width)
                               (aref rock rock-row rock-col))
                          #\@)
                         ((aref field row col) #\#)
                         (T #\.)))))
        do (format T "|~%"))
  (format T "+")
  (dotimes (i (array-dimension field 1))
    (format T "-"))
  (format T "+~%"))

(defun day17-parse-input ()
  (with-open-file (stream (input 17) :if-does-not-exist :error)
    (loop for dir = (case (read-char stream NIL :eof)
                      (#\< -1)
                      (#\> +1)
                      (:eof (return-from day17-parse-input dirs)))
          when dir collect dir into dirs)))


(defun day17-copy-field (from to)
  (let* ((width (array-dimension from 1))
         (old-height (array-dimension from 0))
         (new-height (array-dimension to 0))
         (offset (- new-height old-height)))
    (unless (= width (array-dimension to 1))
      (error "Different widths!"))
    (unless (<= old-height new-height)
      (error "Cannot shrink heights!"))
    (dotimes (row old-height)
      (dotimes (col (array-dimension to 1))
        (setf (aref to (+ row offset) col) (aref from row col))))))

(defstruct day17-history
  (index 0 :type integer)
  (length 0 :type integer)
  (filled NIL :type boolean)
  (array NIL :type (or null array)))

(defun day17-make-history (length)
  (let ((array (make-array length :element-type '(integer 0 64) :initial-element 0)))
    (make-day17-history :length length :array array :index (1- length))))

(defun day17-push-history (value history)
  (let ((array (day17-history-array history))
        (index (mod (1+ (day17-history-index history)) (day17-history-length history))))
    (when (and (null (day17-history-filled history))
               (= index (1- (day17-history-length history))))
      (setf (day17-history-filled history) T))
    (setf (aref array index) value)
    (setf (day17-history-index history) index)))

(defun day17-repeats-p (history)
  (when (day17-history-filled history)
    (let* ((array (day17-history-array history))
           (length (day17-history-length history))
           (third (/ length 3)))
      (loop for pattern-length from 3 below third
            for pattern = NIL
            for found = T
            do (loop for i from 0 below pattern-length
                     for index = (- (day17-history-index history) i)
                     when (< index 0) do (incf index length)
                     do (push (aref array index) pattern))
            do (loop while found
                     for value in pattern
                     for i from (1+ (- (day17-history-index history) (* 2 pattern-length)))
                     for j from (- i pattern-length)
                     for index-i = (if (< i 0) (+ i length) i)
                     for index-j = (if (< j 0) (+ j length) j)
                     unless (and (= value (aref array index-i))
                                 (= value (aref array index-j)))
                     do (setf found NIL))
            when found do (return-from day17-repeats-p
                            (values pattern pattern-length
                                    (loop for value in pattern summing value)))))))

(defun day17-puzzle1 (&optional (count 2022))
  (let* ((field-width 7)
         (field-height 1024)
         (field (make-array (list field-height field-width)
                            :element-type 'boolean :initial-element NIL))
         (height 0)
         (all-moves (day17-parse-input))
         (next-move all-moves)
         (next-rock *day17-rocks*)
         (max-heights (make-array 7 :element-type '(integer 0 *) :initial-element 0))
         (cropped 0)
         (history (day17-make-history (* 3
                                         (if (= 0 (mod (length all-moves)
                                                       (length *day17-rocks*)))
                                             (length all-moves)
                                             (* (length *day17-rocks*)
                                                (length all-moves))))))
         (repeat-found NIL))
    (flet ((move (&optional peek)
             (let ((move (car next-move)))
               (unless peek (setf next-move (or (cdr next-move) all-moves)))
               move))
           (rock ()
             (let ((rock (car next-rock)))
               (setf next-rock (or (cdr next-rock) *day17-rocks*))
               rock))
           (collides-p (rock x y)
             (let ((rock-height (array-dimension rock 0))
                   (rock-width (array-dimension rock 1)))
               (when (or (< x 0) (< field-width (+ x rock-width))
                         (< field-height (+ y rock-height)))
                 (return-from collides-p T))
               (when (< y 0) (error "Field overflow!"))
               (dotimes (row rock-height)
                 (dotimes (col rock-width)
                   (when (and (aref rock row col) (aref field (+ y row) (+ x col)))
                     (return-from collides-p T))))))
           (land (rock x y)
             (dotimes (row (array-dimension rock 0))
               (dotimes (col (array-dimension rock 1))
                 (when (aref rock row col)
                   (let ((field-x (+ x col))
                         (field-y (+ y row)))
                     (when (aref field field-y field-x)
                       (error "Overlap! ~a,~a" x y))
                     (setf (aref field field-y field-x) T)
                     (let ((col-height (- field-height field-y))
                           (max-height (aref max-heights field-x)))
                       (when (< max-height col-height)
                         (setf (aref max-heights field-x) col-height))))))))
           (crop (amount)
             (loop for y from (1- field-height) downto 0
                   do (dotimes (x field-width)
                        (setf (aref field y x) (when (<= amount y)
                                                 (aref field (- y amount) x)))))
             (dotimes (i field-width)
               (decf (aref max-heights i) amount))
             (incf cropped amount)
             (decf height amount)))
      (dotimes (i count)
        (let* ((rock (rock))
               (y (- field-height (+ height 3 (array-dimension rock 0))))
               (x 2))
          ;; (day17-print-rock rock)
          (loop for move = (move)
                ;; do (day17-print-field field 6 rock x y)
                unless (collides-p rock (+ x move) y) do (incf x move)
                ;; do (day17-print-field field 6 rock x y)
                do (incf y)
                until (collides-p rock x y)
                finally (progn
                          (land rock x (1- y))
                          (let ((prev-height height))
                            (setf height (max height (- field-height (1- y))))
                            (day17-push-history (- height prev-height) history)
                            (unless repeat-found
                              (multiple-value-bind (pattern pattern-length delta)
                                  (day17-repeats-p history)
                                (when pattern
                                  (setf repeat-found T)
                                  (multiple-value-bind (repeats leftover)
                                      (floor (- count (+ i 1)) pattern-length)
                                    (let ((final (+ height cropped (* delta repeats))))
                                      (when leftover
                                        (loop repeat leftover
                                              for value in pattern
                                              do (incf final value)))
                                      (return-from day17-puzzle1 final)))))))
                          (when (< (- field-height height) 16)
                            (cond
                              ((find 0 max-heights)
                               (let* ((new-field-height (* 2 field-height))
                                      (new-field (make-array (list new-field-height field-width)
                                                             :element-type 'boolean
                                                             :initial-element NIL)))
                                 (day17-copy-field field new-field)
                                 (setf field new-field)
                                 (setf field-height new-field-height)))
                              (T
                               (let ((to-crop (1- (loop for h across max-heights minimizing h))))
                                 (crop to-crop))))))))))
    (+ height cropped)))

;; 3224

(defun day17-puzzle2 ()
  (day17-puzzle1 1000000000000))

;; 1595988538691
