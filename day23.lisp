#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defclass day23-elf ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (to-x :initform NIL :accessor to-x)
   (to-y :initform NIL :accessor to-y))
  (:default-initargs :x (error "X required")
                     :y (error "Y required")))

(defmethod initialize-instance :after ((elf day23-elf) &key)
  (day23-stop elf))

(defmethod day23-stop ((elf day23-elf))
  (setf (to-x elf) (x elf))
  (setf (to-y elf) (y elf)))

(defmethod day23-move ((elf day23-elf))
  (setf (x elf) (to-x elf))
  (setf (y elf) (to-y elf)))

(defclass day23-quadtree ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (elf :initform NIL :accessor elf)
   (top-left :initform NIL :accessor top-left)
   (top-right :initform NIL :accessor top-right)
   (bottom-left :initform NIL :accessor bottom-left)
   (bottom-right :initform NIL :accessor bottom-right))
  (:default-initargs :x (error "X required")
                     :y (error "Y required")
                     :width (error "WIDTH required")
                     :height (error "HEIGHT required")))

(defmethod initialize-instance :after ((tree day23-quadtree) &key elves)
  (unless (x tree) (error "X required"))
  (unless (y tree) (error "Y required"))
  (unless (width tree) (error "WIDTH required"))
  (unless (height tree) (error "HEIGHT required"))
  (loop for elf in elves do (day23-insert tree elf)))

(defmethod right ((tree day23-quadtree))
  (+ (x tree) (width tree)))

(defmethod bottom ((tree day23-quadtree))
  (+ (y tree) (height tree)))

(defmethod day23-within ((tree day23-quadtree) elf-x elf-y)
  (with-slots (x y width height) tree
    (and (<= x elf-x) (< elf-x (+ x width))
         (<= y elf-y) (< elf-y (+ y height)))))

(defmethod day23-clear ((tree day23-quadtree))
  (setf (elf tree) NIL)
  (with-slots (top-left top-right bottom-left bottom-right) tree
    (when top-left (day23-clear top-left))
    (when top-right (day23-clear top-right))
    (when bottom-left (day23-clear bottom-left))
    (when bottom-right (day23-clear bottom-right))))

(defmethod day23-shift ((tree day23-quadtree) delta-x delta-y)
  (let ((elf (elf tree)))
    (when elf
      (incf (x elf) delta-x)
      (incf (y elf) delta-y)
      (day23-stop elf)))
  (incf (x tree) delta-x)
  (incf (y tree) delta-y)
  (when (top-left tree) (day23-shift (top-left tree) delta-x delta-y))
  (when (top-right tree) (day23-shift (top-right tree) delta-x delta-y))
  (when (bottom-left tree) (day23-shift (bottom-left tree) delta-x delta-y))
  (when (bottom-right tree) (day23-shift (bottom-right tree) delta-x delta-y)))

(defmethod day23-expand ((tree day23-quadtree) direction)
  (with-slots (x y width height) tree
    (let* ((new-x (ecase direction
                    ((:top-left :bottom-left) (- x width))
                    ((:top-right :bottom-right) x)))
           (new-y (ecase direction
                    ((:top-left :top-right) (- y height))
                    ((:bottom-left :bottom-right) y)))
           (new-tree (make-instance 'day23-quadtree :x new-x :y new-y
                                                    :width (* 2 width)
                                                    :height (* 2 height))))
      (ecase direction
        (:top-left (setf (bottom-right new-tree) tree))
        (:top-right (setf (bottom-left new-tree) tree))
        (:bottom-left (setf (top-right new-tree) tree))
        (:bottom-right (setf (top-left new-tree) tree)))
      new-tree)))

(defmethod day23-insert ((tree day23-quadtree) (elf day23-elf))
  (let ((elf-x (x elf))
        (elf-y (y elf)))
    (unless (day23-within tree elf-x elf-y)
      (error "Elf out of bounds: ~a,~a" elf-x elf-y))
    (with-slots (x y width height) tree
      (when (and (<= width 1) (<= height 1))
        (when (elf tree) (error "Position occupied: ~a,~a" elf-x elf-y))
        (setf (elf tree) elf)
        (return-from day23-insert))
      (let* ((half-width (max (floor width 2) 1))
             (half-height (max (floor height 2) 1))
             (left-p (< elf-x (+ x half-width)))
             (top-p (< elf-y (+ y half-height))))
        (cond
          ((and left-p top-p)
           (unless (top-left tree)
             (setf (top-left tree) (make-instance 'day23-quadtree :x x :y y
                                                                  :width half-width
                                                                  :height half-height)))
           (day23-insert (top-left tree) elf))
          ((and left-p (not top-p))
           (unless (bottom-left tree)
             (setf (bottom-left tree) (make-instance 'day23-quadtree :x x :y (+ y half-height)
                                                                     :width half-width
                                                                     :height (- height half-height))))
           (day23-insert (bottom-left tree) elf))
          ((and (not left-p) top-p)
           (unless (top-right tree)
             (setf (top-right tree) (make-instance 'day23-quadtree :x (+ x half-width) :y y
                                                                   :width (- width half-width)
                                                                   :height half-height)))
           (day23-insert (top-right tree) elf))
          ((and (not left-p) (not top-p))
           (unless (bottom-right tree)
             (setf (bottom-right tree) (make-instance 'day23-quadtree
                                                      :x (+ x half-width)
                                                      :y (+ y half-height)
                                                      :width (- width half-width)
                                                      :height (- height half-height))))
           (day23-insert (bottom-right tree) elf)))))))

(defmethod day23-remove ((tree day23-quadtree) elf-x elf-y)
  (unless (day23-within tree elf-x elf-y)
    (error "Removing from out of bounds: ~a,~a" elf-x elf-y))
  (when (elf tree)
    (setf (elf tree) NIL)
    (return-from day23-remove))
  (with-slots (x y width height) tree
    (let* ((half-width (max (floor width 2) 1))
           (half-height (max (floor height 2) 1))
           (left-p (< elf-x (+ x half-width)))
           (top-p (< elf-y (+ y half-height))))
      (cond
        ((and left-p top-p)
         (when (top-left tree)
           (day23-remove (top-left tree) elf-x elf-y)))
        ((and left-p (not top-p))
         (unless (bottom-left tree)
           (day23-remove (bottom-left tree) elf-x elf-y)))
        ((and (not left-p) top-p)
         (unless (top-right tree)
           (day23-remove (top-right tree) elf-x elf-y)))
        ((and (not left-p) (not top-p))
         (unless (bottom-right tree)
           (day23-remove (bottom-right tree) elf-x elf-y)))))))

(defmethod day23-search ((tree day23-quadtree) elf-x elf-y)
  (unless (day23-within tree elf-x elf-y) (return-from day23-search))
  (when (elf tree) (return-from day23-search (elf tree)))
  (with-slots (x y width height) tree
    (let* ((half-width (max (floor width 2) 1))
           (half-height (max (floor height 2) 1))
           (left-p (< elf-x (+ x half-width)))
           (top-p (< elf-y (+ y half-height))))
      (cond
        ((and left-p top-p)
         (when (top-left tree)
           (day23-search (top-left tree) elf-x elf-y)))
        ((and left-p (not top-p))
         (when (bottom-left tree)
           (day23-search (bottom-left tree) elf-x elf-y)))
        ((and (not left-p) top-p)
         (when (top-right tree)
           (day23-search (top-right tree) elf-x elf-y)))
        ((and (not left-p) (not top-p))
         (when (bottom-right tree)
           (day23-search (bottom-right tree) elf-x elf-y)))))))

(defmethod day23-elves ((tree day23-quadtree))
  (with-slots (elf top-left top-right bottom-left bottom-right) tree
    (nconc (when elf (list elf))
           (when top-left (day23-elves top-left))
           (when top-right (day23-elves top-right))
           (when bottom-left (day23-elves bottom-left))
           (when bottom-right (day23-elves bottom-right)))))

(defun day23-parse-input ()
  (with-open-file (stream (input 23) :if-does-not-exist :error)
    (loop with count = 0
          with width = 0
          for height from 0
          for line = (clean (read-line stream NIL))
          while line
          append (loop for ch across line
                       for x from 0
                       for elf-p = (char= ch #\#)
                       when elf-p do (incf count)
                       when elf-p collect (make-instance 'day23-elf :x x :y height) into row
                       when (<= width x) do (setf width (1+ x))
                       finally (return row))
          into elves
          finally (return (values elves count width height)))))

(defun day23-settle (tree direction elf settled)
  ;; Unique value when expanded by a point no each direction.
  (let* ((elf-x (x elf))
         (elf-y (y elf))
         (x (case direction
              (:west (1- elf-x))
              (:east (1+ elf-x))
              (T elf-x)))
         (y (case direction
              (:north (1- elf-y))
              (:south (1+ elf-y))
              (T elf-y)))
         (value (+ (+ (abs (1- (x tree))) x)
                   (* (+ (abs (1- (y tree))) y) (+ 2 (width tree))))))
    (setf (to-x elf) x)
    (setf (to-y elf) y)
    (unless settled
      (return-from day23-settle (list (cons value elf))))
    (when (< value (caar settled))
      (return-from day23-settle (cons (cons value elf) settled)))
    (loop for current = settled then next
          for next = (cdr current)
          while current
          do (cond
               ((= value (caar current))
                (day23-stop (cdar current))
                (day23-stop elf)
                (return-from day23-settle settled))
               ((and (< (caar current) value)
                     (or (null next) (< value (caar next))))
                (let ((new (cons (cons value elf) next)))
                  (setf (cdr current) new)
                  (return-from day23-settle settled)))))))

(defun day23-step (elves tree directions)
  (loop with settling = NIL
        for elf in elves
        for elf-x = (x elf)
        for elf-y = (y elf)
        for direction = (loop with nw = (day23-search tree (1- elf-x) (1- elf-y))
                              with n = (day23-search tree elf-x (1- elf-y))
                              with ne = (day23-search tree (1+ elf-x) (1- elf-y))
                              with w = (day23-search tree (1- elf-x) elf-y)
                              with e = (day23-search tree (1+ elf-x) elf-y)
                              with sw = (day23-search tree (1- elf-x) (1+ elf-y))
                              with s = (day23-search tree elf-x (1+ elf-y))
                              with se = (day23-search tree (1+ elf-x) (1+ elf-y))
                              for dir in directions
                              for valid = (and
                                           (or nw n ne w e sw s se)
                                           (ecase dir
                                             (:north (null (or nw n ne)))
                                             (:south (null (or sw s se)))
                                             (:west (null (or nw w sw)))
                                             (:east (null (or ne e se)))))
                              until valid
                              finally (return (when valid dir)))
        ;; when direction do (format T "~a,~a to ~(~a~)~%" elf-x elf-y direction)
        when direction do (setf settling (day23-settle tree direction elf settling)))
  (loop for elf in elves
        for no-movement-p = (and (= (x elf) (to-x elf)) (= (y elf) (to-y elf)))
        while no-movement-p
        finally (when no-movement-p (return-from day23-step (values tree NIL))))
  (day23-clear tree)
  (loop for elf in elves
        for x = (to-x elf)
        for y = (to-y elf)
        do (day23-move elf)
        do (unless (day23-within tree x y)
             (setf tree (day23-expand tree (cond
                                             ((< x (x tree))
                                              (if (< y (floor (height tree) 2))
                                                  :top-left
                                                  :bottom-left))
                                             ((<= (right tree) x)
                                              (if (< y (floor (height tree) 2))
                                                  :top-right
                                                  :bottom-right))
                                             ((< y (y tree))
                                              (if (< y (floor (width tree) 2))
                                                  :top-left
                                                  :top-right))
                                             ((<= (bottom tree) y)
                                              (if (< y (floor (width tree) 2))
                                                  :bottom-left
                                                  :bottom-right))))))
        do (day23-insert tree elf))
  (values tree T))

(defun day23-rotate-directions (directions)
  (let ((current directions)
        (next (cdr directions)))
    (setf (cdr current) NIL)
    (setf (cdr (last next)) current)
    next))

(defun day23-puzzle1 (&optional (rounds 10))
  (multiple-value-bind (elves count width height)
      (day23-parse-input)
    (let ((tree (make-instance 'day23-quadtree :x 0 :y 0 :width width :height height
                                               :elves elves))
          (directions (list :north :south :west :east)))
      (dotimes (round rounds)
        ;; (format T "Round ~a: ~(~{~a~^, ~}~)~%" (1+ round) directions)
        (multiple-value-bind (new-tree keep-going-p)
            (day23-step elves tree directions)
          (unless keep-going-p (return))
          (setf tree new-tree))
        (setf directions (day23-rotate-directions directions)))
      (loop for elf in elves
            minimizing (x elf) into left
            maximizing (1+ (x elf)) into right
            minimizing (y elf) into top
            maximizing (1+ (y elf)) into bottom
            finally (return (- (* (- right left) (- bottom top)) count))))))

;; 3877

(defun day23-puzzle2 ()
  (multiple-value-bind (elves count width height)
      (day23-parse-input)
    (declare (ignore count))
    (let ((tree (make-instance 'day23-quadtree :x 0 :y 0 :width width :height height
                                               :elves elves))
          (directions (list :north :south :west :east)))
      (loop for round from 1
            ;; do (format T "Round ~a: ~(~{~a~^, ~}~)~%" (1+ round) directions)
            do (multiple-value-bind (new-tree keep-going-p)
                   (day23-step elves tree directions)
                 (unless keep-going-p (return-from day23-puzzle2 round))
                 (setf tree new-tree))
            do (setf directions (day23-rotate-directions directions))))))

;; 982
