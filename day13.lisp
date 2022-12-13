#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day13-parse-list (line)
  (let (root stack current integer)
    (flet ((push-integer ()
             (let ((string (format NIL "~{~a~}" (nreverse integer))))
               (push (parse-integer string) current)
               (setf integer NIL)))
           (push-stack ()
             (push current stack)
             (setf current NIL))
           (pop-stack ()
             (unless stack (error "Missing '['."))
             (setf current (nreverse current))
             (setf root current) ;; The last one will end up as root.
             (let ((parent (pop stack)))
               (push current parent)
               (setf current parent))))
      (loop for ch across line
            do (ecase ch
                 (#\[ (push-stack))
                 (#\]
                  (when integer (push-integer))
                  (pop-stack))
                 (#\, (when integer (push-integer)))
                 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (push ch integer)))))
    (if stack (error "Missing ']'.") root)))

(defun day13-parse-lists ()
  (with-open-file (stream (input 13) :if-does-not-exist :error)
    (loop for first = (clean (read-line stream NIL))
          for second = (clean (read-line stream NIL))
          for third = (clean (read-line stream NIL))
          while first
          unless second do (error "Missing second half of the pair!")
          unless (or (null third) (= 0 (length third))) do (error "Third line was not empty!")
          collect (cons (day13-parse-list first) (day13-parse-list second)))))

(defun day13-compare (left right)
  (declare (type list left right))
  (labels ((compare (left right)
             (cond
               ((and (null left) (null right)) NIL)
               ((null left) :left)
               ((null right) :right)
               ((and (integerp left) (integerp right))
                (when (/= left right) (if (< left right) :left :right)))
               ((integerp left) (compare (list left) right))
               ((integerp right) (compare left (list right)))
               ((and (listp left) (listp right))
                (loop for left-value in left
                      for right-value in right
                      for result = (compare left-value right-value)
                      when result do (return-from compare result)
                      finally (return
                                (let ((left-len (length left))
                                      (right-len (length right)))
                                  (when (/= left-len right-len)
                                    (if (< left-len right-len) :left :right)))))))))
    (not (eql (compare left right) :right))))

(defun day13-puzzle1 ()
  (loop for (left . right) in (day13-parse-lists)
        for i from 1
        when (day13-compare left right) sum i))

;; 6484
