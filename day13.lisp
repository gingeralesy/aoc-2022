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
  (let ((values (cons left right))
        (stacks (cons NIL NIL)))
    (flet ((left () (car values))
           (right () (cdr values))
           (listify-left ()
             (unless (listp (car values))
               (setf (car values) (list (car values)))))
           (listify-right ()
             (unless (listp (cdr values))
               (setf (cdr values) (list (cdr values)))))
           (push-left ()
             (push (car values) (car stacks))
             (setf (car values) (caar values)))
           (push-right ()
             (push (cdr values) (cdr stacks))
             (setf (cdr values) (cadr values)))
           (pop-left ()
             (loop do (setf (car values) (cdr (pop (car stacks))))
                   while (and (null (car values)) (car stacks))))
           (pop-right ()
             (loop do (setf (cdr values) (cdr (pop (cdr stacks))))
                   while (and (null (cdr values)) (cdr stacks)))))
      (loop do (cond
                 ((null (left))
                  (pop-left)
                  (unless (left) (return-from day13-compare T)))
                 ((null (right))
                  (pop-right)
                  (unless (right) (return-from day13-compare NIL)))
                 ((and (integerp (left)) (integerp (right)))
                  (when (/= (left) (right))
                    (return-from day13-compare (< (left) (right))))
                  (pop-left)
                  (pop-right))
                 ((integerp (left)) (listify-left))
                 ((integerp (right)) (listify-right))
                 ((and (listp left) (listp right))
                  (push-left)
                  (push-right)))))))

(defun day13-puzzle1 ()
  (loop for (left . right) in (day13-parse-lists)
        for i from 1
        for result = (day13-compare left right)
        when result sum i into sum
        when result collect i into correct
        finally (return (values sum correct))))
