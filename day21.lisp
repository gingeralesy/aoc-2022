#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day21-monkey-re* (cl-ppcre:create-scanner "^(\\w+): (\\d+|\\w+ [+\\-*/] \\w+)\\s*$"))
(defparameter *day21-operation-re* (cl-ppcre:create-scanner "^(\\w+) ([+\\-*/]) (\\w+)$"))

(defun day21-parse-input ()
  (flet ((monkeyfy (name)
           (when name
             (intern (format NIL "~:@(~a~)" name) :keyword))))
    (with-open-file (stream (input 21) :if-does-not-exist :error)
      (loop with monkeys = (make-hash-table)
            for line = (read-line stream NIL)
            while line
            for (match groups) = (or (multiple-value-list
                                      (cl-ppcre:scan-to-strings *day21-monkey-re* line))
                                     (error "Invalid line: ~a" line))
            for monkey = (monkeyfy (aref groups 0))
            for (op-match op-groups) = (multiple-value-list
                                        (cl-ppcre:scan-to-strings
                                         *day21-operation-re* (aref groups 1)))
            for yell = (if op-match
                           (list :operation (ecase (char (aref op-groups 1) 0)
                                              (#\+ '+)
                                              (#\- '-)
                                              (#\* '*)
                                              (#\/ '/))
                                 :monkeys (cons (or (monkeyfy (aref op-groups 0))
                                                    (error "Invalid left monkey on line: ~a" line))
                                                (or (monkeyfy (aref op-groups 2))
                                                    (error "Invalid right monkey on line: ~a"
                                                           line))))
                           (list :number (parse-integer (aref groups 1))))
            do (setf (gethash monkey monkeys) yell)
            finally (return monkeys)))))

(defun day21-monkey-value (monkey monkeys)
  (declare (type keyword monkey))
  (declare (type hash-table monkeys))
  (let ((yell (gethash monkey monkeys)))
    (declare (type list yell))
    (ecase (car yell)
      (:number (the (or character number) (getf yell :number)))
      (:operation
       (let* ((op-monkeys (getf yell :monkeys))
              (op (getf yell :operation))
              (left (day21-monkey-value (car op-monkeys) monkeys))
              (right (day21-monkey-value (cdr op-monkeys) monkeys))
              (numbersp (and (numberp left) (numberp right))))
         (if numbersp
             (funcall op left right)
             (list op left right)))))))

(defun day21-puzzle1 ()
  (let ((monkeys (day21-parse-input)))
    (day21-monkey-value :root monkeys)))

;; 158731561459602

(defun day21-find-x (left &optional (right 0))
  (declare (type (or list character) left))
  (declare (type number right))
  (when (and (characterp left) (char= left #\X))
    (return-from day21-find-x right))
  (destructuring-bind (op a b) left
    (declare (type symbol op))
    (declare (type (or character number list) a b))
    (unless (or (numberp a) (numberp b))
      (error "Neither side had a number for expression: ~a" left))
    (let* ((a-numberp (numberp a))
           (number (if a-numberp a b))
           (expression (if a-numberp b a)))
      (day21-find-x
       expression
       (ecase op
         (+ (- right number))
         (- (if a-numberp (- number right) (+ right number)))
         (* (/ right number))
         (/ (if a-numberp (/ number right) (* right number)))
         (= number))))))

(defun day21-puzzle2 ()
  (let ((monkeys (day21-parse-input)))
    (setf (getf (gethash :root monkeys) :operation) '=)
    (setf (gethash :humn monkeys) (list :number #\X))
    (let ((root (day21-monkey-value :root monkeys)))
      (day21-find-x root))))

;; 3769668716709
