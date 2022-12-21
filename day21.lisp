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
                           (list :operation (char (or (aref op-groups 1)
                                                      (error "Invalid operation on line: ~a" line))
                                                  0)
                                 :monkeys (cons (or (monkeyfy (aref op-groups 0))
                                                    (error "Invalid left monkey on line: ~a" line))
                                                (or (monkeyfy (aref op-groups 2))
                                                    (error "Invalid right monkey on line: ~a"
                                                           line))))
                           (list :number (parse-integer (aref groups 1))))
            do (setf (gethash monkey monkeys) yell)
            finally (return monkeys)))))

(defun day21-monkey-value (monkey monkeys)
  (let ((yell (gethash monkey monkeys)))
    (unless yell (error "No such monkey: ~a" monkey))
    (ecase (car yell)
      (:number (getf yell :number))
      (:operation
       (let* ((op-monkeys (getf yell :monkeys))
              (left (day21-monkey-value (car op-monkeys) monkeys))
              (right (day21-monkey-value (cdr op-monkeys) monkeys)))
         (ecase (getf yell :operation)
           (#\+ (+ left right))
           (#\- (- left right))
           (#\* (* left right))
           (#\/ (/ left right))))))))

(defun day21-puzzle1 ()
  (let ((monkeys (day21-parse-input)))
    (day21-monkey-value :root monkeys)))

;; 158731561459602
