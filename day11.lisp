#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day11-monkey-re*
  (list
   (cons :monkey (cl-ppcre:create-scanner "Monkey (\\d):"))
   (cons :items (cl-ppcre:create-scanner "Starting items: ((\\d+(,\\s+)?)+)\\s*$"))
   (cons :operation (cl-ppcre:create-scanner "Operation: new = (\\w+) ([*+]) (\\w+)"))
   (cons :test (cl-ppcre:create-scanner "Test: divisible by (\\d+)"))
   (cons :if-true (cl-ppcre:create-scanner "If true: throw to monkey (\\d)"))
   (cons :if-false (cl-ppcre:create-scanner "If false: throw to monkey (\\d)"))))

(defparameter *day11-items-re*
  (cl-ppcre:create-scanner "\\b\\d+\\b"))

(defun day11-parse-monkey (stream)
  (loop for line = (loop for line = (clean (read-line stream NIL))
                         while (and line (= 0 (length line)))
                         finally (return line))
        while line
        for (key . re) in *day11-monkey-re*
        for (match groups) = (multiple-value-list (cl-ppcre:scan-to-strings re line))
        unless match do (error "Invalid line: ~a" line)
        append (list key (ecase key
                           ((:monkey :test :if-true :if-false)
                            (parse-integer (aref groups 0)))
                           (:items (mapcar #'(lambda (x) (parse-integer x))
                                           (cl-ppcre:all-matches-as-strings
                                            *day11-items-re* (aref groups 0))))
                           (:operation (loop for op across groups
                                             collect (cond
                                                       ((string= op "old") :old)
                                                       ((string= op "+") #'+)
                                                       ((string= op "*") #'*)
                                                       (T (parse-integer op)))
                                             into operation
                                             finally (return (list (second operation)
                                                                   (first operation)
                                                                   (third operation)))))))
        into monkey
        finally (return (when monkey (alexandria:plist-hash-table monkey)))))

(defun day11-parse-monkeys ()
  (with-open-file (stream (input 11) :if-does-not-exist :error)
    (loop for monkey = (day11-parse-monkey stream)
          while monkey
          collect (cons (gethash :monkey monkey) monkey) into monkeys
          finally (return (when monkeys (alexandria:alist-hash-table monkeys))))))

(defun day11-puzzle1 (&optional (rounds 20) (worry 3))
  (let* ((monkeys (day11-parse-monkeys))
         (lcm (loop with lcm = 1
                    for monkey in (alexandria:hash-table-values monkeys)
                    do (setf lcm (* lcm (gethash :test monkey)))
                    finally (return lcm))))
    (dotimes (round rounds)
      (loop for monkey in (sort (alexandria:hash-table-values monkeys)
                                #'< :key (lambda (x) (gethash :monkey x)))
            for operation = (gethash :operation monkey)
            for test = (gethash :test monkey)
            for if-true = (gethash :if-true monkey)
            for if-false = (gethash :if-false monkey)
            do (loop for item in (gethash :items monkey)
                     for left = (if (eql (second operation) :old) item (second operation))
                     for right = (if (eql (third operation) :old) item (third operation))
                     for new = (mod (floor (funcall (first operation) left right) worry) lcm)
                     for target = (gethash (if (= 0 (mod new test)) if-true if-false) monkeys)
                     for target-list = (gethash :items target)
                     do (setf (gethash :inspections monkey) (1+ (gethash :inspections monkey 0)))
                     when target-list
                     do (setf (cdr (last target-list)) (cons new NIL))
                     unless target-list
                     do (setf target-list (list new))
                     do (setf (gethash :items target) target-list))
            do (setf (gethash :items monkey) NIL)))
    (loop for monkey in (alexandria:hash-table-values monkeys)
          collect (gethash :inspections monkey) into inspections
          finally (return
                    (let ((inspections (sort inspections #'>)))
                      (* (first inspections) (second inspections)))))))

;; 57348

(defun day11-puzzle2 ()
  (day11-puzzle1 10000 1))

;; 14106266886
