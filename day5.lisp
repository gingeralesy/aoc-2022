#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day5-crate-re* (cl-ppcre:create-scanner "(\\[\\w\\])"))

(defparameter *day5-end-crates-re* (cl-ppcre:create-scanner "^( +\\d)+$"))

(defparameter *day5-command-re* (cl-ppcre:create-scanner "^move (\\d+) from (\\d) to (\\d)"))

(defun day5-parse-stack-row (line)
  (unless (cl-ppcre:scan *day5-end-crates-re* line)
    (loop with stack = (make-array 9 :initial-element NIL)
          for prev = 0 then end
          for (start end) = (multiple-value-list (cl-ppcre:scan *day5-crate-re* line :start prev))
          while start
          do (setf (aref stack (/ start 4)) (char line (1+ start)))
          finally (return stack))))

(defun day5-parse-stacks (stream)
  (loop for line = (clean (read-line stream NIL))
        while (and line (< 0 (length line)))
        for row = (day5-parse-stack-row line)
        when row collect row))

(defun day5-parse-command (line)
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings *day5-command-re* line)
    (unless match (error "Invalid string: ~a" line))
    (list (parse-integer (aref groups 0))
          (1- (parse-integer (aref groups 1)))
          (1- (parse-integer (aref groups 2))))))

(defun day5-puzzle1 ()
  (with-open-file (stream (input 5) :if-does-not-exist :error)
    (loop with stacks = (day5-parse-stacks stream)
          for line = (clean (read-line stream NIL))
          while line
          for (count from to) = (day5-parse-command line)
          do (dotimes (i count)
               (let ((from-row)
                     (to-row))
                 (loop for prev = NIL then row
                       for row in stacks
                       until (and from-row to-row)
                       when (and (null from-row) (aref row from))
                       do (setf from-row row)
                       when (and (null to-row) (aref row to))
                       do (cond
                            (prev (setf to-row prev))
                            (T
                             (setf to-row (make-array 9 :initial-element NIL))
                             (push to-row stacks))))
                 (unless from-row (error "Nothing on stack ~a during '~a'" from line))
                 (when (null to-row)
                   (setf to-row (car (last stacks))))
                 (setf (aref to-row to) (aref from-row from))
                 (setf (aref from-row from) NIL)))
          finally (return
                    (loop with string = (make-array 9 :element-type 'character
                                                      :initial-element #\Nul)
                          for row in stacks
                          do (loop for crate across row
                                   for i from 0
                                   when (and crate (char= #\Nul (aref string i)))
                                   do (setf (aref string i) crate))
                          finally (return string))))))

;; MQTPGLLDN

(defun day5-puzzle2 ()
  (with-open-file (stream (input 5) :if-does-not-exist :error)
    (loop with stacks = (day5-parse-stacks stream)
          for line = (clean (read-line stream NIL))
          while line
          for (count from to) = (day5-parse-command line)
          do (let ((from-rows)
                   (to-rows))
               (loop with found = 0
                     with prev = NIL
                     for row in stacks
                     do (push row prev)
                     when (aref row from) do (incf found)
                     while (< found count)
                     finally (setf from-rows (loop repeat count
                                                   for row in prev
                                                   collect row)))
               (loop for prev = NIL then (cons row prev)
                     for row in stacks
                     until (aref row to)
                     finally (setf to-rows (loop repeat count
                                                 for row in prev
                                                 collect row)))
               (loop while (< (length to-rows) (length from-rows))
                     for new = (make-array 9 :initial-element NIL)
                     do (if to-rows
                            (setf (cdr (last to-rows)) (cons new NIL))
                            (setf to-rows (cons new NIL)))
                     do (push new stacks))
               (loop for from-row in from-rows
                     for to-row in to-rows
                     for crate = (aref from-row from)
                     do (setf (aref from-row from) NIL)
                     do (setf (aref to-row to) crate)))
          finally (return
                    (loop with string = (make-array 9 :element-type 'character
                                                      :initial-element #\Nul)
                          for row in stacks
                          ;; do (loop for crate across row
                          ;;          do (if crate
                          ;;                 (format T "[~a] " crate)
                          ;;                 (format T "    ")))
                          ;; do (format T "~%")
                          do (loop for crate across row
                                   for i from 0
                                   when (and crate (char= #\Nul (aref string i)))
                                   do (setf (aref string i) crate))
                          finally (return string))))))

;; LVZPSTTCZ
