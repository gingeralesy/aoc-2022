#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day2-puzzle1 ()
  (with-open-file (stream (input 2) :if-does-not-exist :error)
    (flet ((parse-rps (string)
             (ecase (char string 0)
               ((#\A #\X) :rock)
               ((#\B #\Y) :paper)
               ((#\C #\Z) :scissors)))
           (rps-result (us them)
             (cond
               ((eql us them) 3)
               ((eql us :rock)
                (if (eql them :scissors) 6 0))
               ((eql us :paper)
                (if (eql them :rock) 6 0))
               ((eql us :scissors)
                (if (eql them :paper) 6 0))
               (T (error "Should not happen"))))
           (rps-value (pick)
             (ecase pick
               (:rock 1)
               (:paper 2)
               (:scissors 3))))
      (loop with line-re = (cl-ppcre:create-scanner "^(\\w) (\\w)$")
            with score = 0
            for line = (clean (read-line stream NIL))
            while line
            do (multiple-value-bind (match groups)
                   (cl-ppcre:scan-to-strings line-re line)
                 (unless match (error "Invalid line: ~a" line))
                 (let* ((them (parse-rps (aref groups 0)))
                        (us (parse-rps (aref groups 1)))
                        (result (rps-result us them))
                        (pick (rps-value us)))
                   (incf score (+ result pick))))
            finally (return score)))))

;; 9177

(defun day2-puzzle2 ()
  (with-open-file (stream (input 2) :if-does-not-exist :error)
    (flet ((parse-rps (string)
             (ecase (char string 0)
               (#\A :rock)
               (#\B :paper)
               (#\C :scissors)))
           (parse-result (string)
             (ecase (char string 0)
               (#\X 0)
               (#\Y 3)
               (#\Z 6)))
           (rps-value (them result)
             (ecase them
               (:rock (ecase result (0 3) (3 1) (6 2)))
               (:paper (ecase result (0 1) (3 2) (6 3)))
               (:scissors (ecase result (0 2) (3 3) (6 1))))))
      (loop with line-re = (cl-ppcre:create-scanner "^(\\w) (\\w)$")
            with score = 0
            for line = (clean (read-line stream NIL))
            while line
            do (multiple-value-bind (match groups)
                   (cl-ppcre:scan-to-strings line-re line)
                 (unless match (error "Invalid line: ~a" line))
                 (let ((them (parse-rps (aref groups 0)))
                       (us (parse-result (aref groups 1))))
                   (incf score (+ us (rps-value them us)))))
            finally (return score)))))

;; 12111
