#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *root-dir* (asdf:component-pathname (asdf:find-system :aoc-2022)))

(defparameter *clean-re* (cl-ppcre:create-scanner "^(.*\\S)?\\s*$"))

(defun input (day)
  (declare  (type (integer 0 25) day))
  (make-pathname :defaults aoc-2022::*root-dir*
                 :name (format NIL "input/day~a" day)
                 :type "dat"))

(defun clean (line)
  (when line
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings *clean-re* line)
      (when match (aref groups 0)))))
