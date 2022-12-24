#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defun day24-parse-input ()
  (with-open-file (stream (input 24) :if-does-not-exist :error)
    (loop with width = 0
          with count = 0
          for height from 0
          for line = (clean (read-line stream NIL))
          while line
          append (loop for x from 0
                       for ch across line
                       for blizzard = (case ch (#\> :east) (#\< :west) (#\^ :north) (#\v :south))
                       when (<= width x) do (setf width (1+ x))
                       when blizzard do (incf count)
                       when blizzard collect (cons blizzard (cons x height)))
          into blizzards
          finally (return (values blizzards count width height)))))

