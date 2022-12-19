#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day19-blueprint-re*
  (cl-ppcre:create-scanner "^\\s*Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.\\s*$"))

(defun day19-parse-input ()
  (with-open-file (stream (input 19) :if-does-not-exist :error)
    (loop for line = (read-line stream NIL)
          while line
          for (match groups) = (multiple-value-list
                                (cl-ppcre:scan-to-strings *day19-blueprint-re* line))
          unless match do (error "Invalid line: ~a" line)
          collect (list :blueprint (parse-integer (aref groups 0))
                        :ore (list :ore (parse-integer (aref groups 1)))
                        :clay (list :ore (parse-integer (aref groups 2)))
                        :obsidian (list :ore (parse-integer (aref groups 3))
                                        :clay (parse-integer (aref groups 4)))
                        :geode (list :ore (parse-integer (aref groups 5))
                                     :obsidian (parse-integer (aref groups 6)))))))

(defun day19-puzzle1 ()
  (let ((blueprint)
        (blueprint-max-costs))
    (labels ((copy-add-robots (robots &optional new)
               (let ((new-robots (copy-list robots)))
                 (when new
                   (setf (getf new-robots new) (1+ (getf new-robots new 0))))
                 new-robots))
             (copy-subtract-resources (resources &optional costs)
               (let ((new-resources (copy-list resources)))
                 (when costs
                   (loop for type in costs by #'cddr
                         for cost in (cdr costs) by #'cddr
                         for old = (getf new-resources type 0)
                         when (< old cost) do (error "Cost of ~a is too high!" type)
                         do (setf (getf new-resources type) (- old cost))))
                 new-resources))
             (craftables (resources)
               (loop for robot in blueprint by #'cddr
                     for costs in (cdr blueprint) by #'cddr
                     when (loop for type in costs by #'cddr
                                for cost in (cdr costs) by #'cddr
                                for resource = (getf resources type)
                                for can-do = (and resource (<= cost resource))
                                while can-do
                                finally (return can-do))
                     collect robot))
             (mine (time resources robots)
               (when (< 0 time)
                 ;; 1. Figure out what things we //should// craft.
                 (let ((craftables (when (< 1 time) (craftables resources))))
                   (unless blueprint-max-costs
                     (loop for robot in blueprint by #'cddr
                           for costs in (cdr blueprint) by #'cddr
                           do (loop for type in costs by #'cddr
                                    for cost in (cdr costs) by #'cddr
                                    for old = (getf blueprint-max-costs type 0)
                                    when (< old cost)
                                    do (setf (getf blueprint-max-costs type) cost))))
                   ;; TODO: How do I improve this?
                   (loop for type in blueprint-max-costs by #'cddr
                         for max-cost in (cdr blueprint-max-costs) by #'cddr
                         for robot-count = (getf robots type 0)
                         when (and (not (eql type :geode))
                                   (find type craftables)
                                   (or (<= (* 2 max-cost) (+ (getf resources type 0) robot-count))
                                       (< 10 robot-count)))
                         do (setf craftables (delete type craftables)))
                   (if craftables
                       (setf (cdr (last craftables)) (list NIL))
                       (setf craftables (list NIL)))
                   ;; 2. Mine resources.
                   (loop for robot in robots by #'cddr
                         for count in (cdr robots) by #'cddr
                         do (setf (getf resources robot)
                                  (+ count (getf resources robot 0))))
                   ;; 3. Mine more.
                   (loop for to-craft in craftables
                         for geodes = (or (mine (1- time)
                                                (copy-subtract-resources
                                                 resources (getf blueprint to-craft))
                                                (copy-add-robots robots to-craft))
                                          (getf resources :geode 0))
                         maximize geodes)))))
      (loop for input in (day19-parse-input)
            for output = NIL
            do (setf blueprint (cddr input))
            do (setf blueprint-max-costs NIL)
            do (setf output (cons (cadr input) (mine 24 NIL (list :ore 1))))
            do (format T "Blueprint ~a: ~a geodes~%" (car output) (cdr output))
            summing (* (car output) (cdr output))))))

;; 1365
