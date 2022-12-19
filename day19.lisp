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

(defun day19-puzzle1 (&optional (total-time 24) limit)
  ;; FIXME: I tried to make this faster with threads. Now it crashes slime if I run it single thread.
  (let ((blueprint)
        (blueprint-max-costs)
        (blueprint-min-costs)
        (blueprint-total-costs)
        (top-level-thread (bordeaux-threads:current-thread))
        (thread-count (bordeaux-threads:make-semaphore :name "thread-count" :count 8)))
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
             (mine (time resources robots previous)
               (unless (< 0 time)
                 (getf resources :geode 0))
               ;; 1. Figure out what things we //should// craft.
               (let ((craftables (when (< 1 time) (craftables resources))))
                 ;; TODO: How do I improve this?
                 (loop for type in blueprint-max-costs by #'cddr
                       for min-cost in (cdr blueprint-min-costs) by #'cddr
                       for max-cost in (cdr blueprint-max-costs) by #'cddr
                       for total-cost in (cdr blueprint-total-costs) by #'cddr
                       for robot-count = (getf robots type 0)
                       for current = (getf resources type 0)
                       ;; when (and (find type craftables)
                       ;;           (eql type previous))
                       unless (or (null (find type craftables))
                                  (eql type :geode)
                                  (not (eql type previous))
                                  (and (< (+ current robot-count) (* 2 max-cost))
                                       (< robot-count 11)))
                       ;; (and (< (+ current robot-count) (* 2 max-cost))
                       ;;      (< robot-count (floor total-time 2))
                       ;;      (< (- min-cost current (* time robot-count))
                       ;;         (* time time)))
                       do (setf craftables (delete type craftables)))
                 (unless (and (null (cdr craftables)) (eql (car craftables) :geode))
                   (if craftables
                       (setf (cdr (last craftables)) (list NIL))
                       (setf craftables (list NIL))))
                 ;; 2. Mine resources.
                 (loop for robot in robots by #'cddr
                       for count in (cdr robots) by #'cddr
                       do (setf (getf resources robot)
                                (+ count (getf resources robot 0))))
                 ;; 3. Mine more.
                 (let ((threads)
                       (max-geodes 0)
                       (top-level-p (eql (bordeaux-threads:current-thread) top-level-thread)))
                   (loop for to-craft in craftables
                         for mine-function = #'(lambda ()
                                                 (mine (1- time)
                                                       (copy-subtract-resources
                                                        resources (getf blueprint to-craft))
                                                       (copy-add-robots robots to-craft)
                                                       to-craft))
                         for thread = (when (and top-level-p
                                                 ;; (eql to-craft :no-such-thing)
                                                 (< 1 (length craftables))
                                                 (bordeaux-threads:wait-on-semaphore thread-count))
                                        (handler-case
                                            (bordeaux-threads:make-thread
                                             #'(lambda ()
                                                 (prog1 (funcall mine-function)
                                                   (bordeaux-threads:signal-semaphore
                                                    thread-count))))
                                          (error (e)
                                            (format T "Error creating thread: ~a~%" e)
                                            (bordeaux-threads:signal-semaphore thread-count)
                                            NIL)))
                         do (if (bordeaux-threads:threadp thread)
                                (push thread threads)
                                (setf max-geodes (max max-geodes (funcall mine-function)))))
                   (loop for thread in threads
                         when (bordeaux-threads:threadp thread)
                         do (let ((geodes (bordeaux-threads:join-thread thread)))
                              (when (< max-geodes geodes)
                                (setf max-geodes geodes))))
                   max-geodes))))
      (loop for input in (day19-parse-input)
            for output = NIL
            for i from 0
            while (or (null limit) (< i limit))
            do (setf blueprint (cddr input)
                     blueprint-min-costs NIL
                     blueprint-max-costs NIL
                     blueprint-total-costs NIL)
            do (loop for robot in blueprint by #'cddr
                     for costs in (cdr blueprint) by #'cddr
                     do (loop for type in costs by #'cddr
                              for cost in (cdr costs) by #'cddr
                              for old-min = (getf blueprint-min-costs type total-time)
                              for old-max = (getf blueprint-max-costs type 0)
                              for total = (+ (getf blueprint-total-costs type 0) cost)
                              when (< cost old-min)
                              do (setf (getf blueprint-min-costs type) cost)
                              when (< old-max cost)
                              do (setf (getf blueprint-max-costs type) cost)
                              do (setf (getf blueprint-total-costs type) total)))
            do (setf output (cons (cadr input) (mine total-time NIL (list :ore 1) NIL)))
            do (format T "Blueprint ~a: ~a geodes~%" (car output) (cdr output))
            summing (* (car output) (cdr output))))))

;; 1365

(defun day19-puzzle2 ()
  ;; FIXME: Currently I interrupt the process and calculate it by hand.
  (day19-puzzle1 32 3))
