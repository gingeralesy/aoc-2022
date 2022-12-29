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
          collect (let ((id (parse-integer (aref groups 0)))
                        (ore->ore (parse-integer (aref groups 1)))
                        (ore->clay (parse-integer (aref groups 2)))
                        (ore->obsidian (parse-integer (aref groups 3)))
                        (clay->obsidian (parse-integer (aref groups 4)))
                        (ore->geode (parse-integer (aref groups 5)))
                        (obsidian->geode (parse-integer (aref groups 6))))
                    (list :blueprint id
                          :ore (list :ore ore->ore)
                          :clay (list :ore ore->clay)
                          :obsidian (list :ore ore->obsidian :clay clay->obsidian)
                          :geode (list :ore ore->geode :obsidian obsidian->geode)
                          :max (list :ore (max ore->ore ore->clay ore->obsidian ore->geode)
                                     :clay clay->obsidian
                                     :obsidian obsidian->geode))))))

(defun day19-make-state ()
  (list :robots (list :ore 1 :clay 0 :obsidian 0 :geode 0)
        :resources (list :ore 0 :clay 0 :obsidian 0 :geode 0)
        :path NIL
        :best 0))

(defun day19-copy-state (state new)
  (list :robots (copy-list (getf state :robots))
        :resources (copy-list (getf state :resources))
        :path (nconc (copy-list (getf state :path)) (list (or new :wait)))
        :best (getf state :best)))

(defun day19-incf (state type resource count)
  (let* ((list (or (getf state type) (error "Invalid type: ~a" type)))
         (current (or (getf list resource) (error "Invalid resource: ~a" resource))))
    (when (< (+ current count) 0) (error "Invalid decrement for ~a" resource))
    (setf (getf list resource) (+ current count))
    (setf (getf state type) list)
    state))

(defun day19-can-build-p (state robot blueprint)
  (let ((costs (getf blueprint robot))
        (resources (getf state :resources))
        (robots (getf state :robots))
        (max (getf blueprint :max)))
    (when (< (getf robots robot) (getf max robot 99))
      (loop for current = costs then (cddr current)
            while current
            for (type cost) = current
            for can-do-p = (<= cost (getf resources type))
            while can-do-p
            finally (return can-do-p)))))

(defun day19-build (state robot blueprint)
  (let ((state (day19-copy-state state robot)))
    (when robot
      (setf state (day19-incf state :robots robot 1))
      (loop for cost = (getf blueprint robot) then (cddr cost)
            while cost
            for (type count) = cost
            do (setf state (day19-incf state :resources type (- count)))))
    state))

(defun day19-max-geodes (state time)
  (+ (getf (getf state :resources) :geode)
     (* time (getf (getf state :robots) :geode))
     (/ (* time (1- time)) 2)))

(defun day19-mine (state blueprint time)
  (unless (< 1 time) ;; Are we done yet?
    (loop repeat time
          for path = (last (getf state :path)) then (cdr path)
          do (setf (cdr path) (list :wait)))
    (return-from day19-mine
      (values (+ (getf (getf state :resources) :geode)
                 (* time (getf (getf state :robots) :geode)))
              state)))
  ;; Check what we can build.
  (let ((maybe-build (loop for type in '(:geode :obsidian :clay :ore)
                           when (day19-can-build-p state type blueprint) collect type)))
    ;; Do any mining we can.
    (loop for robots = (getf state :robots) then (cddr robots)
          while robots
          for (robot count) = robots
          when (< 0 count) do (setf state (day19-incf state :resources robot count)))
    ;; Check if waiting would be worthwhile.
    (if maybe-build
        (loop for type in '(:geode :obsidian :clay :ore)
              for couldnt-before-p = (and (not (find type maybe-build))
                                          (day19-can-build-p state type blueprint))
              when couldnt-before-p do (push NIL maybe-build)
              until couldnt-before-p)
        (push NIL maybe-build))
    ;; Loop and recurse.
    (loop with max-geodes = 0
          with max-state = NIL
          for robot in maybe-build
          for new-state = (day19-build state robot blueprint)
          when (<= (getf state :best) (day19-max-geodes new-state time))
          do (multiple-value-bind (geodes final-state)
                 (day19-mine new-state blueprint (1- time))
               (when (<= max-geodes geodes)
                 (setf max-geodes geodes)
                 (setf max-state final-state)))
          when (< (getf state :best) max-geodes)
          do (setf (getf state :best) max-geodes)
          finally (return (values max-geodes max-state)))))

(defun day19-puzzle1 ()
  (loop for blueprint in (day19-parse-input)
        for id = (getf blueprint :blueprint)
        summing (* id (day19-mine (day19-make-state) blueprint 24))))

;; 1365

(defun day19-puzzle2 ()
  ;; TODO: This is *very* slow, even with multithreading. Optimise it a bit more.
  (loop for blueprint in (subseq (day19-parse-input) 0 3)
        collect (bt2:make-thread #'(lambda () (day19-mine (day19-make-state) blueprint 32)))
        into threads
        finally (return
                  (loop with total = 1
                        for thread in threads
                        for value = (bt2:join-thread thread)
                        do (setf total (* value total))
                        finally (return total)))))

;; 4864
