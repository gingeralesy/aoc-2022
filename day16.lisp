#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day16-valve-re*
  (cl-ppcre:create-scanner
   "^Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (((\\w+)(, )?)+)\\s*$"))

(defparameter *day16-tunnel-re* (cl-ppcre:create-scanner "\\b\\w+\\b"))

(defun day16-parse-line (line)
  (when (and line (< 0 (length line)))
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings *day16-valve-re* line)
      (unless match (error "Invalid input: ~a" line))
      (list (intern (aref groups 0) :keyword)
            (parse-integer (aref groups 1))
            (mapcar #'(lambda (tunnel) (intern tunnel :keyword))
                    (cl-ppcre:all-matches-as-strings *day16-tunnel-re* (aref groups 2)))))))

(defun day16-parse-input ()
  (with-open-file (stream (input 16) :if-does-not-exist :error)
    (loop with map = (make-hash-table)
          for (valve rate tunnels) = (day16-parse-line (read-line stream NIL))
          while valve
          do (setf (gethash valve map) (list :valve valve :rate rate :tunnels tunnels))
          finally (return map))))

(defun day16-distance (from to valves &optional passed)
  (let* ((from (etypecase from (keyword (gethash from valves)) (list from)))
         (to (etypecase to (keyword (gethash to valves)) (list to)))
         (tunnels (getf from :tunnels))
         (passed (append (list (getf from :valve)) passed)))
    (when (find (getf to :valve) tunnels)
      (return-from day16-distance 1))
    (loop with min-distance = NIL
          for tunnel in tunnels
          for from-tunnel = (unless (find tunnel passed)
                              (day16-distance tunnel to valves passed))
          when (and from-tunnel (or (null min-distance) (< (1+ from-tunnel) min-distance)))
          do (setf min-distance (1+ from-tunnel))
          finally (return min-distance))))

(defun day16-build-map (valves)
  ;; TODO: This takes ten seconds. Find a way to optimise it.
  (loop for valve in (alexandria:hash-table-values valves)
        for map = (loop for target in (alexandria:hash-table-keys valves)
                        for distance = (if (eql target (getf valve :valve))
                                           0
                                           (day16-distance valve target valves))
                        append (list target distance))
        do (setf (getf valve :map) map)
        do (setf (gethash (getf valve :valve) valves) valve)))

(defun day16-seek-route (valves current closed rate pressure time)
  (let ((current (etypecase current (keyword (gethash current valves)) (list current))))
    (when (<= time 0) (error "No time!"))
    (unless closed
      (return-from day16-seek-route
        (values (+ pressure (* rate time)) (list (getf current :valve)))))
    (loop with map = (getf current :map)
          with max-pressure = 0
          with max-route = NIL
          for option in closed
          for distance = (getf map option)
          for time-spent = (1+ distance)
          for new-time = (- time time-spent)
          for target = (gethash option valves)
          for new-rate = (+ rate (getf target :rate))
          for new-pressure = (+ pressure (* time-spent rate))
          for (acquired route) = (multiple-value-list
                                  (if (< 0 new-time)
                                      (day16-seek-route
                                       valves target (remove option closed)
                                       new-rate new-pressure new-time)
                                      (values (+ pressure (* rate time)) NIL)))
          when (< max-pressure acquired)
          do (setf max-pressure acquired
                   max-route route)
          finally (return (values max-pressure (append (list (getf current :valve)) max-route))))))

(defun day16-puzzle1 ()
  (let* ((valves (day16-parse-input))
         (closed (loop for valve in (alexandria:hash-table-values valves)
                       when (< 0 (getf valve :rate)) collect (getf valve :valve))))
    (day16-build-map valves)
    (day16-seek-route valves :aa closed 0 0 30)))

;; 2124
