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
          when (gethash valve map) do (error "Duplicate valve: ~a" valve)
          do (setf (gethash valve map)
                   (alexandria:plist-hash-table
                    (list :valve valve
                          :rate rate
                          :tunnels tunnels
                          :table (alexandria:plist-hash-table
                                  (nconc
                                   (list valve (cons valve 0))
                                   (loop for tunnel in tunnels
                                         appending (list tunnel (cons tunnel 1))))))))
          when (< 0 rate) collect valve into relevant
          finally (return (values map relevant)))))

(defun day16-update-table (current target through distance valves)
  (declare (type hash-table current))
  (declare (type keyword target through))
  (declare (type (integer 0 *) distance))
  (let* ((table (gethash :table current))
         (old (gethash target table)))
    (when (or (null old) (< distance (cdr old)))
      (setf (gethash target table) (cons through distance))
      (when old
        (day16-update-table
         (gethash (car old) valves) target (gethash :valve current) (1+ distance) valves)))))

(defun day16-seek (current target passed valves)
  (declare (type hash-table current))
  (declare (type keyword target))
  (declare (type list passed))
  (let ((new-passed (nconc (list (gethash :valve current)) (copy-list passed))))
    (loop for tunnel in (gethash :tunnels current)
          for tunnel-valve = (gethash tunnel valves)
          for tunnel-table = (unless (find tunnel new-passed)
                               (day16-seek tunnel-valve target new-passed valves))
          when tunnel-table ;; Copy distance table
          do (loop for target in (alexandria:hash-table-keys tunnel-table)
                   for new = (gethash target tunnel-table)
                   do (day16-update-table current target tunnel (1+ (cdr new)) valves)))
    (the hash-table (gethash :table current))))

(defun day16-find-distance (from to valves)
  (let* ((from (etypecase from (keyword (gethash from valves)) (hash-table from)))
         (to (etypecase to (keyword to) (hash-table (gethash :valve to))))
         (known (gethash to (gethash :table from)))) ;; Quick return if we already know the distance.
    (when known (return-from day16-find-distance (cdr known)))
    (let ((distance (cdr (gethash to (day16-seek from to NIL valves)))))
      (unless (< 0 distance) (error "Zero distance?"))
      distance)))

(defun day16-seek-route (valves current closed rate pressure time)
  (let ((current (etypecase current (keyword (gethash current valves)) (hash-table current)))
        (viable NIL))
    (when (<= time 0) (error "No time!"))
    (loop with best-option = NIL
          with best-output = 0
          with best-distance = 0
          for option in closed
          for distance = (day16-find-distance current option valves)
          when (< (1+ distance) time)
          do (let* ((target (gethash option valves))
                    (output (* (gethash :rate target) (- time distance 1))))
               (when (< best-output output)
                 (setf best-option target)
                 (setf best-output output)
                 (setf best-distance distance))
               ;; Optimisation.
               (when (or (<= best-output output) (< distance best-distance))
                 (push (cons target distance) viable))))
    (unless viable
      (return-from day16-seek-route (values (+ pressure (* rate time)) NIL)))
    (loop with max-pressure = 0
          with max-route = NIL
          for (target . distance) in viable
          for time-spent = (1+ distance)
          for new-time = (- time time-spent)
          for new-rate = (+ rate (gethash :rate target))
          for new-pressure = (+ pressure (* time-spent rate))
          for (acquired route) = (multiple-value-list
                                  (day16-seek-route
                                   valves target
                                   (remove (gethash :valve target) closed)
                                   new-rate new-pressure new-time))
          when (< max-pressure acquired)
          do (setf max-pressure acquired
                   max-route route)
          finally (return (values
                           max-pressure
                           (nconc (list (gethash :valve current)) max-route))))))

(defun day16-puzzle1 ()
  (multiple-value-bind (valves closed)
      (day16-parse-input)
    (day16-seek-route valves :aa closed 0 0 30)))

;; 2124

(defun day16-permutations (valves length)
  (unless (< 0 length) (return-from day16-permutations))
  (loop for current = valves then rest
        for rest = (rest current)
        while (<= length (length current))
        for results = (day16-permutations rest (1- length))
        appending (if results
                      (loop for result in results
                            collecting (nconc (list (car current)) result))
                      (list (list (car current))))))

(defun day16-rest (valves sub-list)
  (loop for valve in valves
        unless (find valve sub-list) collect valve))

(defun day16-puzzle2 ()
  (multiple-value-bind (valves closed)
      (day16-parse-input)
    (loop for a in (day16-permutations closed (floor (length closed) 2))
          for b = (day16-rest closed a)
          maximizing (+ (day16-seek-route valves :aa a 0 0 26)
                        (day16-seek-route valves :aa b 0 0 26)))))

;; 2775
