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
          finally (return map))))

(defun day16-find-distance (from to valves)
  (let* ((from (etypecase from (keyword (gethash from valves)) (hash-table from)))
         (to (etypecase to (keyword to) (hash-table (gethash :valve to))))
         (known (gethash to (gethash :table from))))
    (when known (return-from day16-find-distance (cdr known)))
    (labels ((update-table (current target through distance)
               (declare (type hash-table current))
               (declare (type keyword target through))
               (declare (type number distance))
               (let* ((table (gethash :table current))
                      (old (gethash target table)))
                 (when (or (null old) (< distance (cdr old)))
                   (setf (gethash target table) (cons through distance))
                   (setf (gethash :table current) table)
                   (when (and old (not (eql (car old) through)))
                     (update-table
                      (gethash (car old) valves) target (gethash :valve current) (1+ distance))))))
             (seek (current target passed)
               (declare (type hash-table current))
               (declare (type keyword target))
               (declare (type list passed))
               (let ((new-passed (nconc (list (gethash :valve current)) (copy-list passed))))
                 (unless (gethash target (gethash :table current)) ;; Already known?
                   (loop for tunnel in (gethash :tunnels current)
                         for tunnel-valve = (gethash tunnel valves)
                         for tunnel-table = (unless (find tunnel new-passed)
                                              (seek tunnel-valve target new-passed))
                         when tunnel-table
                         do (loop for target in (alexandria:hash-table-keys tunnel-table)
                                  for new = (gethash target tunnel-table)
                                  do (update-table current target tunnel (1+ (cdr new))))))
                 (the hash-table (gethash :table current)))))
      (let ((distance (cdr (gethash to (seek from to NIL)))))
        (unless (< 0 distance) (error "Zero distance?"))
        distance))))

(defun day16-seek-route (valves current closed rate pressure time)
  (let ((current (etypecase current (keyword (gethash current valves)) (hash-table current))))
    (when (<= time 0) (error "No time!"))
    (unless closed
      (return-from day16-seek-route
        (values (+ pressure (* rate time)) NIL)))
    (loop with max-pressure = 0
          with max-route = NIL
          for option in closed
          for distance = (day16-find-distance current option valves)
          for time-spent = (1+ distance)
          for new-time = (- time time-spent)
          for target = (gethash option valves)
          for new-rate = (+ rate (gethash :rate target))
          for new-pressure = (+ pressure (* time-spent rate))
          for (acquired route) = (multiple-value-list
                                  (if (< 0 new-time)
                                      (day16-seek-route
                                       valves target (remove option closed)
                                       new-rate new-pressure new-time)
                                      (day16-seek-route
                                       valves current NIL rate pressure time)))
          when (< max-pressure acquired)
          do (setf max-pressure acquired
                   max-route route)
          finally (return (values
                           max-pressure
                           (append (list (gethash :valve current)) max-route))))))

(defun day16-puzzle1 ()
  (let* ((valves (day16-parse-input))
         (closed (loop for valve in (alexandria:hash-table-values valves)
                       when (< 0 (gethash :rate valve)) collect (gethash :valve valve))))
    (day16-seek-route valves :aa closed 0 0 30)))

;; 2124

;; FIXME: This doesn't work. A bit of an experiment. Didn't work earlier either though, so
;;        I'm leaving it in. Fix it someday.

(defun day16-route (from to valves)
  (declare (type keyword to))
  (declare (type hash-table valves))
  (let ((from (etypecase from (keyword (gethash from valves)) (hash-table from))))
    (when (< 0 (day16-find-distance from to valves))
      (loop for current = from then (gethash through valves)
            for table = (gethash :table current)
            for through = (car (gethash to table))
            collect through
            until (eql to through)))))

(defun day16-best (from closed valves time)
  (loop with max = 0
        with best = NIL
        for option in closed
        for valve = (gethash option valves)
        for distance = (day16-find-distance from option valves)
        for produces = (* (gethash :rate valve) (- time (1+ distance)))
        when (or (null best) (< max produces))
        do (setf max produces
                 best option)
        finally (return ;; Check for others along the way.
                  (loop for alternative in (day16-route from best valves)
                        until (find alternative closed)
                        finally (return alternative)))))

(defun day16-traverse (valves state-a state-b closed rate pressure time)
  (when (<= time 0) (error "No time!"))
  (unless closed
    (let ((state (if (< (cdr state-a) (cdr state-b)) state-b state-a))
          (pressure pressure)
          (rate rate)
          (time time))
      (when (and (< 0 (cdr state)) (< (cdr state) time))
        (incf pressure (* rate (cdr state)))
        (decf time (cdr state))
        (incf rate (gethash :rate (gethash (car state) valves))))
      (return-from day16-traverse (+ pressure (* rate time)))))
  (loop with (current . other) = (if (< (cdr state-a) (cdr state-b))
                                     (cons state-a state-b)
                                     (cons state-b state-a))
        repeat 1
        for option = (day16-best (car current) closed valves time)
        for new-closed = (remove option closed)
        for distance = (day16-find-distance (car current) option valves)
        for new = (cons option (1+ distance))
        for target = (gethash (if (< (cdr new) (cdr other)) (car new) (car other)) valves)
        for time-spent = (min (cdr new) (cdr other))
        for new-time = (- time time-spent)
        for new-rate = (+ rate (gethash :rate target))
        for new-pressure = (+ pressure (* time-spent rate))
        maximizing (if (< 0 new-time)
                       (day16-traverse
                        valves
                        (cons (car new) (- (cdr new) time-spent))
                        (cons (car other) (- (cdr other) time-spent))
                        new-closed new-rate new-pressure new-time)
                       (day16-traverse valves current other NIL rate pressure time))))

(defun day16-puzzle2 ()
  (let* ((valves (day16-parse-input))
         (closed (loop for valve in (alexandria:hash-table-values valves)
                       when (< 0 (gethash :rate valve)) collect (gethash :valve valve))))
    (day16-traverse valves (cons :aa 0) (cons :aa 0)
                    (sort closed #'> :key #'(lambda (x) (gethash :rate (gethash x valves))))
                    0 0 26)))
