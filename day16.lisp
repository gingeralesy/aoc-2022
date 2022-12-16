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
                   (when old
                     (update-table
                      (gethash (car old) valves) target (gethash :valve current) (1+ distance))))))
             (seek (current target passed)
               (declare (type hash-table current))
               (declare (type keyword target))
               (declare (type list passed))
               (let ((new-passed (append (list (gethash :valve current)) passed)))
                 (unless (get target (gethash :table current)) ;; Already known?
                   (loop for tunnel in (gethash :tunnels current)
                         for tunnel-valve = (gethash tunnel valves)
                         for tunnel-table = (unless (find tunnel new-passed)
                                              (seek tunnel-valve target new-passed))
                         when tunnel-table
                         do (loop for target in (alexandria:hash-table-keys tunnel-table)
                                  for new = (gethash target tunnel-table)
                                  do (update-table current target tunnel (1+ (cdr new))))))
                 (the hash-table (gethash :table current)))))
      (cdr (gethash to (seek from to NIL))))))

(defun day16-seek-route (valves current closed rate pressure time)
  (let ((current (etypecase current (keyword (gethash current valves)) (hash-table current))))
    (when (<= time 0) (error "No time!"))
    (unless closed
      (return-from day16-seek-route
        (values (+ pressure (* rate time)) (list (gethash :valve current)))))
    (loop with max-pressure = 0
          with max-route = NIL
          for option in closed
          for distance = (day16-find-distance (gethash :valve current) option valves)
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
                                      (values (+ pressure (* rate time)) NIL)))
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
