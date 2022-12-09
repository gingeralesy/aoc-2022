#|
This file is a part of aoc-2022
(c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2022)

(defparameter *day7-command-re* (cl-ppcre:create-scanner "^\\$ (cd|ls)( (\\S+))?$"))

(defparameter *day7-content-re* (cl-ppcre:create-scanner "^(dir|\\d+) (\\S+)$"))

(defun day7-parse-output (line)
  (cond
    ((char= #\$ (char line 0))
     (multiple-value-bind (match groups)
         (cl-ppcre:scan-to-strings *day7-command-re* line)
       (unless match (error "Invalid command: ~a" line))
       (list
        (if (string= "cd" (aref groups 0)) :cd :ls)
        (aref groups 2))))
    (T
     (multiple-value-bind (match groups)
         (cl-ppcre:scan-to-strings *day7-content-re* line)
       (unless match (error "Invalid output: ~a" line))
       (let ((dir-p (string= "dir" (aref groups 0))))
         (if dir-p
             (list :dir (aref groups 1) :size 0 :children NIL)
             (list :file (aref groups 1) :size (parse-integer (aref groups 0)))))))))

(defun day7-find-dir (current name)
  (loop for child in (getf current :children)
        until (string= name (getf child :dir))
        finally (return child)))

(defun day7-dir-tree ()
  (with-open-file (stream (input 7) :if-does-not-exist :error)
    (loop with root = (list :dir "/" :size 0 :children NIL)
          with last-command = NIL
          with dir-stack = NIL
          for line = (clean (read-line stream NIL))
          while line
          do (let ((output (day7-parse-output line)))
               (ecase (first output)
                 (:ls (setf last-command :ls))
                 (:cd
                  (setf last-command :cd)
                  (cond
                    ((string= "/" (second output))
                     (push root dir-stack))
                    ((string= ".." (second output))
                     (pop dir-stack))
                    (T
                     (push (day7-find-dir (car dir-stack) (second output)) dir-stack)))
                  (unless dir-stack (error "Invalid directory: ~a" line)))
                 ((:dir :file)
                  (unless (eql last-command :ls) (error "Invalid output: ~a" line))
                  (push output (getf (car dir-stack) :children)))))
          finally (return root))))

(defun day7-calculate-dir-sizes (dir)
  (unless (eql :dir (first dir)) (error "Not a directory: ~a" (second dir)))
  (loop for child in (getf dir :children)
        when (eql :dir (first child))
        do (day7-calculate-dir-sizes child)
        summing (getf child :size) into size
        finally (setf (getf dir :size) size))
  dir)

(defun day7-puzzle1 ()
  (let ((root (day7-calculate-dir-sizes (day7-dir-tree))))
    (labels ((find-dir (dir)
               (loop for child in (getf dir :children)
                     when (and (eql :dir (first child)))
                     append (find-dir child) into dirs
                     finally (return
                               (if (< (getf dir :size) 100000)
                                   (nconc (list dir) dirs)
                                   dirs)))))
      (loop for dir in (find-dir root)
            sum (getf dir :size)))))

;; 1582412

(defun day7-puzzle2 ()
  (let* ((root (day7-calculate-dir-sizes (day7-dir-tree)))
         (space-needed (- 30000000 (- 70000000 (getf root :size)))))
    (labels ((find-dirs (dir)
               (loop for child in (getf dir :children)
                     when (and (eql :dir (first child)))
                     append (find-dirs child) into dirs
                     finally (return (nconc (list dir) dirs)))))
      (loop with smallest = (getf root :size)
            for dir in (find-dirs root)
            for size = (getf dir :size)
            when (and (<= space-needed size) (< size smallest))
            do (setf smallest size)
            finally (return smallest)))))

;; 3696336
