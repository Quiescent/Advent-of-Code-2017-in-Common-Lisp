;;; day5 --- My solution to day5 -*-

;;; Commentary:
;; My solution to advent of code: day5

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day5
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day5)

;; # PART 1:

(defun day5-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((jump-table (map 'vector #'read-from-string input-elements)))
    (iter
      (for count from 1)
      (with ptr = 0)
      (for add = (aref jump-table ptr))
      (incf (aref jump-table ptr))
      (incf ptr add)
      (while (and (>= ptr 0)
                  (< ptr (length jump-table))))
      (finally (return count)))))

;; # PART 2:

(defun day5-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
(let ((jump-table (map 'vector #'read-from-string input-elements)))
    (iter
      (for count from 1)
      (with ptr = 0)
      (for add = (aref jump-table ptr))
      (incf (aref jump-table ptr) (if (>= add 3) -1 1))
      (incf ptr add)
      (while (and (>= ptr 0)
                  (< ptr (length jump-table))))
      (finally (return count)))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("0"
                   "3"
                   "0"
                   "1"
                   "-3"))
        (expected-1 5))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day5-part-1 input-1))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day5-part-1"))
        (input-2 (file-lines "day5-part-1")))
    (format t "
Part 1: ~s
" (day5-part-1 input-1))
    (format t "
Part 2: ~s
" (day5-part-2 input-2))))

