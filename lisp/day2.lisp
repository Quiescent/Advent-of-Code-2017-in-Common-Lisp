;;; day2 --- My solution to day2 -*-

;;; Commentary:
;; My solution to advent of code: day2

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")

(defpackage :day2
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :iter))

(in-package :day2)

;; # PART 1:

(defun day2-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for line in input-elements)
    (sum (iter
           (for number in line)
           (maximizing number into max-num)
           (minimizing number into min-num)
           (finally (return (- max-num min-num)))))))

;; # PART 2:

(defun day2-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for line in input-elements)
    (sum (iter outer
               (for number in line)
               (iter
                 (for other-number in line)
                 (when (and (not (eq number other-number))
                            (eq 0 (mod (max number other-number)
                                       (min number other-number))))
                   (return-from outer
                     (floor (max number other-number)
                            (min number other-number)))))))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '())
        (expected-1 '())
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day2-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day2-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day2-part-1"))
        (input-2 (file-lines-numbers "day2-part-1")))
    (format t "
Part 1: ~s
" (day2-part-1 input-1))
    (format t "
Part 2: ~s
" (day2-part-2 input-2))))

