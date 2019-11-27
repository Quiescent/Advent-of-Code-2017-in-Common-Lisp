;;; day1 --- My solution to day1 -*-

;;; Commentary:
;; My solution to advent of code: day1

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")

(defpackage :day1
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :iter))

(in-package :day1)

;; # PART 1:

(defun day1-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for char in-string (car input-elements))
    (for p-char previous char)
    (when (eq char p-char)
      (summing (read-from-string (string char)) into total-sum))
    (finally (return (+ total-sum
                        (if (eq (aref (car input-elements)
                                      (1- (length (car input-elements))))
                                (aref (car input-elements) 0))
                            (read-from-string (string (aref (car input-elements) 0)))
                            0))))))

;; # PART 2:

(defun day1-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (with line = (car input-elements))
    (for i from 0 below (length line))
    (when (eq (aref line i)
              (aref line (mod (+ i (floor (length line) 2)) (length line))))
      (summing (read-from-string (string (aref line i)))))))

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day1-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day1-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day1-part-1"))
        (input-2 (file-lines "day1-part-1")))
    (format t "
Part 1: ~s
" (day1-part-1 input-1))
    (format t "
Part 2: ~s
" (day1-part-2 input-2))))

