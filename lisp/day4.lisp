;;; day4 --- My solution to day4 -*-

;;; Commentary:
;; My solution to advent of code: day4

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day4
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day4)

;; # PART 1:

(defun day4-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for line in input-elements)
    (sum (iter
           (for words on line)
           (when (position (car words) (cdr words) :test #'equal)
             (return 0))
           (finally (return 1))))))

;; # PART 2:

(defun day4-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for line in input-elements)
    (sum (iter
           (for words on (mapcar #'letter-content line))
           (when (position (car words) (cdr words) :test #'equal)
             (return 0))
           (finally (return 1))))))

(defun letter-content (word)
  (run-length-encode (sort (map 'list #'identity word) #'char-lessp)))

(defun run-length-encode (xs)
  (labels ((iter (ys y count)
                 (cond ((null ys)       (cons y count))
                       ((eq y (car ys)) (iter (cdr ys) y (1+ count)))
                       (t               (cons (cons y count)
                                              (iter (cdr ys) (car ys) 1))))))
    (iter (cdr xs) (car xs) 1)))

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
" expected-1 (day4-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day4-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-words "day4-part-1"))
        (input-2 (file-lines-words "day4-part-1")))
    (format t "
Part 1: ~s
" (day4-part-1 input-1))
    (format t "
Part 2: ~s
" (day4-part-2 input-2))))

