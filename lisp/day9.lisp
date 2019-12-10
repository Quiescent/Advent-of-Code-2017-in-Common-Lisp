;;; day9 --- My solution to day9 -*-

;;; Commentary:
;; My solution to advent of code: day9

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day9
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day9)

;; # PART 1:

(defun day9-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (parse (car input-elements)))

(defun parse (str)
  (iter
    (with level = 0)
    (with mode  = nil)
    (with skip)
    (for char in-string str)
    (when skip
      (setf skip nil)
      (next-iteration))
    ;(format t "mode: ~a, level: ~a, char: ~a, skip: ~a~%" mode level char skip)
    (case char
      (#\{ (when (not (eq 'garbadge mode))
             (setf mode 'group)
             (incf level)))
      (#\} (when (and (not (eq 'garbadge mode))
                      (> level 0))
             (sum level)
             (decf level)))
      (#\! (when (eq mode 'garbadge)
             (setf skip t)))
      (#\< (when (not (eq 'garbadge mode))
             (setf mode 'garbadge)))
      (#\> (when (eq 'garbadge mode)
             (setf mode 'group))))))

;; # PART 2:

(defun day9-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (count-garbadge (car input-elements)))

(defun count-garbadge (str)
  (iter
    (with level = 0)
    (with mode  = nil)
    (with skip)
    (for char in-string str)
    (when skip
      (setf skip nil)
      (next-iteration))
    ;;(format t "mode: ~a, level: ~a, char: ~a, skip: ~a~%" mode level char skip)
    (count (and (eq mode 'garbadge)
                (not (eq char #\>))
                (not (eq char #\!))))
    (case char
      (#\{ (when (not (eq 'garbadge mode))
             (setf mode 'group)
             (incf level)))
      (#\} (when (and (not (eq 'garbadge mode))
                      (> level 0))
             (decf level)))
      (#\! (when (eq mode 'garbadge)
             (setf skip t)))
      (#\< (when (not (eq 'garbadge mode))
             (setf mode 'garbadge)))
      (#\> (when (eq 'garbadge mode)
             (setf mode 'group))))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("{{<a!>},{<a!>},{<a!>},{<ab>}}"))
        (expected-1 3)
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day9-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day9-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day9-part-1"))
        (input-2 (file-lines "day9-part-1")))
    (format t "
Part 1: ~s
" (day9-part-1 input-1))
    (format t "
Part 2: ~s
" (day9-part-2 input-2))))

