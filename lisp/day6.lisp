;;; day6 --- My solution to day6 -*-

;;; Commentary:
;; My solution to advent of code: day6

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day6
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day6)

;; # PART 1:

(defun day6-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((banks (map 'vector #'identity (car input-elements))))
    (iter
      (for count from 0)
      (with seen = (make-hash-table :test #'equal))
      (when (gethash (map 'list #'identity banks) seen)
        (return (values count banks)))
      ;(print banks)
      (when (> count 10000)
        (format t "BAILING")
        (return nil))
      (setf (gethash (map 'list #'identity banks) seen) t)
      (for (i . block) = (iter (for block in-vector banks) (for i from 0) (finding (cons i block) maximizing block)))
      (setf (aref banks i) 0)
      (incf i)
      (setf i (mod i (length banks)))
      (iter
        (while (> block 0))
        (incf (aref banks i))
        (decf block)
        (incf i)
        (setf i (mod i (length banks)))))))

;; # PART 2:

(defun day6-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (multiple-value-bind (_ start) (day6-part-1 input-elements)
    (day6-part-1 (list (map 'list #'identity start)))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '((0 2 7 0)))
        (expected-1 5)
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day6-part-1 input-1))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day6-part-1"))
        (input-2 (file-lines-numbers "day6-part-1")))
    (format t "
Part 1: ~s
" (day6-part-1 input-1))
    (format t "
Part 2: ~s
" (day6-part-2 input-2))))

