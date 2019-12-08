;;; day7 --- My solution to day7 -*-

;;; Commentary:
;; My solution to advent of code: day7

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day7
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day7)

;; # PART 1:

(defun day7-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((entries (mapcar #'parse-entry input-elements)))
    (iter
      (with all-nodes = (remove-duplicates (mapcar #'cadr entries) :test #'equal))
      (for node in all-nodes)
      (finding node
               such-that
               (not (position-if (lambda (other-entry) (position node (cddr other-entry) :test #'equal))
                                 entries))))))

(defun parse-entry (line)
  (let ((nodes  (remove "" (ppcre:split "[ (),->0-9]" line) :test #'equal))
        (weight (read-from-string
                 (car (remove ""
                              (ppcre:split " " (ppcre:regex-replace-all "[^0-9]" line " "))
                              :test #'equal)))))
    (cons weight nodes)))

;; # PART 2:

(defun day7-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((entries (mapcar #'parse-entry input-elements))
        (root    (day7-part-1 input-elements)))
    (format t "Entries: ~a~%" entries)
    (find-unbalanced root entries)))

(defun find-unbalanced (current entries)
  (let* ((entry   (find current entries :key #'cadr :test #'equal))
         (others  (cddr entry))
         (weights (mapcar (lambda (other) (find-weight other entries)) others)))
    (format t "entry: ~a~%others: ~a~%weights: ~a~%" entry others weights)
    (cond
      ((or (null others) (eq 1 (length (remove-duplicates weights :test #'equal)))) nil)
      (t (let ((sub-unbalanced (find-unbalanced (nth (find-least-common-position weights)
                                                     others)
                                                entries)))
           (if sub-unbalanced
               sub-unbalanced
               (let* ((pos   (find-least-common-position weights)))
                 (- (car (find (nth pos others) entries :key #'cadr :test #'equal))
                    (- (nth pos weights)
                       (find-more-common weights))))))))))

(defun find-weight (current entries)
  (let* ((entry   (find current entries :key #'cadr :test #'equal))
         (weight  (car entry))
         (others  (cddr entry))
         (weights (mapcar (lambda (other) (find-weight other entries)) others)))
    (+ weight (apply #'+ weights))))

(defun find-more-common (xs)
  (iter
    (for x in xs)
    (finding x maximizing (count-if (lambda (y) (equal y x)) xs))))

(defun find-least-common-position (xs)
  (iter
    (for x in xs)
    (for i from 0)
    (finding i minimizing (count-if (lambda (y) (equal y x)) xs))))

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
" expected-1 (day7-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day7-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day7-part-1"))
        (input-2 (file-lines "day7-part-1")))
    (format t "
Part 1: ~s
" (day7-part-1 input-1))
    (format t "
Part 2: ~s
" (day7-part-2 input-2))))

