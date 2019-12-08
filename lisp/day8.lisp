;;; day8 --- My solution to day8 -*-

;;; Commentary:
;; My solution to advent of code: day8

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day8
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day8)

;; # PART 1:

(defun day8-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((instructions (mapcar #'parse-instruction input-elements))
         (symbols      (iter (for instruction in instructions)
                             (unioning (remove-if-not #'symbolp instruction)))))
    (iter
      (for symbol in symbols)
      (set symbol 0))
    (iter
      (for instruction in instructions)
      (when (funcall (nth 3 instruction) (value (nth 4 instruction)) (value (nth 5 instruction)))
        (set (cadr instruction)
             (funcall (car instruction)
                      (value (cadr instruction))
                      (value (caddr instruction))))))
    (iter
      (for symbol in symbols)
      (maximizing (value symbol)))))

(defun value (x)
  (if (symbolp x)
      (symbol-value x)
      x))

(defun != (a b)
  (not (equal a b)))

(defun == (a b)
  (equal a b))

(defun parse-instruction (line)
  (let ((split (ppcre:split " " line)))
    (list (if (equal (cadr split) "dec") #'- #'+)
          (read-from-string (car split))
          (read-from-string (caddr split))
          (symbol-function (read-from-string (nth 5 split)))
          (read-from-string (nth 4 split))
          (read-from-string (nth 6 split)))))

;; # PART 2:

(defun day8-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((instructions (mapcar #'parse-instruction input-elements))
         (symbols      (iter (for instruction in instructions)
                             (unioning (remove-if-not #'symbolp instruction)))))
    (iter
      (for symbol in symbols)
      (set symbol 0))
    (iter
      (for instruction in instructions)
      (when (funcall (nth 3 instruction) (value (nth 4 instruction)) (value (nth 5 instruction)))
        (set (cadr instruction)
             (funcall (car instruction)
                      (value (cadr instruction))
                      (value (caddr instruction)))))
      (maximizing (car (sort (mapcar #'value symbols) #'>))))))

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
;; " expected-1 (day8-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day8-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day8-part-1"))
        (input-2 (file-lines "day8-part-1")))
    (format t "
Part 1: ~s
" (day8-part-1 input-1))
    (format t "
Part 2: ~s
" (day8-part-2 input-2))))

