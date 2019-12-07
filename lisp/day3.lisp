;;; day3 --- My solution to day3 -*-

;;; Commentary:
;; My solution to advent of code: day3

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day3
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day3)

;; # PART 1:

(defun day3-part-1 (address)
  "Run my solution to part one of the problem on the input in ADDRESS."
  (multiple-value-bind (length ring) (ring-stats address)
    (format t "Ring: ~a~%" ring)
    (format t "Len:  ~a~%" (1- length))
    (format t "Mid:  ~a~%" (floor length 2))
    (let ((dist-along (mod (- address (* (- length 2)  (- length 2))) (1- length)))
          (mid        (floor length 2)))
      (format t "Along: ~a~%" dist-along)
      (format t "Dist: ~a~%" (- mid (- (max dist-along mid)
                                       (min dist-along mid))))
      (- (* 2 ring)
         (- mid (- (max dist-along mid)
                   (min dist-along mid)))))))

(defun ring-stats (n)
  (iter
    (for j from 0)
    (for i from 1 by 2)
    (when (> (* i i) n)
      (return (values i j)))))

;; # PART 2:

(defun day3-part-2 (address)
  "Run my solution to part two of the problem on the input in ADDRESS."
  (iter
    (with length = (floor (sqrt address)))
    (with grid   = (make-array (list length length) :initial-element nil))
    (with x = (floor length 2))
    (with y = (floor length 2))
    (initially
     (progn (setf (aref grid x y) 1)
            (incf x)))
    (setf (aref grid x y)
          (sum-surrounding x y grid))
    (while (< (aref grid x y) address))
    (cond
      ((and (aref grid (1- x) y)
            (not (aref grid x (1- y))))
       (decf y))
      ((and (aref grid x (1+ y))
            (not (aref grid (1- x) y)))
       (decf x))
      ((and (aref grid (1+ x) y)
            (not (aref grid x (1+ y))))
       (incf y))
      ((and (aref grid x (1- y))
            (not (aref grid (1+ x) y)))
       (incf x)))
    (finally (return (aref grid x y)))))

(defun sum-surrounding (x y grid)
  (+ (or (aref grid (1+ x) y)      0)
     (or (aref grid (1- x) y)      0)
     (or (aref grid x      (1+ y)) 0)
     (or (aref grid (1+ x) (1+ y)) 0)
     (or (aref grid (1- x) (1+ y)) 0)
     (or (aref grid x      (1- y)) 0)
     (or (aref grid (1+ x) (1- y)) 0)
     (or (aref grid (1- x) (1- y)) 0)))

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 1024)
;;         (expected-1 31)
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day3-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day3-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 368078)
        (input-2 368078))
    (format t "
Part 1: ~s
" (day3-part-1 input-1))
    (format t "
Part 2: ~s
" (day3-part-2 input-2))))

