;;; day10 --- My solution to day10 -*-

;;; Commentary:
;; My solution to advent of code: day10

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day10
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day10)

;; # PART 1:

(defun day10-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((lengths (mapcar #'read-from-string (ppcre:split "," (car input-elements))))
        (numbers (map 'vector #'identity '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255))))
    (iter
      (with start = 0)
      (with skip = 0)
      (for len in lengths)
      (reverse-range numbers start (+ start len))
      (incf start (+ len skip))
      (incf skip))
    (* (aref numbers 0)
       (aref numbers 1))))

(defun reverse-range (xs start end)
  (iter
    (with full-length = (length xs))
    (with sub-length  = (- end start))
    (with original = (copy-seq xs))
    (for i from 0 below sub-length)
    (setf (aref xs (mod (+ start i) full-length))
          (aref original (mod (+ start (1- (- sub-length i))) full-length)))))

;; # PART 2:

(defun day10-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((lengths (concatenate 'list (to-ascii-codes (car input-elements)) '(17 31 73 47 23)))
        (numbers (map 'vector #'identity '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255))))
    (iter
      (for i from 0 below 64)
      (with start = 0)
      (with skip = 0)
      (iter
        (for len in lengths)
        (reverse-range numbers start (+ start len))
        (incf start (+ len skip))
        (incf skip)))
    (apply #'concatenate 'string
           (iter
             (for i from 0 below 255 by 16)
             (collect (format nil "~2,'0x" (reduce #'logxor (subseq numbers i (+ i 16)))))))))

(defun to-ascii-codes (str)
  (iter
    (for char in-string str)
    (collect (char-code char))))

;; 17, 31, 73, 47, 23

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
;; " expected-1 (day10-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day10-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day10-part-1"))
        (input-2 (file-lines "day10-part-1")))
    (format t "
Part 1: ~s
" (day10-part-1 input-1))
    (format t "
Part 2: ~s
" (day10-part-2 input-2))))

