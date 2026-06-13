;;; -*- lexical-binding: t -*-
;;; Package: Combinatorics
;;; Commentary:
;;; A set of simple combinatorics functions.
;;; Code:
(require 'cl-lib)
(require 'range)
(require 'seq)

(defun range-multiply (low hi)
  "Multiply the numbers in range from LOW to HI (inclusive) together."
  (apply '* (range-uncompress (list (cons low hi)))))

(defun factorial (n)
  "Compute the factorial of N."
  (range-multiply 1 n))

(defun choose (n k)
  "Compute the number of choices of K elements of a set of N elements."
  (/ (range-multiply (1+ k) n) (factorial (- n k))))

(defun selections (set)
  "Convert SET into a list of cionsecutive heads and their tails."
  (if (seq-empty-p set)
      nil
    (let ((tail (seq-drop set 1)))
      (cons (cons (seq-first set) tail) (selections tail)))))

(defun combine (sel)
  "Combine first element of SEL with all possible elements of the second element."
  (cons (list (car sel))
        (mapcar (lambda (cont) (cons (car sel) cont)) (choices* (cdr sel)))))

(defun choices* (set)
  "Compute all the possible subsets of SET."
  (mapcan 'combine (selections set)))

(defun choices (set)
  "Compute all the possible subsets of SET, including the empty set."
  (cons nil (choices* set)))

(defun complements (set subset)
  "Pair SUBSET with its complement in SET."
  (let ((cs (cl-set-difference (string-to-list set) (string-to-list subset))))
    (if (eq (type-of set) 'string)
        (apply 'string cs)
      cs)))

(defun splits (set)
  "Create a list of all possible splits of SET into 2 subsets."
  (let ((set* (if (stringp set) (string-trim set) set)))
    (mapcar
     (lambda (subset)
       (list
        (if (eq (type-of set) 'string) (apply 'string subset) subset)
        (complements set* subset)))
     (seq-sort-by 'length '< (choices set*)))))

(provide 'combinatorics)
;;; combinatorics.el ends here
