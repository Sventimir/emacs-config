;;; -*- lexical-binding: t -*-
;;; Package: Combinatoric
;;; Commentary:
;;; A set of simple combinatorics functions.
;;; Code:
(require 'cl-lib)

(defun selections (set)
  "Convert SET into a list of cionsecutive heads and their tails."
  (if (string-empty-p set)
      nil
    (let ((tail (substring set 1)))
      (cons (cons (substring set 0 1) tail) (selections tail)))))

(defun combine (sel)
  "Combine first element of SEL with all possible elements of the second element."
  (cons (list (car sel))
        (mapcar (lambda (cont) (cons (car sel) cont)) (choices* (cdr sel)))))

(defun choices* (set)
  "Compute all the possible subsets of SET."
  (mapcan 'combine (selections set)))

(defun choices (set)
  "Compute all the possible usbsets of SET, including the empty set."
  (cons "" (mapcar (lambda (l) (apply 'concat l)) (choices* set))))

(defun complements (set subset)
  "Pair SUBSET with its complement in SET."
  (apply 'string (cl-set-difference (string-to-list set) (string-to-list subset))))

(defun splits (set)
  "Create a list of all possible splits of SET into 2 subsets."
  (let ((set* (string-trim set)))
    (mapcar
     (lambda (subset) (list subset (complements set* subset)))
     (seq-sort-by 'length '< (choices set*)))))

(provide 'combinatoric)
;;; combinatorics.el ends here
