;;; -*- lexical-binding: t -*-
;;; Package: Combinatorics
;;; Commentary:
;;; A set of simple combinatorics functions.
;;; Code:


(defun seq (from to)
  "Return a list of numbers from FROM to TO."
  (let ((res nil)
        (c from))
    (while (<= c to)
      (push c res)
      (setq c (1+ c)))
    res))

(defun const (val)
  "Return a function that always returns VAL."
  (lambda (_) val))

(defun list-bind (l f)
  "Apply F to every element of L, flattening the resultsing list of lists."
  (let ((res nil))
    (dolist (l (mapcar f l) res)
      (setq res (append res l)))))

(defun all-sums (total &rest bounds)
  "Find all the lists of numbers bounded by BOUNDS summing up to TOTAL."
  (cond ((null bounds) nil)
        ((null (cdr bounds)) (if (>= (car bounds) total) (list (list total))))
        (t (list-bind (seq 0 (min (car bounds) total))
                      (lambda (x) (mapcar (lambda (xs) (cons x xs)) (apply 'all-sums (- total x) (cdr bounds))))))))

(defun prod (k n)
  "Compute the product of natural numbers between K and N."
  (let ((result 1)
        (c k))
    (while (<= c n)
      (setq result (* result c)
            c (1+ c)))
    result))

(defun combs (n k)
  "Compute the number of combinations of K out of N elements."
  (/ (prod (1+ k) n) (prod 2 (- n k))))
       

(defun comb-sets (&rest ss)
  "Given sets SS, being pairs of T and L, compute the probability of drawing exactly L elements out of T frome each."
  (apply '* (mapcar (lambda (s) (apply 'combs s)) ss)))

(provide 'combinatorics)
;;; combinatorics.el ends here
