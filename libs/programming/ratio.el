;;; -*- lexical-binding: t -*-
;;; Package --- Ratio
;;; Commentary:
;;; A package for working with precise rational numbers.
;;; Code:
(require 'calc-misc)
(require 'cl-macs)
(require 'numeric)

(cl-defstruct ratio
  (numerator 0 :read-only t)
  (denominator 1 :read-only t))

(defun ratio (numerator &optional denominator)
  "Construct a ration of NUMERATOR to DENOMINATOR."
  (ratio-normalize (make-ratio :numerator numerator  :denominator (or denominator 1))))

(defun ratio-signum (r)
  "Return -1 if R is negative, 1 if R is positive or 0 if it's 0."
  (cl-signum (ratio-numerator r)))

(defun ratio-normalize (r)
  "Return a canonical form of the ratio R where numerator and denominator are relatively prime."
  (let* ((num (abs (ratio-numerator r)))
         (denom (abs (ratio-denominator r)))
         (d (gcd num denom)))
    (make-ratio :numerator (* (cl-signum (ratio-numerator r)) (cl-signum (ratio-denominator r)) (/ num d))
                :denominator (/ denom d))))

(defun ratio-to-float (r)
  "Convert ratio R to a float."
  (/ (ratio-numerator r) 1.0 (ratio-denominator r)))

(defun ratio+ (&rest rs)
  "Return the sum of RS."
  (let ((denom (seq-reduce
                (lambda (a b) (/ (* a b) (gcd a b)))
                (mapcar 'ratio-denominator (cdr rs))
                (if (car rs) (ratio-denominator (car rs)) 1))))
    (ratio (apply '+ (mapcar (lambda (r) (/ (* denom (ratio-numerator r)) (ratio-denominator r))) rs)) denom)))

(defun ratio- (&rest rs)
  "If RS is longer than 2, return the difference of the first and the sum of the rest; negative element or 0 otherwise."
  (cond ((null rs) (ratio 0))
        ((null (cdr rs)) (ratio (- (ratio-numerator (car rs))) (ratio-denominator (car rs))))
        (t (apply 'ratio+ (car rs) (mapcar 'ratio- (cdr rs))))))

(defun ratio* (&rest rs)
  "Return the product of RS."
  (ratio (apply '* (mapcar 'ratio-numerator rs))
         (apply '* (mapcar 'ratio-denominator rs))))

(defun ratio/ (&rest rs)
  "If RS is singleton, return reciprocal of the element, otherwise divide the first element by the product of the rest."
  (cond ((null rs) (signal 'error '(wrong-number-of-argument ratio/ 0)))
        ((null (cdr rs)) (ratio (ratio-denominator (car rs)) (ratio-numerator (car rs))))
        (t (apply 'ratio* (car rs) (mapcar 'ratio/ (cdr rs))))))

(defun ratio-pow (base exp)
  "Raise ratio BASE to the integral EXP power."
  (let ((r (if (< exp 0) (ratio/ base) base))
        (e (abs exp)))
    (ratio (math-pow (ratio-numerator r) e)
           (math-pow (ratio-denominator r) e))))

(provide 'ratio)
;;; ratio.el ends here
