;;; polynomial -- Polynomial evaluation and arithmetic
;;; Commentary:
;;; Code:

(defun polynomial--eval-intern (acc e p x)
  "Evaluate (expt X E) times (car P) and add to ACC."
  (if (car p)
      (polynomial--eval-intern (+ acc (* (car p) (expt x e))) (+ e 1) (cdr p) x)
      acc))

(defun polynomial-eval (p x)
  "Evaluate polynomial P at X."
  (polynomial--eval-intern 0 0 (reverse p) x))

(defun polynomial-sum (acc ps)
  "Add the sum of first coefficients of PS to ACC and recur."
  (let ((heads (seq-filter 'identity (mapcar 'car ps)))
        (tails (seq-filter 'identity (mapcar 'cdr ps))))
    (if heads
        (polynomial-sum (cons (apply '+ heads) acc) tails)
        acc)))

(defun polynomial-add (&rest ps)
  "Return a polynomial being the sum of PS."
  (polynomial-sum nil (mapcar 'reverse ps)))

(defun polynomial--mult (acc prefix p q)
  "Multiply corresponding coefficients of P and Q into a table ACC, using PREFIX to keep track of positions."
  (if (car p)
      (polynomial--mult
       (cons
        (append prefix (mapcar (lambda (x)
                                 (* (car p) x)) q))
        acc)
       (cons 0 prefix)
       (cdr p)
       q)
    acc))

(defun polynomial--prod (ps)
  "Multiply already reversed coefficients of PS together."
  (let ((fst (car ps))
        (snd (car (cdr ps)))
        (tail (cdr (cdr ps))))
    (if snd
        (polynomial--prod (cons
                           (polynomial-sum nil (polynomial--mult nil nil (reverse fst) (reverse snd)))
                           tail))
      fst)))

(defun polynomial-multiply (&rest ps)
  "Multiply PS together."
  (polynomial--prod ps))

(defun polynomial--show-coefficient (c)
  "Show the coefficient C."
  (cond ((equal c 1) "")
        ((equal c -1) "-")
        (t (number-to-string c))))

(defun polynomial--show-term (coeff e)
  "Show the term COEFF * x ^ E."
  (cond ((equal e 0) (number-to-string coeff))
        ((equal e 1) (format "%sx" (polynomial--show-coefficient coeff)))
        (t (format "%sx^%d" (polynomial--show-coefficient coeff) e))))

(defun polynomial--add-shown-term (acc e terms)
  "Append textual representation of (car TERMS) raised to power E to ACC."
  (if terms
      (polynomial--add-shown-term
       (format "%s %s"
               acc
               (cond ((equal (car terms) 0) "")
                     ((> (car terms) 0) (format "+ %s" (polynomial--show-term (car terms) e)))
                     (t (format "- %s" (polynomial--show-term (abs (car terms)) e)))))
       (- e 1)
       (cdr terms))
    acc))

(defun polynomial-show (p)
  "Return the textual representation of polynomial P."
  (if p
      (polynomial--add-shown-term
       (polynomial--show-term (car p) (- (length p) 1))
       (- (length p) 2)
       (cdr p))
      "0"))

(defun polynomial-line-from-root (root &optional coeff)
  "Return the linear polynomial with given ROOT and COEFF."
  (let ((c (or coeff 1)))
    (list c (* c (- root)))))

(defun polynomial-from-roots (roots &optional coeff)
  "Return the polynomial with given ROOTS and COEFF."
  (let ((root-lines (mapcar 'polynomial-line-from-root roots)))
    (apply 'polynomial-multiply (if coeff
                                    (cons (list coeff) root-lines)
                                    root-lines))))

(provide 'polynomial)
;;; polynomial.el ends here
