;;; numeric --- Standard operations on numbers
;;; Commentary:
;;; Code:

(defun numeric-nat-div-rounded-up (a b)
  "Divide integers A and B rounding up."
  (if (and (natnump a) (natnump b))
      (/ (+ a b -1) b)))

(defun gcd (a b)
  "Find the greates common denominator of A and B."
  (let ((x (max a b)) (y (min a b)))
    (while (> y 0)
      (let ((r (% x y)))
        (setq x y
              y r)))
    x))

(defun insert-sep (str sep block)
  "Insert SEP into STR every BLOCK characters counting from the end."
  (if (> (length str) block)
      (concat (insert-sep (substring str 0 (- (length str) block)) sep block)
              sep
              (substring str (- (length str) block) (length str)))
    str))

(defun insert-sep-region (sep block)
  "Insert SEP into the selected region every BLOCK characters."
  (let* ((str (buffer-substring (mark) (point)))
         (replacement (insert-sep str sep block)))
  (progn
    (delete-region (mark) (point))
    (insert replacement))))

(provide 'numeric)
;;; numeric.el ends here
