;;; numeric --- Standard operations on numbers
;;; Commentary:
;;; Code:

(defun numeric-nat-div-rounded-up (a b)
  "Divide integers A and B rounding up."
  (if (and (natnump a) (natnump b))
      (/ (+ a b -1) b)))

(provide 'numeric)
;;; numeric.el ends here
