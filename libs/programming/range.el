;;; Package --- Summary
;;; Commentary:
;;; Code:

(cl-defstruct range start start-open-p end end-open-p)

(defun in-range (range value)
  "Return t if VALUE is in the RANGE, inclusive."
  (let ((start (range-start range))
        (end (range-end range)))
    (and
     (if (range-start-open-p range) (> value start) (>= value start))
     (if (range-end-open-p range) (< value end) (<= value end)))))

(defun make-open-range (start end)
  "Return a range from START to END, exclusive."
  (make-range :start start :start-open-p t :end end :end-open-p t))

(defun make-closed-range (start end)
  "Return a range from START to END, inclusive."
  (make-range :start start :start-open-p nil :end end :end-open-p nil))

(defun range-enumerate (range)
  "Create a list of all the integers in RANGE."
  (let ((result nil)
        (counter (range-end range))
        (stop (range-start range))
        (cmp (if (range-start-open-p range) '> '>=)))
    (if (range-end-open-p range) (setq counter (1- counter)))
    (while (funcall cmp counter stop)
      (add-to-list 'result counter)
      (setq counter (1- counter)))
    result))

(defun small-letter-p (char)
  "Return t if CHAR is a small letter."
  (in-range (make-closed-range 97 122) char))

(defun capital-letter-p (char)
  "Return t if CHAR is a capital letter."
  (in-range (make-closed-range 65 90) char))

(defun digit-p (char)
  "Return t if CHAR is a digit."
  (in-range (make-closed-range 48 57) char))

(provide 'range)
;;; range.el ends here
