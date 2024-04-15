;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'range)

(defun capitalize-current-char ()
  "Replace current char with its lowercase version."
  (let ((char (char-after)))
    (delete-char 1)
    (insert (char-to-string (+ char 32)))))

(defun to-snake-case-until-end-of-word ()
  "Convert the word at point to snake case."
  (forward-char)
  (let ((char (char-after)))
    (cond
     ((or (small-letter-p char) (digit-p char))
      (progn
        (to-snake-case-until-end-of-word)))
     ((capital-letter-p char) (progn
                                (insert "_")
                                (capitalize-current-char)
                                (to-snake-case-until-end-of-word)))
     (t nil))))

(defun to-snake-case ()
  "Convert the word at point to snake case."
  (interactive)
  (save-excursion
    (forward-char)
    (forward-word -1)
    (let ((c (char-after)))
      (cond
       ((or (small-letter-p c) (digit-p c)) (to-snake-case-until-end-of-word))
       ((capital-letter-p c) (progn
                               (capitalize-current-char)
                               (to-snake-case-until-end-of-word)))
       (t (message "to-snake-case only makes sense for words: '%c'" c))))))

(provide 'editing)
;;; editing.el ends here
