;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'range)

(defun lowercase-current-char ()
  "Replace current char with its lowercase version."
  (let ((char (char-after)))
    (delete-char 1)
    (insert (char-to-string (+ char 32)))
    (backward-char)))

(defun transform-camel-until-end-of-word (trans-cap)
  "Convert the word at point using TRANS-CAP."
  (forward-char)
  (let ((char (char-after)))
    (cond
     ((or (small-letter-p char) (digit-p char))
      (progn
        (transform-camel-until-end-of-word trans-cap)))
     ((capital-letter-p char) (progn
                                (funcall trans-cap nil)
                                (transform-camel-until-end-of-word trans-cap)))
     (t nil))))

(defun cap-to-snake (first-p)
  "Convert character at point to lowercase; insert '_' unless FIRST-P."
  (if (not first-p) (insert "_"))
  (lowercase-current-char))

(defun transform-camel-case (trans-cap)
  "Transform the word at point by applying TRANS-CAP to every capital letter."
  (save-excursion
    (forward-char)
    (forward-word -1)
    (let ((c (char-after)))
      (cond
       ((or (small-letter-p c) (digit-p c)) (transform-camel-until-end-of-word trans-cap))
       ((capital-letter-p c) (progn
                               (funcall trans-cap t)
                               (transform-camel-until-end-of-word trans-cap)))
       (t (message "tramsform-camel-case only makes sense for words: '%c'" c))))))

(defun to-snake-case ()
  "Convert the word at point to snake case."
  (interactive)
  (transform-camel-case 'cap-to-snake))

(defun split-camel-case ()
  "Insert a space before each capital letter in word at point except the first."
  (interactive)
  (transform-camel-case (lambda (first-p)
                          (if (not first-p) (insert " ")))))

(provide 'editing)
;;; editing.el ends here

