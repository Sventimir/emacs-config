;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'range)
(require 'evil)

(defun lowercase-current-char ()
  "Replace current char with its lowercase version."
  (let ((char (char-after)))
    (delete-char 1)
    (insert (char-to-string (+ char 32)))
    (backward-char)))

(defun upcase-current-char ()
  "Replace current char with its uppercase version."
  (let ((char (char-after)))
    (delete-char 1)
    (insert (char-to-string (- char 32)))
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

(defun transform-snake-until-end-of-word (trans-underscore)
  "Convert the snake-case word at point using TRANS-UNDERSCORE."
  (forward-char)
  (let ((char (char-after)))
    (cond
     ((or (small-letter-p char) (digit-p char) (capital-letter-p char))
      (progn
        (transform-snake-until-end-of-word trans-underscore)))
     ((equal char ?_)
      (progn
        (funcall trans-underscore nil)
        (transform-snake-until-end-of-word trans-underscore)))
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
       (t (message "transform-camel-case only makes sense for words: '%c'" c))))))

(defun transform-snake-case (trans-underscore)
  "Transform the word at point by applying TRANS-UNDERSCORE to every underscore."
  (save-excursion
    (forward-char)
    (forward-word -1)
    (let ((c (char-after)))
      (cond
       ((or (small-letter-p c) (digit-p c)) (transform-snake-until-end-of-word trans-underscore))
       ((equal c ?_) (progn
                               (funcall trans-underscore t)
                               (transform-camel-until-end-of-word trans-underscore)))
       (t (message "transform-snake-case only makes sense for words: '%c'" c))))))

(defun to-snake-case ()
  "Convert the word at point to snake case."
  (interactive)
  (transform-camel-case 'cap-to-snake))

(defun split-camel-case ()
  "Insert a space before each capital letter in word at point except the first."
  (interactive)
  (transform-camel-case (lambda (first-p)
                          (if (not first-p) (insert " ")))))

(defun to-camel-case ()
  "Convert the word at point to camel case."
  (interactive)
  (transform-snake-case (lambda (first-p)
                          (if (not first-p) (delete-char 1))
                          (upcase-current-char))))

;; Inserting special characters
(defmacro lambda-insert (chrs)
  "Return a lambda inserting CHRS."
  `(lambda () (interactive) (insert ,chrs)))

(unbind-key (kbd "C-k") evil-insert-state-map)
(unbind-key (kbd "C-k") global-map)
(define-key evil-insert-state-map (kbd "C-k c") (lambda-insert "♣"))
(define-key evil-insert-state-map (kbd "C-k d") (lambda-insert "♦"))
(define-key evil-insert-state-map (kbd "C-k h") (lambda-insert "♥"))
(define-key evil-insert-state-map (kbd "C-k s") (lambda-insert "♠"))

(provide 'editing)
;;; editing.el ends here
