;;; -*- lexical-binding: t -*-
;;; Package --- Window management
;;; Commentary:
;;; Set key bindings for window management
;;; Code:

(defun toggle-selected-window-dedicated ()
  "Toggle dedicated status of the currently selected window."
  (interactive)
  (let* ((window (selected-window))
         (status (not (window-dedicated-p window))))
    (progn
      (set-window-dedicated-p window status)
      (message "The %s %s dedicated now" window (if status "is" "is not")))))

(global-set-key (kbd "C->") (lambda () (interactive) (select-window (next-window))))
(global-set-key (kbd "C-<") (lambda () (interactive) (select-window (previous-window))))
(global-set-key (kbd "M-<down>") 'evil-window-down)
(global-set-key (kbd "M-<up>") 'evil-window-up)
(global-set-key (kbd "M-<left>") 'evil-window-left)
(global-set-key (kbd "M-<right>") 'evil-window-right)
(global-set-key (kbd "C-x w h") 'split-window-right)
(global-set-key (kbd "C-x w v") 'split-window-below)
(global-set-key (kbd "C-x w d") 'toggle-selected-window-dedicated)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)

(provide 'window-management)
;;; window-management.el ends here
