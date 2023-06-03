;;; locstack --- Location stack for Emacs
;;; Commentary:
;;;   Remember current location and revisit it later.
;;; Code:

(defvar locstack-stack nil
  "Stack of locations.")

(defun locstack-current-loc ()
  "Get current location."
  (list (buffer-name) (window-start) (point)))

(defun locstack-push (&optional loc)
  "Push LOC (or current location) onto the stack."
  (interactive)
  (let ((loc (if loc loc (locstack-current-loc))))
    (message "Pushing %s:%d onto location stack." (car loc) (nth 1 loc))
    (setq locstack-stack (cons loc locstack-stack))))

(defun locstack-display (loc)
  "Display LOC (or location from the top of the stack)."
  (switch-to-buffer (nth 0 loc))
  (set-window-start (selected-window) (nth 1 loc))
  (set-window-point (selected-window) (nth 2 loc)))

(defun locstack-view (&optional loc)
  "Display LOC (or location from the top of the stack) without removing it."
  (interactive)
  (cond (loc (locstack-display loc))
        (locstack-stack (locstack-display (car locstack-stack)))
        (t (message "Location stack is empty!"))))

(defun locstack-pop ()
  "Pop location from the stack and display it."
  (interactive)
  (locstack-view)
  (setq locstack-stack (cdr locstack-stack)))

(defun locstack-replace ()
  "Go to the location on top of the stack and push current location in its place."
  (interactive)
  (let ((loc (locstack-current-loc)))
    (locstack-pop)
    (locstack-push loc)))

(defun locstack-clear ()
  "Clear location stack."
  (interactive)
  (setq locstack-stack nil))

(define-minor-mode locstack-mode
  "Toggle locstack mode."
  :lighter " locstack"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c l p") 'locstack-push)
            (define-key map (kbd "C-c l v") 'locstack-view)
            (define-key map (kbd "C-c l g") 'locstack-pop)
            (define-key map (kbd "C-c l r") 'locstack-replace)
            (define-key map (kbd "C-c l c") 'locstack-clear)
            map))

(provide 'locstack)
;;; locstack.el ends here
