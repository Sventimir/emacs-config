;;; Package --- Summary
;;; Commentary: Extensions for the Rust mode
;;; Code:

(defun rust-mode-run-test-at-point ()
  "Run the test at point."
  (interactive)
  (let ((test-name (thing-at-point 'symbol)))
    (compile (concat "cargo test " test-name))))

(defun rust-mode-run-interactive ()
  "Run the current rust project in an interactive (comitn mode) buffer."
  (interactive)
  (compile "cargo run" 't)
  (pop-to-buffer "*compilation*")) ;; 't means comint enabled.

;; Add key bindings:
(eval-after-load 'rust-mode
  '(progn
     (define-key rust-mode-map (kbd "C-c t") 'rust-mode-run-test-at-point)
     (define-key rust-mode-map (kbd "C-c r") 'rust-mode-run-interactive)
     (define-key comint-mode-map (kbd "C-c C-k") 'delete-process)))

(provide 'rust-ext)
;;; rust-ext.el ends here
