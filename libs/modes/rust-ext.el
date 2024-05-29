;;; Package --- Summary
;;; Commentary: Extensions for the Rust mode
;;; Code:

(defun rust-mode-run-test-at-point ()
  "Run the test at point."
  (interactive)
  (let ((test-name (thing-at-point 'symbol)))
    (compile (concat "cargo test " test-name))))

(provide 'rust-ext)
;;; rust-ext.el ends here
