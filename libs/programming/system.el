;;; Packaege --- Summary
;;; Commentary:
;;; Code:

(defun add-to-path (&rest paths)
  "Add each path in PATHS to the PATH environment variable."
  (let ((res (getenv "PATH")))
    (setenv "PATH"
            (dolist (p paths res)
              (setq res (concat res ":" p))))))

(provide 'system)
;;; system.el ends here
