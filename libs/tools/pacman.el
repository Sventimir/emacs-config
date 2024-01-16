;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'org)

(defun pacman-history ()
  "Read history of package upgrqades from pacman's log."
  (interactive)
  (pop-to-buffer "*pacman-history*")
  (erase-buffer)
  (org-mode)
  (insert "| date | package | from version | to version |\n|-\n")
  (shell-command
   "awk -f /home/sven/.emacs.d/libs/tools/pacman-log.awk /var/log/pacman.log"
   "*pacman-history*")
  (org-table-align))

(defun pacman-filter (regex)
  "Filter out history entries not matching REGEX."
  (interactive "sFilter: ")
  (org-table-goto-line 2)
  (let ((package (org-table-get (org-table-current-line) 1)))
    (while (not (string= "" package))
      (if (string-match regex package)
          (org-table-next-row)
        (org-table-kill-row))
      (setq package (org-table-get (org-table-current-line) 1)))))

(provide 'pacman)
;;; pacman.el ends here
