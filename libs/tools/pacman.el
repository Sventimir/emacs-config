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

(provide 'pacman)
;;; pacman.el ends here
