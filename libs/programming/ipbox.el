;;; Package --- Summary
;;; Commentary:
;;; Code:

(require 'nbp)
(require 'org)
(require 'org-table)

(defun ipbox-clone-row ()
  "In a table clone the current row at the bottom of the table."
  (interactive)
  (let* ((table (org-table-to-lisp))
         (line (nth (org-table-current-line) table)))
    (org-table-goto-line (- (length table) 4))
    (org-table-insert-row t)
    (org-table-goto-column 2)
    (org-insert-time-stamp (current-time))
    (dolist (cell (cddr line))
      (org-table-next-field)
      (insert (format "%s" cell)))
    (nbp-load-data-for-table)
    (org-table-recalculate t)))

(provide 'ipbox)
;;; ipbox.el ends here
