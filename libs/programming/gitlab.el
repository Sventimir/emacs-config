;;; gitlab -- Miscellaneous helpers for interaction with GitLab.
;;; Commentary:
;;; Code:

(defun insert-mr-link (id)
  "Insert a link to GitLab Merge Request !ID."
  (interactive "nMR id: ")
  (let ((mr-id (number-to-string id)))
    (insert (concat
             "[[https://gitlab.com/tezos/tezos/-/merge_requests/"
             mr-id
             "][!"
             mr-id
             "]]"))))

(provide 'gitlab)
;;; gitlab.el ends here
