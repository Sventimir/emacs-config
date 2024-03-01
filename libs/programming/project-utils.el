;;; project-utils --- Some basic utils for navigating projects.
;;; Commentary:
;;; Code:

(defun rm-suffix (suf str)
  "Remove SUF if it exists at the end of STR."
  (replace-regexp-in-string (format "%s$" suf) "" str))

(defun find-project-root (regex dir)
  "Starting from DIR, move up the directory tree until the root is found.
The search is based on looking for a file whose name matches REGEX.
Returns the dir where the file was found.  Returns NIL when the root (/)
directory is reached before the file was found."
  (cond ((directory-files dir nil regex) dir)
	((equal dir "/") nil)
	(t (find-project-root regex (rm-suffix "/" (file-name-directory dir))))))

(defun goto-project-root (file)
  "Find the project root directory and move there if it was found.
The search is based on a package FILE that should live at the root.
If the root was found, CD there and return the dir.
If the file was not found, do nothing and returns nil."
  (let ((root (find-project-root file default-directory)))
    (when (stringp root) (cd root))))

(provide 'project-utils)
;;; project-utils.el ends here
