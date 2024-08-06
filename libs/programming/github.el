;;; github -- Miscellaneous helpers for interaction with GitHub.
;;; Commentary:
;;; Code:

(require 'org)
(require 'subr-x)

(setq github-monitored-path "src/app/rosetta"
      github-table-spec '((number ".number" "ID")
                          (author ".author.login" "Author")
                          (files  nil)
                          (url    ".url" nil)
                          (title  ".title" "Title")
                          (nil    nil "Status")
                          (nil    nil "Action")))

(defun github-loaded-fields ()
  "Return the list of fields loaded from GitHub."
  (mapcar (lambda (spec) (symbol-name (car spec)))
          (seq-filter 'car github-table-spec)))

(defun github-json-fields ()
  "Return the list of fields loaded from GitHub."
  (seq-filter (lambda (spec) (nth 1 spec)) github-table-spec))

(defun github-table-columns ()
  "Returnb the list of fields loaded from GitHub."
  (mapcar (lambda (spec) (nth 2 spec))
          (seq-filter (lambda (spec) (nth 2 spec)) github-table-spec)))

(defun github-concat-sep (acc sep els)
  "Concatenate ELS separating them with SEP, accumulating output in ACC."
  (if els
      (github-concat-sep (concat acc sep (car els)) sep (cdr els))
    acc))

(defun github-concat (sep els)
  "Start concatenating ELS separated with SEP."
  (if els
      (github-concat-sep (car els) sep (cdr els))
      ""))

(defun github-output-object (fields)
  "Generate jq query to build the output object with FIELDS."
  (github-concat ","
                 (mapcar (lambda (spec)
                           (let ((key (nth 0 spec))
                                 (val (nth 1 spec)))
                             (format "\"%s\": %s" key val)))
                         (github-json-fields))))

(defun github-json-query ()
  "Return the github JSON query."
  (format "map(select(.files | map(.path) | any(contains(\"%s\"))) | {%s})"
          github-monitored-path
          (github-output-object github-table-spec)))

(defun github-load-pr-data (repo)
  "Load PR data for the REPO from GitHub."
  (progn
    (cd repo)
    (let* ((fields (github-concat "," (github-loaded-fields)))
           (cmd (format "gh pr list --json %s -q '%s'" fields (github-json-query))))
      (json-parse-string (shell-command-to-string cmd)))))

(defun github-table-insert-row (row-data)
  "Insert ROW-DATA into the table at point."
  (progn
    (org-table-goto-column 1)
    (org-insert-link nil (gethash "url" row-data) (number-to-string (gethash "number" row-data)))
    (org-table-next-field)
    (insert (gethash "author" row-data))
    (org-table-next-field)
    (insert (gethash "title" row-data))
    (org-table-next-row)))

(defun github-pr-table (repo)
  "Print a table of open PRs in REPO."
  (interactive "fRepository: ")
  (let ((table-data (github-load-pr-data repo)))
    (progn
      (org-table-create
       (format "%dx1" (seq-length (github-table-columns))))
      (dolist (col-name (github-table-columns))
        (progn
          (org-table-next-field)
          (insert col-name)))
      (org-table-next-field)
      (org-table-insert-hline 'above)
      (seq-do 'github-table-insert-row table-data)
      (org-table-align))))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c g p") 'github-pr-table))

(defun list-drop (n lst)
  "Drop N elements from the begining of LST and return the remainder if any."
  (let ((ret lst))
    (dotimes (_ n ret)
      (setq ret (cdr ret)))))

(defun github-open-file (url)
  "Opens local file corresponding to the URL."
  (interactive "sURL: ")
  (let* ((addr (url-generic-parse-url url))
         (host (url-host addr))
         (path (file-name-split (url-filename addr)))
         (user (nth 1 path))
         (repo (nth 2 path))
         (branch (nth 4 path))
         (filename (mapconcat 'identity (list-drop 5 path) "/"))
         (repo-root (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))
         (remote (shell-command-to-string "git remote -v"))
         (remote-url (cadr (split-string remote "[@\s]"))))
    (if (string= remote-url (format "%s:%s/%s" host user repo))
        (find-file (format "%s/%s" repo-root filename))
      (error "Remote URL does not match current repository"))))

(provide 'github)
;;; github.el ends here
