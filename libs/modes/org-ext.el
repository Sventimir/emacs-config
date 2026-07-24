;;; org-ext --- Extend ORG mode!
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-element)
(require 'evil)
(require 'request)

(defun org-babel-execute:curl (body params)
  "Pass the BODY as a curl request and and PARAMS as options."
  (let* ((url (cdr (assoc :url params)))
         (method (or (cdr (assoc :method params)) "POST"))
         (headers (or (cdr (assoc :headers params)) ""))
         (args (list url "-X" method "-d" body)))
    (with-current-buffer (get-buffer-create "*curl-output*")
      (progn
        (dolist (header (string-split headers "," t "[ \t\n\r]+"))
          (setq args (append args (list "-H" header))))
        (erase-buffer)
        (apply 'call-process "curl" nil "*curl-output*" nil args)
        (forward-line 4) ; Skip the networking stats.
        (condition-case nil
            (json-pretty-print (point) (point-max))
          (error nil))
        (let ((output (buffer-substring (point) (point-max))))
          (if (string= output "") (buffer-string) output))))))

(defun org-table-with-nums (f &rest args)
  "Apply F to ARGS converted to numbers."
  (apply f (mapcar 'string-to-number args)))

(defun org-link--zoom-follow (link)
  "Open Zoom link using Zoom application.  LINK is the target."
  (start-process "*Zoom*" nil "zoom" (concat "zoommtg://" link)))

(defun org-link-custom-open (program link)
  "Open LINK with PROGRAM."
  (call-process program nil nil nil link))

(defun org-link--open-firefox (url &optional _)
  "Open URL with Firefox."
  (org-link-custom-open "firefox" url))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)
   (haskell . t)))

;; Org-mode
(define-key org-mode-map (kbd "C-c SPC") (lambda ()
                                           "Clear table cell at point and enter insert state."
                                           (interactive)
                                           (progn
                                             (org-table-blank-field)
                                             (evil-insert-state))))

(define-key org-mode-map (kbd "C-c l y") (lambda ()
                                           "Copy URL of the link at point."
                                           (interactive)
                                           (kill-new (org-element-property :raw-link (org-element-context)))))

(define-key org-mode-map (kbd "C-c C-RET") 'org-table-insert-row)

;; Encrypted org-mode extension
(add-to-list 'auto-mode-alist '("\\.org.gpg\\'" . org-mode))

(provide 'org-ext)
;;; org-ext.el ends here
