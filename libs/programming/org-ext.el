;;; org-ext --- Extend ORG mode!
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-element)
(require 'request)

(defun org-http-request (url)
  "Construct and HTTP request to the URL.
Take contents of the ORG element at point and use it as
body for an HTTP request.  Display the response in buffer."
  (interactive "sURL: ")
  (let* ((block (org-element-at-point))
         (input-json (org-element-property :value block)))
    (request url
      :type "POST"
      :params nil
      :data input-json
      :parser 'buffer-string
      :complete (cl-function
                 (lambda (&key response &allow-other-keys)
                   (progn
                     (pop-to-buffer "*org-http-response*")
                     (erase-buffer)
                     (message (format "HTTP status code: %s." (request-response-status-code response)))
                     (insert (request-response-data response))
                     (json-pretty-print-buffer)
                     (js-mode)))))))

(defun org-table-with-nums (f &rest args)
  "Apply F to ARGS converted to numbers."
  (apply f (mapcar 'string-to-number args)))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c h") 'org-http-request))

(provide 'org-ext)
;;; org-ext.el ends here
