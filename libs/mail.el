;;; -*- lexical-binding: t -*-
;;; Package --- 
;;; Commentary:
;;; Code:

(require 'mu4e)

(use-package auth-source-pass
  :ensure t)
(use-package smtpmail
  :ensure t)

(setq mu4e-get-mail-command "offlineimap -o"
      mu4e-update-interval (* 10 60)
      mu4e-enable-notifications t
      mu4e-view-show-addresses 't
      message-send-mail-function 'smtpmail-send-it
      auth-sources '(password-store))

(with-eval-after-load 'mu4e
  (setq mu4e-contexts
        `(
        ,(make-mu4e-context
          :name "Sventimir"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/sventimir" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "svantimir@gmail.com")
                  (user-full-name    . "Sventimir")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-user . "svantimir@gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-drafts-folder  . "/sventimir/[Gmail].Wersje robocze")
                  (mu4e-sent-folder  . "/sventimir/[Gmail].Wa&AXw-ne")
                  (mu4e-refile-folder  . "/sventimir/[Gmail].Wszystkie")
                  (mu4e-trash-folder  . "/sventimir/[Gmail].Kosz")))

        ,(make-mu4e-context
          :name "Gmail-MarcinJarmuzynski"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/marcin-jarmuzynski" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "marcin.jarmuzynski@gmail.com")
                  (user-full-name    . "Marcin Jarmużyński")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-user . "marcin.jarmuzynski@gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-drafts-folder  . "/marcin-jarmuzynski/[Gmail].Wersje robocze")
                  (mu4e-sent-folder  . "/marcin-jarmuzynski/[Gmail].Wa&AXw-ne")
                  (mu4e-refile-folder  . "/marcin-jarmuzynski/[Gmail].Wszystkie")
                  (mu4e-trash-folder  . "/marcin-jarmuzynski/[Gmail].Kosz")))

        ,(make-mu4e-context
          :name "Gmail-MarcinPastudzki"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/marcin-pastudzki" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "marcin.pastudzki@gmail.com")
                  (user-full-name    . "Marcin Pastudzki")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-user . "marcin.pastudzki@gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-drafts-folder  . "/marcin-pastudzki/[Gmail].Wersje robocze")
                  (mu4e-sent-folder  . "/marcin-pastudzki/[Gmail].Wa&AXw-ne")
                  (mu4e-refile-folder  . "/marcin-pastudzki/[Gmail].Wszystkie")
                  (mu4e-trash-folder  . "/marcin-pastudzki/[Gmail].Kosz")))))
  )

(defun mu4e-email-addresses ()
  "Return the list of e-mail addresses handled by mu4e."
  (mapcar (lambda (ctx)
            (cdr (assoc 'user-mail-address (mu4e-context-vars ctx))))
          mu4e-contexts))

(defun mu-init (maildir)
  "Initialise mu in MAILDIR."
  (interactive (list (read-directory-name "Mail directory: " "~/.local/share/mail")))
  (shell-command (format
                  "mu init --maildir %s %s"
                  (expand-file-name maildir)
                  (mapconcat (lambda (addr)
                               (format "--my-address %s" addr))
                             (mu4e-email-addresses)
                             " "))))

;; Predefined perspectives
(defun persp-mail ()
  "Create a perspective and open mu4e inside it."
  (interactive)
  (persp-switch "mail")
  (mu4e))

(global-set-key (kbd "C-x m") 'persp-mail)

(provide 'mail)
;;; mail.el ends here
