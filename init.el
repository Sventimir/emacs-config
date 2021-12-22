(load-file ".emacs.d/libs/packages.el")

;; Menus
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Evil mode
(ensure-package 'evil)
(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("43851bb46b91f16e93a3eb85f711e8afefbd4a80ea1a21e25c6d88544eb22c7d" default))
 '(inhibit-startup-screen t)
 '(org-return-follows-link t)
 '(package-selected-packages
   '(helm-ac smtpmail magit tuareg mu4e-overview ac-helm helm evil ##)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Helm
(ensure-package 'helm)
(ensure-package 'ac-helm)

;; Git
(ensure-package 'magit)

;; Mu4e config
(ensure-package 'mu4e-overview)
(ensure-package 'smtpmail)
(setq mu4e-get-mail-command "offlineimap -o"
      mu4e-update-interval (* 10 60)
      mu4e-enable-notifications t
      mu4e-view-show-addresses 't
      message-send-mail-function 'smtpmail-send-it
      auth-sources '(password-store))

(global-set-key (kbd "C-x m") 'mu4e)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

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
          :name "MarcinPastudzki"
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
                  (mu4e-trash-folder  . "/marcin-pastudzki/[Gmail].Kosz")))

        ,(make-mu4e-context
          :name "Lambda-coins"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/lambda-coins" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "marcin.pastudzki@lambda-coins.com")
                  (user-full-name    . "Marcin Pastudzki")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-user . "marcin.pastudzki@lambda-coins.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-drafts-folder  . "/lambda-coins/[Gmail].Wersje robocze")
                  (mu4e-sent-folder  . "/lambda-coins/[Gmail].Wa&AXw-ne")
                  (mu4e-refile-folder  . "/lambda-coins/[Gmail].Wszystkie")
                  (mu4e-trash-folder  . "/lambda-coins/[Gmail].Kosz")))
        ))
  )

;; OCaml
(ensure-package 'tuareg)
(add-to-list 'load-path "/home/sven/work/tezos/_opam/share/emacs/site-lisp")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'merlin-mode "merlin" nil t nil)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
(setq merlin-command "/home/sven/work/tezos/_opam/bin/ocamlmerlin")
