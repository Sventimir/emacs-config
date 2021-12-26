;;; Package --- Emacs initialisation module
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


;; Menus
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Line numbers
(global-display-line-numbers-mode)
(column-number-mode)

;; Parenthese
(electric-pair-mode)

;; Evil mode
(use-package evil
  :ensure t
  :config (evil-mode 1)
          (global-undo-tree-mode)
          (evil-set-undo-system 'undo-tree))

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
 '(evil-undo-system 'undo-tree)
 '(inhibit-startup-screen t)
 '(lsp-keymap-prefix "C-c C-c")
 '(org-return-follows-link t)
 '(package-selected-packages
   '(flycheck-ocaml merlin-eldoc ocp-indent utop dune merlin ocamlformat ocaml-language-server lsp-ocaml yasnippet flycheck lsp-haskell lsp-ui lsp-mode imenu-list helm-ac smtpmail magit tuareg mu4e-overview ac-helm helm evil ##)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Helm
(use-package helm
  :ensure t)
(use-package ac-helm
  :ensure t)

;; Git
(use-package magit
  :ensure t)

;; Mu4e config
(use-package mu4e-overview
  :ensure t)
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

;; Programming language support
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)

(use-package company
  :ensure t)
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c C-c")
  :config (define-key lsp-mode-map (kbd "C-c C-c") lsp-command-map)
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)


;; Haskell
(use-package lsp-haskell
  :ensure t
  :config
    (setq lsp-haskell-server-path "haskell-language-server-wrapper")
    (setq lsp-haskell-server-args ()))

;; Global key bindings
(global-set-key (kbd "C-x m") 'mu4e)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x c c") (lambda ()
				  (interactive)
				  (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c c") 'compile)

(let ((default-directory  "~/.emacs.d/libs/"))
  (normal-top-level-add-subdirs-to-load-path))

(provide 'init)
;;; init.el ends here

