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
(use-package undo-tree
  :ensure t)
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
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(yaml-mode deferred ocaml-lsp helm-lsp company flycheck-ocaml merlin-eldoc ocp-indent utop dune merlin ocamlformat ocaml-language-server lsp-ocaml yasnippet flycheck lsp-haskell lsp-ui lsp-mode imenu-list helm-ac smtpmail magit tuareg mu4e-overview ac-helm helm evil ##))
 '(safe-local-variable-values
   '((eval progn
	   (require 'opam-env)
	   (add-to-list 'load-path "/home/sven/work/tezos/_opam/share/emacs/site-lisp")
	   (set-opam-env "/home/sven/work/tezos/_opam")
	   (setenv "WORKDIR" "/home/sven/work")
	   (setenv "TEZOS" "/home/sven/work/tezos")
	   (setenv "SRCDIR" "/home/sven/work/tezos/src")
	   (add-to-list 'exec-path "/home/sven/work/tezos/_opam/bin")
	   (defun copyright-nl nil "Insert Copyright line for Nomadic Labs."
		  (interactive)
		  (insert "(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
"))))))
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
(use-package deferred
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)

(use-package company
  :ensure t)
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-l")
  :config (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
  :hook
    (haskell-mode . lsp)
    (hack-local-variables . (lambda ()
			      (when (derived-mode-p 'tuareg-mode) (lsp-deferred))))
  :commands (lsp lsp-deferred))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; Haskell
(use-package lsp-haskell
  :ensure t
  :config
    (setq lsp-haskell-server-path "haskell-language-server-wrapper")
    (setq lsp-haskell-server-args ())
    :hook (lsp-haskell . (lambda () (setq-local compile-command "stack build"))))

;; OCaml
(use-package tuareg
  :ensure t)

(use-package dune
  :ensure t)
   
;; Michelson support
(use-package michelson-mode
  :load-path "/home/sven/work/tezos/emacs"
  :mode ("\\.tz\\'" . michelson-mode))

(defun michelson-with-mockup (&optional protocol)
  "Set Michelson mode to work in mockup mode.
Select PROTOCOL to use (defaults to Alpha)."
  (interactive "sProtocol to use (default: Alpha):")
  (setq michelson-client-command
	(format "/home/sven/work/tezos/tezos-client --mode mockup --protocol %s"
		(or protocol "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"))))

(defun michelson-with-node (&optional host)
  "Set Michelson mode to work with given Tezos node.
Select HOST to look for the node on (defaults to localhost.)"
  (interactive "sNode's hostname and port:")
  (setq michelson-client-command
	(format "/home/sven/work/tezos/tezos-client -E http://%s"
		(or host "localhost:8732"))))

(setq michelson-alphanet nil)
(michelson-with-mockup)

;; JSON and YAML
(use-package yaml-mode
  :ensure t)
(use-package json-mode
  :ensure t)

;; Global key bindings
(global-set-key (kbd "C-x m") 'mu4e)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x c c") (lambda ()
				  (interactive)
				  (find-file "~/.emacs.d/init.el")))

;; Window management
(global-set-key (kbd "C-x w s") 'split-window-horizontally)

;; Programming utilities
(global-set-key (kbd "C-c C-c") 'compile)

(let ((default-directory  "~/.emacs.d/libs/"))
  (normal-top-level-add-subdirs-to-load-path))

(provide 'init)
;;; init.el ends here

