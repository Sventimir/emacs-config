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

;; Add libs/ directory to the load path
(let ((default-directory  "~/.emacs.d/libs/"))
  (progn
    (normal-top-level-add-to-load-path (list default-directory))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'bridge)
(require 'gitlab)
(require 'polynomial)
(require 'numeric)
(require 'locstack)

;; Menus
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Line numbers
(global-display-line-numbers-mode)
(column-number-mode)

;; Indentation and tabulation
(setq-default indent-tabs-mode nil
    	      tab-width 4)
(setq indent-line-function 'insert-tab)

;; Parenthese
(electric-pair-mode)
(electric-indent-mode -1)

(use-package quelpa-use-package
  :ensure t)

(use-package request
  :ensure t)

(use-package eshell
  :ensure t)

(use-package recentf
  :ensure t
  :config (recentf-mode 1)
  :init (setq recentf-max-menu-items 25
              recentf-max-saved-items 25)
  :bind (("C-x C-r" . 'recentf-open-files)))

(use-package direnv
  :ensure t)

;; Perspectives
(use-package perspective
  :ensure t
  :bind ("C-x C-b" . persp-list-buffers)
  :custom (persp-mode-prefix-key (kbd "C-q"))
  :init (persp-mode))


;; Evil mode
(use-package undo-tree
  :ensure t)
(use-package evil
  :ensure t
  :config (evil-mode 1)
          (global-undo-tree-mode)
          (evil-set-undo-system 'undo-tree)
          (evil-select-search-module 'evil-search-module 'evil-search)
          (unbind-key (kbd "C-p") evil-normal-state-map)
          (unbind-key (kbd "C-p") evil-emacs-state-map)
          (unbind-key (kbd "C-p") evil-insert-state-map)
          (unbind-key (kbd "C-p") evil-motion-state-map)
          (unbind-key (kbd "C-p") evil-operator-state-map)
          (unbind-key (kbd "C-p") evil-visual-state-map))

;; Qutebrowser
(defun new-qutebrowser-window (url &rest args)
  "Opens given URL in a new qutebrowser window.
Additional ARGS may be passed to the browser if needed."
  (interactive "sURL: ")
  (let ((rest (mapconcat (lambda (str) (if str (format "'%s'" str) "")) args " ")))
    (shell-command (format "qutebrowser --target window %s '%s'" rest url))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(backup-by-copying t)
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist '((".*" . "~/.emacs-backups")))
 '(browse-url-handlers '(("^https://meet.google.com/.*" . new-qutebrowser-window)))
 '(compilation-scroll-output 'first-error)
 '(compile-command "dune build")
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("43851bb46b91f16e93a3eb85f711e8afefbd4a80ea1a21e25c6d88544eb22c7d" default))
 '(evil-undo-system 'undo-tree)
 '(global-undo-tree-mode t)
 '(haskell-compiler-type 'stack)
 '(haskell-emacs-dir "~/.emacs.d/haskell/")
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-type 'stack-ghci)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_GB")
 '(ispell-program-name "hunspell")
 '(js-indent-level 2)
 '(lsp-keymap-prefix "C-c C-c" t)
 '(org-agenda-files
   '("~/doc/mieszkanie/koszty.org" "/home/sven/doc/agentka/payments.org" "/home/sven/work/timed_account.org"))
 '(org-babel-haskell-compiler "ghc -dynamic")
 '(org-babel-load-languages
   '((shell . t)
     (python . t)
     (haskell . t)
     (sql . t)
     (emacs-lisp . t)))
 '(org-link-parameters
   '(("bibtex" :follow org-bibtex-open :store org-bibtex-store-link)
     ("mu4e" :follow mu4e-org-open :store mu4e-org-store-link)
     ("file+sys")
     ("file+emacs")
     ("shell" :follow org-link--open-shell)
     ("news" :follow
      #[514 "\301\300\302\4Q\2\"\207"
            ["news" browse-url ":"]
            6 "\12\12(fn URL ARG)"])
     ("mailto" :follow
      #[514 "\301\300\302\4Q\2\"\207"
            ["mailto" browse-url ":"]
            6 "\12\12(fn URL ARG)"])
     ("https" :follow
      #[514 "\301\300\302\4Q\2\"\207"
            ["https" browse-url ":"]
            6 "\12\12(fn URL ARG)"])
     ("http" :follow
      #[514 "\301\300\302\4Q\2\"\207"
            ["http" browse-url ":"]
            6 "\12\12(fn URL ARG)"])
     ("ftp" :follow
      #[514 "\301\300\302\4Q\2\"\207"
            ["ftp" browse-url ":"]
            6 "\12\12(fn URL ARG)"])
     ("help" :follow org-link--open-help :store org-link--store-help)
     ("file" :complete org-link-complete-file)
     ("elisp" :follow org-link--open-elisp)
     ("doi" :follow org-link-doi-open :export org-link-doi-export)
     ("zoommtg" :follow org-link--zoom-follow)))
 '(org-return-follows-link t)
 '(org-sql-db-config
   '(postgres :database "postgres" :hostname "localhost" :username "sven"))
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(graphql-mode go-mode copilot editorconfig elisp-format gnuplot gnuplot-mode nix-mode typescript-mode request envrc dockerfile-mode direnv nix-buffer json-mode haskell-mode haskell-emacs rust-mode project-utils idris-mode idris yaml-mode deferred ocaml-lsp helm-lsp company flycheck-ocaml merlin-eldoc ocp-indent utop dune merlin ocamlformat ocaml-language-server lsp-ocaml yasnippet flycheck lsp-haskell lsp-ui lsp-mode imenu-list helm-ac smtpmail magit tuareg mu4e-overview ac-helm helm evil ##))
 '(prog-mode-hook '(flyspell-prog-mode copilot-mode))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 2)
 '(rust-indent-offset 2)
 '(safe-local-variable-values
   '((eval progn
           (require 'opam-env)
           (add-to-list 'load-path "/home/sven/.opam/mina/share/emacs/site-lisp")
           (set-opam-env "/home/sven/.opam/mina")
           (setenv "WORKDIR" "/home/sven/work")
           (add-to-list 'exec-path "/home/sven/.opam/mina/bin"))))
 '(sql-connection-alist nil)
 '(typescript-indent-level 2)
 '(undo-tree-auto-save-history nil))
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
  :ensure t
  :hook (mu4e-compose . (lambda () (setq indent-tabs-mode nil))))
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
                  (mu4e-trash-folder  . "/marcin-pastudzki/[Gmail].Kosz")))

        ,(make-mu4e-context
          :name "MinaProtocol"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/mina" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "marcin.pastudzki@minafoundation.com")
                  (user-full-name    . "Marcin Pastudzki")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-user . "marcin.pastudzki@minaprotocol.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-drafts-folder  . "/mina/[Gmail].Wersje robocze")
                  (mu4e-sent-folder  . "/mina/[Gmail].Wa&AXw-ne")
                  (mu4e-refile-folder  . "/mina/[Gmail].Wszystkie")
                  (mu4e-trash-folder  . "/mina/[Gmail].Kosz")))
        ))
  )

;; Programming language support
(use-package envrc
  :ensure t)

(use-package deferred
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t)
  :bind (("C-c e 1" . 'flycheck-first-error)
         ("C-c e n" . 'flycheck-next-error)
         ("C-c e p" . 'flycheck-previous-error)))

(use-package yasnippet
  :ensure t)

(use-package company
  :ensure t)
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-l")
  :config (progn
            (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
            (define-key lsp-command-map (kbd "t") 'lsp-describe-thing-at-point))
  :hook
    (rust-mode . lsp)
    (tuareg-mode . lsp)
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
(use-package haskell-mode
  :ensure t
  :hook (haskell-mode . interactive-haskell-mode)
  :bind (("C-c g" . 'haskell-mode-jump-to-def-or-tag)
         ("C-c i" . 'haskell-add-import)
         ("C-c c" . 'haskell-compile)
         ("C-c C-x" . 'haskell-goto-next-error)
         ("C-c C-t" . 'haskell-doc-show-type)))

;; OCaml
(use-package tuareg
  :ensure t)

(add-hook 'tuareg-mode-hook 'locstack-mode)

(use-package dune
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :config (global-unset-key (kbd "C-p"))
  :bind (("C-p C-p" . 'copilot-mode)
         ("C-p c" . 'copilot-complete)
         ("C-p n" . 'copilot-next-completion)
         ("C-p p" . 'copilot-previous-completion)
         ("C-p RET" . 'copilot-accept-completion)
         ("C-p x" . 'copilot-clear-overlay)))

(use-package graphql-mode
  :ensure t)

;; Michelson support
(use-package michelson-mode
  :load-path "/home/sven/work/tezos/emacs"
  :mode ("\\.tz\\'" . michelson-mode))

(defun michelson-with-mockup (&optional protocol)
  "Set Michelson mode to work in mockup mode.
Select PROTOCOL to use (defaults to Alpha)."
  (interactive "sProtocol to use (default: Alpha):")
  (let ((proto (cond ((not protocol) "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK")
                     ((equal protocol "") "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"))))
    (setq michelson-client-command
	      (format "/home/sven/work/tezos/tezos-client --mode mockup --protocol %s" proto))))

(defun michelson-with-node (&optional host)
  "Set Michelson mode to work with given Tezos node.
Select HOST to look for the node on (defaults to localhost.)"
  (interactive "sNode's hostname and port:")
  (setq michelson-client-command
	(format "/home/sven/work/tezos/tezos-client -E http://%s"
		(or host "localhost:8732"))))

(setq michelson-alphanet nil)
(michelson-with-mockup)

;; Idris
(use-package idris-mode
  :ensure t
  :init
    (require 'project-utils)
    (setq idris-interpreter-path "/home/sven/.idris2/bin/idris2")
  :hook
    (idris-mode . (lambda () (goto-project-root "\.ipkg"))))

;; JSON and YAML
(use-package yaml-mode
  :ensure t)
(use-package json-mode
  :ensure t)

;; Rust
(use-package rust-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package elisp-format
  :ensure t)

;; Spell-checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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


;; Opening Zoom links from Org mode (configured via customize):
(defun org-link--zoom-follow (link)
  "Open Zoom link using Zoom application.  LINK is the target."
  (start-process "*Zoom*" nil "zoom" (concat "zoommtg://" link)))

(defun toggle-selected-window-dedicated-p ()
  "Toggle dedicated status of the currently selected window."
  (interactive)
  (let* ((window (selected-window))
         (status (not (window-dedicated-p window))))
    (progn
      (set-window-dedicated-p window status)
      (message "The %s %s dedicated now" window (if status "is" "is not")))))

;; Executing Org mode's code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)
   (haskell . t)))

;; Plots in ORG mode
(use-package gnuplot
  :ensure t)

;; Global key bindings
(global-set-key (kbd "C-x m") 'mu4e)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x c c") (lambda ()
				  (interactive)
				  (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c s f") 'flycheck-buffer)
(global-set-key (kbd "C-c s c") 'flyspell-correct-word-before-point)
(global-set-key (kbd "C-c s d") 'ispell-change-dictionary)

;; Window management
(global-set-key (kbd "C->") (lambda () (interactive) (select-window (next-window))))
(global-set-key (kbd "C-<") (lambda () (interactive) (select-window (previous-window))))
(global-set-key (kbd "C-x w h") 'split-window-horizontally)
(global-set-key (kbd "C-x w v") 'split-window-vertically)
(global-set-key (kbd "C-x w d") 'toggle-selected-window-dedicated-p)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)

;; Programming utilities
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-x C-g") 'magit-blame)
(global-set-key (kbd "C-c _") (lambda () (interactive) (insert-sep-region "_" 3)))

;; Inserting special characters
(defmacro lambda-insert (chrs)
  "Return a lambda inserting CHRS."
  `(lambda () (interactive) (insert ,chrs)))

(unbind-key (kbd "C-k") evil-insert-state-map)
(unbind-key (kbd "C-k") global-map)
(define-key evil-insert-state-map (kbd "C-k c") (lambda-insert "♣"))
(define-key evil-insert-state-map (kbd "C-k d") (lambda-insert "♦"))
(define-key evil-insert-state-map (kbd "C-k h") (lambda-insert "♥"))
(define-key evil-insert-state-map (kbd "C-k s") (lambda-insert "♠"))

;; Miscellanea
(defun active-minor-modes ()
  "Return a list of active minor modes for the current buffer."
  (interactive)
  (seq-filter 'symbol-value (mapcar 'car minor-mode-alist)))

;; Extended org-mode
(require 'org-ext)
(require 'github)
(require 'nbp)

;; Extending Emacs
;; (use-package haskell-emacs
;;   :ensure t
;;   :config (haskell-emacs-init))

;; Enable envrc
(envrc-global-mode)

;; Enable Merlin for Mina project:
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (load "tuareg-site-file")
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)))

;; Predefined perspectives
(defun persp-mail ()
  "Create a perspective and open mu4e inside it."
  (interactive)
  (persp-switch "mail")
  (mu4e))

(defun ide (workdir &optional name)
  "Create perspective NAME and open the project in WORKDIR inside it."
  (interactive "fWorkdir: ")
  (persp-switch name)
  (split-window-right)
  (magit workdir)
  (toggle-selected-window-dedicated-p)
  (split-window-below)
  (select-window (previous-window)))

(defun mina ()
  "Create a perspective and open the Mina project inside it."
  (interactive)
  (ide "/home/sven/mina"))


(provide 'init)
;;; init.el ends here
