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
(defun load-dir (dir)
  "Add all files in DIR to the load path."
  (let ((default-directory dir))
    (progn
      (normal-top-level-add-to-load-path (list default-directory))
      (normal-top-level-add-subdirs-to-load-path))))

(load-dir "~/.emacs.d/libs/")

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


(use-package request
  :ensure t)

(require 'gitlab)
(require 'polynomial)
(require 'music-meta)
(require 'numeric)
(require 'locstack)
(require 'pacman)
(require 'range)
(require 'editing)
(require 'rust-ext)

;; Extended org-mode
(require 'org-ext)
(require 'github)
(require 'nbp)
(require 'ipbox)

;; Executing Org mode's code blocks
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

;; Plots in ORG mode
(use-package gnuplot
  :ensure t)

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

;; Parentheses
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

(use-package crux
  :ensure t
  :bind (("C-x E" . crux-eval-and-replace)))


;; Tree-sitter support
(use-package tree-sitter
  :ensure t)

(use-package tsc
  :ensure t)

;; Configure Tree-sitter. Unfortunately this cannot
;; be done through Customize at this point.
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
        (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")))

;; Helm
(use-package helm
  :ensure t)
(use-package ac-helm
  :ensure t)

;; Git
(use-package magit
  :ensure t)

;; Programming language support
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t)
  :bind (("C-c e 1" . 'flycheck-first-error)
         ("C-c e n" . 'flycheck-next-error)
         ("C-c e p" . 'flycheck-previous-error)))

(use-package envrc
  :ensure t)

(use-package pyvenv
  :ensure t
  :config (pyvenv-mode t))

(setq pyvenv-post-activate-hooks
      (list (lambda ()
              (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")
                    flycheck-python-pylint-executable (concat pyvenv-virtual-env "bin/python")))))
(setq pyvenv-post-deactivate-hooks
      (list (lambda ()
              (setq python-shell-interpreter "python3"
                    flycheck-python-pylint-executable (concat pyvenv-virtual-env "bin/python")))))

(use-package deferred
  :ensure t)

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
    (python-mode . lsp)
    (tuareg-mode . lsp)
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
(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t)

;; OCaml
(use-package tuareg
  :ensure t
  :bind (("C-c C-c" . 'compile)))

(add-hook 'tuareg-mode-hook 'locstack-mode)

(use-package dune
  :ensure t)

(use-package python
  :ensure t
  :hook (python-mode . (lambda ()
                         (flycheck-add-next-checker 'lsp 'python-pylint))))

(add-hook 'python-mode-hook 'lsp-deferred)

(use-package dockerfile-mode
  :ensure t)

;; Explore and edit running docker containers with Emacs.
(use-package docker-tramp
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

;; Idris
(use-package idris-mode
  :ensure t
  :init
    (require 'project-utils)
    (setq idris-interpreter-path "/home/sven/.idris2/bin/idris2")
  :hook
    (idris-mode . (lambda () (goto-project-root "\.ipkg"))))

;; Rust
(use-package rust-mode
  :ensure t)

(add-hook 'tuareg-mode-hook 'locstack-mode)

(use-package typescript-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :bind (("C-l C-s" . 'lua-start-process)
         ("C-l C-b" . 'lua-send-buffer)
         ("C-l C-r" . 'lua-send-region)
         ("C-l C-l" . 'lua-send-current-line)
         ("C-l C-z" . 'lua-show-process-buffer)
         ("C-l C-f" . 'lua-send-defun)
         ("C-l C-c" . 'lua-restart-with-whole-file)))

;; Wesnoth mode
(add-to-list 'load-path "/usr/share/wesnoth/data/tools/emacs_mode/")
(autoload 'wesnoth-mode "wesnoth-mode" "Major mode for editing WML." t)
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . wesnoth-mode))

(use-package elisp-format
  :ensure t)

(use-package csv-mode
  :ensure t)

;; JSON and YAML
(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package solidity-mode
  :ensure t)

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
 '(compilation-scroll-output 'first-error)
 '(compile-command "cargo build")
 '(custom-enabled-themes '(tango-dark))
 '(evil-undo-system 'undo-tree)
 '(global-undo-tree-mode t)
 '(haskell-compiler-type 'stack)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-type 'stack-ghci)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_GB")
 '(ispell-program-name "hunspell")
 '(js-indent-level 2)
 '(lsp-keymap-prefix "C-l")
 '(lua-indent-level 2)
 '(org-babel-haskell-compiler "ghc -dynamic")
 '(org-babel-load-languages
   '((dot . t)
     (emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t)))
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
     ("firefox" :follow org-link--open-firefox)
     ("zoommtg" :follow org-link--zoom-follow)))
 '(org-return-follows-link t)
 '(org-sql-db-config
   '(postgres :database "postgres" :hostname "localhost" :username "sven"))
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(solidity-mode docker-tramp tree-sitter crux pyenv pylsp lua-mode csv-mode graphql-mode go-mode copilot editorconfig elisp-format gnuplot gnuplot-mode nix-mode typescript-mode request envrc dockerfile-mode direnv nix-buffer json-mode haskell-mode haskell-emacs rust-mode project-utils idris-mode idris yaml-mode deferred ocaml-lsp helm-lsp company flycheck-ocaml merlin-eldoc ocp-indent utop dune merlin ocamlformat ocaml-language-server lsp-ocaml yasnippet flycheck lsp-haskell lsp-ui lsp-mode imenu-list helm-ac smtpmail magit tuareg mu4e-overview ac-helm helm evil ##))
 '(prog-mode-hook '(flyspell-prog-mode copilot-mode))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4)
 '(sh-basic-offset 2)
 '(sql-connection-alist nil)
 '(typescript-indent-level 2)
 '(undo-tree-auto-save-history nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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


;; Spell-checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun toggle-selected-window-dedicated-p ()
  "Toggle dedicated status of the currently selected window."
  (interactive)
  (let* ((window (selected-window))
         (status (not (window-dedicated-p window))))
    (progn
      (set-window-dedicated-p window status)
      (message "The %s %s dedicated now" window (if status "is" "is not")))))

;; Miscellanea
(defun active-minor-modes ()
  "Return a list of active minor modes for the current buffer."
  (interactive)
  (seq-filter 'symbol-value (mapcar 'car minor-mode-alist)))

;; Predefined perspectives
(defun persp-mail ()
  "Create a perspective and open mu4e inside it."
  (interactive)
  (persp-switch "mail")
  (mu4e))

(defun ide (workdir &optional name)
  "Create perspective NAME and open the project in WORKDIR inside it."
  (interactive "fWorkdir: ")
  (let ((default (file-name-base (string-remove-suffix "/" workdir))))
    (persp-switch (or name (read-string "Perspective name: " default))))
  (setq default-directory workdir)
  (load-dir workdir)
  (split-window-right)
  (magit-status-setup-buffer workdir)
  (toggle-selected-window-dedicated-p)
  (split-window-below)
  (switch-to-buffer "*Messages*")
  (setq default-directory workdir)
  (select-window (previous-window)))

(defun config ()
  "Create a perspective for editing Emacs configuration."
  (interactive)
  (ide "~/.emacs.d" "emacs-config")
  (find-file "~/.emacs.d/init.el"))

(defun work (repository)
  "Create a perspective for work in REPOSITORY."
  (interactive (list (read-file-name "Repository: " (format "%s/work/composable-ibc" (getenv "HOME")))))
  (load-dir (format "%s/work/emacs" (getenv "HOME")))
  (require 'work-setup)
  (ide repository))

;; Encrypted org-mode extension
(add-to-list 'auto-mode-alist '("\\.org.gpg\\'" . org-mode))


;; Global key bindings
(global-set-key (kbd "C-x m") 'persp-mail)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x c c") 'config) ;; see config function defined below
(global-set-key (kbd "C-c s f") 'flycheck-buffer)
(global-set-key (kbd "C-c s c") 'flyspell-correct-word-before-point)
(global-set-key (kbd "C-c s d") 'ispell-change-dictionary)

;; Window management
(global-set-key (kbd "C->") (lambda () (interactive) (select-window (next-window))))
(global-set-key (kbd "C-<") (lambda () (interactive) (select-window (previous-window))))
(global-set-key (kbd "M-<down>") 'evil-window-down)
(global-set-key (kbd "M-<up>") 'evil-window-up)
(global-set-key (kbd "M-<left>") 'evil-window-left)
(global-set-key (kbd "M-<right>") 'evil-window-right)
(global-set-key (kbd "C-x w h") 'split-window-right)
(global-set-key (kbd "C-x w v") 'split-window-below)
(global-set-key (kbd "C-x w d") 'toggle-selected-window-dedicated-p)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)

;; Programming utilities
(global-set-key (kbd "C-x C-g b") 'magit-blame)
(global-set-key (kbd "C-x C-g C-f") 'github-open-file)
(global-set-key (kbd "C-x C-g l") 'github-generate-link)
(global-set-key (kbd "C-c _") (lambda () (interactive) (insert-sep-region "_" 3)))

(global-set-key (kbd "C-x C-g g") (lambda () (interactive)
                                    (make-process
                                     :name "gitg"
                                     :buffer "*gitg*"
                                     :command '("gitg"))))

(define-key epa-key-list-mode-map (kbd "C-s") 'epa-mark-key)
(define-key epa-key-list-mode-map (kbd "C-u") 'epa-unmark-key)

;; Enable envrc
(envrc-global-mode)

(provide 'init)
;;; init.el ends here
