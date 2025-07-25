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

;; Evil mode
(use-package undo-tree
  :ensure t)

(use-package evil
  :ensure t
  :config (evil-mode 1)
          (evil-set-initial-state 'special-mode 'emacs)
          (global-undo-tree-mode)
          (evil-set-undo-system 'undo-tree)
          (evil-select-search-module 'evil-search-module 'evil-search)
          (unbind-key (kbd "C-a") evil-motion-state-map)
          (unbind-key (kbd "C-e") evil-motion-state-map)
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
(require 'recorder)

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

(use-package quelpa-use-package
  :ensure t)

(use-package eshell
  :ensure t)

(use-package recentf
  :ensure t
  :config (recentf-mode 1)
  :init (setq recentf-max-menu-items 25
              recentf-max-saved-items 25)
  :bind (("C-x C-r" . 'recentf-open-files)))

;; Direnv
(use-package direnv
  :ensure t)

;; Perspectives
(use-package perspective
  :ensure t
  :bind ("C-x C-b" . persp-list-buffers)
  :custom (persp-mode-prefix-key (kbd "C-q"))
  :init (persp-mode))

;; Crux
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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t)
  :bind-keymap ("C-e" . flycheck-command-map)
  :bind (:map flycheck-command-map
              ("1" . 'flycheck-first-error)
              ("n" . 'flycheck-next-error)
              ("p" . 'flycheck-previous-error)
              ("f" . 'flycheck-buffer)
              ("c" . 'flyspell-correct-word-before-point)
              ("d" . 'ispell-change-dictionary)))

(use-package envrc
  :ensure t)

(use-package deferred
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package company
  :ensure t)

;; Programming language support
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
  :bind-keymap ("C-c" . haskell-mode-map)
  :bind (:map haskell-mode-map
              ("C-c g" . 'haskell-mode-jump-to-def-or-tag)
              ("C-c i" . 'haskell-add-import)
              ("C-c C-c" . 'haskell-compile)
              ("C-c C-x" . 'haskell-goto-next-error)
              ("C-c C-t" . 'haskell-doc-show-type)
              ("C-c t" . (lambda ()
                           (interactive)
                           (setq-local haskell-compile-stack-build-command "stack test")
                           (haskell-compile)
                           (setq-local haskell-compile-stack-build-command "stack build --fast")))))

;; OCaml
(use-package tuareg
  :ensure t
  :bind-keymap ("C-c" . tuareg-mode-map)
  :bind (:map tuareg-mode-map
              ("C-c C-c" . 'compile))
  :hook (tuareg-mode . (lambda () (setq-local compile-command "dune build"))))

(add-hook 'tuareg-mode-hook 'locstack-mode)

(use-package dune
  :ensure t)

(use-package python
  :ensure t
  :hook (python-mode . (lambda ()
                         (flycheck-add-next-checker 'lsp 'python-pylint)))
        ; enable python code execution in org-mode src blocks
        (org-mode . (lambda ()
                      (require 'lsp-diagnostics)
                      (require 'flycheck)
                      (lsp-diagnostics-flycheck-enable)
                      (flycheck-add-next-checker 'python-flake8 'lsp))))

(add-hook 'python-mode-hook 'lsp-deferred)

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

(use-package dockerfile-mode
  :ensure t)

;; Explore and edit running docker containers with Emacs.
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "zerolfx/copilot.el"
;;                    :branch "main"
;;                    :files ("dist" "*.el"))
;;   :config (global-unset-key (kbd "C-p"))
;;   :bind (("C-p C-p" . 'copilot-mode)
;;          ("C-p c" . 'copilot-complete)
;;          ("C-p n" . 'copilot-next-completion)
;;          ("C-p p" . 'copilot-previous-completion)
;;          ("C-p RET" . 'copilot-accept-completion)
;;          ("C-p x" . 'copilot-clear-overlay)))

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
  :ensure t
  :bind (:map rust-mode-map
              ("C-c C-c C-c" . 'compile))
  :hook (rust-mode . (lambda () (setq-local compile-command "cargo build"))))

(add-hook 'tuareg-mode-hook 'locstack-mode)

(use-package typescript-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :bind-keymap ("C-l" . lua-mode-map)
  :bind (:map lua-mode-map
              ("C-l C-s" . 'lua-start-process)
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
 '(custom-enabled-themes '(tango-dark))
 '(ellama-auto-scroll t)
 '(ellama-chat-display-action-function 'pop-to-buffer)
 '(ellama-instant-display-action-function 'display-buffer-at-bottom)
 '(ellama-naming-scheme 'ellama-generate-name-by-llm)
 '(evil-undo-system 'undo-tree)
 '(global-undo-tree-mode t)
 '(haskell-compiler-type 'stack)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-type 'stack-ghci)
 '(idris-interpreter-path "/usr/bin/idris2")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_GB")
 '(ispell-program-name "hunspell")
 '(js-indent-level 2)
 '(lsp-keymap-prefix "C-l")
 '(lua-indent-level 2)
 '(mu4e-drafts-folder "/marcin-jarmuzynski/[Gmail].Wersje robocze")
 '(mu4e-get-mail-command "offlineimap -o")
 '(mu4e-modeline-mode t)
 '(mu4e-notification-support t)
 '(mu4e-refile-folder "/marcin-jarmuzynski/[Gmail].Wszystkie")
 '(mu4e-sent-folder "/marcin-jarmuzynski/[Gmail].Wa&AXw-ne")
 '(mu4e-trash-folder "/marcin-jarmuzynski/[Gmail].Kosz")
 '(mu4e-update-interval 600)
 '(mu4e-use-fancy-chars t)
 '(nbp-currency "EUR")
 '(org-babel-haskell-compiler "ghc -dynamic")
 '(org-babel-load-languages
   '((dot . t) (emacs-lisp . t) (python . t) (shell . t) (sql . t)))
 '(org-link-parameters
   '(("bibtex" :follow org-bibtex-open :store org-bibtex-store-link)
     ("mu4e" :follow mu4e-org-open :store mu4e-org-store-link)
     ("file+sys") ("file+emacs")
     ("shell" :follow org-link--open-shell)
     ("news" :follow
      #[514 "\301\300\302\4Q\2\"\207" ["news" browse-url ":"] 6
            "\12\12(fn URL ARG)"])
     ("mailto" :follow
      #[514 "\301\300\302\4Q\2\"\207" ["mailto" browse-url ":"] 6
            "\12\12(fn URL ARG)"])
     ("https" :follow
      #[514 "\301\300\302\4Q\2\"\207" ["https" browse-url ":"] 6
            "\12\12(fn URL ARG)"])
     ("http" :follow
      #[514 "\301\300\302\4Q\2\"\207" ["http" browse-url ":"] 6
            "\12\12(fn URL ARG)"])
     ("ftp" :follow
      #[514 "\301\300\302\4Q\2\"\207" ["ftp" browse-url ":"] 6
            "\12\12(fn URL ARG)"])
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
 '(package-selected-packages nil)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4)
 '(recorder-default-writing-dir "/home/sven/archive/movie/rec")
 '(recorder-ffmpeg-capture-coords '(1920 150 3840 1080))
 '(recorder-ffmpeg-video-filter
   '((1:v (scale 384:216) cam) (2:v cam (overlay 30:684) video)))
 '(recorder-playback-program "mpv")
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

;; Ellama LLM assistant
;; (use-package ellama
;;   :ensure t
;;   :bind ("C-a" . ellama-transient-main-menu)
;;   :init
;;   ;; setup key bindings
;;   ;; (setopt ellama-keymap-prefix "C-c e")
;;   ;; language you want ellama to translate to
;;   (setopt ellama-language "English")
;;   ;; could be llm-openai for example
;;   (require 'llm-ollama)
;;   (setopt ellama-provider
;;   	  (make-llm-ollama
;;   	   ;; this model should be pulled to use it
;;   	   ;; value should be the same as you print in terminal during pull
;;   	   :chat-model "llama3:8b-instruct-q8_0"
;;   	   :embedding-model "nomic-embed-text"
;;   	   :default-chat-non-standard-params '(("num_ctx" . 8192))))
;;   (setopt ellama-summarization-provider
;;   	  (make-llm-ollama
;;   	   :chat-model "qwen2.5:3b"
;;   	   :embedding-model "nomic-embed-text"
;;   	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
;;   (setopt ellama-coding-provider
;;   	  (make-llm-ollama
;;   	   :chat-model "qwen2.5-coder:3b"
;;   	   :embedding-model "nomic-embed-text"
;;   	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
;;   ;; Predefined llm providers for interactive switching.
;;   ;; You shouldn't add ollama providers here - it can be selected interactively
;;   ;; without it. It is just example.
;;   (setopt ellama-providers
;;   	  '(("zephyr" . (make-llm-ollama
;;   			 :chat-model "zephyr:7b-beta-q6_K"
;;   			 :embedding-model "zephyr:7b-beta-q6_K"))
;;   	    ("mistral" . (make-llm-ollama
;;   			  :chat-model "mistral:7b-instruct-v0.2-q6_K"
;;   			  :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
;;   	    ("mixtral" . (make-llm-ollama
;;   			  :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
;;   			  :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
;;   ;; Naming new sessions with llm
;;   (setopt ellama-naming-provider
;;   	  (make-llm-ollama
;;   	   :chat-model "llama3:8b-instruct-q8_0"
;;   	   :embedding-model "nomic-embed-text"
;;   	   :default-chat-non-standard-params '(("stop" . ("\n")))))
;;   (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
;;   ;; Translation llm provider
;;   (setopt ellama-translation-provider
;;   	  (make-llm-ollama
;;   	   :chat-model "qwen2.5:3b"
;;   	   :embedding-model "nomic-embed-text"
;;   	   :default-chat-non-standard-params
;;   	   '(("num_ctx" . 32768))))
;;   (setopt ellama-extraction-provider (make-llm-ollama
;;   				      :chat-model "qwen2.5-coder:7b-instruct-q8_0"
;;   				      :embedding-model "nomic-embed-text"
;;   				      :default-chat-non-standard-params
;;   				      '(("num_ctx" . 32768))))
;;   ;; customize display buffer behaviour
;;   ;; see ~(info "(elisp) Buffer Display Action Functions")~
;;   (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
;;   (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
;;   :config
;;   ;; send last message in chat buffer with C-c C-c
;;   (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))

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
(global-set-key (kbd "C-x _") (lambda () (interactive) (insert-sep-region "_" 3)))

(global-set-key (kbd "C-x C-g g") (lambda () (interactive)
                                    (make-process
                                     :name "gitg"
                                     :buffer "*gitg*"
                                     :command '("gitg"))))

(define-key epa-key-list-mode-map (kbd "C-s") 'epa-mark-key)
(define-key epa-key-list-mode-map (kbd "C-u") 'epa-unmark-key)

;; Make numpad decimal separator behave like period rather than coma.
(global-set-key [kp-separator] (kbd "."))

;; Enable envrc
(envrc-global-mode)

(provide 'init)
;;; init.el ends here
