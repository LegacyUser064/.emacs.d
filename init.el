;;; init.el -*- lexical-binding: t; -*-

;; Disable garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Change working directory to config directory
(setq default-directory "~/.emacs.d/")

(defconst my-cache-dir (concat user-emacs-directory "cache/"))
(defconst my-config-dir (concat user-emacs-directory "config/"))
(defconst my-local-dir (concat user-emacs-directory "local/"))

;; Disable GUI widgets
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Quiet Startup
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Custom file
(setq custom-file (concat my-config-dir "custom.el"))

;; Disable lock files
(setq create-lockfiles nil)

;; Backup file control
(setq make-backup-files nil
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 1
      backup-directory-alist (list (cons "." (concat my-cache-dir "backups/")))
      tramp-backup-directory-alist backup-directory-alist)

;; Minibuffer
(setq enable-recursive-minibuffers t
      echo-keystrokes 0.02
      minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Autosave
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat my-cache-dir "autosave/")
      tramp-auto-save-directory (concat my-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Rename window
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

;; Yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Tabs
(setq-default indent-tabs-mode nil
	          tab-width 4
	          tab-always-indent t)

;; Fill column
(setq-default fill-column 80)

;; Sentences
(setq sentence-end-double-space nil)

;; Require final newline
(setq require-final-newline t)

;; Bell
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Cursor
(blink-cursor-mode -1)

;; Fringe
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Initialize package manager (Straight)
(defvar bootstrap-version)
(setq straight-base-dir (concat my-local-dir "straight/"))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
(straight-use-package 'diminish)

;; GCMH
(use-package gcmh
  :diminish gcmh-mode
  :hook
  ((emacs-startup . gcmh-mode))
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024)))

;; General
(use-package general
  :config
  (general-create-definer my-major-mode-definer
    :prefix "C-c m")
  (general-create-definer my-toggle-definer
    :prefix "C-c t"))

;; Hungry-Delete
(use-package smart-hungry-delete
  :bind
  (("C-d" . smart-hungry-delete-forward-char)
   ("<backspace>" . smart-hungry-delete-backward-char)))

;; Autorevert
(use-package autorevert
  :defer t
  :diminish auto-revert-mode
  )

;; Recentf
(use-package recentf
  :defer 2
  :config
  (setq recentf-save-file (concat my-cache-dir "recentf")
	    recentf-auto-cleanup 5))

;; Line numbers
(use-package display-line-numbers
  :hook
  ((prog-mode . display-line-numbers-mode))
  :general
  (my-toggle-definer
    "l" 'display-line-numbers-mode))

;; Eldoc
(use-package eldoc
  :defer t
  :diminish eldoc-mode)

;; Dired
(use-package dired+)

;; Which-Key
(use-package which-key
  :diminish which-key-mode
  :hook
  ((after-init . which-key-mode)))

;; Helm
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("M-y" . helm-show-kill-ring)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-h a" . helm-apropos)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :config
  (setq helm-ff-file-name-history-use-recentf t
        helm-M-x-always-save-history t))

(use-package helm-swoop
  :bind
  (("C-s" . helm-swoop)))

;; Avy
(use-package avy
  :bind
  (("C-;" . avy-goto-char-timer)
   ("M-g g" . my-avy-goto-line))
  :config
  (defun my-avy-goto-line ()
    "A wrapper around avy-goto-line that recenters the screen after the jump"
    (interactive)
    (avy-goto-line)
    (recenter)))

;; Highlight Line
(use-package hl-line
  :hook
  ((prog-mode . hl-line-mode))
  :general
  (my-toggle-definer
    "h" 'hl-line-mode))

;; Theme
(use-package zenburn-theme
  :config (load-theme 'zenburn t))

;; Electric Pair
(use-package elec-pair
  :hook
  ((emacs-lisp-mode . electric-pair-mode))
  :general
  (my-toggle-definer
    "p" 'electric-pair-mode)
  :config
  (setq electric-pair-preserve-balance t))

;; Treemacs
(use-package treemacs
  :general
  (my-toggle-definer
    "t" 'treemacs)
  :config
  (setq treemacs-persist-file (concat my-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat my-cache-dir "treemacs-persist-at-last-error")))

(use-package treemacs-projectile
  :after treemacs)

;; Company
(use-package company
  :diminish company-mode
  :hook
  ((emacs-lisp-mode . company-mode))
  :bind
  (:map
   company-active-map
   ("<tab>" . company-complete-common-or-cycle)
   ("<backtab>" . company-select-previous))
  :general
  (my-toggle-definer
    "c" 'company-mode)
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-selection-wrap-around t
	company-show-numbers t
	company-format-margin-function nil)
  
  (setq-default company-backends
		'(company-files
		  company-keywords
		  company-capf
		  company-dabbrev-code
		  company-dabbrev)))

;; Esup
(use-package esup
  :disabled
  :defer t
  :commands (esup))

;; Projectile
(use-package projectile
  :defer 1
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/projects/")
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-cache-file (concat my-cache-dir "projectile.cache")
        projectile-known-projects-file (concat my-cache-dir "known-projects.eld"))

  (projectile-mode 1))

(use-package helm-projectile
  :bind
  (([remap projectile-switch-project] . helm-projectile-switch-project)
   ([remap projectile-find-file] . helm-projectile-find-file)))

;; Transient
(use-package transient
  :defer t
  :config
  (setq transient-history-file (concat my-cache-dir "transient-history.el")))

;; Magit
(use-package magit
  :defer 1)

;; Yasnippet
(use-package yasnippet
  :defer t)

;; Flycheck
(use-package flycheck
  :defer t
  :config
  (unbind-key "C-c !" flycheck-mode-map))

;; LSP
(use-package lsp-mode
  :defer t
  :commands lsp-deferred
  :hook
  ((lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.30
        lsp-auto-configure t
        lsp-completion-enable t
        lsp-enable-snippet t
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-indentation t
        lsp-enable-links nil
        lsp-enable-on-type-formatting nil
        lsp-enable-text-document-color nil
        lsp-enable-xref nil
        lsp-enable-semantic-highlighting nil
        lsp-keep-workspace-alive nil
        lsp-server-install-dir (concat my-local-dir "lsp/")
        lsp-session-file (concat my-cache-dir "lsp-session"))

  ;; Diagnostics
  ;;(setq lsp-diagnostics-provider :)

  ;; Modeline
  (setq lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable t)

  ;; Headerline
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-headerline-breadcrumb-icons-enable nil)

  ;; Lens
  (setq lsp-lens-enable nil))


(use-package helm-lsp
  :bind
  (:map lsp-mode-map
   ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))

;; Tree-Sitter
(use-package tree-sitter
  :defer t
  :config
  (use-package tree-sitter-langs))

;;; EmacsLisp

;;; Python
(use-package lsp-pyright
  :hook
  ((python-mode . (lambda ()
                    (require 'lsp-pyright)
                    (lsp-deferred)
                    (setq-local company-backends '(company-capf))))))

(use-package pyvenv
  :defer 2
  :config
  (pyvenv-mode)
  (pyvenv-tracking-mode))

(use-package python
  :hook
  ((python-mode . (lambda ()
                    (electric-pair-mode)
                    (tree-sitter-mode)
                    (tree-sitter-hl-mode)))))
