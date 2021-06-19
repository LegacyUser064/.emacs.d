;;; init.el -*- lexical-binding: t; -*-

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

;; Autorevert
(use-package autorevert
  :diminish auto-revert-mode)

;; Recentf
(use-package recentf
  :config
  (setq recentf-save-file (concat my-cache-dir "recentf")
	    recentf-auto-cleanup 5)

  (recentf-mode 1))

;; Line numbers
(use-package display-line-numbers
  :hook
  ((prog-mode . display-line-numbers-mode)))

;; Eldoc
(use-package eldoc
  :defer t
  :diminish eldoc-mode)

;; All the icons
(use-package all-the-icons)

;; Dired
(use-package dired
  :defer t)

(use-package dired+
  :after dired)

(use-package all-the-icons-dired
  :hook
  ((dired-mode . all-the-icons-dired-mode)))

;; Which-Key
(use-package which-key
  :diminish which-key-mode
  :hook
  ((after-init . which-key-mode)))

(use-package general
  :config
  (general-create-definer my-major-mode-definer
    :prefix "C-c m"))

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
  (setq helm-ff-file-name-history-use-recentf t))

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
  ((prog-mode . hl-line-mode)))

;; Theme
(use-package zenburn-theme
  :config (load-theme 'zenburn t))

;; Electric Pair
(use-package elec-pair
  :hook
  ((emacs-lisp-mode . electric-pair-mode))
  :config
  (setq electric-pair-preserve-balance t))

;; Treemacs
(use-package treemacs
  :bind
  (("C-c t t" . treemacs))
  :config
  (setq treemacs-persist-file (concat my-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat my-cache-dir "treemacs-persist-at-last-error")))

(use-package treemacs-projectile
  :after (treemacs projectile))

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
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 1
	company-selection-wrap-around t
	company-show-numbers t
	company-format-margin-function nil)
  
  (setq-default company-backends
		'(company-files
		  company-keywords
		  company-capf
		  company-dabbrev-code
		  company-dabbrev)))

;; Projectile
(use-package projectile
  :hook
  ((after-init . projectile-mode))
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/projects/")
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-cache-file (concat my-cache-dir "projectile.cache")
        projectile-known-projects-file (concat my-cache-dir "known-projects.eld")))

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on))

;; Diff-hl
(use-package diff-hl
  :hook
  ((after-init . global-diff-hl-mode))
  :config
  (setq diff-hl-flydiff-delay nil)
  
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode))

;; Transient
(use-package transient
  :defer t
  :config
  (setq transient-history-file (concat my-cache-dir "transient-history.el")))

;; Magit
(use-package magit)

;;; EmacsLisp
