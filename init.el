;;; init.el --- Main configuration -*- lexical-binding: t; no-byte-compile: t -*-

(elpaca-wait)

;;; Core Emacs Settings
(use-package emacs
  :ensure nil
  :init
  ;; Directory organization (consistent with early-init.el)
  (defvar emacs-var-dir (expand-file-name "var/" user-emacs-directory))
  (unless (file-exists-p emacs-var-dir) (make-directory emacs-var-dir t))
  
  ;; User info
  (setq user-mail-address "vp@paulaus.com"
        user-full-name "Vytautas Paulauskas")
  
  :custom
  ;; Basic settings
  (use-short-answers t)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (tab-width 2)
  (truncate-lines t)
  (require-final-newline t)
  
  ;; Performance
  (auto-window-vscroll nil)
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (bidi-paragraph-direction 'left-to-right)
  (so-long-threshold 1000)
  
  ;; Completion (Emacs 30.1)
  (completion-cycle-threshold 3)
  (completions-detailed t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  
  ;; Scrolling
  (scroll-margin 8)
  (scroll-preserve-screen-position t)
  (hscroll-step 1)
  (hscroll-margin 1)
  
  ;; Frame title
  (frame-title-format '("Emacs 30.1 - " (:eval (if (buffer-file-name)
                                                     (abbreviate-file-name (buffer-file-name))
                                                   "%b")))))  ; Close use-package emacs

;;; File Management
(use-package files
  :ensure nil
  :custom
  ;; Backup organization
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist `(("." . ,(expand-file-name "backups/" emacs-var-dir))))
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" emacs-var-dir) t)))
  (auto-save-no-message t)
  (auto-save-interval 100)
  :config
  (let ((auto-save-dir (expand-file-name "auto-save/" emacs-var-dir)))
    (unless (file-exists-p auto-save-dir)
      (make-directory auto-save-dir t))))

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file (expand-file-name "custom.el" emacs-var-dir))
  :hook (elpaca-after-init . (lambda () (load custom-file :noerror))))

;;; Memory & Performance
(use-package gcmh
  :ensure t
  :demand t
  :config (gcmh-mode 1))

;;; Essential UI
(use-package menu-bar :ensure nil :config (menu-bar-mode 1)) ; Keep on macOS
(use-package tool-bar :ensure nil :config (tool-bar-mode -1))
(use-package scroll-bar :ensure nil :config (scroll-bar-mode -1))
(use-package delsel :ensure nil :config (delete-selection-mode 1))
(use-package hl-line :ensure nil :config (global-hl-line-mode 1))
(use-package paren :ensure nil :hook (prog-mode . show-paren-mode))
(use-package autorevert :ensure nil :config (global-auto-revert-mode 1))

;;; History & Persistence
(use-package savehist
  :ensure nil
  :custom
  (history-length 100)
  (savehist-file (expand-file-name "history" emacs-var-dir))
  :config (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :custom
  (save-place-file (expand-file-name "places" emacs-var-dir))
  :config (save-place-mode 1))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 100)
  (recentf-save-file (expand-file-name "recentf" emacs-var-dir))
  (recentf-exclude `(,(regexp-quote emacs-var-dir) "\\.git/"))
  :config (recentf-mode 1))

;;; Fonts & Typography
(use-package font-setup
  :ensure nil
  :hook (after-init . setup-fonts)
  :preface
  (defun font-available-p (font-name)
    "Check if FONT-NAME is available."
    (find-font (font-spec :name font-name)))
  
  (defun setup-fonts ()
    "Setup fonts optimized for macOS."
    (let* ((mono-fonts '("Aeonik Mono", "SF Mono" "Menlo" "Monaco"))
           (prop-fonts '("Codelia", "SF Pro Text" "Helvetica Neue" "Helvetica"))
           (mono-font (cl-find-if #'font-available-p mono-fonts))
           (prop-font (cl-find-if #'font-available-p prop-fonts)))

      (when mono-font
        (set-face-attribute 'default nil :family mono-font :height 200)
        (set-face-attribute 'fixed-pitch nil :family mono-font))
      
      (when prop-font
        (set-face-attribute 'variable-pitch nil :family prop-font :height 1.0))
      
      (message "Fonts: mono='%s' prop='%s'" (or mono-font "default") (or prop-font "default"))))
  (provide 'font-setup))

;;; Theme
(use-package uwu-theme
  :ensure t
  :demand t
  :custom
  (uwu-distinct-line-numbers nil)
  (uwu-height-title-3 1.1)
  (uwu-scale-org-headlines t)
  (uwu-use-variable-pitch t)
  :config
  (load-theme 'uwu t))

;;; Icons
(use-package nerd-icons
  :ensure t
  :demand t)

(use-package doom-modeline
  :ensure t
  :demand t
  :after nerd-icons
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 15)
  :config
  (doom-modeline-mode 1))

;;; Completion Framework
;; Enhanced completion settings
(setopt completion-cycle-threshold 1                   ; TAB cycles candidates
        completion-auto-help 'always                   ; Open completion always; `lazy' another option
        completions-max-height 20                      ; This is arbitrary
        completions-detailed t
        completions-format 'one-column
        completions-group t
        completion-auto-select 'second-tab)            ; Much more eager

(use-package vertico
  :ensure t
  :demand t
  :custom
  (vertico-cycle t)
  (vertico-scroll-margin 2)
  :config (vertico-mode 1))

(use-package marginalia
  :ensure t
  :demand t
  :config (marginalia-mode 1))

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides 
   '((file (styles basic partial-completion))
     (command (styles orderless))
     (variable (styles orderless))
     (symbol (styles orderless)))))

(use-package corfu
  :ensure t
  :demand t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-scroll-margin 2)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  :config (global-corfu-mode 1))

(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; Nerd Icons Integrations
;; For Marginalia
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; For Corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; For dired
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Search & Navigation
(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (search-default-mode #'char-fold-to-regexp))

(use-package consult
  :ensure t
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<"))

(use-package which-key
  :ensure t
  :custom (which-key-idle-delay 0.8)
  :config (which-key-mode 1))

(use-package ace-window
  :ensure t
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config (ace-window-display-mode 1))

;;; Org Mode
(use-package calendar
  :ensure nil
  :custom (calendar-week-start-day 1))

(use-package org
  :ensure nil
  :hook 
  ((org-mode . org-indent-mode)
   (org-mode . visual-line-mode)
   (org-mode . variable-pitch-mode))
  :custom
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  (org-startup-folded 'content)
  (org-capture-bookmark nil)
  (org-id-locations-file (expand-file-name "org-id-locations" emacs-var-dir))
  (org-clock-persist-file (expand-file-name "org-clock-save.el" emacs-var-dir))
  (org-directory "~/Documents/Org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory)))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-todo-faces '(("TODO" :background "red" :foreground "white")
                           ("DONE" :background "green" :foreground "white"))))

;;; Org-roam - Personal Knowledge Management
(use-package org-roam
  :ensure t
  :demand t
  :custom
  (org-roam-directory (file-truename "~/Notes"))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("n" "note" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :note:\n")
      :unnarrowed t)
     ("p" "project" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :project:\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  :config
  ;; Ensure directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Initialize the database
  (org-roam-db-autosync-mode)
  
  ;; Add org-roam files to org-agenda if needed
  (setq org-agenda-files (append org-agenda-files
                                 (list org-roam-directory))))

;; Enhanced search for org-roam with content previews
(use-package consult-org-roam
  :ensure t
  :after (consult org-roam)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  ;; Activate the minor mode
  (consult-org-roam-mode 1))

;;; Tree-sitter & Language Support
(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :custom
  (major-mode-remap-alist
   '((python-mode . python-ts-mode)
     (bash-mode . bash-ts-mode)
     (go-mode . go-ts-mode)
     (markdown-mode . markdown-ts-mode)))
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown"))))

;;; Development Tools
(use-package eglot
  :ensure nil
  :hook
  ((python-mode python-ts-mode) . eglot-ensure)
  ((go-mode go-ts-mode) . eglot-ensure)
  ((markdown-mode markdown-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-connect-timeout 10)
  :config
  (add-to-list 'eglot-server-programs '((go-mode go-ts-mode) . ("gopls")))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pylsp")))
  (add-to-list 'eglot-server-programs '((markdown-mode markdown-ts-mode) . ("marksman"))))

(use-package consult-eglot
  :ensure t
  :after (consult eglot))

(use-package eldoc
  :ensure nil
  :custom (eldoc-echo-area-use-multiline-p nil))

;;; Language Modes
(use-package go-mode
  :ensure t
  :defer t
  :custom
  (gofmt-command "goimports")
  :hook (before-save . gofmt-before-save))

(use-package markdown-mode
  :ensure t
  :defer t
  :hook (markdown-mode . visual-line-mode))

;;; Git
;; Ensure transient is updated first to meet Magit's requirements
(use-package transient
  :ensure t
  :demand t)

(use-package magit
  :ensure t
  :defer t
  :after transient
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 'all))

(use-package diff-hl
  :ensure t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (global-diff-hl-mode 1))

;;; Utilities
(use-package vundo
  :ensure t
  :defer t)

(use-package reverse-im
  :ensure t
  :defer 2
  :custom
  (reverse-im-char-fold t)
  (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode 1))

;;; Golden Ratio
;; Automatically resize windows so the working window is always large enough.
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :custom
  (golden-ratio-auto-scale t)
  :config
  (golden-ratio-mode 1))

;;; Cursor
;; Adaptive cursor width: make cursor the width of the character it is under. i.e. full width of a TAB.
;; https://pragmaticemacs.wordpress.com/2017/10/01/adaptive-cursor-width/
(setopt x-stretch-cursor t)

;;; Cursor trail on focus
(use-package beacon
  :ensure t
  :custom
  (beacon-blink-delay 0.6)
  (beacon-blink-duration 0.6)
  (beacon-color "#C7FF00")
  (beacon-push-mark 35)
  :config
  (beacon-mode 1))

;;; Visual feedback for operations
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-alhF")
  (dired-dwim-target t)
  :bind (:map dired-mode-map
              ;; Colemak HNEI navigation bindings
              ("n" . dired-next-line)
              ("e" . dired-previous-line)
              ("i" . dired-find-file)
              ("h" . dired-up-directory))
  :preface
  (defun dired-open-externally (&optional arg)
    "Open marked or file at point in external application."
    (interactive "P")
    (dired-map-over-marks
     (let ((file (dired-get-filename)))
       (cond
        ((eq system-type 'darwin) (shell-command (concat "open " (shell-quote-argument file))))
        ((eq system-type 'gnu/linux) (shell-command (concat "xdg-open " (shell-quote-argument file))))
        (t (find-file file))))
     arg))
  :config
  ;; Auto-open non-text files with external app
  (defun dired-find-file-or-external ()
    "Find file, but if it's not a text file, open externally."
    (interactive)
    (let* ((file (dired-get-filename))
           (extension (file-name-extension file))
           (text-extensions '("txt" "org" "md" "py" "go" "sh" "el" "lisp" "c" "h" "cpp" "hpp" "js" "html" "css" "json" "yaml" "yml" "xml")))
      (if (and extension (not (member (downcase extension) text-extensions)))
          (dired-open-externally)
        (dired-find-file))))
  ;; Override default find-file binding
  (define-key dired-mode-map (kbd "RET") 'dired-find-file-or-external))

;;; Key Bindings
(use-package bind-key :ensure nil :demand t)

;; TAB completion in minibuffer
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;; Global bindings
(bind-keys
 ("M-o" . ace-window)
 ("M-u" . vundo)
 ("M-g i" . consult-imenu)
 ("M-g g" . consult-goto-line)
 ("M-s r" . consult-ripgrep)
 ("M-s l" . consult-line)
 ("C-x b" . consult-buffer)
 ("C-x r b" . consult-bookmark)
 ("C-x C-r" . recentf)
 ("M-y" . consult-yank-pop))

;; Org bindings
(with-eval-after-load 'org
  (bind-keys
   :prefix "C-c o"
   :prefix-map org-prefix-map
   ("a" . org-agenda)
   ("c" . org-capture)
   ("l" . org-store-link)))

;; Org-roam bindings
(with-eval-after-load 'org-roam
  (bind-keys
   :prefix "C-c n"
   :prefix-map org-roam-prefix-map
   ("f" . org-roam-node-find)
   ("r" . org-roam-node-random)
   ("i" . org-roam-node-insert)
   ("c" . org-roam-capture)
   ("j" . org-roam-dailies-capture-today)
   ("d" . org-roam-dailies-goto-today)
   ("D" . org-roam-dailies-goto-date)
   ("n" . org-roam-dailies-goto-next-note)
   ("p" . org-roam-dailies-goto-previous-note)
   ("t" . org-roam-tag-add)
   ("T" . org-roam-tag-remove)
   ("b" . org-roam-buffer-toggle)
   ("g" . org-roam-graph)
   ("s" . consult-org-roam-search)           ; Search note contents
   ("S" . consult-org-roam-forward-links)    ; Search forward links
   ("B" . consult-org-roam-backlinks)))      ; Search backlinks

;; Development bindings
(bind-keys
 :map prog-mode-map
 ("C-c f" . eglot-format-buffer)
 ("C-c r" . eglot-rename)
 ("C-c a" . eglot-code-actions)
 ("C-c s" . consult-eglot-symbols))

;; Dired bindings
(with-eval-after-load 'dired
  (bind-keys
   :map dired-mode-map
   ("<backspace>" . dired-up-directory)
   ("~" . (lambda () (interactive) (dired "~")))
   ("E" . dired-open-externally))) ; External app shortcut

;; Isearch bindings
(bind-keys
 :map isearch-mode-map
 ("M-s l" . consult-line))

;;; Performance Restoration
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

(provide 'init)
;;; init.el ends here
