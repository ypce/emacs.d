;;; post-init.el --- 4 -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Let's go! System
;;;; Flycheck
;; Recommends using purcell/exec-path-from-shell on macOS to fix the $PATH environment variable:
(use-package exec-path-from-shell
  :init
  ;; Use non-interactive shell
  (setopt exec-path-from-shell-arguments nil
          exec-path-from-shell-variables '("PATH" "MANPATH")) ;; Only initialize specific variables we need, for better performance
  :if (memq window-system '(mac ns x))
  :hook
  (after-init . exec-path-from-shell-initialize))

;;;; after-init-hook for non-use-package modes
(add-hook 'kill-emacs-hook #'recentf-cleanup)

(defun setup-builtin-modes ()
  "Set up all built-in minor modes that should be enabled globally."
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (global-auto-revert-mode 1)
  (auto-save-visited-mode 1)
  (column-number-mode 1)
  (line-number-mode 1)
  (display-time-mode 1)
  (setopt mode-line-position-column-line-format '("%l:%C")))
(add-hook 'after-init-hook #'setup-builtin-modes)

;;;; Auto-save settings
(setopt auto-save-default t
        auto-save-interval 300
        auto-save-timeout 30
        auto-save-visited-interval 5)   ; Save after 5 seconds if inactivity
(auto-save-visited-mode 1)

;;;; Automatic Byte-compiling and Native-compiling of all .el files
(use-package compile-angel
  :custom
  (compile-angel-verbose t)
  :config
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode))

;;;; Use a dedicated file for Emacs customizations.
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))
; ;; Preventing Emacs from saving custom.el
; (setopt custom-file null-device)

;;;; Backup Settings
;; Enabled backups save your changes to a file intermittently
(setopt make-backup-files t
        backup-directory-alist `(("." . ,(expand-file-name "backup/" user-emacs-directory)))
        vc-make-backup-files t
        kept-old-versions 10
        kept-new-versions 10)

;;;; SSH Management
(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 2)  ; Increase for debugging (max is 10)
  (tramp-completion-reread-directory-timeout nil))

;; SSH key management via auth-source
(use-package auth-source
  :ensure nil  ; Built-in
  :custom
  (auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
  :config
  (auth-source-pass-enable))  ; Use password-store if available

;;;; crux utility packages
(use-package crux
  :commands (crux-delete-file-and-buffer crux-rename-file-and-buffer))

;;;; Automatically kill running processes on exit
(setopt confirm-kill-processes nil)

;;;; Local packages (future)
;; Make local packages from ~/.emacs.d/lisp/ available.
(add-to-list 'load-path (expand-file-name "lisp" minimal-emacs-user-directory))

;;; UI
;;;;  Frame Settings
;; Big & Centered
(add-to-list 'default-frame-alist '(width . 130)) ; Set width to 80 columns
(add-to-list 'default-frame-alist '(height . 40)) ; Set height to 24 lines

(defun center-frame ()
  "Center the frame on the screen, respecting the size set in default-frame-alist."
  (interactive)
  (let* ((desired-width
          (or (cdr (assq 'width default-frame-alist)) 80))
         (desired-height
          (or (cdr (assq 'height default-frame-alist)) 24))
         (screen-width (x-display-pixel-width))
         (screen-height (x-display-pixel-height))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         (frame-pixel-width (* desired-width char-width))
         (frame-pixel-height (* desired-height char-height))
         (left (max 0 (/ (- screen-width frame-pixel-width) 2)))
         (top (max 0 (/ (- screen-height frame-pixel-height) 2))))
    (set-frame-size (selected-frame) desired-width desired-height)
    (set-frame-position (selected-frame) left top)))

(add-hook 'window-setup-hook #'center-frame)

;; Full Screen
(defun toggle-fullscreen-or-maximized ()
  "Toggle between fullscreen on macOS and maximized on Linux."
  (interactive)
  (if (eq system-type 'darwin) ; macOS
      (set-frame-parameter
       nil 'fullscreen
       (if (frame-parameter nil 'fullscreen)
           nil
         'fullboth))
    (set-frame-parameter
     nil 'fullscreen
     (if (eq (frame-parameter nil 'fullscreen) 'maximized)
         nil
       'maximized)))) ; Linux

(add-hook 'window-setup-hook #'toggle-fullscreen-or-maximized)

;;;; Smooth Scrolling
;; Use momentum for smooth scrolling
(when (and (eq window-system 'ns) 
           (>= emacs-major-version 29)) ; Pixel scrolling is in Emacs 29+
  (setopt pixel-scroll-precision-use-momentum t) ; Use momentum for smooth feel
  (pixel-scroll-precision-mode 1)) 

;;;; Theme
(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))

;; Add to safe themes list
(setopt custom-safe-themes '("dracula-pro-pro"))
;; (use-package dracula-pro-theme
;;   :config
;;   (load-theme 'dracula-pro-pro t))

(use-package uwu-theme
  :custom
  (uwu-distinct-line-numbers 'nil)
  (uwu-height-title-3 1.1)
  (uwu-scale-org-headlines t)
  (uwu-use-variable-pitch t)
  :config (load-theme 'uwu t))

;;;; Font
(defun font-available-p (font-name)
  (find-font (font-spec :name font-name)))

(let ((mono-spaced-font "Aeonik Mono")
      (proportionately-spaced-font "Codelia"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 200)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 0.8))

(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode))

;;;; Tab-bar
(setopt tab-bar-show 1
        tab-bar-close-button nil
        tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))

(define-key global-map (kbd "C-x t n") 'tab-new)
(define-key global-map (kbd "C-x t k") 'tab-close)

;;;; Modeline
(use-package simple-modeline
  :after nerd-icons
  :custom
  (simple-modeline-segments '((simple-modeline-segment-modified
                               simple-modeline-segment-buffer-name
                               simple-modeline-segment-position)
                              (simple-modeline-segment-input-method
                               simple-modeline-segment-eol
                               simple-modeline-segment-encoding
                               simple-modeline-segment-vc
                               simple-modeline-segment-misc-info
                               simple-modeline-segment-process
                               simple-modeline-segment-major-mode)))
  :config
  (simple-modeline-mode 1))
  

;; After installing nerd-icons, you need to run M-x nerd-icons-install-fonts once.
(use-package nerd-icons)

;; TAB acts more like how it does in the shell
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;;;; More nerd-icons
;; For Marginalia
(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; For Corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; For dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;;; Minibuffer completion
;; TODO Check https://www.masteringemacs.org/articleu nderstanding-minibuffer-completion
(setopt completion-cycle-threshold 1                   ; TAB cycles candidates
        completion-styles '(basic initials substring)  ; Different styles to match input to candidates
        completion-auto-help 'always                   ; Open completion always; `lazy' another option
        completions-max-height 20                      ; This is arbitrary
        completions-detailed t
        completions-format 'one-column
        completions-group t
        completion-auto-select 'second-tab)            ; Much more eager

;;;; Golden ratio
;; Automatically resize windows so the working window is always large enough.
(use-package golden-ratio
  :diminish golden-ratio-mode
  :custom
  (golden-ratio-auto-scale t)
  :config
  (golden-ratio-mode 1))

;;;; Cursor
;; Adaptive cursor width: make cursor the width of the character it is under. i.e. full width of a TAB.
;; https://pragmaticemacs.wordpress.com/2017/10/01/adaptive-cursor-width/
(setopt x-stretch-cursor t)

;;;; cursor trail on focus
(use-package beacon
  :custom
  (beacon-blink-delay 0.6)
  (beacon-blink-duration 0.6)
  (beacon-color "#F7433E")
  (beacon-push-mark 35)
   :config
  (beacon-mode 1))

;;;; Line numbers
;; Display line numbers in programming mode.
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(setopt display-line-numbers-width 3            ; Set a minimum width
        display-line-numbers-type 'relative)    ; Relative line numbers

;;;; Text wrapping
;; Unified text wrapping configuration
(use-package visual-fill-column
  :hook ((text-mode . visual-line-mode)
         (markdown-mode . visual-line-mode)
         (org-mode . visual-line-mode)
         (visual-line-mode . visual-fill-column-mode))
  :custom
  (visual-fill-column-width 90)        ; Set desired column width
  (visual-fill-column-center-text t)   ; Center text in buffer
  (visual-fill-column-fringes-outside-margins t))

;; Ensure consistent behavior across modes
(setopt word-wrap t                    ; Wrap at word boundaries
        truncate-lines nil             ; Don't truncate lines by default
        truncate-partial-width-windows nil) ; Don't truncate in split windows

;;;; Highlight line
(use-package hl-line
  :ensure nil
  :hook ((text-mode prog-mode) . hl-line-mode))

;;;; Underlines
;; Prettier underlines. (Bedrock Emacs)
(setopt x-underline-at-descent-line nil
        show-trailing-whitespace t)    ; By default, do underline trailing spaces.

;;; Window/Buffer Management
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))    ; Track changes in the window configuration

(use-package window-divider
  :ensure nil
  :hook (after-init . window-divider-mode)
  :custom
  (window-divider-default-places t)    ; Show dividers on all edges
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1))

;;;; Unique buffer names
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;;; Editing
;;;; Keybindings
(use-package emacs
  :ensure nil
  :bind
  (("M-k" . (lambda () (interactive) (kill-this-buffer nil)))
   ("C-c C-b" . compile)
   ("M-0" . delete-window)
   ("M-1" . delete-other-windows)
   ("M-2" . split-window-below)
   ("M-3" . split-window-right)
   ("M-u" . upcase-dwim)
   ("M-l" . downcase-dwim)
   ("C-." . xref-find-definitions-other-window)
   ("RET" . newline-and-indent)))

; Disable Ctrl + mouse wheel up/down zooming in/out
(global-unset-key (kbd "C-<wheel-up>"))   ;; Unbind Ctrl + Wheel Up
(global-unset-key (kbd "C-<wheel-down>")) ;; Unbind Ctrl + Wheel Down

;;;; Multiple Cursors (MC)
(use-package multiple-cursors
  :bind
  (("C-M-j" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-M-=" . mc/mark-all-symbols-like-this)))

;;;; Avy
(use-package avy
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;; Undo/Redo
;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
;; system, providing more convenient undo/redo functionality.
(use-package undo-fu
  :commands (undo-fu-only-undo undo-fu-only-redo undo-fu-only-redo-all undo-fu-disable-checkpoint)
  :bind
  (("C-z" . undo-fu-only-undo)
   ("C-S-z" . undo-fu-only-redo))
  :init
  (global-unset-key (kbd "C-z")))

;; The undo-fu-session package complements undo-fu by enabling the saving
;; and restoration of undo history across Emacs sessions, even after restarting.
(use-package undo-fu-session
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

;;;; Paren Match Highlighting
;; Highlights matching parentheses, brackets, and braces when the cursor is positioned at one of them.
(add-hook 'after-init-hook #'show-paren-mode)

;;;; Key Assistance
(use-package which-key
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;;; Completion & Search
;;;; Hitting TAB behavior
(setopt tab-always-indent nil)

;;;; Corfu
(use-package corfu
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  ;; Enable Corfu
  :config
  (global-corfu-mode))

;;;; Cape
(use-package cape
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;; Virtico
(use-package vertico
  :commands vertico-mode
  :hook (after-init . vertico-mode))

;;;; Orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;;; Marginalia
(use-package marginalia
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;;;; Embark
;; Context Menu & Actions
 (use-package embark
   :commands (embark-act
              embark-dwim
              embark-export
              embark-collect
              embark-bindings
              embark-prefix-help-command)
   :bind
   (("C-." . embark-act)         ;; pick some comfortable binding
    ("C-;" . embark-dwim)        ;; good alternative: M-.
    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

   :init
   (setopt prefix-help-command #'embark-prefix-help-command)

   :config
   ;; Hide the mode line of the Embark live/completions buffers
   (add-to-list 'display-buffer-alist
                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                  nil
                  (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Enhanced Search & Navigation
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setopt register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setopt xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setopt consult-narrow-key "<"))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;; File Management
;;;; Dired
(use-package dired
  :ensure nil
  :commands dired
  :custom
  (dired-listing-switches "-ahl")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setopt dired-clean-confirm-killing-deleted-buffers nil)
  (setopt dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (let ((cmd "open"))
    (setopt dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(use-package dired-aux
  :ensure nil
  :after dired
  :custom
  (dired-create-destination-dirs 'always)
  (dired-do-revert-buffer t)
  (dired-vc-rename-file t))

;; Dired fontlock
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;;; Help & Documentation
;;;; Better Help
(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(setopt help-window-select t)
(setopt echo-keystrokes-help nil)

;;; Notes
;;;; Org Mode
(use-package org
  :ensure nil
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-directory "~/Org/")
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-cycle-separator-lines 2)
  (org-fold-core-style 'overlays)
  (imenu-auto-rescan t)
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated nil)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  :config
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (dolist (face '(org-table org-list-dt org-tag org-quote
                            org-code org-link org-todo org-formula
                            org-verbatim org-checkbox
                            org-cite org-date org-hide))
    (set-face-attribute face nil :inherit 'fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))

  :hook
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode))

;;;; Appearance
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;;;; Todo
(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :hook
  ((prog-mode text-mode conf-mode) . hl-todo-mode))

;;;; Org Roam
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Org/Roam"))
  ;; (org-roam-completion-everywhere t)
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c d c" . org-roam-dailies-capture-today)
         ("C-c d t" . org-roam-dailies-goto-today)
         ("C-c d p" . org-roam-dailies-goto-previous-note)
         ("C-c d n" . org-roam-dailies-goto-next-note)
         ("C-c d C" . org-roam-dailies-capture-date)
         ("C-c d d" . org-roam-dailies-goto-date))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setopt org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setopt org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;;;; Org Roam UI
; (elpaca (org-roam-ui :host github :repo "org-roam/org-roam-ui"))
; (use-package org-roam-ui
;   :after org-roam
;   :config
;   (setopt org-roam-ui-sync-theme t
;          org-roam-ui-follow t
;          org-roam-ui-update-on-save t
;          org-roam-ui-open-on-start t))

;;;; Org Agenda
(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                 (todo tag-up priority-down category-keep)
                                 (tags priority-down category-keep)
                                 (search category-keep)))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "[ ](T)" "|" "[X](x!)")))
  (org-refile-use-outline-path 'file)
  (org-refile-targets '(("tasks.org" :maxlevel . 1)
                        ))
  (org-agenda-files `(,(expand-file-name "tasks.org" org-directory)))
  (org-agenda-confirm-kill nil)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-capture-templates
   `(("t" "Tasks" entry (file "tasks.org")
      "* TODO %?")
     ))
  :config
  ;; Add key binding for org-agenda-mode-map
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "q") 'org-agenda-exit))
  
  :hook
  (org-agenda-mode . hl-line-mode)
  (org-agenda-mode . (lambda ()
                       (interactive) (org-element-cache-reset 'all)))
  :config
  ;; Refresh agenda after capturing.
  (add-hook 'org-capture-after-finalize-hook 'org-agenda-maybe-redo)

  ;; Save agenda buffers after doing these actions
  (dolist (hook '(org-refile
                  org-agenda-archive
                  org-agenda-add-note
                  org-agenda-deadline
                  org-agenda-kill
                  org-agenda-todo
                  org-agenda-refile
                  org-agenda-schedule
                  org-agenda-set-property
                  org-agenda-set-tags))
    ;; https://github.com/bbatsov/helm-projectile/issues/51
    (advice-add hook :after (lambda (&rest _) (org-save-all-org-buffers))))

  ;; need this because syncing updates from cloud show categories as ???
  (advice-add #'org-agenda-redo :after (lambda (&rest _) (org-element-cache-reset t)))
  )

(use-package org-super-agenda
  :after (org-agenda)
  :config
  (setopt org-super-agenda-groups
        `(
          (:name "Next" :todo "NEXT")
          (:name "Todo" :todo "TODO")
          ))
  (setopt org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

;;;; Denote
(use-package denote
  :bind (("C-c n n" . denote)
         ("C-c n l" . denote-link-or-create)
         ("C-c n f" . denote-add-front-matter)
         ("C-c n k" . denote-rename-file-keywords)
         ("C-c n r" . denote-rename-file))
  :config
  (setopt denote-directory "~/Org/Notes")
  (which-key-add-key-based-replacements "C-c n" "Notes")

  ;; Accept any symbol in a .dir-locals.el file; makes it easier to use silos.
  ;; See "silos" in the manual: https://protesilaos.com/emacs/denote
  (put 'denote-file-type 'safe-local-variable-p 'symbolp))

(use-package consult-denote
    :after denote)

;;;; Destraction-free writing with darkroom
(use-package darkroom
  :bind (("C-c d r" . darkroom-tentative-mode)
         ("C-c d t" . darkroom-mode))
  :custom
  (darkroom-text-scale-increase 1.5))

;;; Code
;;;; Elisp
;;;; Enables automatic indentation of code while typing
(use-package aggressive-indent
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;;;; Highlights function and variable definitions in Emacs Lisp mode
(use-package highlight-defined
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

;;;; Prevent parenthesis imbalance
(use-package paredit
  :commands paredit-mode
  :hook
  (emacs-lisp-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

;;;; Displays visible indicators for page breaks
(use-package page-break-lines
  :commands (page-break-lines-mode
             global-page-break-lines-mode)
  :hook
  (emacs-lisp-mode . page-break-lines-mode))

;;;; Provides functions to find references to functions, macros, variables, special forms, and symbols in Emacs Lisp
(use-package elisp-refs
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

;;;; Outli Code folding
(use-package outli
  :ensure (:host github :repo "jdtsmith/outli")
  :bind (:map outli-mode-map
              ("C-c C-p" . outline-back-to-heading)
              ("C-c C-n" . outline-next-heading)
              ("C-c C-u" . outline-up-heading)
              ("C-c C-f" . outline-forward-same-level)
              ("C-c C-b" . outline-backward-same-level)
              ("TAB" . outli-toggle-subtree))
  :hook (prog-mode . outli-mode))

;;;; magit
(elpaca transient)
(use-package magit
  ;; Set initial state to insert mode in Magit commit message buffer
  :bind (("C-x g" . magit-status))
  :custom
  ;; Improve readability of diffs
  (magit-diff-refine-hunk 'all))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (prog-mode . diff-hl-flydiff-mode)))

;;;; file type modes
(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))
(use-package yaml-mode)
(use-package json-mode)
(use-package restclient)

; ;;;; Flycheck
; ;; TODO Configure
; ; (use-package flycheck
; ;   :after exec-path-from-shell
; ;   :hook (prog-mode . global-flycheck-mode))

; ;;;; Treesitter
; ;; TODO Configure

;;;; Parenthesis matching
;; Automatically insert matching parenthesis.
(use-package electric
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Indicate pairs of parenthesis by color.
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;; Other Modes 
;;;; Ledger
(use-package ledger-mode
  :commands ledger-mode
  :defer t)

(use-package beancount-mode
  :ensure (:host github :repo "beancount/beancount-mode" :main nil)
  :mode (("\\.beancount\\'" . beancount-mode)
         ("\\.ledger\\'" . beancount-mode))
  :custom
  (beancount-highlight-transaction-at-point t)
  :hook
  (beancount-mode . (lambda ()
                      (goto-char (point-max))
                      (outline-show-entry)))
  :config
  (with-eval-after-load 'beancount-mode
    (define-key beancount-mode-map (kbd "C-c d") #'beancount-insert-date)
    (define-key beancount-mode-map (kbd "C-c q") #'beancount-query)
    (define-key beancount-mode-map (kbd "C-c l") #'beancount-check)
    (define-key beancount-mode-map (kbd "C-c x") #'beancount-context))
  
  (defun private/beancount-balance-sheet ()
    (interactive)
    (let ((compilation-read-command nil))
      (beancount--run beancount-query-program
                      (file-relative-name buffer-file-name)
                      "select account, sum(units(position)) as position from clear where account ~ 'Assets'or account ~ 'Liabilities'group by account, currency order by account, currency")))
  
  (advice-add #'beancount-align-number :override
              (lambda (&rest r) ())))

;;; Terminal 
;;;; Eshell
(with-eval-after-load 'consult
  (defvar  +consult--source-term
    (list :name     "Terminal buffers"
          :narrow   ?t
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items (lambda () (consult--buffer-query
                             :mode '(shell-mode eshell-mode term-mode eat-mode compilation-mode)
                             :sort 'visibility
                             :as #'buffer-name))))
  (add-to-list 'consult-buffer-sources '+consult--source-term 'append))

;;;; vterm
(use-package vterm
  :commands vterm
  :custom
  (vterm-max-scrollback 10000))
