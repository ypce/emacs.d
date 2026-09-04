;;; init.el --- init -*- lexical-binding: t; -*-
;;; Commentary:
;; Built-in-first config for Emacs 31.
;;; Code:

;;; PATH -----
;; The daemon gets no shell PATH; add tool dirs when present.
(dolist (dir '("/etc/profiles/per-user/vp/bin" "/opt/homebrew/bin"))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir ":" (getenv "PATH")))))


;;; Packages -----
;; package-quickstart (early-init) activates packages before init.el;
;; only --batch runs miss that and need package-initialize.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (bound-and-true-p package--activated)
  (package-initialize))

(require 'use-package)
(setopt use-package-always-ensure t)   ; built-ins opt out with :ensure nil

;; Full modifier keys in kitty-protocol terminals (ghostty, wezterm).
(use-package kkp
  :config (global-kkp-mode +1))


;;; Mode line (hand-rolled, zero dependencies) -----
(defface vp/ml-buffer
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the buffer name."
  :group 'mode-line-faces)

(defface vp/ml-dim
  '((t :inherit shadow))
  "Face for secondary mode-line info."
  :group 'mode-line-faces)

(defun vp/ml--status ()
  "Modified / read-only indicator."
  (cond (buffer-read-only    (propertize " ⊘" 'face 'vp/ml-dim))
        ((buffer-modified-p) (propertize " ●" 'face 'error))
        (t                   (propertize " ○" 'face 'vp/ml-dim))))

(defun vp/ml--vc ()
  "Version-control branch, if any."
  (when (and vc-mode buffer-file-name)
    ;; vc-mode is " Backend-branch" or " Backend:branch"; strip by
    ;; backend name length instead of a hardcoded offset.
    (let ((branch (substring-no-properties
                   vc-mode
                   (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
      (concat (propertize "  ⎇ " 'face 'vp/ml-dim)
              (propertize branch 'face 'font-lock-keyword-face)))))

(defun vp/ml--position ()
  "Line:column position."
  (propertize " %l:%c " 'face 'vp/ml-dim))

(defun vp/ml--major-mode ()
  "Pretty major-mode name."
  (propertize (concat " " (format-mode-line mode-name) " ")
              'face 'font-lock-function-name-face))

(defun vp/mode-line ()
  "Custom mode-line format."
  '((:eval (vp/ml--status))
    "  "
    (:eval (propertize (buffer-name) 'face 'vp/ml-buffer))
    "  "
    (:eval (vp/ml--position))
    (:propertize "%p" face vp/ml-dim)
    (:eval (vp/ml--vc))
    mode-line-format-right-align
    (:eval (vp/ml--major-mode))
    " "
    mode-line-misc-info
    " "))

;; Blend mode-line and fringe into the buffer background (box = padding).
;; Runs on frame hooks: at daemon init (face-background 'default)
;; resolves to a wrong tty color. Re-runs on each theme change.
(defun vp/flat-mode-line (&rest _)
  "Give the mode-line and fringe the default background."
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (set-face-attribute face nil
                        :background (face-background 'default)
                        :box `(:line-width 6 :color ,(face-background 'default))))
  (set-face-attribute 'fringe nil :background (face-background 'default)))

(add-hook 'window-setup-hook            #'vp/flat-mode-line)
(add-hook 'server-after-make-frame-hook #'vp/flat-mode-line)
(add-hook 'enable-theme-functions       #'vp/flat-mode-line)


;;; Basic Emacs options -----
(use-package emacs
  :ensure nil
  :init
  ;; Send Customize writes to a throwaway file, not this file. Not
  ;; null-device: custom-save-all reads the file back and aborts on it.
  (setq custom-file (file-name-concat temporary-file-directory "emacs-custom.el"))
  :custom
  (use-short-answers t)
  (confirm-kill-emacs 'yes-or-no-p)   ; the daemon dies with every client
  (scroll-conservatively 101)
  (help-window-select t)
  (help-window-keep-selected t)
  (backup-by-copying t)
  (backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "backup/"))))
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (initial-scratch-message "")
  (initial-major-mode 'text-mode)
  (initial-buffer-choice t)
  (kill-region-dwim 'emacs-word)   ; Emacs 31: C-w with no region kills a word
  (ring-bell-function 'ignore)
  (uniquify-buffer-name-style 'forward)
  (isearch-lazy-count t)
  (sentence-end-double-space nil)
  ;; :custom, not setq: the option's setter restarts show-paren-mode,
  ;; a plain set does nothing while the mode is on.
  (show-paren-delay 0.05)
  (view-read-only t)
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . electric-pair-local-mode)
         ((prog-mode org-mode) . visual-wrap-prefix-mode)
         ((prog-mode text-mode) . completion-preview-mode))
  :config
  (setq-default truncate-lines t
                display-line-numbers-width 3
                indent-tabs-mode nil
                fill-column 100
                tab-width 4
                mode-line-format (vp/mode-line))

  (auto-save-visited-mode 1)
  (menu-bar-mode -1)
  (winner-mode 1)                     ; C-c <left> undoes window layout changes
  (repeat-mode 1)
  (pixel-scroll-precision-mode 1)
  ;; tty: dim … / ↩ instead of $ and \ (GUI frames use fringe arrows)
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'shadow))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'shadow))

  :bind (("M-o" . other-window)
         ("M-u" . capitalize-word)
         ("M-=" . count-words)
         ("<escape>" . keyboard-escape-quit)
         ("<remap> <kill-buffer>" . kill-current-buffer)
         ("<remap> <list-buffers>" . ibuffer)))

(keymap-global-unset "C-x C-z")   ; suspend-frame, too easy to fat-finger

;; File path and modification status in the frame title.
(setq frame-title-format
      '((:eval (when (and (buffer-modified-p) (buffer-file-name)) "* "))
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Emacs"))

(use-package help-mode
  :ensure nil
  :bind (:map help-mode-map
         ("q" . kill-buffer-and-window)
         ("<escape>" . kill-buffer-and-window)))


;;; Small QoL commands -----
(defun vp/copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it."
  (interactive)
  (copy-region-as-kill (point-min) (point-max))
  (message "Buffer content saved to kill ring."))

(keymap-global-set "C-c w" #'vp/copy-buffer-as-kill)

(defun vp/kill-save-line (nlines)
  "Save NLINES lines to the kill ring without deleting them."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-end-position nlines))
  (kill-append "\n" nil)
  (message "Saved line to kill-ring"))

(keymap-global-set "M-k" #'vp/kill-save-line)

(defun vp/remove-system-clipboard-format ()
  "Round-trip the system clipboard through Emacs to strip rich-text formatting."
  (interactive)
  (let ((clipboard-text (gui-get-selection 'CLIPBOARD)))
    (gui-set-selection 'CLIPBOARD clipboard-text)))

(keymap-global-set "C-c r" #'vp/remove-system-clipboard-format)

(defun vp/eval-last-sexp-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(keymap-global-set "C-c C-e" #'vp/eval-last-sexp-and-replace)


;;; File ops (C-c f) -----
(defun vp/file--target ()
  "The current file; in dired the file at point; else the directory."
  ;; expand-file-name: `open' gets the path verbatim, nothing expands ~
  (expand-file-name (or buffer-file-name
                        (and (derived-mode-p 'dired-mode)
                             (dired-get-filename nil t))
                        default-directory)))

(defun vp/file-reveal ()
  "Reveal the current file in Finder. In dired, reveal the file at point."
  (interactive)
  (call-process "open" nil 0 nil "-R" (vp/file--target)))

(defun vp/file-open-default ()
  "Open the current file (in dired: the file at point) with the default app."
  (interactive)
  (call-process "open" nil 0 nil (vp/file--target)))

(defun vp/file-copy-path ()
  "Copy the absolute path of the current file (in dired: the file at point)."
  (interactive)
  (let ((p (vp/file--target)))
    (kill-new p)
    (message "%s" p)))

(defun vp/labeled-keymap (specs)
  "A keymap from SPECS, a list of (KEY LABEL COMMAND); which-key shows LABEL."
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key ,label ,cmd) specs)
      (keymap-set map key (cons label cmd)))
    map))

(keymap-global-set
 "C-c f" (cons "file" (vp/labeled-keymap
                       '(("r" "rename/move file"   rename-visited-file)
                         ("o" "reveal in Finder"   vp/file-reveal)
                         ("e" "open (default app)" vp/file-open-default)
                         ("y" "copy file path"     vp/file-copy-path)))))


;;; Theme + Font -----
;; Vendored Dracula Pro, stripped to the faces this config uses.
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(setopt frame-background-mode 'dark)
;; Set before load: the theme reads it when it builds face specs.
(setopt dracula-pro-pro-enlarge-headings nil)
(load-theme 'dracula-pro-pro t)

;; GUI frames only; terminal frames use the terminal's font.
(set-face-attribute 'default nil
                    :family "AeonikMono Nerd Font Mono" :weight 'light :height 200)

;; "Monospace" does not resolve on macOS and falls back to a
;; proportional font; inherit default so fixed-pitch stays monospace.
(set-face-attribute 'fixed-pitch nil :family 'unspecified :inherit 'default)

(defun vp/transparent-background ()
  "Unset the default background in terminal frames for true transparency."
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook            #'vp/transparent-background)
(add-hook 'server-after-make-frame-hook #'vp/transparent-background)

;; Ignore document colors in eww/HTML; they clash with a dark theme.
(setopt shr-use-colors nil)

;; Serif reading font in eww; matches the markdown preview CSS.
(defun vp/eww-serif-font ()
  (face-remap-add-relative 'variable-pitch :family "Vollkorn"))
(add-hook 'eww-mode-hook #'vp/eww-serif-font)


;;; Saving + Recent -----
(defun vp/in-order-table (collection &rest metadata)
  "A completion table over COLLECTION that keeps its order.
METADATA entries are spliced into the table's metadata."
  (lambda (str pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   ,@metadata)
      (complete-with-action action collection str pred))))

(defun vp/recentf-open ()
  "Open a recent file, most recent first.
Orderless matches basename fragments anywhere in the path."
  (interactive)
  (find-file
   (completing-read "Recent file: "
                    (vp/in-order-table (mapcar #'abbreviate-file-name recentf-list))
                    nil t)))

(defun vp/dired-recent-dir ()
  "Open dired in a recently used directory (derived from recentf)."
  (interactive)
  (let ((dirs (delete-dups
               (mapcar (lambda (f) (abbreviate-file-name (file-name-directory f)))
                       recentf-list))))
    (dired (completing-read "Recent dir: " (vp/in-order-table dirs) nil t))))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom (recentf-max-saved-items 60)
  :bind (("C-x C-r" . vp/recentf-open)   ; shadows find-file-read-only
         ("C-c d" . vp/dired-recent-dir)))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))


;;; Completions (built-in minibuffer UI, Emacs 31) -----
(use-package minibuffer
  :ensure nil
  :custom
  (completion-auto-help t)
  (completion-eager-display t)      ; Emacs 31: list shows without TAB
  (completion-eager-update t)       ; Emacs 31: list filters as you type
  (completion-ignore-case t)
  (completion-show-help nil)
  (completions-format 'one-column)
  (completions-max-height 14)
  (completions-sort 'historical)    ; Emacs 31: recently used first
  (completion-auto-select 'second-tab)
  (completion-pcm-leading-wildcard t)   ; Emacs 31: substring-like file matching
  ;; C-n/C-p move the highlight only. auto-choose would insert the
  ;; candidate, and eager-update would then filter the list to it.
  (minibuffer-completion-auto-choose nil)
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-prompt-properties '(read-only t face minibuffer-prompt))
  :config
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  (keymap-set minibuffer-local-completion-map "C-n" #'minibuffer-next-completion)
  (keymap-set minibuffer-local-completion-map "C-p" #'minibuffer-previous-completion)

  ;; Must-match prompts only: free-text prompts (find-file) keep
  ;; literal RET, or a new name could never be typed.
  (defun vp/minibuffer-ret-dwim ()
    "Exit with an exact match, else choose the highlighted candidate.
With no highlight yet, choose the first candidate - \"M-x e RET\"
runs the top match."
    (interactive)
    (let ((win (get-buffer-window "*Completions*" 0)))
      (cond ((test-completion (minibuffer-contents)
                              minibuffer-completion-table
                              minibuffer-completion-predicate)
             (exit-minibuffer))
            (win
             (with-selected-window win
               (unless (get-text-property (point) 'completion--string)
                 (goto-char (point-min))
                 (next-completion 1)))
             (minibuffer-choose-completion))
            (t (minibuffer-force-complete-and-exit)))))
  (keymap-set minibuffer-local-must-match-map "RET" #'vp/minibuffer-ret-dwim))

;; Match words in any order and fragment.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-expand-substring nil))

;; Act on the thing at point or the current minibuffer candidate.
(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-e" . embark-export)))

(when (executable-find "rg")
  (setopt xref-search-program 'ripgrep))


;;; Editing aids -----
;; Syntax-aware expand-region; repeat with = / - after the first press.
(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)
         (:repeat-map expreg-repeat-map
                      ("=" . expreg-expand)
                      ("-" . expreg-contract)))
  :config
  ;; Stock expreg has no prose steps between word and paragraph; add
  ;; sentence and line regions in text modes.
  (defun vp/expreg--prose ()
    "Return sentence and line regions around point."
    (when (derived-mode-p 'text-mode)
      (let (result)
        (push `(line . ,(cons (line-beginning-position) (line-end-position)))
              result)
        (ignore-errors
          (let* ((beg (save-excursion (backward-sentence) (point)))
                 (end (save-excursion (goto-char beg) (forward-sentence) (point))))
            (push `(sentence . ,(cons beg end)) result)))
        result)))
  (setq-default expreg-functions
                (cons #'vp/expreg--prose (default-value 'expreg-functions))))

(use-package multiple-cursors
  ;; Run commands for all cursors without asking (opt-outs: mc/cmds-to-run-once).
  :custom (mc/always-run-for-all t)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package drag-stuff
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))


;;; Dired -----
(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :init
  ;; ls-lisp instead of ls: BSD ls lacks -v and directory grouping,
  ;; and this drops the coreutils dependency. Remote dired still uses
  ;; the remote ls.
  (setopt ls-lisp-use-insert-directory-program nil
          ls-lisp-dirs-first t)
  :custom
  (dired-listing-switches "-alhv")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)   ; two dired windows: copy/move targets the other one
  (dired-isearch-filenames 'dwim)   ; C-s matches filenames only
  ;; h = up a directory (shadows describe-mode; C-h m remains).
  :bind (:map dired-mode-map
         ("h" . dired-up-directory)))

;; The default font carries the icon glyphs.
(use-package nerd-icons
  :custom (nerd-icons-font-family "AeonikMono Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;;; Programming: treesit + eglot + flymake (all built-in) -----
(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :config
  (setq treesit-language-source-alist
        '((bash     "https://github.com/tree-sitter/tree-sitter-bash")
          (go       "https://github.com/tree-sitter/tree-sitter-go")
          (gomod    "https://github.com/camdencheek/tree-sitter-go-mod")
          (python   "https://github.com/tree-sitter/tree-sitter-python")))
  ;; Build missing grammars on first use (needs git + a C compiler).
  (setopt treesit-auto-install-grammar 'always
          treesit-enabled-modes
          '(python-ts-mode bash-ts-mode go-ts-mode go-mod-ts-mode)))

(use-package eglot
  :ensure nil
  :hook ((python-ts-mode
          go-ts-mode
          bash-ts-mode sh-mode
          markdown-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c C-d" . eldoc-doc-buffer)
         ("C-c e f" . eglot-format-buffer)
         ("C-c e r" . eglot-rename)
         ("C-c e x" . eglot-code-actions)
         ("C-c e o" . eglot-code-action-organize-imports))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0))   ; perf: no event logs
  :config
  (dolist (entry '(((python-ts-mode)       . ("pylsp"))
                   ((go-ts-mode)           . ("gopls"))
                   ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
                   ((markdown-mode)        . ("marksman"))))
    (add-to-list 'eglot-server-programs entry)))

(use-package eldoc
  :ensure nil
  :custom (eldoc-echo-area-use-multiline-p nil))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c l" . flymake-show-buffer-diagnostics)))

;; Go: tabs for indent, format on save via gopls.
(defun vp/eglot-format-on-save ()
  "Format the buffer with eglot before save, when a server is attached."
  (when (eglot-managed-p) (eglot-format-buffer)))

(defun vp/go-setup ()
  "Per-buffer Go setup."
  (setq-local tab-width 4
              indent-tabs-mode t)
  (add-hook 'before-save-hook #'vp/eglot-format-on-save nil t))
(add-hook 'go-ts-mode-hook #'vp/go-setup)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . visual-wrap-prefix-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  (markdown-split-window-direction 'right)   ; preview side by side
  ;; Preview styling, vendored in assets/ so it works without network.
  ;; foghorn-overrides.css loads second: full-width body, base font 80%.
  (markdown-css-paths
   (mapcar (lambda (f) (concat "file://" (expand-file-name f user-emacs-directory)))
           '("assets/foghorn.css" "assets/foghorn-overrides.css"))))

(use-package editorconfig
  :ensure nil
  :config (editorconfig-mode 1))


;;; Version Control -----
;; Notification-based revert instead of magit-auto-revert, whose
;; per-buffer git probe slowed every file open.
(setopt auto-revert-avoid-polling t)
(global-auto-revert-mode 1)

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-auto-revert-mode nil)   ; covered by global-auto-revert above
  ;; Reuse the current window; only diffs split.
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;; Org -----
(defun vp/all-org-files ()
  "All org files under `org-directory' (refile targets)."
  (directory-files-recursively org-directory "\\.org$"))

(defun vp/refresh-agenda-files (&rest _)
  "Set `org-agenda-files': inbox.org, agenda.org, and files with the
:agenda: filetag. Other notes stay out of the agenda but remain
searchable (C-c n f, C-c n g)."
  (interactive)
  (setq org-agenda-files
        (delete-dups
         (append
          (list (expand-file-name "inbox.org" org-directory)
                (expand-file-name "agenda.org" org-directory))
          (when (fboundp 'org-mem-all-entries)
            (mapcar #'org-mem-entry-file
                    (seq-filter
                     (lambda (e) (member "agenda" (org-mem-entry-tags e)))
                     (org-mem-all-entries))))))))

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  (org-ellipsis " ⤵")
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-cycle-separator-lines 1)
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-tags-column 0)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respects-content t)
  (org-clock-persist 'history)
  (org-clock-out-when-done t)
  (org-clock-into-drawer t)
  ;; Skip org startup options in files the agenda visits.
  (org-agenda-inhibit-startup t)
  ;; No splits: agenda and src-block editing use the current window.
  (org-agenda-window-setup 'current-window)
  (org-src-window-setup 'current-window)
  ;; De-noise the agenda: no category column, hide the :agenda: tag.
  (org-agenda-prefix-format '((agenda . "  %?-12t% s")
                              (todo   . "  ")
                              (tags   . "  ")
                              (search . "  ")))
  (org-agenda-hide-tags-regexp "\\`agenda\\'")
  (org-agenda-block-separator ?─)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  :config
  (setopt org-directory (file-truename "~/Notes"))
  (make-directory org-directory t)
  ;; The agenda errors on missing files; create the two anchors.
  (dolist (f '("inbox.org" "agenda.org"))
    (let ((path (expand-file-name f org-directory)))
      (unless (file-exists-p path)
        (with-temp-file path (insert "#+title: " (file-name-base f) "\n")))))
  (vp/refresh-agenda-files)

  ;; `n' is this variable's default value, not a dispatcher built-in;
  ;; keep it when setting the variable.
  (setopt org-agenda-custom-commands
        `(("n" "Agenda and all TODOs"
           ((agenda "") (alltodo "")))
          ("o" "Overview: today · next · waiting · inbox"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "Today")))
            (todo "NEXT" ((org-agenda-overriding-header "Next")))
            (todo "WAIT" ((org-agenda-overriding-header "Waiting")))
            (todo "TODO"
                  ((org-agenda-files (list ,(expand-file-name "inbox.org" org-directory)))
                   (org-agenda-overriding-header "Inbox")))))))

  (setopt org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Inherit stock faces so any theme recolors them.
  (setopt org-todo-keyword-faces
        '(("NEXT"      . (:inherit success :weight bold))
          ("WAIT"      . (:inherit warning))
          ("CANCELLED" . (:inherit shadow :strike-through t))))

  (setopt org-capture-templates
        `(("i" "Inbox" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n/Captured/ %U\n")

          ("m" "Meeting" entry
           (file+headline ,(expand-file-name "agenda.org" org-directory) "Meetings")
           ,(concat "* %? :meeting:\n"
                    "<%<%Y-%m-%d %a %H:00>>\n\n"
                    "** Attendees\n\n"
                    "** Notes\n\n"
                    "** Actions\n"))

          ("e" "Event" entry
           (file+headline ,(expand-file-name "agenda.org" org-directory) "Events")
           "* %?\n<%<%Y-%m-%d %a %H:00>>")))

  ;; Refile across ALL notes, not just the agenda set.
  (setopt org-refile-targets '((vp/all-org-files :maxlevel . 3))
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil)

  (org-clock-persistence-insinuate)
  (add-hook 'org-capture-mode-hook #'delete-other-windows))

(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)


;;; Org visuals -----
(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars "❯")
  (org-modern-list '((?* . "•") (?+ . "›") (?- . "–")))
  (org-modern-checkbox '((?X . "✓") (?\s . "☐") (?- . "–")))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  ;; No pill labels; TODO keywords render as glyphs (vp/org-prettify-todos).
  (org-modern-todo nil)
  (org-modern-tag nil)
  (org-modern-timestamp nil))

;; Show emphasis markers at point; org hides them otherwise.
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))


;;; Org Node (notes) -----
;; org-mem indexes watched org files in a subprocess: instant
;; find/backlinks, no database.
(use-package org-node
  :after org
  :demand t   ; load with org so the indexing modes start
  :init
  ;; Watch ONLY ~/Notes: watch dirs are re-walked in the main thread on
  ;; an idle timer, and big trees cause stutter. Org files elsewhere
  ;; still get indexed once visited.
  (setopt org-mem-do-sync-with-org-id t
          org-mem-watch-dirs (list (file-truename "~/Notes")))
  :config
  (org-mem-updater-mode)
  (org-node-cache-mode)
  ;; Keep the agenda list current as org-mem rescans notes.
  (add-hook 'org-mem-post-full-scan-functions #'vp/refresh-agenda-files)
  (add-hook 'org-mem-post-targeted-scan-functions #'vp/refresh-agenda-files)
  ;; Dailies: YYYY-MM-DD.org under <notes>/daily as a node sequence.
  (require 'org-node-seq)
  (setopt org-node-seq-defs
          (list (org-node-seq-def-on-filepath-sort-by-basename
                 "d" "Daily" (file-name-concat org-directory "daily") nil t)))
  (org-node-seq-mode))

(defun vp/daily-today ()
  "Open today's daily note; create it as a node in the \"d\" sequence if missing."
  (interactive)
  (require 'org)   ; sets org-directory, loads org-node
  (let* ((dir (file-name-concat org-directory "daily"))
         (file (file-name-concat dir (format-time-string "%Y-%m-%d.org"))))
    (make-directory dir t)
    (if (file-exists-p file)
        (find-file file)
      (let ((org-node-creation-fn #'org-node-new-file)
            (org-node-file-directory-ask dir))
        (org-node-create (format-time-string "%Y-%m-%d") (org-id-new) "d")))))

;; Group grep results under one heading per file (Emacs 30).
(setopt grep-use-headings t)

(defun vp/notes-grep (regexp)
  "Grep all notes for REGEXP. In the results, `e' starts Grep Edit."
  (interactive "sGrep notes: ")
  (require 'org)
  (rgrep regexp "*.org" org-directory))

(keymap-global-set
 "C-c n" (cons "notes" (vp/labeled-keymap
                        '(("f" "find/create note"   org-node-find)
                          ("i" "insert link"        org-node-insert-link)
                          ("g" "grep notes"         vp/notes-grep)
                          ("b" "backlinks/context"  org-node-context-toggle)
                          ("d" "daily note (today)" vp/daily-today)
                          ("s" "browse dailies"     org-node-seq-dispatch)))))

;; TODO keywords as glyphs via prettify-symbols; the text stays
;; underneath, and point on a glyph expands it for editing.
(defvar vp/org-todo-glyphs
  '(("TODO"      . ?☐)
    ("NEXT"      . ?▸)
    ("WAIT"      . ?◷)
    ("DONE"      . ?✓)
    ("CANCELLED" . ?✗))
  "Glyph shown for each org todo keyword.")

(defun vp/org-prettify-todos ()
  (setq-local prettify-symbols-alist vp/org-todo-glyphs
              prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook #'vp/org-prettify-todos)

;; The agenda has no font-lock, so compose glyphs after it is built.
;; The todo-state check skips the same words inside titles.
(defun vp/org-agenda-prettify-todos ()
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (let ((re (regexp-opt (mapcar #'car vp/org-todo-glyphs) 'words)))
        (while (re-search-forward re nil t)
          (when (equal (get-text-property (match-beginning 0) 'todo-state)
                       (match-string 0))
            (compose-region (match-beginning 0) (match-end 0)
                            (cdr (assoc (match-string 0) vp/org-todo-glyphs)))
            (end-of-line)))))))
(add-hook 'org-agenda-finalize-hook #'vp/org-agenda-prettify-todos)


;;; Claude Code -----
;; claude-code-ide bridges the Claude Code CLI into Emacs over MCP.
;; ghostel (libghostty as an Emacs module) hosts its TUI.
(use-package ghostel
  :defer t
  :custom
  ;; Outside elpa/ so package upgrades can't clobber a loaded module.
  (ghostel-module-directory (expand-file-name "ghostel/" user-emacs-directory))
  (ghostel-module-auto-install 'download)
  ;; Fallback glyphs shrink to fit the cell grid; the 0.0 default
  ;; crushes tall symbols (⏵ ⏸). 1.0 = natural size, taller rows.
  (ghostel-glyph-scale-floor 0.8))

(use-package claude-code-ide
  ;; :ensure nil is required next to :vc, else both handlers install.
  :ensure nil
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c i" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'ghostel)
  ;; A plain buffer, not a side window: sessions take the whole frame.
  (claude-code-ide-use-side-window nil)
  :config
  (claude-code-ide-emacs-tools-setup)
  (add-to-list 'display-buffer-alist
               '("\\*claude-code\\[" (display-buffer-full-frame))))


;;; Eshell -----
;; Full-screen TUIs (eshell-visual-commands) open in ghostel; the rest
;; stays in eshell.
(add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode)

(setopt eshell-banner-message "")

(defun vp/eshell-prompt ()
  "Basename-only prompt; remote dirs keep their TRAMP prefix."
  (let ((base (file-name-nondirectory
               (directory-file-name (abbreviate-file-name default-directory)))))
    (concat (or (file-remote-p default-directory) "")
            (if (string-empty-p base) "/" base)
            " $ ")))
(setopt eshell-prompt-function #'vp/eshell-prompt)

;; Wrap long lines; with global truncate-lines they hscroll the window.
(defun vp/eshell-wrap-lines ()
  (setq-local truncate-lines nil))

(defun vp/eshell-history ()
  "Pick a command from eshell history with completion, most recent first."
  (interactive)
  (let ((cmd (completing-read
              "History: "
              (vp/in-order-table (delete-dups (ring-elements eshell-history-ring))))))
    (eshell-kill-input)
    (insert cmd)))

;; esh-mode, not eshell: eshell-mode-map lives in esh-mode.el, so a
;; binding hung on the eshell feature fires before the map exists.
(use-package esh-mode
  :ensure nil
  ;; with-editor: commands that read $EDITOR open a buffer in THIS Emacs.
  :hook ((eshell-mode . with-editor-export-editor)
         (eshell-mode . vp/eshell-wrap-lines))
  :bind (:map eshell-mode-map
         ("C-r" . vp/eshell-history))
  :config
  ;; `clear' erases like a terminal; the default only scrolls.
  (defalias 'eshell/clear #'eshell/clear-scrollback))

(defun vp/eshell-here ()
  "Open the shared eshell, cd'd to this buffer's directory.
The cd runs as a real command so eshell history records it."
  (interactive)
  ;; file-truename also normalizes macOS firmlink paths.
  (let ((dir (file-truename default-directory)))
    (eshell)
    (goto-char (point-max))   ; point can be mid-buffer in a reused session
    (unless (string= (file-truename default-directory) dir)
      (eshell-kill-input)
      (insert (format "cd \"%s\"" dir))
      (eshell-send-input))))

(keymap-global-set "C-c s" #'vp/eshell-here)


;;; Remote (TRAMP, built-in) -----
;; Remote is path syntax: /ssh:host:path, /docker:name:…, /sudo:: for root.
(use-package tramp
  :ensure nil
  :defer t
  :custom
  ;; Do not re-save remote buffers over the network on idle.
  (remote-file-name-inhibit-auto-save-visited t)
  ;; Trust cached remote file attributes for 60s instead of 10.
  (remote-file-name-inhibit-cache 60)
  :config
  ;; Do not probe VC over the connection on every remote find-file.
  (setopt vc-ignore-dir-regexp
          (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))


;;; Which Key (built-in since Emacs 30) -----
(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 0.4)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-max-description-length 40)
  (which-key-add-column-padding 2)
  :config (which-key-mode))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
;;; init.el ends here
