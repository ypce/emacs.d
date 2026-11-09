;;; init.el --- init -*- lexical-binding: t; -*-
;;; Commentary:
;; Built-in-first config.
;;; Code:

;;; PATH -----
;; The launchd daemon gets no shell PATH. Reversed: the first dir wins.
(dolist (dir (reverse '("/etc/profiles/per-user/vp/bin" "/opt/homebrew/bin"
                        "/Users/vp/.local/bin")))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir ":" (getenv "PATH")))))


;;; Server -----
;; Child processes (Claude Code's C-g, git) edit in this Emacs.
(setenv "EDITOR" "emacsclient")


;;; Packages -----
;; `emacs --batch -l init.el' skips startup's package activation.
;; package.el loads only to install: it costs ~200 ms per start.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (bound-and-true-p package--activated)
  (package-initialize))

(require 'use-package)
(setopt use-package-always-ensure t)
;; The stock ensure loads package.el to check what is installed.
(setopt use-package-ensure-function
        (lambda (name args state &rest _)
          (unless (and (eq (car args) t)
                       (memq (use-package-as-symbol name) package-activated-list))
            (use-package-ensure-elpa name args state))))


;;; Mode line -----
;; " -branch" or " :branch": the branch starts at index 2.
(setopt vc-display-status 'no-backend)

(defconst vp/mode-line
  '((:eval (cond (buffer-read-only    (propertize " ⊘" 'face 'shadow))
                 ((buffer-modified-p) (propertize " ●" 'face 'error))
                 (t                   (propertize " ○" 'face 'shadow))))
    "  "
    (:propertize "%b" face (font-lock-constant-face bold))
    "  "
    (:propertize " %l:%c %p" face shadow)
    (:eval (when (and vc-mode buffer-file-name)
             (concat (propertize "  ⎇ " 'face 'shadow)
                     (propertize (substring-no-properties vc-mode 2)
                                 'face 'font-lock-keyword-face))))
    mode-line-format-right-align
    (:propertize (" " mode-name " ") face font-lock-function-name-face)
    " "
    mode-line-misc-info
    " "))

;;; GC -----
;; Collect while no frame has focus, so pauses rarely land while typing.
(defun vp/gc-when-unfocused ()
  (unless (seq-some #'frame-focus-state (frame-list))
    (garbage-collect)))
(add-function :after after-focus-change-function #'vp/gc-when-unfocused)


;;; Basic Emacs options -----
(use-package emacs
  :ensure nil
  :init
  ;; Not null-device: custom-save-all reads the file back and aborts.
  (setq custom-file (file-name-concat temporary-file-directory "emacs-custom.el"))
  :custom
  (use-short-answers t)
  (scroll-conservatively 101)
  (scroll-error-top-bottom t)
  (help-window-select t)
  (help-window-keep-selected t)
  (backup-by-copying t)
  (backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "backup/"))))
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (initial-major-mode 'text-mode)
  (initial-buffer-choice t)
  ;; *scratch* is the persistent scratchpad (file: notes).
  (remember-notes-buffer-name "*scratch*")
  (kill-region-dwim 'emacs-word)
  (ring-bell-function 'ignore)
  (uniquify-buffer-name-style 'forward)
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-allow-motion t)
  (isearch-repeat-on-direction-change t)
  ;; "def foo" finds "def very_long_foo"; M-s SPC toggles it off.
  (search-whitespace-regexp ".*?")
  (sentence-end-double-space nil)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (tab-always-indent 'complete)
  (switch-to-buffer-obey-display-actions t)
  (ffap-machine-p-known 'reject)   ; never ping hostnames at point
  (ns-click-through nil)   ; the focus click does not also act
  ;; :custom, not setq: only the setter restarts show-paren-mode.
  (show-paren-delay 0.05)
  (show-paren-context-when-offscreen 'overlay)
  (view-read-only t)
  ;; TAB folds only on a block-start line.
  (hs-cycle-filter 'hs-hideable-block-p)
  (quit-window-kill-buffer '(help-mode))
  :hook ((prog-mode . display-line-numbers-mode)
         ((prog-mode text-mode) . electric-pair-local-mode)
         (prog-mode . hs-minor-mode))
  :config
  (setq-default bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa t)
  (setq-default truncate-lines t
                display-line-numbers-width 3
                indent-tabs-mode nil
                fill-column 100
                tab-width 4
                mode-line-format vp/mode-line)

  (auto-save-visited-mode 1)
  (remember-notes)
  (menu-bar-mode -1)
  (winner-mode 1)
  (undelete-frame-mode 1)
  (repeat-mode 1)
  (pixel-scroll-precision-mode 1)
  ;; No right fringe (early-init.el): mark cut lines with a dim ….
  (prettify-special-glyphs-mode 1)
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'special-glyphs))

  :bind (("M-o" . other-window)
         ("<remap> <capitalize-word>" . capitalize-dwim)
         ("<remap> <upcase-word>" . upcase-dwim)
         ("<remap> <downcase-word>" . downcase-dwim)
         ("M-S-<down>" . duplicate-dwim)
         ("M-=" . count-words)
         ("C-s-f" . toggle-frame-fullscreen)
         ("<escape>" . keyboard-escape-quit)
         ("<remap> <kill-buffer>" . kill-current-buffer)
         ("<remap> <list-buffers>" . ibuffer)))

(keymap-global-unset "C-x C-z")   ; suspend-frame, too easy to fat-finger


;;; File ops -----
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


;;; Theme + Font -----
;; Vendored Dracula Pro, stripped to the faces this config uses.
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(setopt frame-background-mode 'dark)
(load-theme 'dracula-pro-pro t)

(set-face-attribute 'default nil
                    :family "AeonikMono Nerd Font Mono" :weight 'light :height 200)

;; "Monospace" resolves to a proportional font on macOS.
(set-face-attribute 'fixed-pitch nil :family 'unspecified :inherit 'default)

(setopt shr-use-colors nil)

;; Matches the markdown preview CSS.
(defun vp/eww-serif-font ()
  (face-remap-add-relative 'variable-pitch :family "Vollkorn"))
(add-hook 'eww-mode-hook #'vp/eww-serif-font)


;;; Saving + Recent -----
;; So C-x C-r lists the dirs visited in dired and eshell.
(defun vp/recentf-track-dir ()
  "Add the current directory to `recentf-list'."
  (recentf-add-file default-directory))

(add-hook 'dired-mode-hook #'vp/recentf-track-dir)
(add-hook 'eshell-directory-change-hook #'vp/recentf-track-dir)

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-autosave-interval 300)   ; the daemon runs for weeks
  (recentf-show-messages nil)
  ;; pass decrypts entries into temp dirs.
  (recentf-exclude '("\\`/\\(private/\\)?\\(tmp\\|var/folders\\)/" "/\\.git/")))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom (savehist-additional-variables '(search-ring regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :custom (save-place-autosave-interval 300))


;;; Completions (vertico + consult) -----
(use-package minibuffer
  :ensure nil
  :custom
  (completion-ignore-case t)
  (completions-detailed t)
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  :config
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(use-package vertico
  :config (vertico-mode 1))

;; Bundled with vertico.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :bind (("<remap> <switch-to-buffer>" . consult-buffer)
         ("s-b" . consult-buffer)   ; ghostel sends C-x to the TUI
         ("<remap> <yank-pop>" . consult-yank-pop)
         ("<remap> <goto-line>" . consult-goto-line)
         ("<remap> <imenu>" . consult-imenu)
         ("C-x C-r" . consult-recent-file)   ; shadows find-file-read-only
         ("M-s l" . consult-line)
         ("M-g o" . consult-outline))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-expand-substring nil))

(use-package completion-preview
  :ensure nil
  :hook ((prog-mode text-mode) . completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
         ("M-f" . completion-preview-insert-word)))

(when (executable-find "rg")
  (setopt xref-search-program 'ripgrep))


;;; Keys (C-c) -----
;; User commands live on C-c <letter>, which the conventions reserve
;; for users. Modes own C-c with punctuation or control keys.
(defvar-keymap vp/code-map
  :doc "Code commands on C-c l (eglot, eldoc, flymake)."
  "a" '("code actions"     . eglot-code-actions)
  "d" '("doc at point"     . eldoc-doc-buffer)
  "f" '("format buffer"    . eglot-format-buffer)
  "o" '("organize imports" . eglot-code-action-organize-imports)
  "r" '("rename symbol"    . eglot-rename)
  "x" '("diagnostics"      . flymake-show-buffer-diagnostics))

(defvar-keymap vp/file-map
  :doc "File commands on C-c f."
  "r" '("rename/move file"   . rename-visited-file)
  "o" '("reveal in Finder"   . vp/file-reveal)
  "e" '("open (default app)" . vp/file-open-default)
  "y" '("copy file path"     . vp/file-copy-path))

(defvar-keymap vp/mindmap-map
  :doc "Mindmap commands on C-c n m, in org mindmap blocks."
  "d" '("delete node"       . org-mindmap-delete-node)
  "v" '("switch layout"     . org-mindmap-switch-layout)
  "p" '("switch compaction" . org-mindmap-switch-compaction)
  "m" '("list to mindmap"   . org-mindmap-list-to-mindmap)
  "l" '("mindmap to list"   . org-mindmap-to-list))

(defvar-keymap vp/notes-map
  :doc "Notes and org commands on C-c n."
  "a" '("agenda"             . vp/agenda)
  "c" '("capture (menu)"     . org-capture)
  "f" '("find/create note"   . org-node-find)
  "i" '("insert link"        . org-node-insert-link)
  "g" '("grep notes"         . vp/notes-grep)
  "b" '("backlinks/context"  . org-node-context-toggle)
  "n" '("capture to inbox"   . vp/capture-inbox)
  "d" '("daily note (today)" . vp/daily-today)
  "p" '("new project hub"    . vp/new-project)
  "s" '("browse dailies"     . org-node-seq-dispatch)
  "m" (cons "mindmap" vp/mindmap-map))

;; Shells open at the project root: C-x p e eshell, C-x p t ghostel.
(keymap-set project-prefix-map "t" #'ghostel-project)

(define-keymap :keymap mode-specific-map
  "a" '("claude"      . claude-code-ide-menu)
  "e" '("sidebar"     . vp/sidebar-toggle)
  "f" (cons "file" vp/file-map)
  "l" (cons "code" vp/code-map)       ; l for LSP
  "n" (cons "notes" vp/notes-map)
  "s" '("search (rg)" . consult-ripgrep)
  "v" '("git"         . magit-status))  ; v for VC


;;; Dired -----
(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :init
  ;; BSD ls lacks -v and dirs-first; ls-lisp avoids coreutils.
  (setopt ls-lisp-use-insert-directory-program nil
          ls-lisp-dirs-first t
          ;; Two date columns for old and new files (see vp/dired-show-mtime).
          ls-lisp-use-localized-time-format t
          ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
  ;; Keep the date visible while sorted by date. Column 8: remote ls.
  (defun vp/dired-show-mtime ()
    (setq-local dired-hide-details-preserved-columns
                (and (string-match-p dired-sort-by-date-regexp dired-actual-switches)
                     '(6 7 8))))
  (add-hook 'dired-before-readin-hook #'vp/dired-show-mtime)
  :custom
  (dired-listing-switches "-alhv")
  (dired-hide-details-hide-absolute-location t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  (dired-isearch-filenames 'dwim)
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-recursive-copies 'always)
  (dired-mouse-drag-files t)
  (dired-vc-rename-file t)
  (dired-create-destination-dirs 'ask)
  :bind (:map dired-mode-map
         ("h" . dired-up-directory)
         ("<mouse-2>" . dired-mouse-find-file)))

(use-package nerd-icons
  :custom (nerd-icons-font-family "AeonikMono Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after nerd-icons
  :config (nerd-icons-completion-mode 1))


;;; Programming -----
(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :config
  ;; Needs git and a C compiler. Each mode pins its grammar source.
  (setopt treesit-auto-install-grammar 'always
          treesit-enabled-modes
          '(python-ts-mode bash-ts-mode go-ts-mode go-mod-ts-mode)))

(use-package eglot
  :ensure nil
  :hook ((python-ts-mode
          go-ts-mode
          bash-ts-mode sh-mode
          markdown-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0)))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-help-at-pt t))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

;; Must equal tab-width.
(setopt go-ts-indent-offset 4)

;; User saves only: auto-save-visited-mode saves with this-command nil.
(declare-function eglot-format-buffer "eglot")
(defun vp/eglot-format-on-save ()
  "Format the buffer with eglot before a user save, when a server is attached."
  (when (and this-command (eglot-managed-p))
    (eglot-format-buffer)))

(defun vp/go-format-on-save ()
  (add-hook 'before-save-hook #'vp/eglot-format-on-save nil t))
(add-hook 'go-ts-mode-hook #'vp/go-format-on-save)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . visual-wrap-prefix-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  ;; Renders ```mermaid fences to inline SVG with mmdc.
  (markdown-command (concat "pandoc --lua-filter="
                            (expand-file-name "pandoc/mermaid-inline.lua"
                                              user-emacs-directory)))
  (markdown-split-window-direction 'right)
  ;; Vendored, so the preview works offline.
  (markdown-css-paths
   (mapcar (lambda (f) (concat "file://" (expand-file-name f user-emacs-directory)))
           '("assets/foghorn.css" "assets/foghorn-overrides.css"))))

(use-package editorconfig
  :ensure nil
  :hook (after-init . editorconfig-mode))


;;; Version Control -----
;; Not magit-auto-revert: its per-buffer git probe slows every file open.
(setopt auto-revert-avoid-polling t)
(global-auto-revert-mode 1)

(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  (magit-define-global-key-bindings nil)   ; C-c v
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;; Windows, frames (super tier) -----
;; i3-style on super: Colemak hnei moves between windows, shift moves
;; the window itself. Lowercase is buffers (sidebar), uppercase frames.
(use-package windmove
  :ensure nil
  :bind (("s-h" . windmove-left)
         ("s-n" . windmove-down)
         ("s-e" . windmove-up)
         ("s-i" . windmove-right)
         ("s-H" . windmove-swap-states-left)
         ("s-N" . windmove-swap-states-down)
         ("s-E" . windmove-swap-states-up)
         ("s-I" . windmove-swap-states-right)))

;; iTerm convention.
(keymap-global-set "s-d" #'split-window-right)
(keymap-global-set "s-D" #'split-window-below)
(keymap-global-set "s-q" #'delete-window)
(keymap-global-set "s-Q" #'delete-other-windows)
(keymap-global-set "s-T" #'make-frame-command)
(keymap-global-set "s-W" #'delete-frame)
(keymap-global-set "s-O" #'other-frame)
(keymap-global-set "s-t" #'project-dired)

;;; Buffer sidebar -----
;; Super keys pass through ghostel, so they work in Claude sessions.
;; In lisp/, not user-lisp/: the user-lisp scrape costs 150 ms per start.
(use-package vp-sidebar
  :ensure nil
  :load-path "lisp/"
  :commands (vp/sidebar-toggle
             vp/sidebar-attention-mark
             vp/sidebar-retitle)
  :bind (("s-l" . vp/sidebar-next-buffer)
         ("s-u" . vp/sidebar-previous-buffer)
         ("s-w" . vp/sidebar-kill-buffer)
         ("s-f" . vp/sidebar-fold)))


;;; Org -----
(setopt org-directory (file-truename "~/Notes"))

(defun vp/all-org-files ()
  "All org files under `org-directory' except archive/ (refile targets)."
  (directory-files-recursively
   org-directory "\\.org\\'" nil
   (lambda (dir) (not (string= (file-name-nondirectory dir) "archive")))))

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
  :hook ((org-mode . visual-line-mode)
         (org-mode . visual-wrap-prefix-mode))
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
  (org-return-follows-link t)
  (org-insert-heading-respects-content t)
  (org-clock-persist 'history)
  (org-agenda-inhibit-startup t)
  ;; inbox.org and agenda.org appear on first capture.
  (org-agenda-skip-unavailable-files t)
  (org-agenda-window-setup 'current-window)
  (org-src-window-setup 'current-window)
  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t% s")
                              (todo   . "  %-12:c")
                              (tags   . "  %-12:c")
                              (search . "  ")))
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo   category-keep priority-down)
     (tags   category-keep priority-down)
     (search category-keep)))
  (org-agenda-hide-tags-regexp "\\`agenda\\'")
  (org-agenda-block-separator ?─)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  :config
  (make-directory org-directory t)
  ;; Not <file>.org_archive: only *.org is indexed and greppable.
  (setopt org-archive-location (concat org-directory "/archive/%s::"))
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

  (setopt org-refile-targets '((vp/all-org-files :maxlevel . 3))
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil)

  (org-clock-persistence-insinuate)
  (add-hook 'org-capture-mode-hook #'delete-other-windows))

(defun vp/capture-inbox ()
  "Capture a TODO to inbox.org without the template menu."
  (interactive)
  (org-capture nil "i"))

(defun vp/agenda (&optional arg)
  "Open the overview agenda. With a prefix ARG, show the dispatcher."
  (interactive "P")
  (if arg (org-agenda) (org-agenda nil "o")))


;;; Org visuals -----
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars "❯")
  (org-modern-list '((?* . "•") (?+ . "›") (?- . "–")))
  (org-modern-checkbox '((?X . "✓") (?\s . "☐") (?- . "–")))
  ;; No pill labels: plain keywords in `org-todo-keyword-faces'.
  (org-modern-todo nil)
  (org-modern-tag nil)
  (org-modern-timestamp nil))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; Needs mmdc (mermaid-cli). Mermaid only draws, so no y/n prompt.
(use-package ob-mermaid
  :after org
  :config
  (setopt org-confirm-babel-evaluate
          (lambda (lang _body) (not (string= lang "mermaid")))))


;;; Org Node (notes) -----
(use-package org-node
  :after org   ; load with org so the indexing modes start
  :init
  ;; Watch dirs are re-walked on the main thread; big trees stutter.
  (setopt org-mem-do-sync-with-org-id t
          org-mem-watch-dirs (list org-directory))
  :config
  (setopt org-node-file-directory-ask
          (file-name-concat org-directory "topics"))
  (make-directory org-node-file-directory-ask t)
  (org-mem-updater-mode)
  (org-node-cache-mode)
  (add-hook 'org-mem-post-full-scan-functions #'vp/refresh-agenda-files)
  (add-hook 'org-mem-post-targeted-scan-functions #'vp/refresh-agenda-files)
  (require 'org-node-seq)
  (setopt org-node-seq-defs
          (list (org-node-seq-def-on-filepath-sort-by-basename
                 "d" "Daily" (file-name-concat org-directory "daily") nil t)))
  (org-node-seq-mode))

;; Insert a block with C-c C-, m.
(use-package org-mindmap
  :hook (org-mode . org-mindmap-mode))

(defun vp/daily-today ()
  "Open today's daily note; create it as a node in the \"d\" sequence if missing."
  (interactive)
  (require 'org)   ; loads org-node
  (make-directory (file-name-concat org-directory "daily") t)
  (org-node-seq-goto "d" (format-time-string "%Y-%m-%d")))

(defun vp/new-project (title)
  "Create the project hub projects/TITLE.org from the policy skeleton.
Open the hub if it already exists."
  (interactive "sProject title: ")
  (require 'org)   ; loads org-node
  (let* ((dir (file-name-concat org-directory "projects"))
         (file (file-name-concat
                dir (concat (funcall org-node-file-slug-fn title) ".org"))))
    (make-directory dir t)
    (find-file file)
    (when (= (buffer-size) 0)
      (insert ":PROPERTIES:\n"
              ":ID:       " (org-id-new) "\n"
              ":END:\n"
              "#+title: " title "\n"
              "#+filetags: :agenda:\n"
              "- dirs :: \n"
              "* Tasks\n\n"
              "* Log\n")
      ;; Save so org-mem indexes the hub and the agenda picks it up.
      (save-buffer))))

(setopt grep-use-headings t)

(defun vp/notes-grep ()
  "Live ripgrep over all notes."
  (interactive)
  (consult-ripgrep org-directory))


;;; Claude Code -----
(use-package ghostel
  :defer t
  :custom
  ;; Outside elpa/ so package upgrades can't clobber a loaded module.
  (ghostel-module-directory (expand-file-name "ghostel/" user-emacs-directory))
  (ghostel-module-auto-install 'download)
  ;; The 0.0 default crushes tall symbols (⏵ ⏸).
  (ghostel-glyph-scale-floor 0.8)
  ;; Not a rename: the sidebar shows Claude Code's session title.
  (ghostel-buffer-name-function #'vp/sidebar-retitle)
  :config
  ;; OSC 9 from a TUI that needs input. Desktop notifications still fire.
  (add-hook 'ghostel-notification-functions #'vp/sidebar-attention-mark))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands claude-code-ide-menu
  :custom
  (claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-use-side-window nil)
  :config
  (claude-code-ide-emacs-tools-setup)
  (add-to-list 'display-buffer-alist
               '("\\`\\*claude-code\\[" (display-buffer-full-frame))))


;;; Eshell -----
(add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode)

(setopt eshell-banner-message "")

;; In a remote dir, a leading / means the remote root.
(with-eval-after-load 'esh-module
  (add-to-list 'eshell-modules-list 'eshell-elecslash))

(defun vp/eshell-wrap-lines ()
  (setq-local truncate-lines nil))

;; Not with-editor locally: pass splits its quoted sh -c string.
;; Remote emacsclient cannot reach this Emacs, so use it there.
(defun vp/eshell-export-editor ()
  "Set $EDITOR in this eshell for the local or remote directory."
  (if (file-remote-p default-directory)
      (with-editor-export-editor)
    (setenv "EDITOR" "emacsclient")))

;; Like zsh inc_append_history; the default writes on buffer kill.
(defun vp/eshell-save-history (&rest _)
  "Append new history entries to the history file."
  (eshell-write-history nil t))

;; Fish-style suggestions. Preview commands only, so TAB still pcompletes.
(defun vp/eshell-history-capf ()
  "Complete the whole input line from eshell history, newest first."
  (when (and (memq this-command completion-preview-commands)
             (>= (point) eshell-last-output-end)
             (eolp))
    (list (marker-position eshell-last-output-end) (point)
          (completion-table-with-metadata
           (ring-elements eshell-history-ring)
           '((display-sort-function . identity)
             (cycle-sort-function . identity)))
          :exclusive 'no)))

(defun vp/eshell-completion-setup ()
  (add-hook 'completion-at-point-functions #'vp/eshell-history-capf nil t)
  (add-hook 'eshell-input-filter-functions #'vp/eshell-save-history 90 t)
  (setq-local completion-preview-minimum-symbol-length 1)
  (completion-preview-mode 1))

;; esh-mode, not eshell: eshell-mode-map lives there.
(use-package esh-mode
  :ensure nil
  :hook (((eshell-mode eshell-directory-change) . vp/eshell-export-editor)
         (eshell-mode . vp/eshell-wrap-lines)
         (eshell-mode . vp/eshell-completion-setup))
  :bind (:map eshell-mode-map
         ("C-r" . consult-history))
  :custom
  (eshell-history-size 10000)
  (eshell-hist-ignoredups 'erase)
  (eshell-history-append t)
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-cmpl-ignore-case t)
  (eshell-aliases-file (expand-file-name "etc/eshell-aliases" user-emacs-directory))
  (eshell-cp-interactive-query t)
  (eshell-mv-interactive-query t))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))


;;; Remote (TRAMP, built-in) -----
(use-package tramp
  :ensure nil
  :defer t
  :custom
  (remote-file-name-inhibit-auto-save-visited t)
  (remote-file-name-inhibit-cache 60)
  (remote-file-name-inhibit-locks t)
  :config
  ;; Plain `ssh host cmd', no login-shell handshake. Costs: no
  ;; interrupt-process, no multi-hop.
  (connection-local-set-profile-variables
   'vp/tramp-direct-async
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh") 'vp/tramp-direct-async)
  ;; Centrify sudo: /ssh:host|dzdo:host:/path.
  (add-to-list 'tramp-methods
               `("dzdo"
                 (tramp-login-program "dzdo")
                 (tramp-login-args (("-u" "%u") ("-s") ("-H") ("%l")))
                 ,@(nthcdr 3 (assoc "sudo" tramp-methods))))
  (add-to-list 'tramp-default-user-alist '("dzdo" nil "root"))
  ;; No VC probes over the connection.
  (setopt vc-ignore-dir-regexp
          (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))


;;; Which Key -----
(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 0.4)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-max-description-length 40)
  (which-key-add-column-padding 2)
  :config
  ;; Hide ghostel's C-c chords.
  (add-to-list 'which-key-replacement-alist '((nil . "\\`ghostel-") . t))
  ;; An empty stock prefix.
  (keymap-unset mode-specific-map "^" t)
  (which-key-mode))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
;;; init.el ends here
