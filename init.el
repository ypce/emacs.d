;;; init.el --- init -*- lexical-binding: t; -*-
;;; Code:

;; Nix bin path (guarded so config still works on non-Nix systems)
(let ((nix-bin "/etc/profiles/per-user/vp/bin"))
  (when (file-directory-p nix-bin)
    (add-to-list 'exec-path nix-bin)
    (setenv "PATH" (concat nix-bin ":" (getenv "PATH")))))

;; Homebrew bin path (guarded so config still works on non-Homebrew systems)
(let ((brew-bin "/opt/homebrew/bin"))
  (when (file-directory-p brew-bin)
    (add-to-list 'exec-path brew-bin)
    (setenv "PATH" (concat brew-bin ":" (getenv "PATH")))))

;; NOTE on daemon truecolor: Emacs detects 24-bit tty color via C getenv
;; (COLORTERM) or terminfo flags - lisp setenv can't influence it, and the
;; launchd daemon has no COLORTERM. Terminals must therefore advertise
;; truecolor through terminfo (Tc flag) installed in ~/.terminfo; see
;; README.org "Daemon truecolor".


;;; Package setup (built-in package.el) -----
;; GNU + NonGNU ELPA are the built-in defaults; MELPA added for the rest.
;; No lockfiles - versions float; M-x package-upgrade-all to update.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Normal startup activates packages BEFORE init.el (package-quickstart,
;; see early-init.el) - initializing again here re-reads every package
;; descriptor and archive index, measured at 390ms. Only --batch runs
;; skip that early activation and need it. A fresh machine bootstraps
;; without a refresh call: when :ensure meets a missing package,
;; use-package runs package-refresh-contents itself.
(unless (bound-and-true-p package--activated)
  (package-initialize))

(require 'use-package)
(setq use-package-always-ensure t)   ; built-ins opt out with :ensure nil

;; --- tier:2 | standalone: terminal keyboard protocol ---
(use-package kkp   ; GNU ELPA
  :config (global-kkp-mode +1))


;;; Mode line -----
;; --- tier:2 | standalone: mode-line format ---
(defface vp/ml-buffer
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the buffer name.")

(defface vp/ml-dim
  '((t :inherit shadow))
  "Face for secondary mode-line info.")

(defun vp/ml--status ()
  "Modified / read-only indicator."
  (cond (buffer-read-only    (propertize " ⊘" 'face 'vp/ml-dim))
        ((buffer-modified-p) (propertize " ●" 'face 'error))
        (t                   (propertize " ○" 'face 'vp/ml-dim))))

(defun vp/ml--vc ()
  "Version-control branch, if any."
  (when vc-mode
    (concat (propertize "  ⎇ " 'face 'vp/ml-dim)
            (propertize (substring-no-properties vc-mode 5)
                        'face 'font-lock-keyword-face))))

(defun vp/ml--position ()
  "Line:column plus percentage through buffer."
  (propertize " %l:%c " 'face 'vp/ml-dim))

(defun vp/ml--major-mode ()
  "Pretty major-mode name (uses `mode-name', so \"Emacs-Lisp\" not \"emacs-lisp-mode\")."
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

(set-face-attribute 'mode-line nil
                     :box '(:line-width 6 :style flat-button))
(set-face-attribute 'mode-line-inactive nil
                     :box '(:line-width 6 :style flat-button))


;;; Basic Emacs options -----
(use-package emacs
  :ensure nil
  :init
  ;; Customize would otherwise append custom-set-variables blobs to this
  ;; hand-maintained file; everything here is set in lisp, so send them to a
  ;; throwaway temp file that is gone on reboot. Do not use null-device here:
  ;; custom-save-all reads the file back, and reading /dev/null overflows the
  ;; buffer-size limit and aborts init.
  (setq custom-file (make-temp-file "emacs-custom-"))
  (setq use-short-answers t
        scroll-conservatively 101
        confirm-kill-emacs 'yes-or-no-p
        help-window-select t
        help-window-keep-selected t
        backup-by-copying t
        backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "backup/")))
        create-lockfiles nil
        delete-by-moving-to-trash t
        initial-scratch-message ""
        initial-major-mode 'text-mode
        ring-bell-function 'ignore
        initial-buffer-choice t)
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default truncate-lines t
                display-line-numbers-width 3
                indent-tabs-mode nil
                fill-column 100
                tab-width 4
                mode-line-format (vp/mode-line))

  (auto-save-visited-mode 1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (xterm-mouse-mode 1)
  ;; smooth trackpad scrolling in GUI frames (no-op on tty).
  ;; (repeat-mode is deliberately absent: its post-command transient map
  ;; outranks meow's normal state.)
  (pixel-scroll-precision-mode 1)
  ;; tty truncation indicator: a dim … instead of the ancient `$'
  ;; (GUI frames use fringe arrows and never consult this)
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'shadow))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'shadow))

  :bind (("M-u" . capitalize-word)
         ("M-=" . count-words)
         ;; stock C-x C-b is the ancient list-buffers; ibuffer succeeds it
         ("C-x C-b" . ibuffer)
         ;; buffer-switch reflex (outline nav: built-in M-g i imenu)
         ("M-b" . switch-to-buffer)
         ("<escape>" . keyboard-escape-quit)))

(keymap-global-unset "C-x C-z")   ; suspend-frame, too easy to fat-finger

(use-package help-mode
  :ensure nil
  :bind (:map help-mode-map
         ("q" . kill-buffer-and-window)
         ("<escape>" . kill-buffer-and-window)))


;;; Clipboard -----
;; tty kills reach the system clipboard via OSC 52 (xterm.el's
;; setSelection). That code only runs for terminals whose TERM resolves
;; to a term/ init file: ghostty's TERM=xterm-ghostty falls back to
;; term/xterm.el by prefix, but wezterm's TERM=wezterm matches nothing -
;; alias it. NOT forcing modifyOtherKeys: kkp owns the keyboard protocol.
(add-to-list 'term-file-aliases '("wezterm" . "xterm-256color"))
(setq xterm-extra-capabilities '(setSelection))

;; …and the reverse direction: OSC 52 is write-only (terminals refuse
;; clipboard READS for security), so on tty frames C-y additionally
;; consults the macOS pasteboard via pbpaste. GUI frames keep the
;; native path. Returning nil means "kill-ring already has it".
(defun vp/interprogram-paste ()
  "System-clipboard text for `yank', on GUI and tty frames alike."
  (if (display-graphic-p)
      (gui-selection-value)
    (let ((text (with-temp-buffer
                  (call-process "pbpaste" nil t nil)
                  (buffer-string))))
      (unless (or (string-empty-p text)
                  (equal text (car kill-ring)))
        text))))
;; pbpaste is macOS-only; other systems keep the stock paste function
(when (executable-find "pbpaste")
  (setq interprogram-paste-function #'vp/interprogram-paste))


;;; Modal editing: meow -----
;; Kakoune-style selection-first grammar, zero dependencies. Official
;; Colemak layout from meow's KEYBINDING_COLEMAK.org.
;; SPC is the leader in normal state - it opens `vp/leader-map' (see
;; Command menu; keypad translation chains are disabled there).
;; Special modes (dired, agenda) keep their native keys via motion
;; state.
;; --- tier:5 | keybindings: modal editing | injects-into: eshell/ghostel modes ---
(use-package meow
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
    (meow-motion-define-key
     ;; Use e to move up, n to move down.
     ;; Since special modes usually use n to move down, we only overwrite e here.
     '("e" . meow-prev)
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("/" . meow-visit)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("e" . meow-prev)
     '("E" . meow-prev-expand)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-right)
     '("I" . meow-right-expand)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("s" . meow-insert)
     '("S" . meow-open-above)
     '("t" . meow-till)
     '("u" . meow-undo)
     ;; kak parity: U redoes (meow-undo-in-selection is M-x-only)
     '("U" . undo-redo)
     '("v" . meow-search)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-delete)
     '("X" . meow-backward-delete)
     '("y" . meow-save)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  ;; Shells/terminals open in insert state - typing must reach the prompt
  ;; (meow's default drops unlisted modes into normal state, where letters
  ;; are editing commands). ESC still pops to normal for copying output;
  ;; s re-enters insert.
  (dolist (mode '((eshell-mode  . insert)
                  (ghostel-mode . insert)))
    (add-to-list 'meow-mode-state-list mode))
  (meow-global-mode 1))


;;; Saving + Recent -----
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom (recentf-max-saved-items 60))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package saveplace
  :ensure nil
  :config (save-place-mode 1))


;;; Themes + Visuals -----
;; --- tier:2 | standalone: theme, font, tty transparency ---
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(setq frame-background-mode 'dark)
;; uniform text size everywhere - org headings by color/bold only
;; (consulted when the theme builds its face specs, so set before load)
(setq dracula-pro-pro-enlarge-headings nil)
(load-theme 'dracula-pro-pro t)

;; Font - match wezterm/ghostty: Aeonik Mono Medium 18pt.
;; Only affects GUI frames; terminal frames use the terminal's own font.
(set-face-attribute 'default nil
                    :family "Aeonik Mono" :weight 'medium :height 180)

(defun vp/transparent-background ()
  "Unset the default background in terminal frames for true transparency."
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook            #'vp/transparent-background)
(add-hook 'server-after-make-frame-hook #'vp/transparent-background)

;; shr (the HTML renderer behind eww and HTML mail) uses theme faces,
;; not the document's own colors - they clash with a dark theme
(setq shr-use-colors nil)


;;; Completions -----
;; Minibuffer UI: vertical candidate list, one per line, no manual
;; truncation/clip-left hacks needed - vertico does this natively.
;; --- tier:2 | standalone: minibuffer completion UI ---
(use-package vertico   ; GNU ELPA
  :init (vertico-mode 1)
  :config (setq vertico-cycle t))

;; Completion style: words match in any order/fragment, not just as
;; a contiguous prefix. Orthogonal to vertico - just a matching rule.
;; --- tier:2 | standalone: completion style ---
(use-package orderless   ; GNU ELPA
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Minibuffer annotations: file size/date, command docstrings, etc.
;; --- tier:2 | standalone: minibuffer annotations ---
(use-package marginalia   ; GNU ELPA
  :init (marginalia-mode 1))

;; Live-narrowing search/navigation commands (grep, buffers, xref,
;; imenu, marks…), with preview on selection. Needs vertico's hooks
;; for the async-refresh commands (consult-ripgrep etc.) to live-update.
;; --- tier:3 | coupled stack: consult (needs vertico) ---
(use-package consult   ; GNU ELPA
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Act on minibuffer/consult candidates without selecting them first;
;; embark-consult teaches `embark-export' to turn consult-ripgrep
;; results into a real, editable grep-mode buffer.
;; --- tier:3 | coupled stack: embark + embark-consult (needs consult) ---
(use-package embark   ; GNU ELPA
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-e" . embark-export)))

(use-package embark-consult   ; GNU ELPA
  :after (embark consult))

;; wgrep-change-to-wgrep-mode (bound to `e' in grep-mode buffers)
;; makes an embark-exported results buffer directly editable; saving
;; it (C-c C-c) writes every edit back to its source file.
;; --- tier:3 | coupled stack: wgrep (acts on grep-mode buffers) ---
(use-package wgrep)   ; GNU ELPA

;; In-buffer: completion-preview ghost text from the buffer's capf
;; sources (eglot feeds these) - TAB accepts, M-i completes up to the
;; shared prefix and lists the candidates.
;; Hook-based, NOT global: in eshell it would re-run pcomplete on
;; every keystroke, which lags typing.
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)

;; project-find-regexp / consult-ripgrep search with ripgrep
(when (executable-find "rg")
  (setq xref-search-program 'ripgrep))


;;; Custom tools + programming domain -----
;; tools.el: self-contained commands (ask-ai, file ops, zoxide, eshell)
;; programming.el: treesit, eglot, flymake, languages, git
(dolist (f '("tools.el" "programming.el"))
  (let ((path (expand-file-name f user-emacs-directory)))
    (when (file-exists-p path) (load path nil 'nomessage))))


;;; Dired -----
;; Nerd Font glyphs - terminal frames get them via ghostty/wezterm font
;; fallback; GUI frames via Symbols Nerd Font (brew cask, see README).
;; nerd-icons itself installs as this package's dependency.
;; --- tier:4 | hook: dired-mode | deps: nerd-icons ---
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :init
  ;; --group-directories-first is GNU-only; on macOS use coreutils gls if
  ;; available, else fall back to switches BSD ls understands.
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls"))
  :custom
  (dired-listing-switches
   (if (or (not (eq system-type 'darwin)) (executable-find "gls"))
       "-alh --group-directories-first"
     "-alh"))
  (dired-kill-when-opening-new-dired-buffer t)
  ;; with two dired windows open, copy/move defaults to the other one
  (dired-dwim-target t)
  ;; yazi/colemak navigation: h up, i enter (n/e via meow motion).
  ;; Everything situational (sort, details, marks…) is stock keys + `?'.
  :bind (:map dired-mode-map
         ("h" . dired-up-directory)
         ("i" . dired-find-file)
         ("RET" . vp/dired-find-file-smart)
         ("<mouse-1>" . vp/dired-find-file-smart)))

;; <escape> closes a transient like it closes everything else here;
;; stock transient leaves it unbound and only C-g backs out
;; --- tier:2 | deps: transient (loads on demand) ---
(with-eval-after-load 'transient
  (keymap-set transient-map "<escape>" #'transient-quit-one))

;;; Org Mode -----
(defun vp/all-org-files ()
  "All org files under `org-directory' (refile targets)."
  (directory-files-recursively org-directory "\\.org$"))

(defun vp/refresh-agenda-files (&rest _)
  "Set `org-agenda-files' to the curated agenda set.
Always inbox.org and agenda.org, plus any indexed file that carries the
:agenda: filetag - the per-project opt-in: put `#+filetags: :agenda:'
at the top of a file and its todos join the agenda. Everything else
(legacy todos, project READMEs) stays out of the agenda but remains
searchable through the org-mem index (SPC n f, SPC n /)."
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

;; Emacs 30 bundles org 9.7 - the built-in satisfies org-modern/org-node
;; version requirements, so package.el never downloads org.
;; --- tier:3 | coupled stack: org (loads on first org buffer / agenda) ---
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
  ;; Don't run org startup options (indent etc.) in files the agenda visits
  (org-agenda-inhibit-startup t)
  ;; No splits: agenda and src-block editing take over the current window
  (org-agenda-window-setup 'current-window)
  (org-src-window-setup 'current-window)
  ;; De-noise the agenda: no category gutter column, no done-item echoes,
  ;; hide the :agenda: plumbing tag, thin block separator
  (org-agenda-prefix-format '((agenda . "  %?-12t% s")
                              (todo   . "  ")
                              (tags   . "  ")
                              (search . "  ")))
  (org-agenda-hide-tags-regexp "\\`agenda\\'")
  (org-agenda-block-separator ?─)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  :config
  (setq org-directory (file-truename "~/Notes"))
  (make-directory org-directory t)
  ;; The two agenda anchors always exist (the agenda errors on missing files)
  (dolist (f '("inbox.org" "agenda.org"))
    (let ((path (expand-file-name f org-directory)))
      (unless (file-exists-p path)
        (with-temp-file path (insert "#+title: " (file-name-base f) "\n")))))
  (vp/refresh-agenda-files)

  ;; The daily-driver view is `o'. `n' is re-added explicitly: it is not a
  ;; dispatcher built-in but the DEFAULT VALUE of this variable, so setting
  ;; the variable without it silently removes the "view all" entry.
  (setq org-agenda-custom-commands
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

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
        '(("NEXT"      . (:foreground "#50fa7b" :weight bold))
          ("WAIT"      . (:foreground "#f1fa8c"))
          ("CANCELLED" . (:foreground "#6272a4" :strike-through t))))

  (setq org-capture-templates
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

  ;; Refile across ALL notes, not just the narrowed agenda set
  (setq org-refile-targets '((vp/all-org-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (org-clock-persistence-insinuate)
  (add-hook 'org-capture-mode-hook #'delete-other-windows))

;;; Command menu (SPC leader) -----
;; A dedicated keymap: SPC is ONLY this menu - meow's keypad
;; translation chains (SPC x → C-x C-… etc.) are disabled below, so
;; every letter is a menu key and real chords are typed as real chords
;; (C-x C-f, C-h k - muscle memory stays portable). C-c stays purely
;; mode-specific (org C-c C-*, eglot C-c e, …). Bindings are
;; (LABEL . COMMAND) menu items - which-key shows LABEL natively.
;; --- tier:5 | keybindings: SPC leader | deps: meow (keypad dispatch) ---
(defvar-keymap vp/leader-file-map)
(pcase-dolist (`(,key ,label ,cmd)
               '(("r" "rename/move file"   rename-visited-file)
                 ("o" "reveal in Finder"   vp/file-reveal)
                 ("e" "open (default app)" vp/file-open-default)
                 ("y" "copy file path"     vp/file-copy-path)))
  (keymap-set vp/leader-file-map key (cons label cmd)))

(defvar-keymap vp/leader-notes-map)
(pcase-dolist (`(,key ,label ,cmd)
               '(("f" "find/create note"   org-node-find)
                 ("i" "insert link"        org-node-insert-link)
                 ("/" "grep notes"         org-node-grep)
                 ("b" "backlinks/context"  org-node-context-toggle)
                 ("d" "daily note (today)" vp/daily-today)
                 ("s" "browse dailies"     org-node-seq-dispatch)))
  (keymap-set vp/leader-notes-map key (cons label cmd)))

(defvar-keymap vp/leader-ai-map)
(pcase-dolist (`(,key ,label ,cmd)
               '(("a" "ask ai"             vp/ai-ask)
                 ("m" "ask ai (follow-up)" vp/ai-ask-more)
                 ("c" "claude code"        claude-code-ide-menu)))
  (keymap-set vp/leader-ai-map key (cons label cmd)))

(defvar-keymap vp/leader-map)
(pcase-dolist (`(,key ,label ,cmd)
               `(("f" "find file (project)" project-find-file)
                 ("." "find file (path)"    find-file)
                 ("u" "file ops"             ,vp/leader-file-map)
                 ("r" "recent files"         consult-recent-file)
                 ("j" "jump dir (z)"         vp/zoxide-jump)
                 ("d" "dired here"           dired-jump)
                 ("b" "switch buffer"        consult-buffer)
                 ("/" "grep project"         consult-ripgrep)
                 ("k" "close buffer"         kill-current-buffer)
                 ("a" "agenda"               org-agenda)
                 ("v" "git status"           magit-status)
                 ("c" "capture"              org-capture)
                 ("n" "notes"                ,vp/leader-notes-map)
                 ("h" "help"                 ,help-map)
                 ("i" "ai"                   ,vp/leader-ai-map)
                 ("s" "eshell here"          vp/eshell-here)
                 ("?" "meow cheatsheet"      meow-cheatsheet)))
  (keymap-set vp/leader-map key (cons label cmd)))

;; no keypad translation chains - SPC dispatches straight into the menu
(setq meow-keypad-start-keys nil
      meow-keypad-meta-prefix nil
      meow-keypad-ctrl-meta-prefix nil
      meow-keypad-literal-prefix nil
      meow-keypad-leader-dispatch vp/leader-map)

;; mail entry only where mu4e exists (Nix machines). The binding lives
;; here, not in mu4e's :init: a tier 3 block must not reach up into a
;; tier 5 keymap.
(when (locate-library "mu4e")
  (keymap-set vp/leader-map "m" (cons "mail" #'mu4e)))


;;; Org Extensions -----
;; --- tier:3 | coupled stack: org visuals ---
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
  ;; no pill labels - flat is the aesthetic here; TODO states render as
  ;; glyphs instead (see vp/org-prettify-todos below)
  (org-modern-todo nil)
  (org-modern-tag nil)
  (org-modern-timestamp nil))

;; TODO states as glyphs - built-in prettify-symbols composes the
;; keyword into a symbol; the real text stays underneath (point on it
;; expands the word for editing). org-todo-keyword-faces still colors
;; them. Note: the agenda shows the plain words - it isn't org-mode.
(defun vp/org-prettify-todos ()
  (setq-local prettify-symbols-alist
              '(("TODO"      . ?☐)
                ("NEXT"      . ?▸)
                ("WAIT"      . ?◷)
                ("DONE"      . ?✓)
                ("CANCELLED" . ?✗))
              prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook #'vp/org-prettify-todos)

;; --- tier:3 | coupled stack: org visuals ---
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))


;;; Org Node -----
;; org-roam replacement: no SQLite database, nodes indexed by org-mem.
;; Creating a new node is just `org-node-find' with a name that doesn't exist.
;; --- tier:3 | coupled stack: org-node + org-mem notes index ---
(use-package org-node
  :after org
  :demand t   ; load with org so indexing modes come on, not on first C-c n
  :init
  ;; Watch ONLY ~/Notes. Watch dirs are re-traversed in the MAIN thread
  ;; (stat on every file) by a repeating idle timer - pointing them at
  ;; ~/Projects, ~/Git and OneDrive meant walking ~75k files (some of
  ;; them cloud placeholders) every ~15s of idle, felt as microstutters
  ;; everywhere. Org files in code projects still get indexed:
  ;; `org-mem-do-look-everywhere' (default t) picks them up from
  ;; recentf/org-id/org-agenda once visited.
  (setq org-mem-do-sync-with-org-id t
        org-mem-watch-dirs (list org-directory))
  :config
  (org-mem-updater-mode)
  (org-node-cache-mode)
  ;; Keep the narrowed agenda list current as org-mem (re)scans notes
  (add-hook 'org-mem-post-full-scan-functions #'vp/refresh-agenda-files)
  (add-hook 'org-mem-post-targeted-scan-functions #'vp/refresh-agenda-files)
  ;; Dailies: YYYY-MM-DD.org files under <notes>/daily as a node sequence
  ;; (org-node-seq-dispatch browses/creates by calendar date)
  (require 'org-node-seq)
  (setq org-node-seq-defs
        (list (org-node-seq-def-on-filepath-sort-by-basename
               "d" "Daily" (file-name-concat org-directory "daily") nil t)))
  (org-node-seq-mode))

(autoload 'org-node-seq-dispatch "org-node-seq" nil t)

;; Notes content search is org-node's own `org-node-grep' (bound at
;; SPC n /): now that consult is a dependency anyway, no wrapper is
;; needed - it live-narrows through consult-ripgrep directly. Bulk
;; replace across results: from the minibuffer, `embark-export'
;; (C-c C-e) turns a consult-ripgrep/org-node-grep session into a
;; wgrep-editable buffer; `e' there makes it editable, C-c C-c saves.

(defun vp/daily-today ()
  "Open today's daily note, creating it as a node if missing.
Mirrors the `:creator' of the \"d\" sequence so created files get an
ID and join the sequence."
  (interactive)
  (require 'org)       ; org's config sets org-directory, loads org-node
  (let* ((dir (file-name-concat org-directory "daily"))
         (file (file-name-concat dir (format-time-string "%Y-%m-%d.org"))))
    (make-directory dir t)
    (if (file-exists-p file)
        (find-file file)
      (let ((org-node-creation-fn #'org-node-new-file)
            (org-node-file-directory-ask dir))
        (org-node-create (format-time-string "%Y-%m-%d") (org-id-new) "d")))))


;;; Claude Code (MCP-integrated coding agent) -----
;; claude-code-ide bridges the Claude Code CLI into Emacs over MCP:
;; Claude sees buffers/selections, uses xref/imenu/flymake as tools, and
;; proposes edits through ediff. ghostel (libghostty - the Ghostty core
;; as an Emacs module) hosts its heavy TUI: fewest rendering artifacts
;; of the backends; pure-elisp terminals can't redraw it smoothly.
;; --- tier:3 | coupled stack: ghostel + claude-code-ide ---
(use-package ghostel
  :defer t
  :custom
  ;; outside elpa/ so package upgrades can't clobber a loaded module
  (ghostel-module-directory (expand-file-name "ghostel/" user-emacs-directory))
  (ghostel-module-auto-install 'download))   ; prebuilt, from GitHub releases

(use-package claude-code-ide   ; github-only, fetched by package-vc
  ;; :ensure nil is REQUIRED next to :vc under use-package-always-ensure,
  ;; else the ensure and vc handlers both install and collide.
  :ensure nil
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands (claude-code-ide-menu)   ; bound to SPC i c in the leader map
  :custom
  (claude-code-ide-terminal-backend 'ghostel)
  ;; a plain buffer, not a pinned side window: each new session takes
  ;; the whole frame instead of splitting the window in use
  (claude-code-ide-use-side-window nil)
  :config
  (claude-code-ide-emacs-tools-setup)
  (add-to-list 'display-buffer-alist
               '("\\*claude-code\\[" (display-buffer-full-frame))))


;;; Remote (TRAMP, built-in) -----
;; No "connect" step: remote is path syntax. SPC . /ssh:host:path
;; (hosts TAB-complete from ~/.ssh/config), eshell cd /ssh:host:…
;; runs builtins remotely, /docker:name:/… for containers,
;; /ssh:host|sudo:: for root. One ssh channel per host, multiplexed
;; automatically; M-x tramp-cleanup-all-connections when one wedges.
(use-package tramp
  :ensure nil
  :defer t
  :custom
  ;; auto-save-visited-mode would re-save remote buffers over the
  ;; network on every idle pause; keep that a local-files behavior
  (remote-file-name-inhibit-auto-save-visited t)
  ;; trust cached remote file attributes for 60s instead of 10 -
  ;; browsing re-stats far less; a stale listing costs a `g' revert
  (remote-file-name-inhibit-cache 60)
  :config
  ;; don't probe VC through the connection on every remote find-file -
  ;; the main remote slowdown; magit still works when invoked
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))


;;; Which Key (built-in since Emacs 30) -----
;; Full-width bottom panel (the minibuffer display crams everything into
;; an unreadable wall). Sorted by description so it reads like a menu;
;; C-h while it's up pages through long maps (C-h n / C-h p).
;; --- tier:5 | keybindings: binding discovery panel ---
(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 0.4)
  (which-key-sort-order 'which-key-description-order)
  (which-key-max-description-length 40)
  (which-key-add-column-padding 2)
  :config
  (which-key-mode))


;;; Mail (mu4e) -----
;; mu4e ships with mu (Nix-provided on nix-config machines); the whole mail
;; setup is skipped on machines where it isn't installed.
;; --- tier:3 | coupled stack: mu4e + msmtp (skipped when mu is absent) ---
(let ((nix-mu4e-file (expand-file-name "nix-mu4e.el" user-emacs-directory)))
  (when (file-exists-p nix-mu4e-file)
    (load nix-mu4e-file nil 'nomessage)))

(use-package mu4e
  :ensure nil                                ; Nix-provided; never from archives
  :when (locate-library "mu4e")
  :commands (mu4e mu4e-update-mail-and-index)
  :custom
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-change-filenames-when-moving t)      ; REQUIRED with mbsync, else UID clashes
  (mu4e-confirm-quit nil)
  (mu4e-sent-messages-behavior 'delete)      ; Proton saves Sent server-side; no 2nd copy
  :config
  ;; Folders are relative to the mu root (~/Mail). Account subdir is `proton`.
  ;; VERIFY exact names with `ls ~/Mail/proton` and adjust if needed.
  (setq mu4e-drafts-folder "/proton/Drafts"
        mu4e-sent-folder   "/proton/Sent"
        mu4e-trash-folder  "/proton/Trash"
        mu4e-refile-folder "/proton/Archive")

  (setq mu4e-maildir-shortcuts
        '((:maildir "/proton/Inbox"   :key ?i)
          (:maildir "/proton/Sent"    :key ?s)
          (:maildir "/proton/Drafts"  :key ?d)
          (:maildir "/proton/Archive" :key ?a)
          (:maildir "/proton/Trash"   :key ?t)))

  ;; Send via msmtp -> Bridge. f-is-evil + --read-envelope-from is the canonical
  ;; msmtp/message-mode pairing; account is picked from the From: header.
  (setq sendmail-program (executable-find "msmtp")
        message-send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-kill-buffer-on-exit t)

  (setq user-mail-address "vp@paulaus.com"
        user-full-name "Vytautas"))
;;; init.el ends here
