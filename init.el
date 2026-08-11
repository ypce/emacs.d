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
(unless (bound-and-true-p package--initialized)
  (package-initialize))          ; normal startup activates earlier; --batch doesn't
(unless package-archive-contents
  (package-refresh-contents))    ; fresh machine: fetch archive indexes once

(require 'use-package)
(setq use-package-always-ensure t)   ; built-ins opt out with :ensure nil

(use-package kkp   ; GNU ELPA
  :config (global-kkp-mode +1))


;;; Mode line -----
(defun vp/mode-line ()
  "Custom mode-line format."
  '(" - "
    (:eval (propertize (buffer-name) 'face 'font-lock-constant-face))
    "%6l:%c (%o) "
    (:eval (when vc-mode
             (concat " | ⇅ " (substring-no-properties vc-mode 5))))
    mode-line-format-right-align
    (:eval (concat "  " (symbol-name major-mode)))
    "  " mode-line-misc-info))


;;; Basic Emacs options -----
(use-package emacs
  :ensure nil
  :init
  ;; Customize would otherwise append custom-set-variables blobs to this
  ;; hand-maintained file; everything here is set in lisp, so discard them.
  (setq custom-file null-device)
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

;; Meow command menu (SPC ?) in the casual style: a transient that
;; shows the full normal-state map in groups. The keys in the menu are
;; the real meow keys, so the menu also teaches the bindings. transient
;; loads on first use, not at startup (same reason as casual below).
(with-eval-after-load 'transient
  (transient-define-prefix vp/meow-tmenu ()
    "Meow normal-state commands (digits 0-9 expand the last motion)."
    [["Move"
      ("h" "left"          meow-left)
      ("i" "right"         meow-right)
      ("e" "up"            meow-prev)
      ("n" "down"          meow-next)
      ("b" "back word"     meow-back-word)
      ("w" "next word"     meow-next-word)
      ("B" "back symbol"   meow-back-symbol)
      ("W" "next symbol"   meow-next-symbol)
      ("L" "goto line"     meow-goto-line)]
     ["Find"
      ("f" "find char"     meow-find)
      ("t" "till char"     meow-till)
      ("/" "visit regexp"  meow-visit)
      ("v" "search next"   meow-search)
      (";" "reverse"       meow-reverse)
      ("z" "pop selection" meow-pop-selection)]
     ["Select"
      ("l" "line"          meow-line)
      ("m" "word"          meow-mark-word)
      ("M" "symbol"        meow-mark-symbol)
      ("o" "block"         meow-block)
      ("O" "to block"      meow-to-block)
      ("g" "cancel"        meow-cancel-selection)
      ("G" "grab"          meow-grab)]
     ["Thing"
      ("," "inner"         meow-inner-of-thing)
      ("." "bounds"        meow-bounds-of-thing)
      ("[" "begin"         meow-beginning-of-thing)
      ("]" "end"           meow-end-of-thing)]]
    [["Edit"
      ("k" "kill"          meow-kill)
      ("x" "delete"        meow-delete)
      ("X" "delete back"   meow-backward-delete)
      ("c" "change"        meow-change)
      ("r" "replace"       meow-replace)
      ("j" "join"          meow-join)]
     ["Copy/Undo"
      ("y" "copy"          meow-save)
      ("p" "paste"         meow-yank)
      ("u" "undo"          meow-undo)
      ("U" "redo"          undo-redo)]
     ["Insert"
      ("s" "insert"        meow-insert)
      ("a" "append"        meow-append)
      ("S" "open above"    meow-open-above)
      ("A" "open below"    meow-open-below)]
     ["Expand"
      ("H" "left"          meow-left-expand)
      ("I" "right"         meow-right-expand)
      ("E" "up"            meow-prev-expand)
      ("N" "down"          meow-next-expand)]
     ["Other"
      ("'" "repeat"        repeat)
      ("q" "quit window"   meow-quit)]]))

(defun vp/meow-menu ()
  "Open the meow command menu, a transient in the casual style."
  (interactive)
  (require 'transient)
  (vp/meow-tmenu))


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


;;; Completions (all built-in) -----
;; Minibuffer: fido-vertical, flex matching.
(fido-vertical-mode 1)

;; One line per candidate, like fzf. Without this, long paths (SPC j,
;; recentf) soft-wrap and the list turns into a ragged block.
;; truncate-lines is the backstop; the advice below does the real work.
(defun vp/icomplete-truncate ()
  (setq-local truncate-lines t))
(add-hook 'icomplete-minibuffer-setup-hook #'vp/icomplete-truncate)

;; Keep the tail of over-wide candidates (fzf --keep-right): paths
;; differ at the leaf, so clip the left and prefix an ellipsis.
;; icomplete pads every line with spaces to the longest candidate, so
;; strip that padding first; without this one long entry makes every
;; line measure as over-wide and all of them get clipped.
;; Display-only: the advice edits the rendered text, not the candidates.
(defun vp/icomplete-clip-left (ret)
  (let ((w (max 20 (1- (window-width (or (active-minibuffer-window)
                                         (selected-window)))))))
    (mapconcat (lambda (line)
                 (let ((line (string-trim-right line)))
                   (if (> (string-width line) w)
                       (concat "…" (substring line (- (length line) (- w 1))))
                     line)))
               (split-string ret "\n")
               "\n")))
(advice-add 'icomplete-completions :filter-return #'vp/icomplete-clip-left)

;; In-buffer: completion-preview ghost text from the buffer's capf
;; sources (eglot feeds these) - TAB accepts, M-i completes up to the
;; shared prefix and lists the candidates.
;; Hook-based, NOT global: in eshell it would re-run pcomplete on
;; every keystroke, which lags typing.
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)

;; project-find-regexp (SPC /) searches with ripgrep; `r' in its
;; results buffer is project-wide query-replace
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
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; TAB unfolds a directory inline (yazi/modern-editor tree behavior)
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
         ("TAB" . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil))

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
         ("i" . dired-find-file)))

;; casual - its tmenu commands are package autoloads; loading eagerly
;; would drag the whole suite + transient (~300ms) into the first dired
;; buffer, so only the key bindings are wired here.
;; Convention: bare ? = "this buffer's menu" in special modes; C-? in
;; editing modes (where ? self-inserts). casual covers more modes
;; (calc, man, bookmarks, image, …) - a mode earns its line here the
;; day a real session wants it.
(use-package casual :defer t)
;; <escape> closes a transient like it closes everything else here;
;; stock transient leaves it unbound and only C-g backs out
(with-eval-after-load 'transient
  (keymap-set transient-map "<escape>" #'transient-quit-one))
;; preloaded maps bind directly; the rest bind when their mode loads
;; C-?, not bare ?: in isearch every printing char extends the search
;; string, so isearch counts as an editing context under the menu
;; convention (bare ? only where keys don't self-insert)
(keymap-set isearch-mode-map    "C-?" #'casual-isearch-tmenu)
(keymap-set emacs-lisp-mode-map "C-?" #'casual-elisp-tmenu)
(pcase-dolist (`(,feature ,map ,key ,cmd)
               '((dired      dired-mode-map       "?"   casual-dired-tmenu)
                 (ibuffer    ibuffer-mode-map     "?"   casual-ibuffer-tmenu)
                 (info       Info-mode-map        "?"   casual-info-tmenu)
                 (org-agenda org-agenda-mode-map  "?"   casual-agenda-tmenu)
                 (calendar   calendar-mode-map    "?"   casual-calendar-tmenu)
                 (help-mode  help-mode-map        "?"   casual-help-tmenu)
                 (compile    compilation-mode-map "?"   casual-compile-tmenu)
                 (org        org-mode-map         "C-?" casual-org-tmenu)))
  (with-eval-after-load feature
    (keymap-set (symbol-value map) key cmd)))

;; ediff builds its keymap at session start (ediff-mode-map is defvar'd
;; nil), so binding the map symbol at init breaks whenever ediff.el got
;; loaded early - use ediff's own keymap-setup hook.
(defun vp/ediff-casual-key ()
  (define-key ediff-mode-map "?" #'casual-ediff-tmenu))
(add-hook 'ediff-keymap-setup-hook #'vp/ediff-casual-key)

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
(defvar-keymap vp/leader-file-map)
(pcase-dolist (`(,key ,label ,cmd)
               '(("r" "rename/move file"   rename-visited-file)
                 ("o" "reveal in Finder"   vp/file-reveal)
                 ("O" "open (default app)" vp/file-open-default)
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

(defvar-keymap vp/leader-map)
(pcase-dolist (`(,key ,label ,cmd)
               `(("f" "find file (project)" project-find-file)
                 ("." "find file (path)"    find-file)
                 ("F" "file ops"             ,vp/leader-file-map)
                 ("r" "recent files"         vp/recentf-open)
                 ("j" "jump dir (z)"         vp/zoxide-jump)
                 ("D" "dired here"           dired-jump)
                 ("b" "switch buffer"        switch-to-buffer)
                 ("/" "grep project"         project-find-regexp)
                 ("d" "close buffer"         kill-current-buffer)
                 ("a" "agenda"               org-agenda)
                 ("v" "git status"           magit-status)
                 ("c" "capture"              org-capture)
                 ("n" "notes"                ,vp/leader-notes-map)
                 ("h" "help"                 ,help-map)
                 ("i" "ask ai"               vp/ai-ask)
                 ("I" "ask ai (follow-up)"   vp/ai-ask-more)
                 ("t" "claude code"          claude-code-ide-menu)
                 ;; the everything-else menu: rectangles, registers, sort…
                 ("o" "edit menu (casual)"   casual-editkit-main-tmenu)
                 ("s" "eshell here"          vp/eshell-here)
                 ("?" "meow menu"            vp/meow-menu)))
  (keymap-set vp/leader-map key (cons label cmd)))

;; no keypad translation chains - SPC dispatches straight into the menu
(setq meow-keypad-start-keys nil
      meow-keypad-meta-prefix nil
      meow-keypad-ctrl-meta-prefix nil
      meow-keypad-literal-prefix nil
      meow-keypad-leader-dispatch vp/leader-map)


;;; Org Extensions -----
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

;; Author recommends :config (not :hook) and a high hook depth so this attaches
;; after org-indent has set up. See https://github.com/jdtsmith/org-modern-indent
(use-package org-modern-indent   ; github-only, fetched by package-vc
  ;; :ensure nil is REQUIRED next to :vc under use-package-always-ensure,
  ;; else the ensure and vc handlers both install and collide.
  :ensure nil
  :vc (:url "https://github.com/jdtsmith/org-modern-indent" :rev :newest)
  :after org
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))


;;; Org Node -----
;; org-roam replacement: no SQLite database, nodes indexed by org-mem.
;; Creating a new node is just `org-node-find' with a name that doesn't exist.
(use-package org-node
  :after org
  :demand t   ; load with org so indexing modes come on, not on first C-c n
  :init
  ;; Index org files EVERYWHERE notes live - ~/Notes plus org files nested
  ;; in code projects. This feeds find/grep/backlinks (SPC n …), NOT the
  ;; agenda: agenda membership is curated, see `vp/refresh-agenda-files'.
  (setq org-mem-do-sync-with-org-id t
        org-mem-watch-dirs
        (seq-filter #'file-directory-p
                    (list org-directory "~/Projects" "~/Git")))
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
(use-package ghostel
  :defer t
  :custom
  ;; outside elpa/ so package upgrades can't clobber a loaded module
  (ghostel-module-directory (expand-file-name "ghostel/" user-emacs-directory))
  (ghostel-module-auto-install 'download))   ; prebuilt, from GitHub releases

(use-package claude-code-ide   ; github-only, fetched by package-vc
  :ensure nil                  ; required next to :vc, see org-modern-indent
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands (claude-code-ide-menu)   ; bound to SPC t in the leader map
  :custom
  (claude-code-ide-terminal-backend 'ghostel)
  :config
  (claude-code-ide-emacs-tools-setup))


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
(let ((nix-mu4e-file (expand-file-name "nix-mu4e.el" user-emacs-directory)))
  (when (file-exists-p nix-mu4e-file)
    (load nix-mu4e-file nil 'nomessage)))

(setq shr-use-colors nil)
(use-package mu4e
  :ensure nil                                ; Nix-provided; never from archives
  :when (locate-library "mu4e")
  :commands (mu4e mu4e-update-mail-and-index)
  :init
  (keymap-set vp/leader-map "m" (cons "mail" #'mu4e))
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
