;;; init.el --- init -*- lexical-binding: t; -*-
;;; Commentary:
;; Built-in-first config for Emacs 31.
;;; Code:

;;; PATH -----
;; The launchd daemon gets no shell PATH; add tool dirs when present.
;; Each dir goes to the front, so reverse: the first dir listed wins.
(dolist (dir (reverse '("/etc/profiles/per-user/vp/bin" "/opt/homebrew/bin"
                        "/Users/vp/.local/bin")))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir ":" (getenv "PATH")))))


;;; Server -----
;; The launchd daemon owns the emacsclient socket. This guard is a
;; safety net: a stray GUI instance must not steal the socket.
(require 'server)
(unless (or (daemonp) (server-running-p))
  (server-start))
;; Child processes (Claude Code's C-g, git in ghostel) edit in a buffer
;; of this Emacs. C-x # finishes the edit. Without this they use vi.
(setenv "EDITOR" "emacsclient")

;; C-c C-c also finishes, like magit's commit buffer. Temp files only
;; (Claude's prompt, pass): an org or markdown file opened with
;; emacsclient keeps its own C-c C-c.
(define-minor-mode vp/temp-edit-mode
  "C-c C-c finishes an emacsclient edit of a temp file."
  :keymap (define-keymap "C-c C-c" #'server-edit))

(defun vp/temp-edit-maybe ()
  (when (file-in-directory-p buffer-file-name temporary-file-directory)
    (vp/temp-edit-mode 1)))
(add-hook 'server-visit-hook #'vp/temp-edit-maybe)


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
;; Loads on the first tty frame; GUI frames never need it.
(use-package kkp
  :hook (tty-setup . global-kkp-mode))


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
;; Runs on frame hooks so the first GUI frame applies it after daemon
;; init. Re-runs on each theme change.
(defun vp/flat-mode-line (&rest _)
  "Give the mode-line and fringe the default background.
No-op in non-graphic frames: there (face-background \='default) is a
tty pseudo-color (\"unspecified-bg\") that would poison the global
faces and render as white boxes in GUI frames."
  (when (display-graphic-p)
    (dolist (face '(mode-line mode-line-active mode-line-inactive))
      (set-face-attribute face nil
                          :background (face-background 'default)
                          :box `(:line-width 6 :color ,(face-background 'default))))
    (set-face-attribute 'fringe nil :background (face-background 'default))))

(add-hook 'window-setup-hook            #'vp/flat-mode-line)
(add-hook 'server-after-make-frame-hook #'vp/flat-mode-line)
(add-hook 'enable-theme-functions       #'vp/flat-mode-line)


;;; GC -----
;; Collect when no frame has focus, so a pause rarely lands while
;; typing. The threshold still forces a collection when it must.
(defun vp/gc-when-unfocused ()
  (unless (seq-some #'frame-focus-state (frame-list))
    (garbage-collect)))
(add-function :after after-focus-change-function #'vp/gc-when-unfocused)


;;; Basic Emacs options -----
(use-package emacs
  :ensure nil
  :init
  ;; Send Customize writes to a throwaway file, not this file. Not
  ;; null-device: custom-save-all reads the file back and aborts on it.
  (setq custom-file (file-name-concat temporary-file-directory "emacs-custom.el"))
  :custom
  (use-short-answers t)
  (scroll-conservatively 101)
  (fast-but-imprecise-scrolling t)
  (scroll-error-top-bottom t)   ; move point to the boundary before erroring
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
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-allow-motion t)     ; M-< / M-> jump to first/last match
  (isearch-repeat-on-direction-change t)
  ;; A space in C-s matches any text on the line: "def foo" finds
  ;; "def very_long_foo". M-s SPC toggles it back to literal spaces.
  (search-whitespace-regexp ".*?")
  (sentence-end-double-space nil)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (tab-always-indent 'complete)
  (switch-to-buffer-obey-display-actions t)
  (ffap-machine-p-known 'reject)   ; never ping hostnames at point
  (ns-click-through nil)   ; the focus click does not also act in the buffer
  ;; :custom, not setq: the option's setter restarts show-paren-mode,
  ;; a plain set does nothing while the mode is on.
  (show-paren-delay 0.05)
  (show-paren-context-when-offscreen 'overlay)
  (view-read-only t)
  ;; ghostty renders the real cursor shape; meow states become visible
  ;; in tty frames (bar in insert, box in normal).
  (xterm-update-cursor t)
  ;; TAB on a block-start line cycles folding (hs-minor-mode, Emacs 31);
  ;; elsewhere TAB keeps indent/complete.
  (hs-cycle-filter 'hs-hideable-block-p)
  ;; q kills help buffers instead of burying them.
  (quit-window-kill-buffer '(help-mode))
  :hook ((prog-mode . display-line-numbers-mode)
         ((prog-mode text-mode) . electric-pair-local-mode)
         (prog-mode . hs-minor-mode)   ; code folding (rewritten in Emacs 31)
         (org-mode . visual-wrap-prefix-mode)
         ((prog-mode text-mode) . completion-preview-mode))
  :config
  ;; Assume left-to-right text; skip the bidi scans on long lines.
  (setq-default bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa t)
  (setq-default truncate-lines t
                display-line-numbers-width 3
                indent-tabs-mode nil
                fill-column 100
                tab-width 4
                mode-line-format (vp/mode-line))

  (auto-save-visited-mode 1)
  (menu-bar-mode -1)
  (winner-mode 1)                     ; C-c <left> undoes window layout changes
  (undelete-frame-mode 1)             ; M-x undelete-frame recovers a closed client frame
  (repeat-mode 1)
  (pixel-scroll-precision-mode 1)
  ;; Dim … / ↩ instead of $ and \: tty frames, and GUI frames on the
  ;; right, which has no fringe (early-init.el)
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'shadow))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'shadow))

  :bind (("M-o" . other-window)
         ;; dwim case commands: act on the region when active, else the word
         ("<remap> <capitalize-word>" . capitalize-dwim)
         ("<remap> <upcase-word>" . upcase-dwim)
         ("<remap> <downcase-word>" . downcase-dwim)
         ("M-S-<down>" . duplicate-dwim)
         ("M-=" . count-words)
         ("C-s-f" . toggle-frame-fullscreen)   ; macOS-native ⌃⌘F
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

;; q kills via quit-window-kill-buffer; escape mirrors it.
(use-package help-mode
  :ensure nil
  :bind (:map help-mode-map
         ("<escape>" . quit-window)))


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


;;; Text to speech (kokoro-tts) -----
;; Self-contained module; the autoloads load it on first use.
(use-package vp-tts
  :ensure nil
  :load-path "lisp/"
  :commands (vp/tts-speak-dwim vp/tts-speed-up vp/tts-speed-down)
  :bind ("s-'" . vp/tts-speak-dwim)      ; GUI frames; C-c t t everywhere
  :init
  (keymap-global-set
   "C-c t" (cons "tts" (vp/labeled-keymap
                        '(("t" "speak (dwim)" vp/tts-speak-dwim)
                          ("+" "faster"       vp/tts-speed-up)
                          ("-" "slower"       vp/tts-speed-down))))))


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
;; Dired and eshell visits land in recentf as directory entries
;; (trailing slash), so recent DIRS are real visits, not just parents
;; of recent files. In consult-buffer they open dired.
(defun vp/recentf-track-dir ()
  "Add the current directory to `recentf-list'."
  (recentf-add-file default-directory))

(add-hook 'dired-mode-hook #'vp/recentf-track-dir)
(add-hook 'eshell-directory-change-hook #'vp/recentf-track-dir)

(defun vp/recent-dirs ()
  "Recent directories, most recent first.
Tracked dired/eshell visits plus parents of recent files."
  (delete-dups
   (mapcar (lambda (f) (abbreviate-file-name (file-name-directory f)))
           recentf-list)))

(defun vp/recent-dirs-table ()
  "A completion table over `vp/recent-dirs' that keeps its order."
  (completion-table-with-metadata
   (vp/recent-dirs) '((display-sort-function . identity)
                      (cycle-sort-function . identity))))

(defun vp/read-recent-dir ()
  (completing-read "Recent dir: " (vp/recent-dirs-table) nil t))

(defun vp/dired-recent-dir ()
  "Open dired in a recent directory."
  (interactive)
  (dired (vp/read-recent-dir)))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  ;; Dirs share the list with files (vp/recentf-track-dir); keep room.
  :custom
  (recentf-max-saved-items 300)
  ;; The daemon runs for weeks; do not lose the list on a crash.
  (recentf-autosave-interval 300)
  ;; Temp dirs (pass decrypts entries there) and git internals are
  ;; never places to go back to.
  (recentf-exclude '("\\`/\\(private/\\)?\\(tmp\\|var/folders\\)/" "/\\.git/"))
  :bind ("C-c d" . vp/dired-recent-dir))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom (savehist-additional-variables '(search-ring regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  ;; The daemon runs for weeks; do not lose places on a crash.
  :custom (save-place-autosave-interval 300))


;;; Completions (vertico + consult) -----
(use-package minibuffer
  :ensure nil
  :custom
  (completion-ignore-case t)
  (completions-detailed t)   ; annotations in describe-* prompts
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  :config
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;; Vertical candidate list; RET takes the highlighted candidate, M-RET
;; the literal input (a new file name that prefixes an existing one).
(use-package vertico
  :config (vertico-mode 1))

;; Bundled with vertico. DEL in a file prompt deletes a path component.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Remaps upgrade the stock keys; new commands live on M-s and M-g.
(use-package consult
  :bind (("<remap> <switch-to-buffer>" . consult-buffer)   ; + recent and project files
         ("s-b" . consult-buffer)   ; super keys reach ghostel, C-x b does not
         ("<remap> <yank-pop>" . consult-yank-pop)
         ("<remap> <goto-line>" . consult-goto-line)
         ("<remap> <imenu>" . consult-imenu)
         ("C-x C-r" . consult-recent-file)   ; shadows find-file-read-only
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep)
         ("M-g o" . consult-outline))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;; Match words in any order and fragment.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-expand-substring nil))

(when (executable-find "rg")
  (setopt xref-search-program 'ripgrep))


;;; Modal editing (meow) -----
;; Official Colemak layout. SPC is not the meow keypad: it opens
;; vp/leader-map, then every C-c map active in the buffer (mode maps
;; before the global one). Real chords stay real chords, so the chord
;; muscle memory stays portable.
(defvar-keymap vp/leader-map
  :doc "Direct SPC entries in meow normal and motion states.
They relieve the most frequent C-x chords. They live here, not on
C-c, because they only matter in modal states."
  "b" #'switch-to-buffer
  "x" #'execute-extended-command
  "k" #'kill-current-buffer
  "w" #'save-buffer
  "j" #'dired-jump
  "." #'find-file
  "/" #'consult-ripgrep
  "v" #'magit-status   ; v for vc
  "p" (cons "project" project-prefix-map))

(defun vp/leader (_)
  "Compose `vp/leader-map' with the C-c maps active in this buffer."
  (make-composed-keymap
   (cons vp/leader-map
         (seq-filter #'keymapp
                     (mapcar (lambda (map) (keymap-lookup map "C-c"))
                             (current-active-maps))))))

(use-package meow
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow-motion-define-key
   ;; e moves up. Special modes keep n for their own movement.
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
   ;; Stock U is meow-undo-in-selection; undo-redo gives kak parity.
   '("U" . undo-redo)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (keymap-set vp/leader-map "?" #'meow-cheatsheet)
  ;; A filter, so the composition happens per key press in the buffer.
  (dolist (map (list meow-normal-state-keymap meow-motion-state-keymap))
    (keymap-set map "SPC" '(menu-item "leader" nil :filter vp/leader)))
  ;; Shells open in insert state.
  (dolist (mode '(eshell-mode ghostel-mode))
    (add-to-list 'meow-mode-state-list (cons mode 'insert)))
  (meow-global-mode 1))


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
          ls-lisp-dirs-first t
          ;; One date format for new and old files: always 2 columns.
          ls-lisp-use-localized-time-format t
          ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
  ;; While s sorts by date (newest first), the date stays visible in
  ;; dired-hide-details-mode (columns 6-7, 8 for the 3-word date of a
  ;; remote ls).
  (defun vp/dired-show-mtime ()
    (setq-local dired-hide-details-preserved-columns
                (and (string-match-p dired-sort-by-date-regexp dired-actual-switches)
                     '(6 7 8))))
  (add-hook 'dired-before-readin-hook #'vp/dired-show-mtime)
  :custom
  (dired-listing-switches "-alhv")
  ;; Header shows the basename only, not the absolute path.
  (dired-hide-details-hide-absolute-location t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)   ; two dired windows: copy/move targets the other one
  (dired-isearch-filenames 'dwim)   ; C-s matches filenames only
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-mouse-drag-files t)   ; drag files from dired into other apps
  (dired-vc-rename-file t)     ; R in a repo does git mv
  (dired-create-destination-dirs 'ask)
  ;; h = up a directory (shadows describe-mode; C-h m remains).
  ;; Mouse clicks open in the same window, like RET does.
  :bind (:map dired-mode-map
         ("h" . dired-up-directory)
         ("<mouse-2>" . dired-mouse-find-file)))

;; File-type icons where files are picked: dired and the minibuffer
;; (C-x C-f, C-x b, recent files). The default font carries the glyphs.
(use-package nerd-icons
  :custom (nerd-icons-font-family "AeonikMono Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after nerd-icons
  :config (nerd-icons-completion-mode 1))


;;; Programming: treesit + eglot + flymake (all built-in) -----
(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :config
  ;; Build missing grammars on first use (needs git + a C compiler).
  ;; Each mode registers its grammar source at a tested commit.
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
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  ;; Flymake diagnostics at point show in the echo area without C-c l.
  (eldoc-help-at-pt t))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c l" . flymake-show-buffer-diagnostics)))

;; Go: tabs for indent, format on save via gopls.
;; auto-save-visited-mode saves from an idle timer, where this-command
;; is nil. Format only on user saves, not while the user types.
(defun vp/eglot-format-on-save ()
  "Format the buffer with eglot before a user save, when a server is attached."
  (when (and this-command (eglot-managed-p))
    (eglot-format-buffer)))

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
  ;; The Lua filter renders ```mermaid fences to inline SVG via mmdc,
  ;; cached by content hash in ~/.cache/mermaid-pandoc/.
  (markdown-command (concat "pandoc --lua-filter="
                            (expand-file-name "pandoc/mermaid-inline.lua"
                                              user-emacs-directory)))
  (markdown-split-window-direction 'right)   ; preview side by side
  ;; Preview styling, vendored in assets/ so it works without network.
  ;; foghorn-overrides.css loads second: full-width body, base font 80%.
  (markdown-css-paths
   (mapcar (lambda (f) (concat "file://" (expand-file-name f user-emacs-directory)))
           '("assets/foghorn.css" "assets/foghorn-overrides.css"))))

(use-package editorconfig
  :ensure nil
  :hook (after-init . editorconfig-mode))


;;; Version Control -----
;; Notification-based revert instead of magit-auto-revert, whose
;; per-buffer git probe slowed every file open.
(setopt auto-revert-avoid-polling t)
(global-auto-revert-mode 1)

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-auto-revert-mode nil)   ; covered by global-auto-revert above
  (magit-define-global-key-bindings nil)   ; C-x g above is enough
  ;; Reuse the current window; only diffs split.
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;; Windows, frames (super tier) -----
;; i3-style management on super. Windows (splits) navigate on Colemak
;; hnei, buffers live on lowercase letters (see Buffer sidebar), frames
;; on uppercase. Shift is the heavier variant: move a window instead of
;; focusing it, close a frame instead of a buffer. Overwritten NS
;; defaults keep their chords: quit C-x C-c, hide via menu.
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

;; Splits follow the iTerm convention: d right, shift-D below.
(keymap-global-set "s-d" #'split-window-right)
(keymap-global-set "s-D" #'split-window-below)
(keymap-global-set "s-q" #'delete-window)
(keymap-global-set "s-Q" #'delete-other-windows)
(keymap-global-set "s-T" #'make-frame-command)
(keymap-global-set "s-W" #'delete-frame)
(keymap-global-set "s-O" #'other-frame)

;;; Buffer sidebar (ibuffer) -----
;; SPC e: a left side window that lists buffers by remote host, org,
;; or project. The super keys follow its order: s-[ / s-] step (wrap
;; at the ends; a folded group is one stop), s-w kills and shows the
;; next row, s-t opens dired at the project root, s-b picks a buffer
;; by name, s-. folds the group (or click its name). Super keys pass
;; through ghostel, so they also work in Claude sessions. With no
;; sidebar on screen, s-[ / s-] are plain previous-buffer /
;; next-buffer. Loads ibuffer on first use.
(defconst vp/sidebar-name "*Sidebar*")

;; Attention marks: a buffer name turns red in the sidebar while it
;; waits for user action. Ghostel notifications feed the list (see the
;; ghostel block); the mark clears when the buffer is shown.
(defface vp/attention '((t :foreground "#ff2d55"))
  "Face for a buffer that waits for user action.")

(defvar vp/attention-buffers nil
  "Buffers that wait for user action while not displayed.")

(defun vp/sidebar--refresh ()
  (when-let* ((buf (get-buffer vp/sidebar-name)))
    (with-current-buffer buf (ibuffer-update nil t))))

(defun vp/attention-mark ()
  "Mark the current buffer as waiting for user action, unless visible."
  (unless (or (get-buffer-window nil t)
              (memq (current-buffer) vp/attention-buffers))
    (push (current-buffer) vp/attention-buffers)
    (vp/sidebar--refresh)))

(defun vp/attention-clear (_frame)
  "Drop attention marks for buffers that are now on screen."
  (when-let* ((gone (seq-filter (lambda (buf)
                                  (or (not (buffer-live-p buf))
                                      (get-buffer-window buf t)))
                                vp/attention-buffers)))
    (setq vp/attention-buffers (seq-difference vp/attention-buffers gone))
    (vp/sidebar--refresh)))
(add-hook 'window-buffer-change-functions #'vp/attention-clear)

(use-package ibuffer
  :ensure nil
  :bind (("s-[" . vp/sidebar-previous-buffer)
         ("s-]" . vp/sidebar-next-buffer)
         ("s-w" . vp/sidebar-kill-buffer)
         ("s-t" . vp/sidebar-dired)
         ("s-." . vp/sidebar-fold)
         :map vp/leader-map
         ("e" . vp/sidebar-toggle))
  :config
  (require 'ibuf-ext)
  (require 'hl-line)
  (require 'nerd-icons)

  (defun vp/sidebar-p ()
    "Non-nil in the sidebar buffer."
    (equal (buffer-name) vp/sidebar-name))

  (add-to-list 'display-buffer-alist
               `(,(regexp-quote vp/sidebar-name)
                 display-buffer-in-side-window
                 (side . left) (slot . 0)   ; width: vp/sidebar--fit
                 (preserve-size . (t . nil))
                 ;; Else other-window displays reuse it as the LRU window.
                 (dedicated . t)
                 (window-parameters (no-delete-other-windows . t)
                                    (no-other-window . t))))

  (defvar-local vp/sidebar--group nil
    "(DIRECTORY . GROUP) cache; project lookup is too slow per redraw.
Keyed on the directory, so dired and eshell regroup when they move.")

  (defun vp/sidebar-group ()
    "Group name of the current buffer: remote host, org, project, or other."
    (unless (equal (car vp/sidebar--group) default-directory)
      (setq vp/sidebar--group
            (cons default-directory
                  (cond ((file-remote-p default-directory)
                         (concat "@" (file-remote-p default-directory 'host)))
                        ((file-in-directory-p default-directory org-directory) "org")
                        ((when-let* ((pr (project-current nil)))
                           (file-name-nondirectory (directory-file-name (project-root pr)))))
                        (t "other")))))
    (cdr vp/sidebar--group))

  (defun vp/sidebar-hidden-p (buf)
    "Hide internal buffers, keep agent and shell buffers."
    (let ((name (buffer-name buf)))
      (or (equal name vp/sidebar-name)
          (and (string-prefix-p "*" name)
               (not (string-match-p "\\`\\*\\(?:cc \\|ghostel\\|eshell\\)" name))))))

  (defun vp/sidebar--listed ()
    (seq-remove (lambda (b) (or (string-prefix-p " " (buffer-name b))
                                (vp/sidebar-hidden-p b)))
                (buffer-list)))

  (defun vp/sidebar-groups ()
    (mapcar (lambda (g) (list g (cons 'predicate `(equal (vp/sidebar-group) ,g))))
            (sort (delete-dups
                   (mapcar (lambda (b) (with-current-buffer b (vp/sidebar-group)))
                           (vp/sidebar--listed)))
                  #'string<)))

  (defun vp/sidebar--group-buffers (group)
    "Buffers of GROUP in sidebar order, also when it is folded."
    (with-current-buffer vp/sidebar-name
      (mapcar #'car (ibuffer-sort-bufferlist
                     (mapcar (lambda (b) (cons b ?\s))
                             (seq-filter (lambda (b)
                                           (equal (with-current-buffer b (vp/sidebar-group))
                                                  group))
                                         (vp/sidebar--listed)))))))

  (defun vp/sidebar--regroup (&rest _)
    "Rebuild groups before each redraw, so new projects get a header."
    (when (vp/sidebar-p)
      (setq ibuffer-filter-groups (vp/sidebar-groups))))
  (advice-add 'ibuffer-update :before #'vp/sidebar--regroup)

  (defun vp/sidebar--no-title (&rest _)
    "Drop the column titles that ibuffer puts in the header line."
    (when (vp/sidebar-p)
      (setq header-line-format nil)))
  (advice-add 'ibuffer-update-title-and-summary :after #'vp/sidebar--no-title)

  (defvar vp/sidebar--indent nil
    "Non-nil while rows of a group with a name row are inserted.")

  (defun vp/sidebar--spacer ()
    "Insert a thin blank line, unless one is just above."
    (unless (or (bobp) (get-text-property (1- (point)) 'vp/sidebar-spacer))
      (insert (propertize "\n" 'font-lock-face '(:height 0.2) 'vp/sidebar-spacer t))))

  (defun vp/sidebar--group-header (fn name display-name filter-string format bmarklist)
    "A dim name with a fold triangle instead of bold [ name ].
A group of one buffer gets no name row. Thin gaps go around groups
with a name. A folded group shows its buffer count and turns red when
a buffer in it waits for user action."
    (if (not (vp/sidebar-p))
        (funcall fn name display-name filter-string format bmarklist)
      (let* ((folded (member name ibuffer-hidden-filter-groups))
             (titled (or folded (cdr bmarklist)))
             (hidden (and folded (vp/sidebar--group-buffers name)))
             (face (if (seq-some (lambda (b) (memq b vp/attention-buffers)) hidden)
                       'vp/attention
                     'shadow)))
        (when titled
          (vp/sidebar--spacer)
          (insert (propertize
                   (concat " " (propertize (if folded "▸" "▾") 'font-lock-face 'shadow)
                           " " (propertize name 'font-lock-face face)
                           (when folded
                             (propertize (format "  %d" (length hidden))
                                         'font-lock-face 'shadow))
                           "\n")
                   'ibuffer-filter-group-name name)))
        (when bmarklist
          (put-text-property
           (point)
           (let ((vp/sidebar--indent titled))
             (dolist (entry bmarklist)
               (ibuffer-insert-buffer-line (car entry) (cdr entry) format))
             (point))
           'ibuffer-filter-group name))
        (when titled (vp/sidebar--spacer)))))
  (advice-add 'ibuffer-insert-filter-group :around #'vp/sidebar--group-header)

  ;; Aligns the icon under the group name, past the chevron.
  (define-ibuffer-column indent (:name "")
    (if vp/sidebar--indent "  " ""))

  (define-ibuffer-column icon (:name " ")
    (if (string-prefix-p "*cc " (buffer-name))
        (nerd-icons-mdicon "nf-md-robot")   ; agent sessions are ghostel too
      (nerd-icons-icon-for-buffer)))

  ;; Not the stock name column: that one carries a keymap whose mouse-1
  ;; toggles marks, and text-property keymaps win over `vp/sidebar-map'.
  ;; Plain names: only attention gets a face.
  (define-ibuffer-column label (:name "Name")
    (propertize (string-trim (string-remove-prefix "*cc " (buffer-name)) ; icon says it
                             "\\*" "\\*")
                'font-lock-face
                (and (memq buffer vp/attention-buffers) 'vp/attention)))

  (defun vp/sidebar--main-window ()
    "Leave the sidebar for the last used main window."
    (when (vp/sidebar-p)
      (select-window (get-mru-window nil nil t))))

  (defun vp/sidebar-visit ()
    "Show the buffer at point in the last used main window.
On a group name, fold or unfold the group."
    (interactive)
    (if (get-text-property (point) 'ibuffer-filter-group-name)
        (vp/sidebar-fold)
      (when-let* ((buf (ibuffer-current-buffer)))
        (vp/sidebar--main-window)
        (switch-to-buffer buf))))

  (defun vp/sidebar-fold ()
    "Fold or unfold the group at point in the sidebar, else the group
of the current buffer. A group of one buffer has no name row to fold
into, so it stays open."
    (interactive)
    (when-let* ((side (get-buffer vp/sidebar-name))
                (group (if (eq (current-buffer) side)
                           (or (get-text-property (point) 'ibuffer-filter-group-name)
                               (get-text-property (point) 'ibuffer-filter-group))
                         (vp/sidebar-group))))
      (with-current-buffer side
        (cond ((member group ibuffer-hidden-filter-groups)
               (setq ibuffer-hidden-filter-groups
                     (delete group ibuffer-hidden-filter-groups)))
              ((text-property-any (point-min) (point-max)
                                  'ibuffer-filter-group-name group)
               (push group ibuffer-hidden-filter-groups)))
        (ibuffer-update nil t))))

  (defun vp/sidebar-mouse-visit (event)
    (interactive "e")
    (mouse-set-point event)
    (vp/sidebar-visit))

  ;; Navigation only: no ibuffer parent, so no mark, kill, sort, or
  ;; filter keys. s-w kills. meow motion state supplies e for up.
  (defvar-keymap vp/sidebar-map
    :suppress t
    "n" #'next-line
    "RET" #'vp/sidebar-visit
    "<down-mouse-1>" #'ignore            ; no drag region, no flashing
    "<drag-mouse-1>" #'ignore
    "<mouse-1>" #'vp/sidebar-mouse-visit
    "<mouse-2>" #'vp/sidebar-mouse-visit
    "q" #'vp/sidebar-toggle)

  (defun vp/sidebar--anchor (buf)
    "Position of BUF's row in the sidebar, else of its folded group's name."
    (let ((group (with-current-buffer buf (vp/sidebar-group)))
          header)
      (with-current-buffer vp/sidebar-name
        (save-excursion
          (goto-char (point-min))
          (catch 'found
            (while (not (eobp))
              (when (eq (car (get-text-property (point) 'ibuffer-properties)) buf)
                (throw 'found (point)))
              (when (equal (get-text-property (point) 'ibuffer-filter-group-name) group)
                (setq header (point)))
              (forward-line 1))
            (and header
                 (member group ibuffer-hidden-filter-groups)
                 (not (vp/sidebar-hidden-p buf))
                 header))))))

  (defun vp/sidebar-follow (&rest _)
    "Move the sidebar highlight to the buffer in the selected window."
    (when-let* ((win (get-buffer-window vp/sidebar-name))
                (buf (window-buffer (selected-window)))
                ((not (eq win (selected-window)))))
      (let ((pos (vp/sidebar--anchor buf)))
        (with-selected-window win
          (goto-char (or pos (point-max)))
          (hl-line-highlight)))))

  (defun vp/sidebar--fit ()
    "Fit the sidebar width to its longest line plus right padding."
    (when-let* ((win (get-buffer-window vp/sidebar-name)))
      (let ((width (+ 1 (save-excursion
                          (goto-char (point-min))
                          (let ((max 0))
                            (while (not (eobp))
                              (setq max (max max (string-width
                                                  (buffer-substring (pos-bol) (pos-eol)))))
                              (forward-line 1))
                            max)))))
        ;; IGNORE: preserve-size would block the resize.
        (unless (= width (window-body-width win))
          (window-resize win (- width (window-body-width win)) t t)))))

  (defun vp/sidebar--after-update (&rest _)
    "Redraws erase the buffer, collapse the hl-line overlay, and can
change the longest name."
    (when (vp/sidebar-p)
      (vp/sidebar--fit)
      (vp/sidebar-follow)))
  (advice-add 'ibuffer-update :after #'vp/sidebar--after-update)

  (defun vp/sidebar--stops ()
    "Stops in sidebar order: one list per buffer row, one list of all
its buffers per folded group."
    (with-current-buffer vp/sidebar-name
      (save-excursion
        (goto-char (point-min))
        (let (acc)
          (while (not (eobp))
            (if-let* ((b (car (get-text-property (point) 'ibuffer-properties))))
                (push (list b) acc)
              (when-let* ((g (get-text-property (point) 'ibuffer-filter-group-name))
                          ((member g ibuffer-hidden-filter-groups)))
                (push (vp/sidebar--group-buffers g) acc)))
            (forward-line 1))
          (nreverse acc)))))

  (defun vp/sidebar--neighbor (n)
    "The buffer N stops away from the current one, wrapping around.
A folded group is one stop and stays folded: it yields its most
recently used buffer, and the highlight sits on the group name."
    (when-let* ((stops (vp/sidebar--stops)))
      (let* ((i (seq-position stops (current-buffer) (lambda (s b) (memq b s))))
             (stop (nth (mod (if i (+ i n) (if (> n 0) 0 -1)) (length stops)) stops)))
        (seq-find (lambda (b) (memq b stop)) (buffer-list)))))

  (defun vp/sidebar--cycle (n)
    (if (get-buffer-window vp/sidebar-name)
        (progn (vp/sidebar--main-window)
               (when-let* ((buf (vp/sidebar--neighbor n)))
                 (switch-to-buffer buf)))
      (if (> n 0) (next-buffer) (previous-buffer))))

  (defun vp/sidebar-next-buffer ()
    (interactive)
    (vp/sidebar--cycle 1))

  (defun vp/sidebar-previous-buffer ()
    (interactive)
    (vp/sidebar--cycle -1))

  (defun vp/sidebar-kill-buffer ()
    "Kill the current buffer and show the next one in sidebar order."
    (interactive)
    (vp/sidebar--main-window)
    (let ((next (and (get-buffer-window vp/sidebar-name)
                     (vp/sidebar--neighbor 1)))
          (buf (current-buffer)))
      (when (and (kill-buffer buf)
                 next (not (eq next buf)) (buffer-live-p next))
        (switch-to-buffer next))))

  (defun vp/sidebar-dired ()
    "Open dired at the project root, else at this buffer's directory."
    (interactive)
    (vp/sidebar--main-window)
    (dired (if-let* ((pr (project-current nil)))
               (project-root pr)
             default-directory)))

  (defun vp/sidebar-toggle ()
    "Open and focus the sidebar; focus it if open; close it if focused.
The sidebar is not in the `other-window' cycle."
    (interactive)
    (let ((win (get-buffer-window vp/sidebar-name)))
      (cond ((eq win (selected-window)) (delete-window win))
            (win (select-window win))
            (t (save-selected-window
                 (ibuffer nil vp/sidebar-name nil t nil nil   ; groups: vp/sidebar--regroup
                          '((" " indent icon " " (label 0 26 :left :elide)))))
               (with-current-buffer vp/sidebar-name
                 (setq-local ibuffer-sorting-mode 'alphabetic   ; stable positions
                             ibuffer-show-empty-filter-groups nil
                             ibuffer-display-summary nil
                             ibuffer-use-header-line 'title   ; title leaves the buffer text
                             ibuffer-never-show-predicates (list #'vp/sidebar-hidden-p)
                             hl-line-sticky-flag t
                             cursor-in-non-selected-windows nil
                             mode-line-format nil)
                 (use-local-map vp/sidebar-map)
                 (ibuffer-auto-mode 1)
                 (hl-line-mode 1)
                 (ibuffer-update nil t))
               (let ((win (get-buffer-window vp/sidebar-name)))
                 ;; No fringes: the frame border alone is the gutter, as
                 ;; at the top. Rows pad one column on each side inside
                 ;; the highlight.
                 (set-window-fringes win 0 0)
                 (select-window win))))))

  ;; No cursor: the highlight shows the position. meow resets
  ;; cursor-type on each window switch, so the rule goes to meow.
  (with-eval-after-load 'meow
    (add-to-list 'meow-update-cursor-functions-alist
                 (cons #'vp/sidebar-p (lambda () (setq cursor-type nil)))))

  (add-hook 'window-buffer-change-functions #'vp/sidebar-follow)
  (add-hook 'window-selection-change-functions #'vp/sidebar-follow))


;;; Org -----
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
  (org-return-follows-link t)
  (org-insert-heading-respects-content t)
  (org-clock-persist 'history)
  (org-clock-out-when-done t)
  (org-clock-into-drawer t)
  ;; Skip org startup options in files the agenda visits.
  (org-agenda-inhibit-startup t)
  ;; No splits: agenda and src-block editing use the current window.
  (org-agenda-window-setup 'current-window)
  (org-src-window-setup 'current-window)
  ;; Show the category (file name or #+category:) and keep the items
  ;; of one category together. Hide the :agenda: tag.
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
  (setopt org-directory (file-truename "~/Notes"))
  (make-directory org-directory t)
  ;; Archive subtrees into archive/<file>.org, not <file>.org_archive
  ;; next to the hub: the policy forbids per-project archive files, and
  ;; only *.org stays indexed and greppable.
  (setopt org-archive-location (concat org-directory "/archive/%s::"))
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

(defun vp/capture-inbox ()
  "Capture a TODO to inbox.org without the template menu."
  (interactive)
  (org-capture nil "i"))

(defun vp/agenda (&optional arg)
  "Open the overview agenda. With a prefix ARG, show the dispatcher."
  (interactive "P")
  (if arg (org-agenda) (org-agenda nil "o")))

(keymap-global-set "C-c a" #'vp/agenda)
(keymap-global-set "C-c c" #'org-capture)


;;; Org visuals -----
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars "❯")
  (org-modern-list '((?* . "•") (?+ . "›") (?- . "–")))
  (org-modern-checkbox '((?X . "✓") (?\s . "☐") (?- . "–")))
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

;; Render mermaid src blocks with C-c C-c (needs mmdc from mermaid-cli).
;; Mermaid only draws a diagram, so it runs without the y/n prompt.
(use-package ob-mermaid
  :after org
  :config
  (setopt org-confirm-babel-evaluate
          (lambda (lang _body) (not (string= lang "mermaid")))))


;;; Org Node (notes) -----
;; org-mem indexes watched org files in a subprocess: instant
;; find/backlinks, no database.
(use-package org-node
  :after org   ; load with org so the indexing modes start
  :init
  ;; Watch ONLY ~/Notes: watch dirs are re-walked in the main thread on
  ;; an idle timer, and big trees cause stutter. Org files elsewhere
  ;; still get indexed once visited.
  (setopt org-mem-do-sync-with-org-id t
          org-mem-watch-dirs (list (file-truename "~/Notes")))
  :config
  ;; New notes from org-node-find are reference notes per the policy;
  ;; create them in topics/ without a prompt. Hubs use vp/new-project.
  (setopt org-node-file-directory-ask
          (file-name-concat org-directory "topics"))
  (make-directory org-node-file-directory-ask t)
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

;; Editable Unicode mind maps inside org "mindmap" blocks. Insert a
;; block with C-c C-, m; RET/TAB add siblings/children, M-RET edits,
;; M-arrows move nodes. The rest lives on the C-c m prefix.
(use-package org-mindmap
  ;; :ensure nil is required next to :vc, else both handlers install.
  :ensure nil
  :vc (:url "https://github.com/krvkir/org-mindmap" :rev :newest)
  :hook (org-mode . org-mindmap-mode)
  :bind (:map org-mindmap-mode-map
         ("C-c m d" . org-mindmap-delete-node)
         ("C-c m v" . org-mindmap-switch-layout)
         ("C-c m p" . org-mindmap-switch-compaction)
         ("C-c m m" . org-mindmap-list-to-mindmap)
         ("C-c m l" . org-mindmap-to-list)))

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

(defun vp/new-project (title)
  "Create the project hub projects/TITLE.org from the policy skeleton.
Open the hub if it already exists."
  (interactive "sProject title: ")
  (require 'org)   ; sets org-directory, loads org-node
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

;; Group grep results under one heading per file (Emacs 30).
(setopt grep-use-headings t)

(defun vp/notes-grep ()
  "Live ripgrep over all notes."
  (interactive)
  (require 'org)
  (consult-ripgrep org-directory))

(keymap-global-set
 "C-c n" (cons "notes" (vp/labeled-keymap
                        '(("f" "find/create note"   org-node-find)
                          ("i" "insert link"        org-node-insert-link)
                          ("g" "grep notes"         vp/notes-grep)
                          ("b" "backlinks/context"  org-node-context-toggle)
                          ("n" "capture to inbox"   vp/capture-inbox)
                          ("d" "daily note (today)" vp/daily-today)
                          ("p" "new project hub"    vp/new-project)
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
  (ghostel-glyph-scale-floor 0.8)
  :config
  ;; Claude Code and other TUIs send an OSC 9 notification when they
  ;; need input.  Mark the buffer in the sidebar; the default hook
  ;; entry still sends the desktop notification.
  (defun vp/ghostel-notify (_title _body)
    "Mark this buffer as waiting for input."
    (vp/attention-mark))
  (add-hook 'ghostel-notification-functions #'vp/ghostel-notify))

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
  ;; *cc proj* and *cc proj:instance*, short for tab names.
  (defun vp/claude-buffer-name (dir &optional instance)
    (format "*cc %s%s*"
            (file-name-nondirectory (directory-file-name dir))
            (if instance (concat ":" instance) "")))
  (setopt claude-code-ide-buffer-name-function #'vp/claude-buffer-name)
  (claude-code-ide-emacs-tools-setup)
  (add-to-list 'display-buffer-alist
               '("\\`\\*cc " (display-buffer-full-frame))))


;; One terminal per directory, like vp/eshell-here for eshell.
(defun vp/ghostel-here (&optional arg)
  "Show the ghostel terminal for this buffer's directory, or start one.
With a prefix ARG, start one more terminal there."
  (interactive "P")
  ;; Load first: a defcustom that loads inside the let gets no
  ;; global value.
  (require 'ghostel)
  (let ((ghostel-buffer-name
         (format "*ghostel %s*" (abbreviate-file-name default-directory))))
    (ghostel arg)))
(keymap-global-set "C-c o" #'vp/ghostel-here)


;;; Eshell -----
;; Full-screen TUIs (eshell-visual-commands) open in ghostel; the rest
;; stays in eshell.
(add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode)

(setopt eshell-banner-message "")

(defun vp/git-branch ()
  "The git branch of `default-directory', or the short commit if detached.
Reads HEAD directly: no git process per prompt."
  (when-let* ((root (locate-dominating-file default-directory ".git"))
              (git (expand-file-name ".git" root))
              (dir (if (file-directory-p git)
                       git
                     ;; A worktree: .git is a file that points to the git dir.
                     (with-temp-buffer
                       (insert-file-contents git)
                       (and (re-search-forward "^gitdir: \\(.+\\)" nil t)
                            (expand-file-name (match-string 1) root)))))
              (head (expand-file-name "HEAD" dir))
              ((file-readable-p head)))
    (with-temp-buffer
      (insert-file-contents head)
      (if (looking-at "ref: refs/heads/\\(.+\\)")
          (match-string 1)
        (buffer-substring 1 (min 8 (point-max)))))))

(defun vp/eshell-prompt ()
  "Dir name and git branch, then ❯ on its own line (red after a failure).
Remote dirs keep their TRAMP prefix and skip the branch."
  (let ((base (file-name-nondirectory
               (directory-file-name (abbreviate-file-name default-directory))))
        (remote (file-remote-p default-directory)))
    (concat (propertize (concat (or remote "") (if (string-empty-p base) "/" base))
                        'face 'eshell-prompt)
            (when-let* (((not remote)) (branch (vp/git-branch)))
              (propertize (concat "  " branch) 'face 'shadow))
            "\n"
            (propertize "❯" 'face (if (eq eshell-last-command-status 0) 'success 'error))
            " ")))
(setopt eshell-prompt-function #'vp/eshell-prompt)

;; Wrap long lines; with global truncate-lines they hscroll the window.
(defun vp/eshell-wrap-lines ()
  (setq-local truncate-lines nil))

;; Commands that read $EDITOR open a buffer in this Emacs (C-x # to
;; finish). Locally, do not use the with-editor sleeping editor: it is
;; a quoted sh -c string, and pass expands $EDITOR without quotes, so
;; the string breaks into wrong words. On a remote host, emacsclient
;; cannot reach this Emacs, so use the sleeping editor there.
(defun vp/eshell-export-editor ()
  "Set $EDITOR in this eshell for the local or remote directory."
  (if (file-remote-p default-directory)
      (with-editor-export-editor)
    (setenv "EDITOR" "emacsclient")))

;; Write each command to the history file at once, like zsh
;; inc_append_history; the default writes only when the buffer dies.
(defun vp/eshell-save-history (&rest _)
  "Append new history entries to the history file."
  (eshell-write-history nil t))

;; fish-style autosuggestion: completion-preview shows the newest
;; history line that starts with the input. Preview commands only: on
;; TAB with no preview, pcomplete completes files and commands.
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

;; Recent dirs, zoxide-style: the list that C-c d and dired use.
(defun eshell/z (&rest words)
  "cd to the most recent directory whose path contains all WORDS.
A WORDS that names a recent dir exactly (as `pcomplete/z' inserts
it) goes there. With no WORDS, pick one."
  (let* ((words (mapcar (lambda (w) (format "%s" w)) words))
         (dirs (vp/recent-dirs))
         (case-fold-search t))
    (eshell/cd
     (cond ((null words) (vp/read-recent-dir))
           ((and (length= words 1)
                 (car (member (concat "~/" (car words)) dirs))))
           ((seq-find (lambda (dir)
                        (seq-every-p (lambda (w) (string-match-p (regexp-quote w) dir))
                                     words))
                      dirs))
           (t (error "z: no recent dir matches %s" (string-join words " ")))))))

;; Home-relative candidates: pcomplete would escape a leading ~.
(defun pcomplete/z ()
  "Complete z from the recent dirs."
  (pcomplete-here
   (completion-table-with-metadata
    (mapcar (lambda (dir) (string-remove-prefix "~/" dir)) (vp/recent-dirs))
    '((display-sort-function . identity)))))

;; cd TAB: local folders first; with no local match, the recent dirs
;; (absolute, so the path stays valid from any directory).
(with-eval-after-load 'pcmpl-unix
  (defun pcomplete/cd ()
    "Complete local directories, then recent ones."
    (while (pcomplete-here
            (completion-table-in-turn
             (pcomplete-dirs)
             (completion-table-with-metadata
              (mapcar #'expand-file-name (vp/recent-dirs))
              '((display-sort-function . identity))))))))

(defun vp/eshell-recent-dir ()
  "Pick a recent dir and cd there as a real command."
  (interactive)
  (let ((dir (vp/read-recent-dir)))
    (goto-char (point-max))
    (eshell-kill-input)
    (insert (format "cd \"%s\"" dir))
    (eshell-send-input)))

;; esh-mode, not eshell: eshell-mode-map lives in esh-mode.el, so a
;; binding hung on the eshell feature fires before the map exists.
(use-package esh-mode
  :ensure nil
  :hook (((eshell-mode eshell-directory-change) . vp/eshell-export-editor)
         (eshell-mode . vp/eshell-wrap-lines)
         (eshell-mode . vp/eshell-completion-setup))
  :bind (:map eshell-mode-map
         ("C-r" . consult-history)
         ("C-c d" . vp/eshell-recent-dir))   ; dired elsewhere
  :custom
  (eshell-history-size 10000)
  (eshell-hist-ignoredups 'erase)   ; a rerun moves to the top
  (eshell-history-append t)         ; vp/eshell-save-history appends
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-cmpl-ignore-case t)
  ;; Tracked, unlike eshell/ (history and other state).
  (eshell-aliases-file (expand-file-name "etc/eshell-aliases" user-emacs-directory))
  ;; Like zsh cp -i / mv -i; rm already moves to the trash.
  (eshell-cp-interactive-query t)
  (eshell-mv-interactive-query t)
  :config
  ;; `clear' erases like a terminal; the default only scrolls.
  (defalias 'eshell/clear #'eshell/clear-scrollback))

;; Colors the command line as you type; unknown commands show red.
(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

;; M-f takes one word of the grey suggestion; TAB takes all of it.
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "M-f" #'completion-preview-insert-word))

;; TAB completion everywhere lists candidates in vertico, not a
;; *Completions* window.
(setopt completion-in-region-function #'consult-completion-in-region)

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
  ;; Do not create or check lockfiles on remote saves.
  (remote-file-name-inhibit-locks t)
  :config
  ;; Run remote async processes (eshell commands, compile, grep) as a
  ;; plain `ssh host cmd', skipping TRAMP's login-shell handshake.
  ;; Direct-async processes do not see tramp-remote-path additions.
  (connection-local-set-profile-variables
   'vp/tramp-direct-async
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh") 'vp/tramp-direct-async)
  ;; dzdo (Centrify sudo) as a hop, root by default:
  ;; /ssh:host|dzdo:host:/path. Same login as the stock sudo method.
  (add-to-list 'tramp-methods
               `("dzdo"
                 (tramp-login-program "dzdo")
                 (tramp-login-args (("-u" "%u") ("-s") ("-H") ("%l")))
                 ,@(nthcdr 3 (assoc "sudo" tramp-methods))))
  (add-to-list 'tramp-default-user-alist '("dzdo" nil "root"))
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
  :config
  ;; Show own commands without the vp/ namespace prefix. Hide the
  ;; ghostel-mode C-c chords; the keys still work.
  (add-to-list 'which-key-replacement-alist '((nil . "\\`vp/") . (nil . "")))
  (add-to-list 'which-key-replacement-alist '((nil . "\\`ghostel-") . t))
  ;; Stock Emacs leaves an empty C-c ^ prefix in the popup.
  (keymap-unset mode-specific-map "^" t)
  (which-key-mode))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
;;; init.el ends here
