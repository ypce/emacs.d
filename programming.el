;;; programming.el --- Programming configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Programming modes with eglot, treesit, flymake. Minimal deps.

;;; Code:

;;; Tree-sitter (Emacs 29+) -----
;; --- tier:2 | built-in, but runs per-grammar availability checks at startup ---
(use-package treesit
  :ensure nil
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :config
  (setq treesit-language-source-alist
        '((bash     "https://github.com/tree-sitter/tree-sitter-bash")
          (go       "https://github.com/tree-sitter/tree-sitter-go")
          (gomod    "https://github.com/camdencheek/tree-sitter-go-mod")
          (python   "https://github.com/tree-sitter/tree-sitter-python")))

  ;; Fresh setup: build any missing grammar (one-time; needs git + a C compiler).
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (condition-case err
          (treesit-install-language-grammar lang)
        (error (message "treesit: could not install %s grammar: %s" lang err)))))

  ;; Remap only when the grammar is usable, so machines where a grammar
  ;; failed to build degrade to the plain modes instead of unfontified
  ;; ts-modes. (markdown stays on markdown-mode; markdown-ts-mode is much
  ;; poorer and not installed everywhere.)
  (pcase-dolist (`(,lang ,from ,to) '((python python-mode python-ts-mode)
                                      (bash   sh-mode     bash-ts-mode)
                                      (go     go-mode     go-ts-mode)))
    (when (treesit-language-available-p lang)
      (add-to-list 'major-mode-remap-alist (cons from to))))

  ;; No non-ts go-mode is installed, so claim Go files directly.
  (when (treesit-language-available-p 'go)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))
  (when (treesit-language-available-p 'gomod)
    (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))))


;;; Language Server Protocol (eglot - built-in, drives flymake) -----
;; --- tier:3 | coupled stack: eglot + eldoc + flymake ---
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((python-ts-mode
          go-ts-mode
          bash-ts-mode sh-mode
          markdown-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c C-d" . eldoc-doc-buffer))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0)) ; perf: don't accumulate event logs
  :config
  ;; C-c e menu - (LABEL . COMMAND) menu items, same pattern as the leader
  (defvar-keymap vp/eglot-menu-map)
  (pcase-dolist (`(,key ,label ,cmd)
                 '(("f" "format buffer"    eglot-format-buffer)
                   ("R" "rename symbol"    eglot-rename)
                   ("x" "code actions"     eglot-code-actions)
                   ("o" "organize imports" eglot-code-action-organize-imports)
                   ("d" "doc"              eldoc-doc-buffer)
                   ("r" "reconnect"        eglot-reconnect)))
    (keymap-set vp/eglot-menu-map key (cons label cmd)))
  (keymap-set eglot-mode-map "C-c e" (cons "eglot" vp/eglot-menu-map))
  (dolist (entry '(((python-ts-mode)            . ("pylsp"))
                   ((go-ts-mode)                . ("gopls"))
                   ((bash-ts-mode sh-mode)      . ("bash-language-server" "start"))
                   ((markdown-mode)             . ("marksman"))))
    (add-to-list 'eglot-server-programs entry)))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil))


;;; Flymake bindings (used by eglot for diagnostics) -----
;; --- tier:4 | hook: prog-mode | deps: flymake (built-in) ---
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c l" . flymake-show-buffer-diagnostics)))


;;; Language modes -----

;; Go (ts-mode only; major-mode-remap-alist redirects go-mode to it)
;; --- tier:3 | deps: treesit, eglot (format-on-save hook) ---
(use-package go-ts-mode
  :ensure nil
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :defer t
  :hook (go-ts-mode . vp/go-setup)
  :preface
  ;; gofmt-before-save belongs to the go-mode package, which is not
  ;; installed; gopls formats through eglot instead. Guarded so a save
  ;; still works when no server is attached.
  (defun vp/eglot-format-on-save ()
    "Format the buffer with eglot before save, when a server is attached."
    (when (eglot-managed-p) (eglot-format-buffer)))
  (defun vp/go-setup ()
    "Per-buffer Go setup: tabs for indent, format on save via gopls."
    (setq-local tab-width 4
                indent-tabs-mode t)
    (add-hook 'before-save-hook #'vp/eglot-format-on-save nil t)))

;; Markdown
;; --- tier:2 | standalone language modes ---
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :hook ((markdown-mode . visual-line-mode)
         ;; continuation lines keep the list/quote indent (Emacs 30)
         (markdown-mode . visual-wrap-prefix-mode))
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  ;; foghorn theme (jasonm23/markdown-css-themes), vendored in assets/
  ;; so previews style without network. foghorn-overrides.css loads
  ;; second and widens the fixed 700px body to GitHub's 980px.
  (markdown-css-paths
   (mapcar (lambda (f) (concat "file://"
                                (expand-file-name f user-emacs-directory)))
           '("assets/foghorn.css" "assets/foghorn-overrides.css"))))

;; Nix
(use-package nix-mode :defer t)

;; CSV/TSV - field-wise editing: kill/yank whole columns, sort by
;; field, align for reading (display-only; the file stays plain CSV).
;; Commands act on the region; without one they auto-select all
;; records around point and prompt.
(use-package csv-mode
  :defer t
  :mode "\\.[ct]sv\\'"
  :hook (csv-mode . csv-align-mode)
  :bind (:map csv-mode-map ("C-?" . vp/csv-menu)))

;; Menu on C-? (the editing-mode convention, same as org and elisp).
;; csv-mode ships the commands but no discoverable menu.
(with-eval-after-load 'transient
  (transient-define-prefix vp/csv-tmenu ()
    "csv-mode commands (columns are 1-based field numbers)."
    [["Columns"
      ("k" "kill column(s)"    csv-kill-fields)
      ("i" "insert column"     csv-insert-column)
      ("y" "yank column(s)"    csv-yank-fields)
      ("Y" "yank as new table" csv-yank-as-new-table)
      ("t" "transpose"         csv-transpose)]
     ["Sort"
      ("s" "sort by column"    csv-sort-fields)
      ("n" "sort numeric"      csv-sort-numeric-fields)
      ("r" "reverse rows"      csv-reverse-region)
      ("d" "toggle descending" csv-toggle-descending)]
     ["Display"
      ("a" "align (toggle)"    csv-align-mode)
      ("u" "unalign"           csv-unalign-fields)
      ("h" "header line"       csv-header-line)
      ("w" "hide/cap column"   csv-align-set-column-width)
      ("v" "hide separators"   csv-toggle-invisibility)]
     ["Separator"
      ("," "set separator"     csv-set-separator)
      ("g" "guess separator"   csv-guess-set-separator)]]))

(defun vp/csv-menu ()
  "Open the csv-mode command menu, a transient over csv-mode commands."
  (interactive)
  (require 'transient)
  (vp/csv-tmenu))

;; Beancount (plain-text accounting). Syntax highlighting, posting
;; alignment, and account/payee completion through completion-at-point,
;; which fido and completion-preview pick up. The in-mode commands live
;; on C-c (which-key lists them); `beancount-fava' starts fava and opens
;; the ledger in the browser.
(use-package beancount
  :defer t
  :mode (("\\.beancount\\'" . beancount-mode)
         ("\\.bean\\'"      . beancount-mode)
         ;; .ledger is the Ledger-format convention, but this ledger is
         ;; beancount syntax in .ledger files, so send it to beancount-mode.
         ("\\.ledger\\'"    . beancount-mode)))


;;; Version Control -----
;; SPC v opens the status buffer; everything else lives in magit's
;; own transient menus (? inside the status buffer).
;; Buffers pick up on-disk changes (git checkout, external edits…) via
;; file notifications instead of magit-auto-revert-mode, whose per-buffer
;; "is this file git-tracked?" probe spawned git on EVERY file open
;; (~65ms - the "dired open feels slower than helix" tax).
;; --- tier:4 | global mode: file-notification revert ---
(setq auto-revert-avoid-polling t)
(global-auto-revert-mode 1)

;; --- tier:2 | standalone: git UI (fully deferred) ---
(use-package magit
  :defer t   ; magit-status is autoloaded; bound to SPC v in the leader map
  :custom
  ;; redundant given global-auto-revert-mode above, and its per-buffer
  ;; git probe is what made opening any file slow
  (magit-auto-revert-mode nil)
  ;; no splits: magit reuses the current window (diffs still split, which
  ;; is the one case a split earns its keep)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun vp/diff-hl-dired-maybe ()
  "Enable the dired git gutter for local directories only.
Remote dired pays ssh round-trips for a VC probe that is almost
always meaningless there (/var/log isn't a repo)."
  (unless (file-remote-p default-directory)
    (diff-hl-dired-mode 1)))

;; --- tier:4 | hooks: prog-mode, dired-mode, magit refresh ---
(use-package diff-hl
  :hook ((prog-mode  . diff-hl-mode)
         (dired-mode . vp/diff-hl-dired-maybe)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  ;; don't tag gitignored files in dired (untracked handling is below)
  (diff-hl-dired-extra-indicators nil)
  :config
  ;; untracked files render as `?' by default (vc's "unknown"); paint
  ;; them like additions instead - new-to-git is green whether staged
  ;; or not. Staging questions belong to magit, not the dired gutter.
  (defun vp/diff-hl-dired-bmp (type pos)
    (diff-hl-fringe-bmp-from-type (if (eq type 'unknown) 'insert type) pos))
  (setq diff-hl-dired-fringe-bmp-function #'vp/diff-hl-dired-bmp)
  (with-eval-after-load 'diff-hl-dired
    (set-face-attribute 'diff-hl-dired-unknown nil
                        :inherit 'diff-hl-dired-insert)))


;;; Utilities -----
;; Built-in since Emacs 30
;; --- tier:4 | global mode: editorconfig (built-in) ---
(use-package editorconfig
  :ensure nil
  :config (editorconfig-mode 1))

;;; programming.el ends here
