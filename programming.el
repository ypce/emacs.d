;;; programming.el --- Programming configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Programming modes with eglot, treesit, flymake. Minimal deps.

;;; Code:

;;; Tree-sitter (Emacs 29+) -----
(use-package treesit
  :ensure nil
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :config
  (setq treesit-language-source-alist
        '((bash     "https://github.com/tree-sitter/tree-sitter-bash")
          (go       "https://github.com/tree-sitter/tree-sitter-go")
          (gomod    "https://github.com/camdencheek/tree-sitter-go-mod")
          (python   "https://github.com/tree-sitter/tree-sitter-python")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (fish     "https://github.com/ram02z/tree-sitter-fish")))

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
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c l" . flymake-show-buffer-diagnostics)))


;;; Language modes -----

;; Go (ts-mode only; major-mode-remap-alist redirects go-mode to it)
(use-package go-ts-mode
  :ensure nil
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :defer t
  :hook (go-ts-mode . vp/go-setup)
  :preface
  (defun vp/go-setup ()
    "Per-buffer Go setup: tabs for indent, gofmt on save."
    (setq-local tab-width 4
                indent-tabs-mode t)
    (when (executable-find "goimports")
      (setq gofmt-command "goimports"))
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

;; Markdown
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
  (markdown-fontify-code-blocks-natively t))

;; Fish shell (auto-mode entry comes from the package autoloads)
(use-package fish-mode :defer t)

;; Nix
(use-package nix-mode :defer t)

;; CSV files stay plain text. For heavy column surgery, pipe the
;; buffer through mlr or csvsql with M-|.


;;; Version Control -----
;; SPC v opens the status buffer; everything else lives in magit's
;; own transient menus (? inside the status buffer).
(use-package magit
  :defer t   ; magit-status is autoloaded; bound to SPC v in the leader map
  :custom
  ;; no splits: magit reuses the current window (diffs still split, which
  ;; is the one case a split earns its keep)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun vp/diff-hl-dired-maybe ()
  "Enable the dired git gutter for local directories only.
Remote dired pays ssh round-trips for a VC probe that is almost
always meaningless there (/var/log isn't a repo)."
  (unless (file-remote-p default-directory)
    (diff-hl-dired-mode 1)))

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
(use-package editorconfig
  :ensure nil
  :config (editorconfig-mode 1))

(use-package direnv
  :when (executable-find "direnv")
  :hook (after-init . direnv-mode)
  :custom
  (direnv-always-show-summary nil))

;;; programming.el ends here
