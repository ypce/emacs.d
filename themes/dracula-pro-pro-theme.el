;;; dracula-pro-pro-theme.el --- Dracula Pro -*- lexical-binding: t; -*-

;; Copyright (C) 2020-Today Dracula Theme.

;; Author: Dracula Team
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://draculatheme.com/pro

;;; Commentary:
;; Dracula PRO color scheme, stripped to the faces this config actually
;; uses: built-ins (font-lock, dired, isearch, show-paren, org, outline,
;; mode-line, completions) plus magit and markdown-mode. Face
;; groups for packages not installed here (helm, company, icicles,
;; powerline, gnus, mu4e, js2/3, web-mode, undo-tree, whitespace, …)
;; were removed from the upstream file; restore them from
;; mine/themes/dracula-pro-pro-theme.el if such a package returns.
;; GUI colors only: this config has no terminal frames.

;;; Code:

(deftheme dracula-pro-pro
  "Dracula PRO - Pro Variant")

(let ((bg        "#22212C")
      (fg        "#F8F8F2")
      (current   "#454158")
      (comment   "#7970A9")
      (cyan      "#80FFEA")
      (green     "#8AFF80")
      (orange    "#FFCA80")
      (pink      "#FF80BF")
      (purple    "#9580FF")
      (red       "#FF9580")
      (yellow    "#FFFF80")
      (bg2       "#201F2E")
      (bg3       "#2B293D")
      (bg4       "#36334C")
      (fg2       "#EDEDDE")
      (fg3       "#D6D6C2")
      (fg4       "#BABAAB")
      (alt-blue  "#8A75F0"))
  (apply
   #'custom-theme-set-faces
   'dracula-pro-pro
   `(;; default
     (cursor ((t :background ,fg3)))
     (completions-first-difference ((t :foreground ,pink :weight bold)))
     (default ((t :background ,bg :foreground ,fg)))
     (fringe ((t :background ,bg :foreground ,fg4)))
     (highlight ((t :foreground ,fg3 :background ,bg3)))
     (hl-line ((t :background ,current :extend t)))
     (lazy-highlight ((t :foreground ,fg2 :background ,bg2)))
     (link ((t :foreground ,cyan :underline t)))
     (line-number ((t :slant italic :foreground ,bg4 :background ,bg)))
     (match ((t :background ,yellow :foreground ,bg)))
     (minibuffer-prompt ((t :weight bold :foreground ,pink)))
     (read-multiple-choice-face ((t :inherit completions-first-difference)))
     (region ((t :inherit match :extend t)))
     (trailing-whitespace ((t :foreground unspecified :background ,orange)))
     (vertical-border ((t :foreground ,bg2)))
     (success ((t :foreground ,green)))
     (warning ((t :foreground ,orange)))
     (error ((t :foreground ,red)))
     (header-line ((t :background ,bg)))
     ;; ansi terminal palette (ghostel, eshell, compilation)
     (ansi-color-black ((t :foreground ,bg :background ,bg)))
     (ansi-color-red ((t :foreground ,red :background ,red)))
     (ansi-color-green ((t :foreground ,green :background ,green)))
     (ansi-color-yellow ((t :foreground ,yellow :background ,yellow)))
     (ansi-color-blue ((t :foreground ,purple :background ,purple)))
     (ansi-color-magenta ((t :foreground ,pink :background ,pink)))
     (ansi-color-cyan ((t :foreground ,cyan :background ,cyan)))
     (ansi-color-white ((t :foreground ,fg :background ,fg)))
     (ansi-color-bright-black ((t :foreground ,comment :background ,comment)))
     (ansi-color-bright-red ((t :foreground "#FFAA99" :background "#FFAA99")))
     (ansi-color-bright-green ((t :foreground "#A2FF99" :background "#A2FF99")))
     (ansi-color-bright-yellow ((t :foreground "#FFFF99" :background "#FFFF99")))
     (ansi-color-bright-blue ((t :foreground "#AA99FF" :background "#AA99FF")))
     (ansi-color-bright-magenta ((t :foreground "#FF99CC" :background "#FF99CC")))
     (ansi-color-bright-cyan ((t :foreground "#99FFEE" :background "#99FFEE")))
     (ansi-color-bright-white ((t :foreground "#FFFFFF" :background "#FFFFFF")))
     ;; syntax
     (font-lock-builtin-face ((t :foreground ,orange)))
     (font-lock-comment-face ((t :foreground ,comment)))
     (font-lock-constant-face ((t :foreground ,cyan)))
     (font-lock-doc-face ((t :foreground ,comment)))
     (font-lock-function-name-face ((t :foreground ,green :weight bold)))
     (font-lock-keyword-face ((t :weight bold :foreground ,pink)))
     (font-lock-negation-char-face ((t :foreground ,cyan)))
     (font-lock-preprocessor-face ((t :foreground ,orange)))
     (font-lock-regexp-grouping-backslash ((t :foreground ,cyan)))
     (font-lock-regexp-grouping-construct ((t :foreground ,purple)))
     (font-lock-string-face ((t :foreground ,yellow)))
     (font-lock-type-face ((t :foreground ,purple)))
     (font-lock-variable-name-face ((t :foreground ,fg
                                       :weight bold)))
     (font-lock-warning-face ((t :foreground ,orange :background ,bg2)))
     ;; dired
     (dired-directory ((t :foreground ,green :weight normal)))
     (dired-flagged ((t :foreground ,pink)))
     (dired-header ((t :foreground ,fg3 :background ,bg)))
     (dired-mark ((t :foreground ,fg :weight bold)))
     (dired-marked ((t :foreground ,orange :weight bold)))
     (dired-perm-write ((t :foreground ,fg3 :underline t)))
     (dired-symlink ((t :foreground ,yellow :weight normal :slant italic)))
     (dired-warning ((t :foreground ,orange :underline t)))
     ;; isearch
     (isearch ((t :inherit match :weight bold)))
     (isearch-fail ((t :foreground ,bg :background ,orange)))
     ;; magit
     (magit-branch-local ((t :foreground ,cyan)))
     (magit-branch-remote ((t :foreground ,green)))
     (magit-tag ((t :foreground ,orange)))
     (magit-section-heading ((t :foreground ,pink :weight bold)))
     (magit-section-highlight ((t :background ,bg3 :extend t)))
     (magit-diff-context-highlight ((t :background ,bg3
                                       :foreground ,fg3
                                       :extend t)))
     (magit-diff-revision-summary ((t :foreground ,orange
                                      :background ,bg
                                      :weight bold)))
     (magit-diff-revision-summary-highlight ((t :foreground ,orange
                                                :background ,bg3
                                                :weight bold
                                                :extend t)))
     ;; the four following lines are just a patch of the
     ;; upstream color to add the extend keyword.
     (magit-diff-added ((t :background "#335533"
                           :foreground "#ddffdd"
                           :extend t)))
     (magit-diff-added-highlight ((t :background "#336633"
                                     :foreground "#cceecc"
                                     :extend t)))
     (magit-diff-removed ((t :background "#553333"
                             :foreground "#ffdddd"
                             :extend t)))
     (magit-diff-removed-highlight ((t :background "#663333"
                                       :foreground "#eecccc"
                                       :extend t)))
     (magit-diff-file-heading ((t :foreground ,fg)))
     (magit-diff-file-heading-highlight ((t :inherit magit-section-highlight)))
     (magit-diffstat-added ((t :foreground ,green)))
     (magit-diffstat-removed ((t :foreground ,red)))
     (magit-hash ((t :foreground ,fg2)))
     (magit-hunk-heading ((t :background ,bg3)))
     (magit-hunk-heading-highlight ((t :background ,bg3)))
     (magit-log-author ((t :foreground ,fg3)))
     (magit-process-ng ((t :foreground ,orange :weight bold)))
     (magit-process-ok ((t :foreground ,green :weight bold)))
     ;; markdown
     (markdown-blockquote-face ((t :foreground ,orange)))
     (markdown-code-face ((t :foreground ,orange)))
     (markdown-footnote-face ((t :foreground ,alt-blue)))
     (markdown-header-face ((t :weight normal)))
     (markdown-header-face-1 ((t :inherit bold :foreground ,pink)))
     (markdown-header-face-2 ((t :inherit bold :foreground ,purple)))
     (markdown-header-face-3 ((t :foreground ,green)))
     (markdown-header-face-4 ((t :foreground ,yellow)))
     (markdown-header-face-5 ((t :foreground ,cyan)))
     (markdown-header-face-6 ((t :foreground ,orange)))
     (markdown-header-face-7 ((t :foreground ,alt-blue)))
     (markdown-header-face-8 ((t :foreground ,fg)))
     (markdown-inline-code-face ((t :foreground ,yellow)))
     (markdown-plain-url-face ((t :inherit link)))
     (markdown-pre-face ((t :foreground ,orange)))
     (markdown-table-face ((t :foreground ,purple)))
     ;; mode-line
     ;; Flat: the box pads the text, both blend into the buffer.
     (mode-line ((t :background ,bg :inverse-video nil
                    :box (:line-width 6 :color ,bg))))
     (mode-line-inactive ((t :background ,bg :inverse-video nil
                             :foreground ,fg
                             :box (:line-width 6 :color ,bg))))
     ;; org
     (org-agenda-date ((t :foreground ,cyan :underline nil)))
     (org-agenda-dimmed-todo-face ((t :foreground ,comment)))
     (org-agenda-done ((t :foreground ,green)))
     (org-agenda-structure ((t :foreground ,purple)))
     (org-block ((t :foreground ,orange)))
     (org-code ((t :foreground ,yellow)))
     (org-column ((t :background ,bg4)))
     (org-column-title ((t :inherit org-column :weight bold :underline t)))
     (org-date ((t :foreground ,cyan :underline t)))
     (org-document-info ((t :foreground ,alt-blue)))
     (org-document-info-keyword ((t :foreground ,comment)))
     (org-document-title ((t :weight bold :foreground ,orange)))
     (org-done ((t :foreground ,green)))
     (org-ellipsis ((t :foreground ,comment)))
     (org-footnote ((t :foreground ,alt-blue)))
     (org-formula ((t :foreground ,pink)))
     (org-headline-done ((t :foreground ,comment
                            :weight normal :strike-through t)))
     (org-hide ((t :foreground ,bg :background ,bg)))
     (org-level-1 ((t :inherit bold :foreground ,pink)))
     (org-level-2 ((t :inherit bold :foreground ,purple)))
     (org-level-3 ((t :weight normal :foreground ,green)))
     (org-level-4 ((t :weight normal :foreground ,yellow)))
     (org-level-5 ((t :weight normal :foreground ,cyan)))
     (org-level-6 ((t :weight normal :foreground ,orange)))
     (org-level-7 ((t :weight normal :foreground ,alt-blue)))
     (org-level-8 ((t :weight normal :foreground ,fg)))
     (org-link ((t :foreground ,cyan :underline t)))
     (org-priority ((t :foreground ,cyan)))
     (org-scheduled ((t :foreground ,green)))
     (org-scheduled-previously ((t :foreground ,yellow)))
     (org-scheduled-today ((t :foreground ,green)))
     (org-sexp-date ((t :foreground ,fg4)))
     (org-special-keyword ((t :foreground ,yellow)))
     (org-table ((t :foreground ,purple)))
     (org-tag ((t :foreground ,pink :weight bold :background ,bg2)))
     (org-todo ((t :foreground ,orange :weight bold :background ,bg2)))
     (org-upcoming-deadline ((t :foreground ,yellow)))
     (org-warning ((t :weight bold :foreground ,pink)))
     ;; outline
     (outline-1 ((t :foreground ,pink)))
     (outline-2 ((t :foreground ,purple)))
     (outline-3 ((t :foreground ,green)))
     (outline-4 ((t :foreground ,yellow)))
     (outline-5 ((t :foreground ,cyan)))
     (outline-6 ((t :foreground ,orange)))
     ;; show-paren
     (show-paren-match ((t :background unspecified
                           :foreground ,cyan
                           :weight bold)))
     (show-paren-match-expression ((t :inherit match)))
     (show-paren-mismatch ((t :inherit font-lock-warning-face))))))

(provide-theme 'dracula-pro-pro)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dracula-pro-pro-theme.el ends here
