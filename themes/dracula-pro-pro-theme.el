;;; dracula-pro-pro-theme.el --- Dracula Pro -*- lexical-binding: nil; -*-

;; Copyright (C) 2020-Today Dracula Theme.

;; Author: Dracula Team
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://draculatheme.com/pro

;;; Commentary:
;; Dracula PRO color scheme, stripped to the faces this config actually
;; uses: built-ins (font-lock, dired, isearch, show-paren, org, outline,
;; term, mode-line, completions) plus magit and markdown-mode. Face
;; groups for packages not installed here (helm, company, icicles,
;; powerline, gnus, mu4e, js2/3, web-mode, undo-tree, whitespace, …)
;; were removed from the upstream file; restore them from
;; mine/themes/dracula-pro-pro-theme.el if such a package returns.

;;; Code:

(require 'cl-lib)
(deftheme dracula-pro-pro
  "Dracula PRO - Pro Variant")


;;;; Configuration options:

(defgroup dracula-pro-pro nil
  "Dracula theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom dracula-pro-pro-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'dracula-pro-pro)

(defcustom dracula-pro-pro-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'dracula-pro-pro)

(defcustom dracula-pro-pro-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'dracula-pro-pro)

(defcustom dracula-pro-pro-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'dracula-pro-pro)

(defcustom dracula-pro-pro-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'dracula-pro-pro)

(defcustom dracula-pro-pro-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'dracula-pro-pro)


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [TTY-COLOR]
(let ((colors '(;; Upstream theme color
                (dracula-pro-pro-bg       "#22212C" "#201F2E" nil)             ; Background
                (dracula-pro-pro-fg       "#F8F8F2" "#F9F9F1" "brightwhite")   ; Foreground
                (dracula-pro-pro-current  "#454158" "#433D5C" "brightblack")   ; Current-line/selection
                (dracula-pro-pro-comment  "#7970A9" "#756AAF" "blue")          ; Comment
                (dracula-pro-pro-cyan     "#80FFEA" "#86F9E6" "brightcyan")    ; Cyan
                (dracula-pro-pro-green    "#8AFF80" "#8FF986" "green")         ; Green
                (dracula-pro-pro-orange   "#FFCA80" "#F9C986" "brightred")     ; Orange
                (dracula-pro-pro-pink     "#FF80BF" "#F986BF" "magenta")       ; Pink
                (dracula-pro-pro-purple   "#9580FF" "#9986F9" "brightmagenta") ; Purple
                (dracula-pro-pro-red      "#FF9580" "#F99986" "red")           ; Red
                (dracula-pro-pro-yellow   "#FFFF80" "#F9F986" "yellow")        ; Yellow
                ;; Other colors
                (dracula-pro-pro-bg2      "#201F2E" "#2B293D" "brightblack")
                (dracula-pro-pro-bg3      "#2B293D" "#35334D" "brightblack")
                (dracula-pro-pro-bg4      "#36334C" "#3F3D5C" "brightblack")
                (dracula-pro-pro-fg2      "#EDEDDE" "#EBEBE0" "brightwhite")
                (dracula-pro-pro-fg3      "#D6D6C2" "#D1D1C7" "white")
                (dracula-pro-pro-fg4      "#BABAAB" "#B3B3B3" "white")
                (dracula-pro-pro-alt-blue "#8A75F0" "#846EF7" "brightblue")))
      (faces '(;; default
               (cursor :background ,dracula-pro-pro-fg3)
               (completions-first-difference :foreground ,dracula-pro-pro-pink :weight bold)
               (default :background ,dracula-pro-pro-bg :foreground ,dracula-pro-pro-fg)
               (default-italic :slant italic)
               (ffap :foreground ,dracula-pro-pro-fg4)
               (fringe :background ,dracula-pro-pro-bg :foreground ,dracula-pro-pro-fg4)
               (highlight :foreground ,dracula-pro-pro-fg3 :background ,dracula-pro-pro-bg3)
               (hl-line :background ,dracula-pro-pro-current :extend t)
               (info-quoted-name :foreground ,dracula-pro-pro-orange)
               (info-string :foreground ,dracula-pro-pro-yellow)
               (lazy-highlight :foreground ,dracula-pro-pro-fg2 :background ,dracula-pro-pro-bg2)
               (link :foreground ,dracula-pro-pro-cyan :underline t)
               (line-number :slant italic :foreground ,dracula-pro-pro-bg4 :background ,dracula-pro-pro-bg)
               (match :background ,dracula-pro-pro-yellow :foreground ,dracula-pro-pro-bg)
               (minibuffer-prompt
                ,@(if dracula-pro-pro-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-pro-fg)
                    (list :weight 'bold :foreground dracula-pro-pro-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground unspecified :background ,dracula-pro-pro-orange)
               (vertical-border :foreground ,dracula-pro-pro-bg2)
               (success :foreground ,dracula-pro-pro-green)
               (warning :foreground ,dracula-pro-pro-orange)
               (error :foreground ,dracula-pro-pro-red)
               (header-line :background ,dracula-pro-pro-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,dracula-pro-pro-orange)
               (font-lock-comment-face :foreground ,dracula-pro-pro-comment)
               (font-lock-comment-delimiter-face :foreground ,dracula-pro-pro-comment)
               (font-lock-constant-face :foreground ,dracula-pro-pro-cyan)
               (font-lock-doc-face :foreground ,dracula-pro-pro-comment)
               (font-lock-function-name-face :foreground ,dracula-pro-pro-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,dracula-pro-pro-pink)
               (font-lock-negation-char-face :foreground ,dracula-pro-pro-cyan)
               (font-lock-preprocessor-face :foreground ,dracula-pro-pro-orange)
               (font-lock-reference-face :foreground ,dracula-pro-pro-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,dracula-pro-pro-cyan)
               (font-lock-regexp-grouping-construct :foreground ,dracula-pro-pro-purple)
               (font-lock-string-face :foreground ,dracula-pro-pro-yellow)
               (font-lock-type-face :foreground ,dracula-pro-pro-purple)
               (font-lock-variable-name-face :foreground ,dracula-pro-pro-fg
                                             :weight bold)
               (font-lock-warning-face :foreground ,dracula-pro-pro-orange :background ,dracula-pro-pro-bg2)
               ;; dired
               (dired-directory :foreground ,dracula-pro-pro-green :weight normal)
               (dired-flagged :foreground ,dracula-pro-pro-pink)
               (dired-header :foreground ,dracula-pro-pro-fg3 :background ,dracula-pro-pro-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,dracula-pro-pro-fg :weight bold)
               (dired-marked :foreground ,dracula-pro-pro-orange :weight bold)
               (dired-perm-write :foreground ,dracula-pro-pro-fg3 :underline t)
               (dired-symlink :foreground ,dracula-pro-pro-yellow :weight normal :slant italic)
               (dired-warning :foreground ,dracula-pro-pro-orange :underline t)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,dracula-pro-pro-orange))
               (flyspell-incorrect :underline (:style wave :color ,dracula-pro-pro-red))
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,dracula-pro-pro-bg :background ,dracula-pro-pro-orange)
               ;; magit
               (magit-branch-local :foreground ,dracula-pro-pro-cyan)
               (magit-branch-remote :foreground ,dracula-pro-pro-green)
               (magit-tag :foreground ,dracula-pro-pro-orange)
               (magit-section-heading :foreground ,dracula-pro-pro-pink :weight bold)
               (magit-section-highlight :background ,dracula-pro-pro-bg3 :extend t)
               (magit-diff-context-highlight :background ,dracula-pro-pro-bg3
                                             :foreground ,dracula-pro-pro-fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,dracula-pro-pro-orange
                                            :background ,dracula-pro-pro-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,dracula-pro-pro-orange
                                                      :background ,dracula-pro-pro-bg3
                                                      :weight bold
                                                      :extend t)
               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,dracula-pro-pro-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,dracula-pro-pro-green)
               (magit-diffstat-removed :foreground ,dracula-pro-pro-red)
               (magit-hash :foreground ,dracula-pro-pro-fg2)
               (magit-hunk-heading :background ,dracula-pro-pro-bg3)
               (magit-hunk-heading-highlight :background ,dracula-pro-pro-bg3)
               (magit-item-highlight :background ,dracula-pro-pro-bg3)
               (magit-log-author :foreground ,dracula-pro-pro-fg3)
               (magit-process-ng :foreground ,dracula-pro-pro-orange :weight bold)
               (magit-process-ok :foreground ,dracula-pro-pro-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,dracula-pro-pro-orange)
               (markdown-code-face :foreground ,dracula-pro-pro-orange)
               (markdown-footnote-face :foreground ,dracula-pro-pro-alt-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,dracula-pro-pro-pink
                ,@(when dracula-pro-pro-enlarge-headings
                    (list :height dracula-pro-pro-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,dracula-pro-pro-purple
                ,@(when dracula-pro-pro-enlarge-headings
                    (list :height dracula-pro-pro-height-title-2)))
               (markdown-header-face-3
                :foreground ,dracula-pro-pro-green
                ,@(when dracula-pro-pro-enlarge-headings
                    (list :height dracula-pro-pro-height-title-3)))
               (markdown-header-face-4 :foreground ,dracula-pro-pro-yellow)
               (markdown-header-face-5 :foreground ,dracula-pro-pro-cyan)
               (markdown-header-face-6 :foreground ,dracula-pro-pro-orange)
               (markdown-header-face-7 :foreground ,dracula-pro-pro-alt-blue)
               (markdown-header-face-8 :foreground ,dracula-pro-pro-fg)
               (markdown-inline-code-face :foreground ,dracula-pro-pro-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,dracula-pro-pro-orange)
               (markdown-table-face :foreground ,dracula-pro-pro-purple)
               ;; mode-line
               (mode-line :background ,dracula-pro-pro-current
                          :box ,dracula-pro-pro-current :inverse-video nil
                          ,@(if dracula-pro-pro-alternate-mode-line-and-minibuffer
                                (list :foreground dracula-pro-pro-fg3)
                              (list :foreground 'unspecified)))
               (mode-line-inactive
                :inverse-video nil
                ,@(if dracula-pro-pro-alternate-mode-line-and-minibuffer
                      (list :foreground dracula-pro-pro-comment :background dracula-pro-pro-bg
                            :box dracula-pro-pro-bg)
                    (list :foreground dracula-pro-pro-fg :background dracula-pro-pro-bg2 :box dracula-pro-pro-bg2)))
               ;; org
               (org-agenda-date :foreground ,dracula-pro-pro-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,dracula-pro-pro-comment)
               (org-agenda-done :foreground ,dracula-pro-pro-green)
               (org-agenda-structure :foreground ,dracula-pro-pro-purple)
               (org-block :foreground ,dracula-pro-pro-orange)
               (org-code :foreground ,dracula-pro-pro-yellow)
               (org-column :background ,dracula-pro-pro-bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,dracula-pro-pro-cyan :underline t)
               (org-document-info :foreground ,dracula-pro-pro-alt-blue)
               (org-document-info-keyword :foreground ,dracula-pro-pro-comment)
               (org-document-title :weight bold :foreground ,dracula-pro-pro-orange
                                   ,@(when dracula-pro-pro-enlarge-headings
                                       (list :height dracula-pro-pro-height-doc-title)))
               (org-done :foreground ,dracula-pro-pro-green)
               (org-ellipsis :foreground ,dracula-pro-pro-comment)
               (org-footnote :foreground ,dracula-pro-pro-alt-blue)
               (org-formula :foreground ,dracula-pro-pro-pink)
               (org-headline-done :foreground ,dracula-pro-pro-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,dracula-pro-pro-bg :background ,dracula-pro-pro-bg)
               (org-level-1 :inherit bold :foreground ,dracula-pro-pro-pink
                            ,@(when dracula-pro-pro-enlarge-headings
                                (list :height dracula-pro-pro-height-title-1)))
               (org-level-2 :inherit bold :foreground ,dracula-pro-pro-purple
                            ,@(when dracula-pro-pro-enlarge-headings
                                (list :height dracula-pro-pro-height-title-2)))
               (org-level-3 :weight normal :foreground ,dracula-pro-pro-green
                            ,@(when dracula-pro-pro-enlarge-headings
                                (list :height dracula-pro-pro-height-title-3)))
               (org-level-4 :weight normal :foreground ,dracula-pro-pro-yellow)
               (org-level-5 :weight normal :foreground ,dracula-pro-pro-cyan)
               (org-level-6 :weight normal :foreground ,dracula-pro-pro-orange)
               (org-level-7 :weight normal :foreground ,dracula-pro-pro-alt-blue)
               (org-level-8 :weight normal :foreground ,dracula-pro-pro-fg)
               (org-link :foreground ,dracula-pro-pro-cyan :underline t)
               (org-priority :foreground ,dracula-pro-pro-cyan)
               (org-scheduled :foreground ,dracula-pro-pro-green)
               (org-scheduled-previously :foreground ,dracula-pro-pro-yellow)
               (org-scheduled-today :foreground ,dracula-pro-pro-green)
               (org-sexp-date :foreground ,dracula-pro-pro-fg4)
               (org-special-keyword :foreground ,dracula-pro-pro-yellow)
               (org-table :foreground ,dracula-pro-pro-purple)
               (org-tag :foreground ,dracula-pro-pro-pink :weight bold :background ,dracula-pro-pro-bg2)
               (org-todo :foreground ,dracula-pro-pro-orange :weight bold :background ,dracula-pro-pro-bg2)
               (org-upcoming-deadline :foreground ,dracula-pro-pro-yellow)
               (org-warning :weight bold :foreground ,dracula-pro-pro-pink)
               ;; outline
               (outline-1 :foreground ,dracula-pro-pro-pink)
               (outline-2 :foreground ,dracula-pro-pro-purple)
               (outline-3 :foreground ,dracula-pro-pro-green)
               (outline-4 :foreground ,dracula-pro-pro-yellow)
               (outline-5 :foreground ,dracula-pro-pro-cyan)
               (outline-6 :foreground ,dracula-pro-pro-orange)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,dracula-pro-pro-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,dracula-pro-pro-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,dracula-pro-pro-purple :background ,dracula-pro-pro-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,dracula-pro-pro-pink :background ,dracula-pro-pro-bg
                            :box (:line-width 2 :color ,dracula-pro-pro-bg :style nil))
               (tab-bar-tab-inactive :foreground ,dracula-pro-pro-purple :background ,dracula-pro-pro-bg2
                                     :box (:line-width 2 :color ,dracula-pro-pro-bg2 :style nil))
               ;; term (eshell visual commands, ghostel fallbacks)
               (term :foreground ,dracula-pro-pro-fg :background ,dracula-pro-pro-bg)
               (term-color-black :foreground ,dracula-pro-pro-bg :background ,dracula-pro-pro-bg)
               (term-color-blue :foreground ,dracula-pro-pro-purple :background ,dracula-pro-pro-purple)
               (term-color-cyan :foreground ,dracula-pro-pro-cyan :background ,dracula-pro-pro-cyan)
               (term-color-green :foreground ,dracula-pro-pro-green :background ,dracula-pro-pro-green)
               (term-color-magenta :foreground ,dracula-pro-pro-pink :background ,dracula-pro-pro-pink)
               (term-color-red :foreground ,dracula-pro-pro-red :background ,dracula-pro-pro-red)
               (term-color-white :foreground ,dracula-pro-pro-fg :background ,dracula-pro-pro-fg)
               (term-color-yellow :foreground ,dracula-pro-pro-yellow :background ,dracula-pro-pro-yellow))))

  (apply #'custom-theme-set-faces
         'dracula-pro-pro
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (term-colors (mapcar #'car (mapcar #'cddr colors)))
               (tty-colors (mapcar #'car (mapcar #'last colors)))
               ;; nil colors (tty column) are fine inside nested plists
               ;; like :box, but deprecated as top-level attribute values
               ;; (Emacs warns per face per frame) - emit `unspecified'.
               (expand-for-kind (lambda (kind spec)
                                  (let ((attrs (cl-progv color-names kind
                                                 (eval `(backquote ,spec)))))
                                    (cl-loop for (key val) on attrs by #'cddr
                                             append (list key
                                                          (if (and (memq key '(:foreground :background))
                                                                   (null val))
                                                              'unspecified
                                                            val)))))))
           (cl-loop for (face . spec) in faces
                    collect `(,face
                              ((((min-colors 16777216)) ; fully graphical envs
                                ,(funcall expand-for-kind graphic-colors spec))
                               (((min-colors 256))      ; terminal withs 256 colors
                                ,(funcall expand-for-kind term-colors spec))
                               (t                       ; should be only tty-like envs
                                ,(funcall expand-for-kind tty-colors spec))))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula-pro-pro)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; dracula-pro-pro-theme.el ends here
