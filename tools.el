;;; tools.el --- self-contained custom commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Tools that aren't configuration of any package: ask-ai, file
;; operations, the zoxide bridge, and the eshell suite. Rule of the
;; split: package glue lives beside its package (init.el /
;; programming.el); standalone tools live here.

;;; Code:

;;; Ask AI -----
;; Prompts for a question; `claude -p' (headless) answers under the
;; rules in emacs-ask.md with this config as grounding, async into a
;; small bottom window (q dismisses). Follow-up continues the same
;; conversation.
(defun vp/ai-ask--system-prompt ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "emacs-ask.md" user-emacs-directory))
    (dolist (f '("init.el" "tools.el" "programming.el"))
      (goto-char (point-max))
      (insert (format "\n\n=== config: %s ===\n" f))
      (goto-char (point-max))
      (insert-file-contents (expand-file-name f user-emacs-directory)))
    (buffer-string)))

(defvar-local vp/ai-ask--spinner nil
  "Cons of (TIMER . MARKER) for the in-buffer thinking indicator.")

(defun vp/ai-ask--spinner-stop ()
  "Remove the thinking indicator in the current buffer, if any."
  (when vp/ai-ask--spinner
    (cancel-timer (car vp/ai-ask--spinner))
    (let ((inhibit-read-only t))
      (delete-region (cdr vp/ai-ask--spinner) (point-max)))
    (setq vp/ai-ask--spinner nil)))

(defun vp/ai-ask--spinner-start ()
  "Animate a dim \"thinking…\" indicator at the end of the current buffer."
  (let* ((buf (current-buffer))
         (n 0)
         (marker (copy-marker (point-max)))
         timer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert (propertize "thinking" 'face 'shadow))))
    (setq timer
          (run-with-timer
           0.4 0.4
           (lambda ()
             (if (not (buffer-live-p buf))
                 (cancel-timer timer)
               (with-current-buffer buf
                 (when vp/ai-ask--spinner
                   (setq n (mod (1+ n) 4))
                   (let ((inhibit-read-only t))
                     (save-excursion
                       (goto-char marker)
                       (delete-region marker (point-max))
                       (insert (propertize
                                (concat "thinking" (make-string n ?.))
                                'face 'shadow))))))))))
    (setq vp/ai-ask--spinner (cons timer marker))))

(defun vp/ai-ask--run (question &optional more)
  (unless (executable-find "claude")
    (user-error "ai-ask: claude CLI not found in PATH"))
  ;; Run from user-emacs-directory (no CLAUDE.md here) with read-only tools
  ;; so answers can be verified against the installed package sources in
  ;; elpa/ - the model's training is stale for young packages like
  ;; org-node and meow.
  (let* ((default-directory user-emacs-directory)
         (q (if (and major-mode (not (eq major-mode 'fundamental-mode)))
                (format "%s (buffer major-mode: %s)" question major-mode)
              question))
         (buf (get-buffer-create "*ai-ask*")))
    (with-current-buffer buf
      (special-mode)
      (visual-line-mode 1)              ; soft-wrap long answer lines
      ;; no hollow box-cursor while the window is unselected (it lands
      ;; on the thinking indicator)
      (setq-local cursor-in-non-selected-windows nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "❯ " question "\n\n"))
      (vp/ai-ask--spinner-start))
    (display-buffer buf '((display-buffer-at-bottom)
                          (window-height . 6)
                          (dedicated . t)))
    (make-process
     :name "ai-ask" :noquery t
     :command (append (list "claude" "-p")
                      (when more (list "--continue"))
                      ;; haiku: 3-line keybinding lookups don't need a
                      ;; frontier model; verified it still answers from
                      ;; the config grounding
                      (list "--model" "haiku"
                            "--allowedTools" "Read,Glob,Grep"
                            "--append-system-prompt" (vp/ai-ask--system-prompt)
                            "--" q))
     :filter (lambda (_ out)
               ;; claude resets the terminal on exit: ESC sequences plus
               ;; bare control bytes (\x0e/\x0f = SO/SI charset switching)
               (setq out (replace-regexp-in-string
                          "\\(?:\e\\[[^a-zA-Z\e]*[a-zA-Z]\\|\e\\][^\a\e]*\a?\\|\e[()][0-9A-Za-z]\\|\e[0-9=><]\\|[\r\x0e\x0f]\\)"
                          "" out))
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (vp/ai-ask--spinner-stop)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert out)))
                 (when-let ((w (get-buffer-window buf)))
                   (fit-window-to-buffer w 15 4))))
     :sentinel (lambda (p _)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (vp/ai-ask--spinner-stop)
                     (unless (zerop (process-exit-status p))
                       (let ((inhibit-read-only t))
                         (goto-char (point-max))
                         (insert "\n[ai-ask failed - see above]")))))))))

(defun vp/ai-ask (question)
  "Ask the AI a how-to QUESTION about this Emacs setup."
  (interactive "sai: ")
  (vp/ai-ask--run question))

(defun vp/ai-ask-more (question)
  "Follow up on the last `vp/ai-ask' QUESTION."
  (interactive "sai follow-up: ")
  (vp/ai-ask--run question t))


;;; File ops (SPC u): act on the visited file itself -----
(defun vp/file-reveal ()
  "Reveal the current file (or directory) in Finder."
  (interactive)
  ;; expand-file-name: dired's default-directory is ~-abbreviated, and
  ;; shell-quote-argument's quoting stops the shell expanding the ~
  (shell-command
   (concat "open -R " (shell-quote-argument
                       (expand-file-name (or buffer-file-name default-directory))))))

(defun vp/file-open-default ()
  "Open the current file with the macOS default app.
In dired, opens the file at point (falls back to the directory)."
  (interactive)
  (shell-command
   (concat "open " (shell-quote-argument
                    (expand-file-name
                     (or buffer-file-name
                         (and (derived-mode-p 'dired-mode)
                              (dired-get-filename nil t))
                         default-directory))))))

(defvar vp/dired-emacs-extensions
  '("" "el" "org" "md" "markdown" "txt" "py" "js" "jsx" "ts" "tsx"
    "json" "yaml" "yml" "toml" "sh" "zsh" "bash" "conf" "cfg" "ini" "log"
    "csv" "html" "css" "scss" "go" "rs" "c" "h" "hpp" "cpp" "cc" "java"
    "rb" "lua" "nix" "xml" "sql" "gitignore" "dockerfile")
  "Extensions (no dot, lower-case; empty string is extensionless files)
that dired opens inside Emacs. Anything else opens with the macOS
default app.")

(defun vp/dired-emacs-file-p (file)
  "Return non-nil if FILE should open inside Emacs, not the system app."
  (or (file-directory-p file)
      (member (downcase (or (file-name-extension file) ""))
              vp/dired-emacs-extensions)))

(defun vp/dired-find-file-smart ()
  "Open the file at point: text/code in Emacs, else the macOS default app.
Bypass this and always open in Emacs with `dired-find-file' (bound to i)."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (or (null file) (vp/dired-emacs-file-p file))
        (dired-find-file)
      (vp/file-open-default))))

(defun vp/file-copy-path ()
  "Copy the current file's absolute path."
  (interactive)
  (let ((p (expand-file-name (or buffer-file-name default-directory))))
    (kill-new p)
    (message "%s" p)))


;;; Recent files -----
;; recentf-list is most-recent-first, but recentf-open feeds a plain
;; list to completing-read and fido re-sorts it. This table pins the
;; recency order, the same trick as the zoxide picker below.
(defun vp/recentf-open ()
  "Open a recent file; the list stays in most-recent-first order."
  (interactive)
  (let* ((files (mapcar #'abbreviate-file-name recentf-list))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . file)
                                 (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action files str pred)))))
    (find-file (completing-read "Recent: " table nil t))))


;;; Zoxide -----
;; The shell's `z` inside Emacs, against the SAME frecency database:
;; visiting files/dirs here bumps entries zsh sees and vice versa.
;; SPC j picks a frecent dir (zoxide's ranking, not alphabetical) and
;; lands in dired. Remote dirs are deliberately excluded - zsh can't
;; cd to /ssh: paths, and the db is shared.
(defun vp/zoxide-add ()
  "Silently bump `default-directory' in zoxide's database."
  (when-let* ((dir default-directory)
              ((not (file-remote-p dir)))
              ((file-directory-p dir))
              ((executable-find "zoxide")))
    (call-process "zoxide" nil 0 nil "add" (expand-file-name dir))))
;; --- tier:4 | hooks: find-file, dired-mode (shells out to zoxide) ---
(my/at-tier 4
(add-hook 'find-file-hook #'vp/zoxide-add)
(add-hook 'dired-mode-hook #'vp/zoxide-add)
)

(defun vp/zoxide-pick ()
  "Pick a frecent directory from zoxide's database (frecency order)."
  (unless (executable-find "zoxide")
    (user-error "zoxide not found in PATH"))
  (let* ((dirs (mapcar #'abbreviate-file-name
                       (process-lines "zoxide" "query" "--list")))
         ;; completion table that keeps zoxide's frecency order (the
         ;; minibuffer UI would otherwise re-sort alphabetically)
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . file)
                                 (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action dirs str pred)))))
    (completing-read "z: " table nil t)))

(defun vp/zoxide-jump ()
  "Jump to a frecent directory in dired (shell `z' equivalent)."
  (interactive)
  (dired (vp/zoxide-pick)))


;;; Eshell -----
;; Stock eshell renders colors and basic control codes itself
;; (eshell-handle-ansi-color/-control-codes in the default output
;; filters). Full-screen TUIs from `eshell-visual-commands' (htop,
;; yazi …) open in a ghostel buffer - the same emulator that hosts
;; Claude Code; `ghostel CMD' runs any command there ad hoc.
(add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode)

;; no login banner; a shell starts at its prompt
(setq eshell-banner-message "")

;; Prompt: only the last directory name - deep OneDrive paths drown
;; the command line otherwise. The full path is one `pwd' away.
;; Remote dirs keep their /ssh:host: prefix so a remote shell is
;; never mistaken for a local one.
(defun vp/eshell-prompt ()
  "Basename-only eshell prompt; remote dirs show their TRAMP prefix."
  (let ((base (file-name-nondirectory
               (directory-file-name (abbreviate-file-name default-directory)))))
    (concat (or (file-remote-p default-directory) "")
            (if (string-empty-p base) "/" base)
            " $ ")))
(setq eshell-prompt-function #'vp/eshell-prompt)

(defun vp/eshell-history ()
  "Insert a history entry picked with minibuffer completion (C-r reflex)."
  (interactive)
  (let ((cmd (completing-read "history: "
                              (delete-dups (ring-elements eshell-history-ring))
                              nil t)))
    (eshell-kill-input)
    (insert cmd)))

;; Long prompts (deep OneDrive paths) with the global truncate-lines
;; hscroll the whole window and hide previous output. Terminals wrap;
;; do the same here.
(defun vp/eshell-wrap-lines ()
  (setq-local truncate-lines nil))

;; esh-mode, not eshell: eshell-mode-map lives in esh-mode.el, so a
;; binding hung on the eshell feature fires before the map exists.
(use-package esh-mode
  :ensure nil
  ;; commands that read $EDITOR (git commit, crontab …) open a buffer
  ;; in THIS Emacs - with-editor ships with magit
  :hook ((eshell-mode . with-editor-export-editor)
         (eshell-mode . vp/eshell-wrap-lines))
  :bind (:map eshell-mode-map
         ("C-r" . vp/eshell-history)))

;; zoxide in eshell: `z foo` jumps like zsh; every cd feeds the shared db
(defun eshell/z (&rest args)
  "Jump to the zoxide match for ARGS (no args: home)."
  (if (null args)
      (eshell/cd)
    (let ((dir (string-trim
                (shell-command-to-string
                 (concat "zoxide query "
                         (string-join (mapcar #'shell-quote-argument args) " "))))))
      (if (file-directory-p dir)
          (eshell/cd dir)
        (user-error "zoxide: no match for %s" (string-join args " "))))))

(defun eshell/zi (&rest _)
  "Interactive zoxide picker (like zsh `zi'): choose a frecent dir, cd."
  (eshell/cd (vp/zoxide-pick)))

;; `clear' erases like a terminal (the default merely scrolls content
;; out of view, leaving a stray prompt at the window bottom)
(with-eval-after-load 'esh-mode
  (defalias 'eshell/clear #'eshell/clear-scrollback))

(defun vp/eshell-here ()
  "Open the shared eshell, cd'd to this buffer's directory.
One session that follows you: invoked from dired or a file buffer, it
cds there - as a real command, so history and the zoxide hook see it."
  (interactive)
  ;; file-truename also normalizes macOS firmlink routes like
  ;; "/Volumes/Macintosh HD/Users/…" back to the canonical path
  (let ((dir (file-truename default-directory)))
    (eshell)
    (goto-char (point-max))   ; point may be mid-buffer in a reused session
    (unless (string= (file-truename default-directory) dir)
      (eshell-kill-input)
      (insert (format "cd \"%s\"" dir))
      (eshell-send-input))))

;; vp/zoxide-add (not a raw call-process) - it guards against remote dirs
;; and a missing zoxide binary, both of which would error on every cd
(add-hook 'eshell-directory-change-hook #'vp/zoxide-add)

;;; tools.el ends here
