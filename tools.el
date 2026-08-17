;;; tools.el --- self-contained custom commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Tools that aren't configuration of any package: ask-ai, file
;; operations, and the eshell suite. Rule of the split: package glue
;; lives beside its package (init.el / programming.el); standalone
;; tools live here.

;;; Code:

;;; Ask AI -----
;; Prompts for a question; `claude -p' (headless) answers under the
;; rules in emacs-ask.md with this config as grounding, async into a
;; small bottom window (q dismisses). Follow-up continues the same
;; conversation.
(defvar vp/ai-ask-model "haiku"
  "Model `vp/ai-ask' passes to `claude -p'. Set via `vp/ai-ask-set-model'.")

(defvar vp/ai-ask-effort nil
  "Effort level `vp/ai-ask' passes to `claude -p', or nil for the CLI default.
Set via `vp/ai-ask-set-effort'.")

(defvar vp/ai-ask-history nil
  "Minibuffer history for `vp/ai-ask' and `vp/ai-ask-more'.")

(defun vp/ai-ask-set-model (model)
  "Set the model `vp/ai-ask' uses to MODEL."
  (interactive (list (completing-read "ai-ask model: "
                                      '("haiku" "sonnet" "opus") nil t)))
  (setq vp/ai-ask-model model))

(defun vp/ai-ask-set-effort (effort)
  "Set the effort level `vp/ai-ask' uses to EFFORT, or unset it."
  (interactive
   (list (let ((choice (completing-read "ai-ask effort: "
                                        '("default" "low" "medium" "high" "xhigh" "max")
                                        nil t)))
           (unless (equal choice "default") choice))))
  (setq vp/ai-ask-effort effort))

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
                      ;; haiku default: 3-line keybinding lookups don't
                      ;; need a frontier model; verified it still answers
                      ;; from the config grounding. `vp/ai-ask-set-model'
                      ;; switches to sonnet/opus for harder questions.
                      (list "--model" vp/ai-ask-model)
                      ;; effort is independent of model choice; nil
                      ;; leaves it at the CLI's own default
                      (when vp/ai-ask-effort (list "--effort" vp/ai-ask-effort))
                      (list "--allowedTools" "Read,Glob,Grep"
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
  (interactive (list (read-string "ai: " nil 'vp/ai-ask-history)))
  (vp/ai-ask--run question))

(defun vp/ai-ask-more (question)
  "Follow up on the last `vp/ai-ask' QUESTION."
  (interactive (list (read-string "ai follow-up: " nil 'vp/ai-ask-history)))
  (vp/ai-ask--run question t))


;;; AI usage report -----
;; ccusage (via npx) parses Claude Code's local session logs for
;; tokens/cost, but reports each session by its raw UUID. Every
;; session's own JSONL log carries an `ai-title' record with the
;; actual human title - this stitches the two together.
(defun vp/ai-usage--title (session-id)
  "Human title for SESSION-ID, or the id itself if none is on disk yet."
  (or (when-let* ((file (car (file-expand-wildcards
                              (expand-file-name
                               (format "projects/*/%s.jsonl" session-id)
                               "~/.claude")))))
        (with-temp-buffer
          (call-process "grep" nil t nil "-m1" "-o"
                        "\"aiTitle\":\"[^\"]*\"" file)
          (goto-char (point-min))
          (when (re-search-forward "\"aiTitle\":\"\\([^\"]*\\)\"" nil t)
            (match-string 1))))
      session-id))

(defun vp/ai-usage--format-number (n)
  "Format integer N with thousands separators."
  (let ((s (number-to-string (truncate n))) (out ""))
    (while (> (length s) 3)
      (setq out (concat "," (substring s -3) out)
            s (substring s 0 -3)))
    (concat s out)))

(defun vp/ai-usage--day-label (day)
  "Human label for DAY (a YYYY-MM-DD string): Today, Yesterday, or DAY."
  (cond ((equal day (format-time-string "%Y-%m-%d")) "Today")
        ((equal day (format-time-string "%Y-%m-%d" (time-subtract nil (days-to-time 1))))
         "Yesterday")
        (t day)))

(defun vp/ai-usage (&optional since)
  "Show Claude Code token/cost usage per session, today and yesterday.
With a prefix arg, prompt for SINCE as YYYYMMDD."
  (interactive
   (list (when current-prefix-arg (read-string "since (YYYYMMDD): "))))
  (unless (executable-find "npx")
    (user-error "ai-usage: npx not found in PATH"))
  (let* ((since (or since (format-time-string "%Y%m%d" (time-subtract nil (days-to-time 1)))))
         (data (with-temp-buffer
                 (call-process "npx" nil t nil "--yes" "ccusage@latest"
                               "session" "--json" "--since" since)
                 (goto-char (point-min))
                 (json-parse-string (buffer-string) :object-type 'alist :array-type 'list)))
         (rows (mapcar
                (lambda (s)
                  (let ((activity (alist-get 'lastActivity (alist-get 'metadata s))))
                    (list (format-time-string "%Y-%m-%d" (encode-time (iso8601-parse activity)))
                          activity
                          (vp/ai-usage--title (alist-get 'period s))
                          (alist-get 'totalTokens s)
                          (alist-get 'totalCost s))))
                (alist-get 'session data)))
         (rows (sort rows (lambda (a b) (string> (cadr a) (cadr b)))))
         (days (delete-dups (mapcar #'car rows)))
         (buf (get-buffer-create "*ai-usage*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (day days)
          (let ((day-rows (seq-filter (lambda (r) (equal (car r) day)) rows)))
            (insert (propertize (vp/ai-usage--day-label day) 'face 'bold) "\n")
            (dolist (row day-rows)
              (pcase-let ((`(,_ ,activity ,title ,tokens ,cost) row))
                (insert (format "  %5s  $%-7.2f %10s  %s\n"
                                (format-time-string "%H:%M" (encode-time (iso8601-parse activity)))
                                cost
                                (vp/ai-usage--format-number tokens)
                                title))))
            (insert (format "  %-13s $%.2f\n\n" "day total"
                            (apply #'+ (mapcar (lambda (r) (nth 4 r)) day-rows))))))
        (insert (format "Total: $%.2f\n"
                        (apply #'+ (mapcar (lambda (r) (nth 4 r)) rows))))))
    (display-buffer buf)))


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
Files macOS has no app for (kLSApplicationNotFoundErr) fall back to
Emacs. Bypass this and always open in Emacs with `dired-find-file'
(bound to i)."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (or (null file)
            (vp/dired-emacs-file-p file)
            (/= 0 (call-process "open" nil nil nil (expand-file-name file))))
        (dired-find-file))))

(defun vp/file-copy-path ()
  "Copy the current file's absolute path."
  (interactive)
  (let ((p (expand-file-name (or buffer-file-name default-directory))))
    (kill-new p)
    (message "%s" p)))


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
         ("C-r" . consult-history)))

;; `clear' erases like a terminal (the default merely scrolls content
;; out of view, leaving a stray prompt at the window bottom)
(with-eval-after-load 'esh-mode
  (defalias 'eshell/clear #'eshell/clear-scrollback))

(defun vp/eshell-here ()
  "Open the shared eshell, cd'd to this buffer's directory.
One session that follows you: invoked from dired or a file buffer, it
cds there - as a real command, so eshell's history sees it."
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

;;; tools.el ends here
