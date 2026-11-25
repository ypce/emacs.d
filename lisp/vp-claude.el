;;; vp-claude.el --- Claude Code sessions: resume, guard, restore -*- lexical-binding: t; -*-
;;; Commentary:
;; Glue for claude-code-ide on ghostel. It reads Claude Code's own
;; files, which are not a public API:
;;   ~/.claude/projects/*/<id>.jsonl  transcripts (cwd, aiTitle)
;;   ~/.claude/sessions/<pid>.json    the session of a live process
;;; Code:

(require 'claude-code-ide)

(defvar desktop-save-buffer)

(defun vp/claude-buffer-setup ()
  "Guard a Claude session against kills and save it with the desktop.
claude is the terminal's own process, so the default `auto' query
never sees a running command and kills a live session without asking."
  (when (string-prefix-p "*claude-code[" (buffer-name))
    (setq-local ghostel-query-before-killing t
                desktop-save-buffer #'vp/claude-desktop-misc)))

(defun vp/claude-desktop-misc (_dirname)
  "This session as (ID DIR INSTANCE), from Claude Code's pid file.
An unreadable file skips the session; an error would stop the whole
desktop save."
  (when-let* ((session claude-code-ide--session)
              (pid (claude-code-ide-mcp-session-cli-pid session))
              (s (ignore-errors
                   (with-temp-buffer
                     (insert-file-contents (format "~/.claude/sessions/%s.json" pid))
                     (json-parse-buffer :object-type 'alist)))))
    (list (alist-get 'sessionId s) (alist-get 'cwd s)
          (claude-code-ide-mcp-session-instance-name session))))

(defun vp/claude-desktop-restore (_file _name misc)
  "Resume the session in MISC, (ID DIR INSTANCE), without showing it.
Return the new buffer; the frameset restore puts it in its window."
  (pcase-let ((`(,id ,dir ,instance) misc))
    (when (and id (file-directory-p dir))
      (let ((display-buffer-overriding-action
             '(display-buffer-no-window (allow-no-window . t)))
            ;; The prefix forces the name prompt, which claude-code-ide
            ;; skips for a project's first instance; the stub answers it.
            (current-prefix-arg '(4))
            (before (buffer-list)))
        (cl-letf (((symbol-function 'claude-code-ide--read-instance-name)
                   (lambda (&rest _) instance)))
          (vp/claude--start dir id))
        (seq-find (lambda (b) (and (not (memq b before))
                                   (buffer-local-value 'claude-code-ide--session b)))
                  (buffer-list))))))

(defun vp/claude--start (dir id)
  "Start claude-code-ide in DIR, resuming session ID.
Claude Code resumes a session only from its own cwd, so start there,
not at the project root."
  (let ((default-directory (file-name-as-directory dir))
        (project-find-functions nil)
        (claude-code-ide-cli-extra-flags
         (string-join (list claude-code-ide-cli-extra-flags "--resume" id) " ")))
    (claude-code-ide)))

(defun vp/claude--field (key backward)
  "Decoded JSON string KEY, searched BACKWARD or forward from point."
  (when (funcall (if backward #'re-search-backward #'re-search-forward)
                 (format "\"%s\":\\(\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\)" key) nil t)
    (json-parse-string (match-string 1))))

(defun vp/claude--sessions ()
  "Saved Claude Code sessions, newest first, as (LABEL ID . DIR).
The transcript head has the cwd; the newest title is near its end.
A session whose cwd is gone is struck through."
  (let (rows)
    (pcase-dolist (`(,f . ,attrs)
                   (sort (mapcar (lambda (f) (cons f (file-attributes f)))
                                 (file-expand-wildcards "~/.claude/projects/*/*.jsonl"))
                         :key (lambda (fa) (file-attribute-modification-time (cdr fa)))
                         :reverse t))
      (with-temp-buffer
        (insert-file-contents f nil 0 65536)
        (when-let* ((dir (vp/claude--field "cwd" nil)))
          (let ((size (file-attribute-size attrs)))
            (erase-buffer)
            (insert-file-contents f nil (max 0 (- size 262144)) size)
            (goto-char (point-max))
            (push (cons (format "%s  %s  %s"
                                (format-time-string
                                 "%m-%d %H:%M" (file-attribute-modification-time attrs))
                                (propertize
                                 (or (vp/claude--field "aiTitle" t) "untitled")
                                 'face (and (not (file-directory-p dir))
                                            '(:strike-through t)))
                                (propertize (abbreviate-file-name dir) 'face 'shadow))
                        (cons (file-name-base f) dir))
                  rows)))))
    (nreverse rows)))

(defun vp/claude-resume ()
  "Resume a saved Claude Code session from any directory."
  (interactive)
  (let* ((rows (vp/claude--sessions))
         (pick (cdr (assoc (completing-read
                            "Resume: "
                            (completion-table-with-metadata
                             rows '((display-sort-function . identity)))
                            nil t)
                           rows))))
    (unless pick
      (user-error "No session"))
    (unless (file-directory-p (cdr pick))
      (user-error "Directory is gone: %s" (cdr pick)))
    (vp/claude--start (cdr pick) (car pick))))

(provide 'vp-claude)
;;; vp-claude.el ends here
