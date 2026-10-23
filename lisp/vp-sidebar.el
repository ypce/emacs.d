;;; vp-sidebar.el --- Buffer sidebar -*- lexical-binding: t; -*-
;;; Commentary:
;; A left side window that lists buffers by remote host, org, or
;; project, with Claude Code sessions last: live ones as their
;; buffers, recent dormant ones as rows that resume them. The super
;; keys follow its order (see `vp/sidebar-next-buffer'). Each line
;; carries one `vp/sidebar' text property: a buffer, a group name, or
;; a dormant session. init.el autoloads the commands, so this file
;; loads on first use.
;;; Code:

(require 'hl-line)
(require 'project)
(require 'nerd-icons)
(require 'text-property-search)

(declare-function claude-code-ide "claude-code-ide")
(defvar claude-code-ide-cli-extra-flags)

(defconst vp/sidebar-name "*Sidebar*")

(defconst vp/sidebar-claude "claude"
  "Group of Claude Code sessions, drawn last.")

(defvar vp/sidebar-claude-directory "~/.claude/"
  "Claude Code state: sessions/ (live registry) and projects/ (transcripts).")

(defvar vp/sidebar-dormant-max 5
  "Number of dormant Claude Code sessions to list, newest first.
The full history stays in the claude --resume picker.")

(defface vp/sidebar-attention '((t :foreground "#ff2d55"))
  "Face for a buffer that waits for user action."
  :group 'convenience)

(defvar vp/sidebar--attention nil
  "Buffers that wait for user action while not displayed.")

(defvar vp/sidebar--folded nil
  "Names of folded groups.")

(defvar vp/sidebar--groups nil
  "Groups as last drawn: ((GROUP (NAME . BUFFER)...)...).")

(defvar vp/sidebar--dormant nil
  "Dormant Claude Code sessions as (ID CWD TITLE), newest first.")

(defvar vp/sidebar--transcripts (make-hash-table :test #'equal)
  "Transcript FILE to (MTIME . SESSION); SESSION is nil if not listed.")

(defvar vp/sidebar--timer nil)

(defvar-local vp/sidebar--group-cache nil
  "(DIRECTORY . GROUP); project lookup is too slow per redraw.
Keyed on the directory, so Dired and Eshell regroup when they move.")

(defvar-local vp/sidebar--session nil
  "Session ID that this buffer resumed, until the registry lists it.")

;;; Model

(defun vp/sidebar--listed-p (buf)
  "Non-nil if BUF gets a row.
Internal buffers stay out; agent and shell buffers stay in."
  (let ((name (buffer-name buf)))
    (not (or (string-prefix-p " " name)
             (and (string-prefix-p "*" name)
                  (not (string-match-p "\\`\\*\\(?:cc \\|ghostel\\|eshell\\)"
                                       name)))))))

(defun vp/sidebar--group (buf)
  "Group of BUF: Claude, remote host, org, project, or other."
  (with-current-buffer buf
    (if (string-prefix-p "*cc " (buffer-name))
        vp/sidebar-claude
      (unless (equal (car vp/sidebar--group-cache) default-directory)
        (setq vp/sidebar--group-cache
              (cons default-directory
                    (cond ((file-remote-p default-directory)
                           (concat "@" (file-remote-p default-directory 'host)))
                          ((and (bound-and-true-p org-directory)
                                (file-in-directory-p default-directory org-directory))
                           "org")
                          ((when-let* ((pr (project-current)))
                             (file-name-nondirectory
                              (directory-file-name (project-root pr)))))
                          (t "other")))))
      (cdr vp/sidebar--group-cache))))

(defun vp/sidebar--collect ()
  "Listed buffers as ((GROUP (NAME . BUFFER)...)...).
Groups and rows sort by name, so positions stay stable; Claude is last."
  (let (groups)
    (dolist (buf (buffer-list))
      (when (vp/sidebar--listed-p buf)
        (push (cons (buffer-name buf) buf)
              (alist-get (vp/sidebar--group buf) groups nil nil #'equal))))
    (sort (mapcar (lambda (g) (cons (car g) (sort (cdr g) :key #'car))) groups)
          :key #'car
          :lessp (lambda (a b)
                   (and (not (equal a vp/sidebar-claude))
                        (or (equal b vp/sidebar-claude) (string< a b))))
          :in-place t)))

;;; Claude Code sessions

(defun vp/sidebar--live-ids ()
  "Session IDs of running Claude Code processes."
  (let (ids)
    (dolist (file (file-expand-wildcards
                   (expand-file-name "sessions/*.json" vp/sidebar-claude-directory)))
      (when-let* ((s (ignore-errors
                       (with-temp-buffer
                         (insert-file-contents file)
                         (json-parse-buffer))))
                  ((eq 0 (ignore-errors (signal-process (gethash "pid" s) 0)))))
        (push (gethash "sessionId" s) ids)))
    (dolist (buf (buffer-list) ids)
      (when-let* ((id (buffer-local-value 'vp/sidebar--session buf)))
        (push id ids)))))

(defun vp/sidebar--field (key type backward)
  "Value of KEY in the first record of TYPE in this buffer.
BACKWARD means the last record. TYPE nil matches any record. Only KEY
is decoded: the records hold confidential conversation text."
  (goto-char (if backward (point-max) (point-min)))
  (let ((needle (format "\"%s\":" key))
        value)
    (while (and (not value)
                (if backward
                    (search-backward needle nil t)
                  (search-forward needle nil t)))
      ;; Lines cut at the chunk edges do not parse and are skipped.
      (when-let* ((rec (ignore-errors
                         (json-parse-string
                          (decode-coding-string
                           (buffer-substring-no-properties (pos-bol) (pos-eol))
                           'utf-8))))
                  ((or (null type) (equal (gethash "type" rec) type))))
        (setq value (gethash key rec)))
      (goto-char (if backward (pos-bol) (pos-eol))))
    value))

(defun vp/sidebar--read-transcript (file size)
  "Session (ID CWD TITLE) of transcript FILE of SIZE bytes, or nil.
A session that moved to a new ID, or that never got a title, is nil.
The cwd is in the first records and the title records repeat, so the
first and last 128 KB are enough."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((chunk 131072) cwd)
      (insert-file-contents-literally file nil 0 (min size chunk))
      (setq cwd (vp/sidebar--field "cwd" nil nil))
      (erase-buffer)
      (insert-file-contents-literally file nil (max 0 (- size chunk)) size)
      (when-let* ((cwd)
                  ((not (vp/sidebar--field "continuedInSessionId" "continued-in" t)))
                  (title (or (vp/sidebar--field "agentName" "agent-name" t)
                             (vp/sidebar--field "aiTitle" "ai-title" t))))
        (list (file-name-base file) cwd title)))))

(defun vp/sidebar--scan ()
  "Set `vp/sidebar--dormant' from the transcripts on disk.
Reads only the newest transcripts until it has enough, and each file
once per modification time."
  (let ((live (vp/sidebar--live-ids))
        (files (mapcar (lambda (f) (cons f (file-attributes f)))
                       (file-expand-wildcards
                        (expand-file-name "projects/*/*.jsonl"
                                          vp/sidebar-claude-directory))))
        found)
    (catch 'done
      (dolist (f (sort files :key (lambda (f) (file-attribute-modification-time (cdr f)))
                       :lessp #'time-less-p :reverse t :in-place t))
        (unless (member (file-name-base (car f)) live)
          (let* ((mtime (file-attribute-modification-time (cdr f)))
                 (cached (gethash (car f) vp/sidebar--transcripts))
                 (session (if (equal (car cached) mtime)
                              (cdr cached)
                            (cdr (puthash (car f)
                                          (cons mtime (vp/sidebar--read-transcript
                                                       (car f) (file-attribute-size (cdr f))))
                                          vp/sidebar--transcripts)))))
            (when session
              (push session found)
              (when (length= found vp/sidebar-dormant-max)
                (throw 'done nil)))))))
    (setq vp/sidebar--dormant (nreverse found))))

(defun vp/sidebar--resume (id cwd)
  "Resume Claude Code session ID in its directory CWD."
  (require 'claude-code-ide)
  (let* ((before (buffer-list))
         (default-directory (file-name-as-directory cwd))
         ;; claude-code-ide starts at the project root, but the CLI
         ;; finds a session only from the directory it ran in.
         (project-find-functions (list (lambda (dir) (cons 'transient dir))))
         (claude-code-ide-cli-extra-flags
          (string-trim (concat claude-code-ide-cli-extra-flags
                               " --resume " (shell-quote-argument id)))))
    (claude-code-ide)
    ;; The registry lists the new process only after it starts up.
    (when-let* ((buf (seq-find (lambda (b)
                                 (and (not (memq b before))
                                      (string-prefix-p "*cc " (buffer-name b))))
                               (buffer-list))))
      (with-current-buffer buf
        (setq vp/sidebar--session id)))))

;;; Drawing

(defun vp/sidebar--spacer ()
  "Insert a thin blank line, unless one is just above."
  (unless (or (bobp) (get-text-property (1- (point)) 'vp/sidebar-spacer))
    (insert (propertize "\n" 'face '(:height 0.2) 'vp/sidebar-spacer t))))

(defun vp/sidebar--label (text face)
  "TEXT elided to fit, with one column of padding, in FACE."
  (propertize (concat (truncate-string-to-width text 26 nil nil t) " ")
              'face face))

(defun vp/sidebar--insert-row (row indent)
  "Insert a line for ROW, a buffer or a dormant session, after INDENT."
  (insert
   (propertize
    (concat
     " " indent
     (if (bufferp row)
         (let ((name (buffer-name row)))
           (concat (if (string-prefix-p "*cc " name)
                       (nerd-icons-mdicon "nf-md-robot") ; agents are ghostel too
                     (with-current-buffer row (nerd-icons-icon-for-buffer)))
                   " "
                   (vp/sidebar--label
                    (string-trim (string-remove-prefix "*cc " name) "\\*" "\\*")
                    (and (memq row vp/sidebar--attention) 'vp/sidebar-attention))))
       (concat (nerd-icons-mdicon "nf-md-robot" :face 'shadow) " "
               (vp/sidebar--label (nth 2 row) 'shadow)))
     "\n")
    'vp/sidebar row
    'help-echo (and (consp row) (abbreviate-file-name (nth 1 row))))))

(defun vp/sidebar--insert-group (name rows)
  "Insert group NAME with ROWS (buffers and dormant sessions).
A group of one row gets no name row. Thin gaps go around groups with
a name. A folded group shows its row count and turns red when a
buffer in it waits for user action."
  (let* ((folded (member name vp/sidebar--folded))
         (titled (or folded (cdr rows))))
    (when titled
      (vp/sidebar--spacer)
      (insert (propertize
               (concat " " (propertize (if folded "▸" "▾") 'face 'shadow)
                       " " (propertize name 'face
                                       (if (and folded
                                                (seq-some (lambda (r) (memq r vp/sidebar--attention))
                                                          rows))
                                           'vp/sidebar-attention
                                         'shadow))
                       (when folded
                         (propertize (format "  %d" (length rows)) 'face 'shadow))
                       " \n")
               'vp/sidebar name)))
    (unless folded
      (dolist (row rows)
        ;; Aligns the icon under the group name, past the triangle.
        (vp/sidebar--insert-row row (if titled "  " ""))))
    (when titled (vp/sidebar--spacer))))

(defun vp/sidebar--draw ()
  "Redraw the sidebar from `vp/sidebar--groups' and fit its width."
  (with-current-buffer vp/sidebar-name
    (let ((at (get-text-property (point) 'vp/sidebar))
          (groups (mapcar (lambda (g) (cons (car g) (mapcar #'cdr (cdr g))))
                          vp/sidebar--groups))
          (inhibit-read-only t))
      (when vp/sidebar--dormant
        (let ((claude (assoc vp/sidebar-claude groups)))
          (unless claude
            (setq claude (list vp/sidebar-claude)
                  groups (nconc groups (list claude))))
          (setcdr claude (append (cdr claude) vp/sidebar--dormant))))
      (erase-buffer)
      (pcase-dolist (`(,name . ,rows) groups)
        (vp/sidebar--insert-group name rows))
      (goto-char (or (vp/sidebar--pos at) (point-min))))
    ;; Not `fit-window-to-buffer': it measures only the rows on screen.
    (when-let* ((win (get-buffer-window)))
      (let ((cols (ceiling (car (window-text-pixel-size win nil nil t))
                           (frame-char-width (window-frame win)))))
        ;; IGNORE: preserve-size would block the resize. Then preserve
        ;; the new width, else a frame resize restores the old one.
        (window-resize-no-error win (- cols (window-body-width win)) t t)
        (window-preserve-size win t t)))))

(defun vp/sidebar--pos (value)
  "Position of the line whose `vp/sidebar' property is VALUE, or nil."
  (when value
    (save-excursion
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward 'vp/sidebar value t)))
        (prop-match-beginning match)))))

(defun vp/sidebar--follow (&rest _)
  "Move the highlight to the buffer of the selected window.
A buffer in a folded group highlights the group name."
  (when-let* ((win (get-buffer-window vp/sidebar-name))
              ((not (eq win (selected-window)))))
    (let ((buf (window-buffer)))
      (with-current-buffer vp/sidebar-name
        (if-let* ((pos (or (vp/sidebar--pos buf)
                           (and (vp/sidebar--listed-p buf)
                                (let ((group (vp/sidebar--group buf)))
                                  (and (member group vp/sidebar--folded)
                                       (vp/sidebar--pos group)))))))
            (progn (goto-char pos)
                   (set-window-point win pos)
                   (hl-line-highlight))
          (hl-line-unhighlight))))))

(defun vp/sidebar--update (&optional force)
  "Redraw the sidebar if it is on screen and its rows changed.
FORCE non-nil redraws anyway; `rescan' also rereads Claude sessions."
  (setq vp/sidebar--timer nil)
  (when (get-buffer-window vp/sidebar-name t)
    (let ((groups (vp/sidebar--collect)))
      (when (or (eq force 'rescan)
                ;; A session that starts or ends changes the dormant list.
                (not (equal (assoc vp/sidebar-claude groups)
                            (assoc vp/sidebar-claude vp/sidebar--groups))))
        (vp/sidebar--scan)
        (setq force t))
      (when (or force (not (equal groups vp/sidebar--groups)))
        (setq vp/sidebar--groups groups)
        (vp/sidebar--draw)))
    (vp/sidebar--follow)))

(defun vp/sidebar--schedule (&rest _)
  "Update the sidebar once the current command is done.
The buffer list changes several times per command, and a new buffer
gets its name, mode, and directory after the change."
  (unless (or vp/sidebar--timer (not (get-buffer-window vp/sidebar-name t)))
    (setq vp/sidebar--timer (run-at-time 0 nil #'vp/sidebar--update))))

(add-hook 'buffer-list-update-hook #'vp/sidebar--schedule)
(add-hook 'eshell-directory-change-hook #'vp/sidebar--schedule)
(add-hook 'window-buffer-change-functions #'vp/sidebar--follow)
(add-hook 'window-selection-change-functions #'vp/sidebar--follow)

;;; Attention

(defun vp/sidebar-attention-mark (&rest _)
  "Mark the current buffer as waiting for user action, unless visible.
Fits `ghostel-notification-functions'."
  (unless (or (get-buffer-window nil t)
              (memq (current-buffer) vp/sidebar--attention))
    (push (current-buffer) vp/sidebar--attention)
    (vp/sidebar--update t)))

(defun vp/sidebar--attention-clear (_frame)
  "Drop the marks of buffers that are on screen or dead."
  (when-let* ((gone (seq-filter (lambda (buf)
                                  (or (not (buffer-live-p buf))
                                      (get-buffer-window buf t)))
                                vp/sidebar--attention)))
    (setq vp/sidebar--attention (seq-difference vp/sidebar--attention gone))
    (vp/sidebar--update t)))
(add-hook 'window-buffer-change-functions #'vp/sidebar--attention-clear)

;;; Commands

(defun vp/sidebar--main-window ()
  "Leave the sidebar for the last used main window."
  (when (derived-mode-p 'vp/sidebar-mode)
    (select-window (get-mru-window nil nil t))))

(defun vp/sidebar-next ()
  "Move to the next row."
  (interactive)
  (when-let* ((match (text-property-search-forward 'vp/sidebar nil nil t)))
    (goto-char (prop-match-beginning match))))

(defun vp/sidebar-previous ()
  "Move to the previous row."
  (interactive)
  (text-property-search-backward 'vp/sidebar nil nil t))

(defun vp/sidebar-visit ()
  "Show the buffer at point in the last used main window.
On a dormant session, resume it. On a group name, fold or unfold."
  (interactive)
  (pcase (get-text-property (pos-bol) 'vp/sidebar)
    ((pred stringp) (vp/sidebar-fold))
    ((and (pred bufferp) buf) (vp/sidebar--main-window) (switch-to-buffer buf))
    (`(,id ,cwd . ,_) (vp/sidebar--main-window) (vp/sidebar--resume id cwd))))

(defun vp/sidebar-mouse-visit (event)
  "Visit the row at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (vp/sidebar-visit))

(defun vp/sidebar-fold ()
  "Fold or unfold the group at point in the sidebar.
Elsewhere, fold the group of the current buffer. A group without a
name row stays open."
  (interactive)
  (when-let* ((side (get-buffer vp/sidebar-name))
              (group (if (eq (current-buffer) side)
                         (pcase (get-text-property (pos-bol) 'vp/sidebar)
                           ((and (pred stringp) g) g)
                           ((and (pred bufferp) b) (vp/sidebar--group b))
                           ((pred consp) vp/sidebar-claude))
                       (vp/sidebar--group (current-buffer)))))
    (cond ((member group vp/sidebar--folded)
           (setq vp/sidebar--folded (delete group vp/sidebar--folded)))
          ((with-current-buffer side (vp/sidebar--pos group))
           (push group vp/sidebar--folded)))
    (vp/sidebar--update t)))

(defun vp/sidebar--neighbor (n)
  "The buffer N stops away from the current one, wrapping around.
A folded group is one stop and stays folded: it yields its most
recently used buffer, and the highlight sits on the group name."
  (when-let* ((stops (mapcan (lambda (g)
                               (let ((bufs (mapcar #'cdr (cdr g))))
                                 (if (member (car g) vp/sidebar--folded)
                                     (list bufs)
                                   (mapcar #'list bufs))))
                             (vp/sidebar--collect))))
    (let* ((i (seq-position stops (current-buffer) (lambda (s b) (memq b s))))
           (stop (nth (mod (if i (+ i n) (if (> n 0) 0 -1)) (length stops)) stops)))
      (seq-find (lambda (b) (memq b stop)) (buffer-list)))))

(defun vp/sidebar--cycle (n)
  "Show the buffer N stops away in sidebar order.
With no sidebar on screen, step N buffers in the window's history."
  (if (get-buffer-window vp/sidebar-name)
      (progn (vp/sidebar--main-window)
             (when-let* ((buf (vp/sidebar--neighbor n)))
               (switch-to-buffer buf)))
    (if (> n 0) (next-buffer) (previous-buffer))))

(defun vp/sidebar-next-buffer ()
  "Show the next buffer in sidebar order."
  (interactive)
  (vp/sidebar--cycle 1))

(defun vp/sidebar-previous-buffer ()
  "Show the previous buffer in sidebar order."
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
  "Open Dired at the project root, else at this buffer's directory."
  (interactive)
  (vp/sidebar--main-window)
  (dired (if-let* ((pr (project-current)))
             (project-root pr)
           default-directory)))

(defvar-keymap vp/sidebar-mode-map
  :doc "Navigation only: s-w kills, g rescans Claude sessions."
  "n" #'vp/sidebar-next
  "e" #'vp/sidebar-previous
  "RET" #'vp/sidebar-visit
  "<down-mouse-1>" #'ignore             ; no drag region, no flashing
  "<drag-mouse-1>" #'ignore
  "<mouse-1>" #'vp/sidebar-mouse-visit
  "<mouse-2>" #'vp/sidebar-mouse-visit)

(define-derived-mode vp/sidebar-mode special-mode "Sidebar"
  "Buffers by remote host, org, or project; Claude Code sessions last."
  (setq-local mode-line-format nil
              truncate-lines t
              cursor-type nil           ; the highlight shows the row
              cursor-in-non-selected-windows nil
              ;; The frame border is the gutter. Rows pad one column on
              ;; each side inside the highlight.
              left-fringe-width 0
              right-fringe-width 0
              hl-line-sticky-flag t
              revert-buffer-function (lambda (&rest _)
                                       (vp/sidebar--update 'rescan)))
  (hl-line-mode 1))

(defun vp/sidebar-toggle ()
  "Open and focus the sidebar; focus it if open; close it if focused.
The sidebar is not in the `other-window' cycle."
  (interactive)
  (let ((win (get-buffer-window vp/sidebar-name)))
    (cond ((eq win (selected-window)) (delete-window win))
          (win (select-window win))
          (t (with-current-buffer (get-buffer-create vp/sidebar-name)
               (unless (derived-mode-p 'vp/sidebar-mode)
                 (vp/sidebar-mode)))
             (setq win (display-buffer
                        vp/sidebar-name
                        '(display-buffer-in-side-window
                          (side . left) (slot . 0)
                          (preserve-size . (t . nil))
                          ;; Else other-window displays reuse it.
                          (dedicated . t)
                          (window-parameters (no-delete-other-windows . t)
                                             (no-other-window . t)))))
             (vp/sidebar--update 'rescan)
             (select-window win)))))

(provide 'vp-sidebar)
;;; vp-sidebar.el ends here
