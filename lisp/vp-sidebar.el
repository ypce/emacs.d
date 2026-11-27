;;; vp-sidebar.el --- Buffer sidebar -*- lexical-binding: t; -*-
;;; Commentary:
;; A left side window that lists buffers by remote host, org,
;; project, or else directory. Claude Code sessions go in the group
;; of their directory, named by the terminal title that Claude Code
;; sets. The super keys follow its order (see
;; `vp/sidebar-next-buffer'). Each buffer line carries its buffer in
;; the `vp/sidebar' text property. A group tag carries its key in
;; `vp/sidebar-group' instead, so row motion and hl-line skip it.
;;; Code:

(require 'hl-line)
(require 'project)
(require 'nerd-icons)
(require 'text-property-search)

(defvar ghostel-title)
(defvar system-taskbar-mode)
(declare-function system-taskbar-badge "system-taskbar")
(declare-function magit-status-setup-buffer "magit-status")
(defvar desktop-globals-to-save)

(defconst vp/sidebar-name "*Sidebar*")

(defvar vp/sidebar-width 22
  "Sidebar width in columns. It stays fixed; the display cuts long rows.")

(defface vp/sidebar-attention '((t :foreground "#ff2d55"))
  "Face for a buffer that waits for user action."
  :group 'convenience)

(defface vp/sidebar-tag '((t :weight bold))
  "Face for a group name."
  :group 'convenience)

(defvar vp/sidebar-tag-width 10
  "Columns for the name in a group tag. The display cuts longer names.")

(defface vp/sidebar-dim '((t :foreground "#6E6A86"))
  "Face for the dim parts: dot, parent folder, unsaved mark, idle session."
  :group 'convenience)

(defface vp/sidebar-claude '((t :foreground "#d97757"))
  "Face for the spinner of a working Claude Code session (Claude orange)."
  :group 'convenience)

(defconst vp/sidebar-spinner ["·" "✢" "✳" "✶" "✻" "✽" "✻" "✶" "✳" "✢"]
  "Frames of the working spinner, as Claude Code draws it.")

(defconst vp/sidebar-shimmer ["#f6cbb7" "#eaa283"]
  "Colors of the shimmer peak and its edges on a working session's name.
The rest of the name is `vp/sidebar-claude'.")

(defvar vp/sidebar-git-interval 10
  "Seconds before the git state of a group is checked again.")

(defvar vp/sidebar--attention nil
  "Buffers that wait for user action while not displayed.")

(defvar vp/sidebar--folded nil
  "Keys of the folded groups. The desktop saves them.")

(defvar vp/sidebar--groups nil
  "Groups as last drawn: ((GROUP (STATE . BUFFER)...)...).
STATE is (LABEL MODIFIED WORKING); see `vp/sidebar--row-state'.")

(defvar vp/sidebar--timer nil)

(defvar vp/sidebar--tick-timer nil
  "Timer that turns the spinner while a session works.")

(defvar vp/sidebar--ticks 0
  "Animation steps so far: the spinner frame and the shimmer position.")

(defvar vp/sidebar--git (make-hash-table :test #'equal)
  "Group key -> (STATE . CHECKED-AT). STATE is t (dirty), nil, or `none'.")

(defvar-local vp/sidebar--group-cache nil
  "(DIRECTORY . GROUP); project lookup is too slow per redraw.
Keyed on the directory, so Dired and Eshell regroup when they move.")

(defvar-local vp/sidebar--working nil
  "Non-nil while a Claude session works on a prompt.
Claude Code hooks set it; see `vp/sidebar-set-working'.")

(defvar-local vp/sidebar--activity nil
  "What a working Claude session does now, such as \"Edit init.el\".")

;;; Model

(defun vp/sidebar--listed-p (buf)
  "Non-nil if BUF gets a row.
Internal buffers stay out; terminals (Claude Code too), eshells and
file buffers with a starred name (the *scratch* notes) stay in."
  (let ((name (buffer-name buf)))
    (not (or (string-prefix-p " " name)
             (and (string-prefix-p "*" name)
                  (not (buffer-file-name buf))
                  (not (provided-mode-derived-p
                        (buffer-local-value 'major-mode buf)
                        '(ghostel-mode eshell-mode))))))))

(defun vp/sidebar--session-p (buf)
  "Non-nil if BUF is a Claude Code session."
  (string-prefix-p "*claude-code[" (buffer-name buf)))

(defun vp/sidebar--dir-group (dir)
  "Group key of directory DIR: @host, org, the project root, or DIR.
A path keys the group, so folders that share a name stay apart;
`vp/sidebar--group-label' shows only its last part."
  (cond ((file-remote-p dir) (concat "@" (file-remote-p dir 'host)))
        ((and (bound-and-true-p org-directory)
              (file-in-directory-p dir org-directory))
         "org")
        (t (abbreviate-file-name
            (directory-file-name
             (if-let* ((pr (project-current nil dir))) (project-root pr) dir))))))

(defun vp/sidebar--group-label (group)
  "GROUP as shown: the last part of a path, else GROUP itself."
  (let ((base (file-name-nondirectory group)))
    (if (or (string-empty-p base) (not (file-name-absolute-p group)))
        group
      base)))

(defun vp/sidebar--group-parent (group)
  "Name of the folder above GROUP if GROUP is a path, else nil.
It shows after the group name: B4-4 alone says little."
  (when-let* (((file-name-absolute-p group))
              (dir (file-name-directory group))
              (dir (directory-file-name dir))
              ((not (equal dir group))))   ; / is its own parent
    (vp/sidebar--group-label dir)))

(defun vp/sidebar--group-dir (group)
  "Directory of GROUP if GROUP is a local path, else nil."
  (and (file-name-absolute-p group) (expand-file-name group)))

(defun vp/sidebar--group (buf)
  "Group of BUF: the group of its directory."
  (with-current-buffer buf
    (unless (equal (car vp/sidebar--group-cache) default-directory)
      (setq vp/sidebar--group-cache
            (cons default-directory (vp/sidebar--dir-group default-directory))))
    (cdr vp/sidebar--group-cache)))

(defun vp/sidebar--row-label (buf)
  "Label of BUF: a session's activity or terminal title, else its name.
Claude Code sets the title from the first request; /rename changes it."
  (let ((name (buffer-name buf)))
    (or (and (vp/sidebar--session-p buf)
             (or (buffer-local-value 'vp/sidebar--activity buf)
                 (buffer-local-value 'ghostel-title buf)))
        ;; *claude-code[proj:x]* -> proj:x, *eshell*<2> -> eshell<2>
        (replace-regexp-in-string
         "\\`\\*\\(?:claude-code\\[\\(.*\\)\\]\\|\\(.*?\\)\\)\\*\\(<[0-9]+>\\)?\\'"
         "\\1\\2\\3" name))))

(defun vp/sidebar--working-p (buf)
  "Non-nil if BUF, a Claude session, works on a prompt."
  (buffer-local-value 'vp/sidebar--working buf))

(defun vp/sidebar--row-state (buf)
  "(LABEL MODIFIED WORKING) of BUF: what its row shows.
MODIFIED is non-nil for a file buffer with unsaved changes, WORKING
for a Claude session that works on a prompt."
  (list (vp/sidebar--row-label buf)
        (and (buffer-file-name buf) (buffer-modified-p buf) t)
        (and (vp/sidebar--session-p buf) (vp/sidebar--working-p buf) t)))

(defun vp/sidebar--buffer-path (buf)
  "File of BUF, else its directory, abbreviated."
  (abbreviate-file-name (or (buffer-file-name buf)
                            (buffer-local-value 'default-directory buf))))

(defun vp/sidebar--collect ()
  "Listed buffers as ((GROUP (STATE . BUFFER)...)...).
Groups sort by label, rows by buffer name, so positions stay stable.
A change of STATE makes `vp/sidebar--update' redraw."
  (let (groups)
    (dolist (buf (buffer-list))
      (when (vp/sidebar--listed-p buf)
        (push (cons (vp/sidebar--row-state buf) buf)
              (alist-get (vp/sidebar--group buf) groups nil nil #'equal))))
    (sort (mapcar (lambda (g)
                    (cons (car g) (sort (cdr g) :key (lambda (r) (buffer-name (cdr r))))))
                  groups)
          :key (lambda (g) (list (vp/sidebar--group-label (car g)) (car g)))
          :in-place t)))

;;; Working sessions
;; Claude Code hooks report the state: UserPromptSubmit starts work,
;; PreToolUse names the activity, Stop ends it (see `vp/claude-hook').

(defun vp/sidebar-set-working (buf working &optional activity)
  "Mark BUF, a Claude session, as WORKING with ACTIVITY, or idle.
While it works, ACTIVITY replaces the title in its row."
  (with-current-buffer buf
    (setq vp/sidebar--working working
          vp/sidebar--activity (and working activity)))
  (when (and working (not vp/sidebar--tick-timer))
    (setq vp/sidebar--tick-timer (run-with-timer 0 0.15 #'vp/sidebar--tick)))
  (vp/sidebar--schedule))

(defun vp/sidebar--spinner-frame ()
  "The current frame of `vp/sidebar-spinner'."
  (aref vp/sidebar-spinner (mod vp/sidebar--ticks (length vp/sidebar-spinner))))

(defun vp/sidebar--shimmer-face (i peak)
  "Face of character I of a name whose shimmer peak is at PEAK."
  (pcase (abs (- i peak))
    (0 `(:foreground ,(aref vp/sidebar-shimmer 0)))
    (1 `(:foreground ,(aref vp/sidebar-shimmer 1)))
    (_ 'vp/sidebar-claude)))

(defun vp/sidebar--tick ()
  "Step the animation of working sessions; stop when none works.
Only text properties change, not the rows: the spinner glyph's
`display', and the faces of the name, where a bright peak sweeps
from left to right as in Claude Code's status line."
  (setq vp/sidebar--ticks (1+ vp/sidebar--ticks))
  ;; Nobody sees it: the sidebar is closed or Emacs is in the background.
  (when-let* ((win (get-buffer-window vp/sidebar-name t))
              ((frame-focus-state (window-frame win))))
    (with-current-buffer (window-buffer win)
      (with-silent-modifications
        (let ((pos (point-min)))
          (while (setq pos (text-property-any pos (point-max) 'vp/sidebar-spinner t))
            (put-text-property pos (1+ pos) 'display (vp/sidebar--spinner-frame))
            (setq pos (1+ pos))))
        (let ((pos (point-min)))
          (while (setq pos (text-property-any pos (point-max) 'vp/sidebar-shimmer t))
            (let* ((end (next-single-property-change pos 'vp/sidebar-shimmer nil (point-max)))
                   (n (- end pos))
                   ;; The peak runs a few columns past each end.
                   (peak (- (mod vp/sidebar--ticks (+ n 6)) 3)))
              (dotimes (i n)
                (put-text-property (+ pos i) (+ pos i 1) 'face
                                   (vp/sidebar--shimmer-face i peak)))
              (setq pos end)))))))
  (unless (seq-some #'vp/sidebar--working-p (buffer-list))
    (cancel-timer vp/sidebar--tick-timer)
    (setq vp/sidebar--tick-timer nil)))

;;; Git state
;; `git status' runs async per group, at most once per
;; `vp/sidebar-git-interval'; a save or a Magit refresh forces it.

(defun vp/sidebar--git-dirty-p (group)
  "Non-nil if GROUP has uncommitted changes, as last checked."
  (eq (car (gethash group vp/sidebar--git)) t))

(defun vp/sidebar--git-check (groups)
  "Start `git status' for each group in GROUPS whose state is stale."
  (dolist (g groups)
    (let* ((group (car g))
           (entry (gethash group vp/sidebar--git))
           (dir (vp/sidebar--group-dir group)))
      (when (and dir (file-directory-p dir)
                 ;; Not a repo: check again only when forced stale.
                 (not (and (eq (car entry) 'none) (> (cdr entry) 0)))
                 (> (- (float-time) (or (cdr entry) 0)) vp/sidebar-git-interval))
        (puthash group (cons (car entry) (float-time)) vp/sidebar--git)
        (vp/sidebar--git-run group dir (car entry))))))

(defun vp/sidebar--git-run (group dir old)
  "Run `git status' in DIR for GROUP; redraw if it differs from OLD."
  (let ((default-directory dir)
        (dirty nil))
    (make-process
     :name "vp-sidebar-git"
     :command '("git" "status" "--porcelain")
     :noquery t
     :filter (lambda (_proc _out) (setq dirty t))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((state (cond ((/= (process-exit-status proc) 0) 'none)
                            (dirty t))))
           (puthash group (cons state (float-time)) vp/sidebar--git)
           (unless (eq state old)
             (vp/sidebar--update t))))))))

(defun vp/sidebar--git-stale (&rest _)
  "Make the next update check git again: all groups, or the current one.
Fits `after-save-hook' and `magit-post-refresh-hook'."
  (if (buffer-file-name)
      (when-let* ((entry (gethash (vp/sidebar--group (current-buffer)) vp/sidebar--git)))
        (setcdr entry 0))
    (maphash (lambda (_ entry) (setcdr entry 0)) vp/sidebar--git))
  (vp/sidebar--schedule))

(add-hook 'after-save-hook #'vp/sidebar--git-stale)
(add-hook 'magit-post-refresh-hook #'vp/sidebar--git-stale)

;;; Drawing

(defun vp/sidebar--spacer ()
  "Insert a thin blank line, unless one is just above."
  (unless (or (bobp) (get-text-property (1- (point)) 'vp/sidebar-spacer))
    (insert (propertize "\n" 'face '(:height 0.2) 'vp/sidebar-spacer t))))

(defun vp/sidebar--insert-row (row indent)
  "Insert a line for ROW, a (STATE . BUFFER) pair, after INDENT.
A dim ● follows the label of a buffer with unsaved changes. A working
session shows the Claude spinner instead of its icon and a shimmer on
its name (see `vp/sidebar--tick'); an idle one, a dim robot."
  (pcase-let ((`((,label ,modified ,working) . ,buf) row))
    (insert
     (propertize
      (concat
       " " indent
       (cond (working (propertize "·" 'face 'vp/sidebar-claude
                                  'display (vp/sidebar--spinner-frame)
                                  'vp/sidebar-spinner t))
             ((vp/sidebar--session-p buf) ; sessions are ghostel too
              (nerd-icons-mdicon "nf-md-robot" :face 'vp/sidebar-dim))
             (t (with-current-buffer buf (nerd-icons-icon-for-buffer))))
       " "
       (cond (working (propertize label 'face 'vp/sidebar-claude
                                  'vp/sidebar-shimmer t))
             ((memq buf vp/sidebar--attention)
              (propertize label 'face 'vp/sidebar-attention))
             (t label))
       (when modified (propertize " ●" 'face 'vp/sidebar-dim))
       " \n")
      'vp/sidebar buf
      ;; A new list per row: equal values on adjacent rows would
      ;; highlight the whole group as one span.
      'mouse-face (list 'highlight)
      'help-echo (vp/sidebar--buffer-path buf)))))

(defun vp/sidebar--insert-group (name rows)
  "Insert group NAME (a key) as a bold tag, then ROWS (STATE . BUFFER).
Thin gaps go around groups. A dot leads the tag: ○ open, ● folded.
A * after the name means uncommitted git changes. A folded group
hides its rows and shows their count after the tag. The parent
folder comes last, so a narrow sidebar cuts only that; hovering
shows the full key."
  (let* ((folded (member name vp/sidebar--folded))
         (line (concat
                (propertize (if folded "●" "○") 'face 'vp/sidebar-dim)
                " " (propertize (concat (truncate-string-to-width
                                         (vp/sidebar--group-label name)
                                         vp/sidebar-tag-width nil nil "…")
                                        (when (vp/sidebar--git-dirty-p name) "*")
                                        (when folded (format " %d" (length rows))))
                                'face 'vp/sidebar-tag)
                (when-let* ((parent (vp/sidebar--group-parent name)))
                  (propertize (concat " " parent) 'face 'vp/sidebar-dim))
                " \n")))
    (vp/sidebar--spacer)
    (insert (propertize line 'vp/sidebar-group name 'help-echo name))
    (unless folded
      (dolist (row rows)
        ;; The icon goes under the tag.
        (vp/sidebar--insert-row row " ")))
    (vp/sidebar--spacer)))

(defun vp/sidebar--draw ()
  "Redraw the sidebar from `vp/sidebar--groups'."
  (with-current-buffer vp/sidebar-name
    (let ((at (get-text-property (point) 'vp/sidebar))
          (inhibit-read-only t))
      (erase-buffer)
      (pcase-dolist (`(,name . ,rows) vp/sidebar--groups)
        (vp/sidebar--insert-group name rows))
      (goto-char (or (vp/sidebar--pos at) (point-min)))
      ;; Point stays on rows: hl-line would mark a tag or a gap.
      (unless (get-text-property (point) 'vp/sidebar)
        (vp/sidebar-next)))))

(defun vp/sidebar--pos (value)
  "Position of the line whose `vp/sidebar' property is VALUE, or nil."
  (when value
    (save-excursion
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward 'vp/sidebar value t)))
        (prop-match-beginning match)))))

(defun vp/sidebar--follow (&rest _)
  "Move the highlight to the buffer of the selected window."
  (when-let* ((win (get-buffer-window vp/sidebar-name))
              ((not (eq win (selected-window))))
              ;; Else every minibuffer entry drops the highlight.
              ((not (minibufferp (window-buffer)))))
    (let ((buf (window-buffer)))
      (with-current-buffer vp/sidebar-name
        (if-let* ((pos (vp/sidebar--pos buf)))
            (progn (goto-char pos)
                   (set-window-point win pos)
                   (hl-line-highlight))
          (hl-line-unhighlight))))))

(defun vp/sidebar--update (&optional force)
  "Redraw the sidebar if it is on screen and its rows changed.
FORCE non-nil redraws anyway."
  (setq vp/sidebar--timer nil)
  (when (get-buffer-window vp/sidebar-name t)
    (let ((groups (vp/sidebar--collect)))
      (vp/sidebar--git-check groups)
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

(defun vp/sidebar-retitle (_title)
  "Update the sidebar for a new terminal title or directory; return nil.
Fits `ghostel-buffer-name-function', which ghostel calls on both;
nil keeps the buffer name, which claude-code-ide looks sessions up by."
  (ignore (vp/sidebar--schedule)))

(add-hook 'buffer-list-update-hook #'vp/sidebar--schedule)
;; claude-code-ide clears `ghostel-buffer-name-function' in its
;; buffers, so `vp/sidebar-retitle' misses session titles.
(add-variable-watcher 'ghostel-title #'vp/sidebar--schedule)
(add-hook 'eshell-directory-change-hook #'vp/sidebar--schedule)
;; The unsaved mark: the first change, a revert; a save is in
;; `vp/sidebar--git-stale'.
(add-hook 'first-change-hook #'vp/sidebar--schedule)
(add-hook 'after-revert-hook #'vp/sidebar--schedule)
(add-hook 'window-buffer-change-functions #'vp/sidebar--follow)
(add-hook 'window-selection-change-functions #'vp/sidebar--follow)

;;; Attention

(defun vp/sidebar--badge ()
  "Show the number of waiting buffers on the Dock icon."
  (when (bound-and-true-p system-taskbar-mode)
    (system-taskbar-badge (and vp/sidebar--attention
                               (length vp/sidebar--attention)))))

(defun vp/sidebar-attention-mark (&rest _)
  "Mark the current buffer as waiting for user action, unless visible.
Fits `ghostel-notification-functions'."
  (unless (or (get-buffer-window nil t)
              (memq (current-buffer) vp/sidebar--attention))
    (push (current-buffer) vp/sidebar--attention)
    (vp/sidebar--badge)
    (vp/sidebar--update t)))

(defun vp/sidebar--attention-clear (_frame)
  "Drop the marks of buffers that are on screen or dead."
  (let ((kept (seq-remove (lambda (buf)
                            (or (not (buffer-live-p buf))
                                (get-buffer-window buf t)))
                          vp/sidebar--attention)))
    (unless (length= kept (length vp/sidebar--attention))
      (setq vp/sidebar--attention kept)
      (vp/sidebar--badge)
      (vp/sidebar--update t))))
(add-hook 'window-buffer-change-functions #'vp/sidebar--attention-clear)

(defun vp/sidebar--attention-forget ()
  "Drop the mark of the buffer being killed. Fits `kill-buffer-hook'."
  (when (memq (current-buffer) vp/sidebar--attention)
    (setq vp/sidebar--attention (delq (current-buffer) vp/sidebar--attention))
    (vp/sidebar--badge)
    (vp/sidebar--schedule)))
(add-hook 'kill-buffer-hook #'vp/sidebar--attention-forget)

;;; Commands

(defun vp/sidebar--main-window ()
  "Leave the sidebar window for the last used main window."
  (when (window-parameter nil 'window-side)
    (select-window (get-mru-window nil nil t))))

(defun vp/sidebar--show-buffer (buf)
  "Show BUF in the last used main window."
  (vp/sidebar--main-window)
  (switch-to-buffer buf))

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
  "Show the buffer at point in the last used main window."
  (interactive)
  (when-let* ((buf (get-text-property (pos-bol) 'vp/sidebar)))
    (vp/sidebar--show-buffer buf)))

(defun vp/sidebar--at-event (event)
  "(BUFFER . GROUP) under EVENT: a row's buffer and its group, or nil
and the group of a tag."
  (let* ((posn (event-start event))
         (pos (posn-point posn)))
    ;; The sidebar need not be the current buffer.
    (with-current-buffer (window-buffer (posn-window posn))
      (when pos
        (if-let* ((buf (get-text-property pos 'vp/sidebar)))
            (cons buf (vp/sidebar--group buf))
          (cons nil (get-text-property pos 'vp/sidebar-group)))))))

(defun vp/sidebar-mouse-visit (event)
  "Visit the row at EVENT, or fold the group whose tag is at EVENT.
Point does not move to a tag or a gap."
  (interactive "e")
  (pcase (vp/sidebar--at-event event)
    (`(,(and (pred bufferp) buf) . ,_)
     (mouse-set-point event)
     (vp/sidebar--show-buffer buf))
    (`(nil . ,(and (pred stringp) group))
     (vp/sidebar--toggle group))))

(defun vp/sidebar-mouse-kill (event)
  "Kill the buffer of the row at EVENT."
  (interactive "e")
  (when-let* ((buf (car (vp/sidebar--at-event event))))
    (kill-buffer buf)))

(defun vp/sidebar--kill-group (group)
  "Kill the listed buffers of GROUP."
  (dolist (row (alist-get group (vp/sidebar--collect) nil nil #'equal))
    (kill-buffer (cdr row))))

(defun vp/sidebar--finder (path)
  "Show PATH in Finder: a file selected in its folder, a folder opened."
  (if (file-directory-p path)
      (call-process "open" nil 0 nil path)
    (call-process "open" nil 0 nil "-R" path)))

(defun vp/sidebar-mouse-menu (event)
  "Pop up the actions for the row or tag at EVENT.
Only actions that clicks do not cover: visit, kill and fold have
their own mouse buttons."
  (interactive "e")
  (pcase-let* ((`(,buf . ,group) (vp/sidebar--at-event event))
               (dir (and group (vp/sidebar--group-dir group))))
    (when group
      (popup-menu
       `("Sidebar"
         ,@(when buf
             (let ((path (expand-file-name (vp/sidebar--buffer-path buf))))
               `(["Copy path" (kill-new ,(abbreviate-file-name path))]
                 ["Show in Finder" (vp/sidebar--finder ,path)
                  :active ,(not (file-remote-p path))]
                 "---")))
         ["Kill group" (vp/sidebar--kill-group ,group)]
         ["Open group in Finder" (vp/sidebar--finder ,dir) :active ,(and dir t)]
         ["Dired" (progn (vp/sidebar--main-window) (dired ,dir)) :active ,(and dir t)]
         ["Magit status"
          (progn (vp/sidebar--main-window) (magit-status-setup-buffer ,dir))
          :active ,(and dir (fboundp 'magit-status-setup-buffer) t)])
       event))))

(defun vp/sidebar-wheel (event)
  "Scroll the sidebar with the wheel EVENT, only when its rows do not fit."
  (interactive "e")
  (let ((win (posn-window (event-start event))))
    (with-current-buffer (window-buffer win)
      (unless (and (= (window-start win) (point-min))
                   (pos-visible-in-window-p (point-max) win))
        (mwheel-scroll event)))))

(defun vp/sidebar--toggle (group)
  "Fold GROUP if it is open, else open it."
  (setq vp/sidebar--folded (if (member group vp/sidebar--folded)
                               (delete group vp/sidebar--folded)
                             (cons group vp/sidebar--folded)))
  (vp/sidebar--update t))

(defun vp/sidebar-fold ()
  "Fold or open the group of the row at point in the sidebar.
Elsewhere, the group of the current buffer."
  (interactive)
  (when-let* ((buf (if (derived-mode-p 'vp/sidebar-mode)
                       (get-text-property (pos-bol) 'vp/sidebar)
                     (current-buffer)))
              ((vp/sidebar--listed-p buf)))
    (vp/sidebar--toggle (vp/sidebar--group buf))))

(defun vp/sidebar--neighbor (n)
  "The buffer N stops away from the current one, wrapping around.
A folded group is one stop and stays folded: it yields its most
recently used buffer."
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

(defvar-keymap vp/sidebar-mode-map
  :doc "n / e step, RET visits, g redraws. Mouse: 1 visits a row or
folds a group, 2 kills, 3 opens the menu."
  "n" #'vp/sidebar-next
  "e" #'vp/sidebar-previous
  ;; Arrows and C-n / C-p also step by row, past the spacer lines.
  "<remap> <next-line>" #'vp/sidebar-next
  "<remap> <previous-line>" #'vp/sidebar-previous
  "RET" #'vp/sidebar-visit
  "<down-mouse-1>" #'ignore             ; no drag region, no flashing
  "<drag-mouse-1>" #'ignore
  "<mouse-1>" #'vp/sidebar-mouse-visit
  "<down-mouse-2>" #'ignore
  "<mouse-2>" #'vp/sidebar-mouse-kill
  "<down-mouse-3>" #'ignore
  "<mouse-3>" #'vp/sidebar-mouse-menu
  "<wheel-up>" #'vp/sidebar-wheel
  "<wheel-down>" #'vp/sidebar-wheel
  "<wheel-left>" #'ignore
  "<wheel-right>" #'ignore)

(define-derived-mode vp/sidebar-mode special-mode "Sidebar"
  "Buffers by remote host, org, project, or directory."
  (setq-local mode-line-format nil
              truncate-lines t
              cursor-type nil           ; the highlight shows the row
              cursor-in-non-selected-windows nil
              ;; The frame border is the gutter. Rows pad one column on
              ;; each side inside the highlight.
              left-fringe-width 0
              right-fringe-width 0
              hl-line-sticky-flag t
              ;; Else its map shadows the wheel keys above.
              pixel-scroll-precision-mode nil
              revert-buffer-function (lambda (&rest _) (vp/sidebar--update t)))
  (hl-line-mode 1))

(defun vp/sidebar--show ()
  "Show the sidebar without selecting it; return its window.
It is not in the `other-window' cycle."
  (with-current-buffer (get-buffer-create vp/sidebar-name)
    (unless (derived-mode-p 'vp/sidebar-mode)
      (vp/sidebar-mode)))
  (prog1 (display-buffer
          vp/sidebar-name
          `(display-buffer-in-side-window
            (side . left) (slot . 0)
            (window-width . ,vp/sidebar-width)
            ;; Else a frame resize changes the width.
            (preserve-size . (t . nil))
            ;; Else other-window displays reuse it.
            (dedicated . t)
            (window-parameters (no-delete-other-windows . t)
                               (no-other-window . t))))
    (vp/sidebar--update t)))

(defun vp/sidebar-toggle ()
  "Open and focus the sidebar; focus it if open; close it if focused."
  (interactive)
  (let ((win (get-buffer-window vp/sidebar-name)))
    (cond ((eq win (selected-window)) (delete-window win))
          (win (select-window win))
          (t (select-window (vp/sidebar--show))))))

;;; Desktop
;; A flag, not the buffer: the frameset restore drops the window
;; parameters and preserved width that `vp/sidebar--show' sets.

(defvar vp/sidebar-open nil
  "Non-nil if the sidebar was on screen at the last desktop save.")

(defun vp/sidebar-desktop-save ()
  "Record whether the sidebar is on screen. Fits `desktop-save-hook'."
  (setq vp/sidebar-open (and (get-buffer-window vp/sidebar-name t) t)))

(defun vp/sidebar-desktop-restore ()
  "Show the sidebar again if it was on screen at the last save.
Fits `desktop-after-read-hook'."
  (when vp/sidebar-open
    (vp/sidebar--show)))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'vp/sidebar-open)
  (add-to-list 'desktop-globals-to-save 'vp/sidebar--folded)
  (add-hook 'desktop-save-hook #'vp/sidebar-desktop-save))

(provide 'vp-sidebar)
;;; vp-sidebar.el ends here
