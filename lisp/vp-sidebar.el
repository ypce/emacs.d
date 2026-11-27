;;; vp-sidebar.el --- Buffer sidebar -*- lexical-binding: t; -*-
;;; Commentary:
;; A left side window that lists buffers by remote host, org,
;; project, or else directory. Claude Code sessions go in the group
;; of their directory, named by the terminal title that Claude Code
;; sets. The super keys follow its order (see
;; `vp/sidebar-next-buffer'). Each line carries one `vp/sidebar' text
;; property: a buffer or a group key.
;;; Code:

(require 'hl-line)
(require 'project)
(require 'nerd-icons)
(require 'text-property-search)

(defvar ghostel-title)
(defvar system-taskbar-mode)
(declare-function system-taskbar-badge "system-taskbar")
(defvar desktop-globals-to-save)

(defconst vp/sidebar-name "*Sidebar*")

(defvar vp/sidebar-width 22
  "Sidebar width in columns. It stays fixed; the display cuts long rows.")

(defface vp/sidebar-attention '((t :foreground "#ff2d55"))
  "Face for a buffer that waits for user action."
  :group 'convenience)

(defface vp/sidebar-group-parent '((t :inherit shadow :height 0.85))
  "Face for the parent folder after a group name."
  :group 'convenience)

(defface vp/sidebar-claude '((t :foreground "#d97757"))
  "Face for the icon of a live Claude Code session (Claude orange)."
  :group 'convenience)

(defvar vp/sidebar--attention nil
  "Buffers that wait for user action while not displayed.")

(defvar vp/sidebar--folded nil
  "Names of the folded groups.")

(defvar vp/sidebar--groups nil
  "Groups as last drawn: ((GROUP (LABEL . BUFFER)...)...).")

(defvar vp/sidebar--timer nil)

(defvar-local vp/sidebar--group-cache nil
  "(DIRECTORY . GROUP); project lookup is too slow per redraw.
Keyed on the directory, so Dired and Eshell regroup when they move.")

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

(defun vp/sidebar--group (buf)
  "Group of BUF: the group of its directory."
  (with-current-buffer buf
    (unless (equal (car vp/sidebar--group-cache) default-directory)
      (setq vp/sidebar--group-cache
            (cons default-directory (vp/sidebar--dir-group default-directory))))
    (cdr vp/sidebar--group-cache)))

(defun vp/sidebar--row-label (buf)
  "Label of BUF: a session's terminal title, else its name.
Claude Code sets the title from the first request."
  (let ((name (buffer-name buf)))
    (or (and (vp/sidebar--session-p buf)
             (buffer-local-value 'ghostel-title buf))
        ;; *claude-code[proj:x]* -> proj:x, *eshell*<2> -> eshell<2>
        (replace-regexp-in-string
         "\\`\\*\\(?:claude-code\\[\\(.*\\)\\]\\|\\(.*?\\)\\)\\*\\(<[0-9]+>\\)?\\'"
         "\\1\\2\\3" name))))

(defun vp/sidebar--collect ()
  "Listed buffers as ((GROUP (LABEL . BUFFER)...)...).
Groups sort by label, rows by buffer name, so positions stay stable.
A title change changes the label, so `vp/sidebar--update' redraws."
  (let (groups)
    (dolist (buf (buffer-list))
      (when (vp/sidebar--listed-p buf)
        (push (cons (vp/sidebar--row-label buf) buf)
              (alist-get (vp/sidebar--group buf) groups nil nil #'equal))))
    (sort (mapcar (lambda (g)
                    (cons (car g) (sort (cdr g) :key (lambda (r) (buffer-name (cdr r))))))
                  groups)
          :key (lambda (g) (list (vp/sidebar--group-label (car g)) (car g)))
          :in-place t)))

;;; Drawing

(defun vp/sidebar--spacer ()
  "Insert a thin blank line, unless one is just above."
  (unless (or (bobp) (get-text-property (1- (point)) 'vp/sidebar-spacer))
    (insert (propertize "\n" 'face '(:height 0.2) 'vp/sidebar-spacer t))))

(defun vp/sidebar--label (text face)
  "TEXT with one column of padding, in FACE."
  (propertize (concat text " ") 'face face))

(defun vp/sidebar--insert-row (row indent)
  "Insert a line for ROW, a (LABEL . BUFFER) pair, after INDENT."
  (let ((buf (cdr row)))
    (insert
     (propertize
      (concat
       " " indent
       (if (vp/sidebar--session-p buf) ; sessions are ghostel too
           (nerd-icons-mdicon "nf-md-robot" :face 'vp/sidebar-claude)
         (with-current-buffer buf (nerd-icons-icon-for-buffer)))
       " "
       (vp/sidebar--label (car row)
                          (and (memq buf vp/sidebar--attention) 'vp/sidebar-attention))
       "\n")
      'vp/sidebar buf
      'help-echo (buffer-name buf)))))

(defun vp/sidebar--insert-group (name rows)
  "Insert group NAME, a key, with ROWS, (LABEL . BUFFER) pairs.
Thin gaps go around groups. A folded group shows its buffer count
and turns red when a buffer in it waits for user action. The parent
folder comes last, so a narrow sidebar cuts only that; hovering
shows the full key."
  (let ((folded (member name vp/sidebar--folded))
        (bufs (mapcar #'cdr rows)))
    (vp/sidebar--spacer)
    (insert (propertize
               (concat " " (propertize (if folded "▸" "▾") 'face 'shadow)
                       " " (propertize
                            (vp/sidebar--group-label name) 'face
                            (if (and folded
                                     (seq-some (lambda (r) (memq r vp/sidebar--attention))
                                               bufs))
                                'vp/sidebar-attention
                              'shadow))
                       (when folded
                         (propertize (format "  %d" (length bufs)) 'face 'shadow))
                       (when-let* ((parent (vp/sidebar--group-parent name)))
                         (propertize (concat "  " parent) 'face 'vp/sidebar-group-parent))
                       " \n")
               'vp/sidebar name 'help-echo name))
    (unless folded
      (dolist (row rows)
        ;; Aligns the icon under the group name, past the triangle.
        (vp/sidebar--insert-row row "  ")))
    (vp/sidebar--spacer)))

(defun vp/sidebar--draw ()
  "Redraw the sidebar from `vp/sidebar--groups'."
  (with-current-buffer vp/sidebar-name
    (let ((at (get-text-property (point) 'vp/sidebar))
          (inhibit-read-only t))
      (erase-buffer)
      (pcase-dolist (`(,name . ,rows) vp/sidebar--groups)
        (vp/sidebar--insert-group name rows))
      (goto-char (or (vp/sidebar--pos at) (point-min))))))

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
              ((not (eq win (selected-window))))
              ;; Else every minibuffer entry drops the highlight.
              ((not (minibufferp (window-buffer)))))
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
FORCE non-nil redraws anyway."
  (setq vp/sidebar--timer nil)
  (when (get-buffer-window vp/sidebar-name t)
    (let ((groups (vp/sidebar--collect)))
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
On a group name, fold or unfold."
  (interactive)
  (pcase (get-text-property (pos-bol) 'vp/sidebar)
    ((pred stringp) (vp/sidebar-fold))
    ((and (pred bufferp) buf) (vp/sidebar--main-window) (switch-to-buffer buf))))

(defun vp/sidebar-mouse-visit (event)
  "Visit the row at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (vp/sidebar-visit))

(defun vp/sidebar-fold ()
  "Fold or unfold the group at point in the sidebar.
Elsewhere, fold the group of the current buffer."
  (interactive)
  (when-let* ((side (get-buffer vp/sidebar-name))
              (group (if (eq (current-buffer) side)
                         (pcase (get-text-property (pos-bol) 'vp/sidebar)
                           ((and (pred stringp) g) g)
                           ((and (pred bufferp) b) (vp/sidebar--group b)))
                       (vp/sidebar--group (current-buffer)))))
    (setq vp/sidebar--folded (if (member group vp/sidebar--folded)
                                 (delete group vp/sidebar--folded)
                               (cons group vp/sidebar--folded)))
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

(defvar-keymap vp/sidebar-mode-map
  :doc "Navigation only: n / e step, RET visits, g redraws."
  "n" #'vp/sidebar-next
  "e" #'vp/sidebar-previous
  ;; Arrows and C-n / C-p also step by row, past the spacer lines.
  "<remap> <next-line>" #'vp/sidebar-next
  "<remap> <previous-line>" #'vp/sidebar-previous
  "RET" #'vp/sidebar-visit
  "<down-mouse-1>" #'ignore             ; no drag region, no flashing
  "<drag-mouse-1>" #'ignore
  "<mouse-1>" #'vp/sidebar-mouse-visit
  "<mouse-2>" #'vp/sidebar-mouse-visit)

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
  (add-hook 'desktop-save-hook #'vp/sidebar-desktop-save))

(provide 'vp-sidebar)
;;; vp-sidebar.el ends here
