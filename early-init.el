;;; early-init.el --- early init -*- lexical-binding: t; -*-

;; 16 MB between collections, from the first form on: startup does 2
;; GCs instead of 23, and later pauses stay short.
(setq gc-cons-threshold (* 16 1024 1024))

;; Activate packages from one pre-built autoload file. package.el
;; refreshes it on install/upgrade/delete; M-x package-quickstart-refresh
;; rebuilds it by hand.
(setopt package-quickstart t)

;; Log async native-comp warnings without popping a buffer.
(setopt native-comp-async-report-warnings-errors 'silent)

;; No background native compilation on battery (Emacs 31).
(setopt native-comp-async-on-battery-power nil)

;; LSP/subprocess throughput: read big chunks, deliver them at once.
(setq read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil)

;; Bars off before the first frame paints.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Rounded corners, no title bar.
(push '(undecorated-round . t) default-frame-alist)

;; Padding between frame edge and text (GUI frames only).
(push '(internal-border-width . 12) default-frame-alist)
;; No right fringe: the border alone pads the right edge, as at the
;; top. Wrapped and cut lines show ↩ / … in the text (see init.el).
(push '(right-fringe . 0) default-frame-alist)
;;; early-init.el ends here
