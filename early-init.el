;;; early-init.el --- early init -*- lexical-binding: t; -*-

;; Big GC during startup, sensible defaults after.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defun vp/restore-gc ()
  "Restore normal GC thresholds after startup."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1))
(add-hook 'after-init-hook #'vp/restore-gc)

;; Activate packages from one pre-built autoload file. package.el
;; refreshes it on install/upgrade/delete; M-x package-quickstart-refresh
;; rebuilds it by hand.
(setopt package-quickstart t)

;; Log async native-comp warnings without popping a buffer.
(setopt native-comp-async-report-warnings-errors 'silent)

;; No background native compilation on battery (Emacs 31).
(setopt native-comp-async-on-battery-power nil)

;; LSP/subprocess throughput.
(setq read-process-output-max (* 1024 1024))

;; Bars off before the first frame paints.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Rounded corners, no title bar.
(push '(undecorated-round . t) default-frame-alist)

;; Padding between frame edge and text (GUI frames only).
(push '(internal-border-width . 12) default-frame-alist)
;;; early-init.el ends here
