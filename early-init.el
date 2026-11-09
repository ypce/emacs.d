;;; early-init.el --- early init -*- lexical-binding: t; -*-

;; Startup does 2 GCs instead of 23; pauses stay short.
(setq gc-cons-threshold (* 16 1024 1024))

(setopt package-quickstart t)

(setopt native-comp-async-report-warnings-errors 'silent)

;; LSP replies are often larger than the 64 KB default chunk.
(setq read-process-output-max (* 1024 1024))

;; Before the first frame paints.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(push '(undecorated-round . t) default-frame-alist)

(push '(internal-border-width . 12) default-frame-alist)
;; Cut lines show … in the text instead (init.el).
(push '(right-fringe . 0) default-frame-alist)
;;; early-init.el ends here
