;;; early-init.el --- early init -*- lexical-binding: t; -*-

;; Big GC during startup, sensible defaults after
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defun vp/restore-gc ()
  "Restore normal GC thresholds after startup."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1))
(add-hook 'after-init-hook #'vp/restore-gc)

;; Async native-comp warnings ("…not known to be defined") are expected
;; noise from macro-expansion order; log them without popping a buffer.
(setq native-comp-async-report-warnings-errors 'silent)

;; Helpful for LSP/subprocess throughput (applies globally)
(setq read-process-output-max (* 1024 1024))

;; Adaptive buffering batches small process outputs for throughput at the
;; cost of latency — the wrong trade for terminal echo (eat/claude-code).
(setq process-adaptive-read-buffering nil)

;; Rounded corners
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; breathing room between the frame edge and text (GUI frames only;
;; terminal frames get padding from ghostty/wezterm's own config)
(add-to-list 'default-frame-alist '(internal-border-width . 12))
;;; early-init.el ends here
