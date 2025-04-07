;;; pre-init.el --- 3 -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Startup Optimizations
;; Temporarily set very high GC threshold to avoid garbage collection during startup
(defvar default-gc-cons-threshold gc-cons-threshold
  "Default value of `gc-cons-threshold'.")

;; Set GC threshold to 100MB for startup
(setq gc-cons-threshold (* 100 1024 1024))

;;;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 4 1024 1024))  ; 4MB for even better performance

;;;; Save existing clipboard text into the kill ring before replacing it
(setq save-interprogram-paste-before-kill t)

;;;; Disable Tooltips Completely (faster and cleaner UI)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;;;; File name handler optimization for faster startup
;; Save the original value
(defvar default-file-name-handler-alist file-name-handler-alist
  "Default value of `file-name-handler-alist'.")

;; Set to nil for faster startup
(setq file-name-handler-alist nil)

;;;; Restore defaults after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore file-name-handler-alist
            (setq file-name-handler-alist default-file-name-handler-alist)
            ;; GC will be managed by gcmh after startup
            ))

;;;; Dynamic garbage collection for better performance
(use-package gcmh
  :ensure t
  :defer 2  ; Defer loading by 2 seconds for faster startup
  :init
  ;; Setup but don't enable until after startup
  (setq gcmh-idle-delay 5  ; Trigger GC after 5 seconds idle (more responsive)
        gcmh-auto-idle-delay-factor 5  ; Less aggressive factor
        gcmh-high-cons-threshold (* 32 1024 1024)  ; 32MB when idle
        gcmh-low-cons-threshold (* 16 1024 1024))  ; 16MB when in use
  :config
  ;; Only collect garbage when truly idle, not when scrolling, etc.
  (add-function :before after-focus-change-function
                (lambda () (unless (frame-focus-state) (gcmh-idle-garbage-collect))))
  :hook
  (emacs-startup . gcmh-mode))  ; Enable after startup
