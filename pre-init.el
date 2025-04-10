;;; pre-init.el --- 3 -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Disable package.el to avoid conflicts with Elpaca
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;;; Elpaca Setup
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Optional: Install use-package support
;; If you enable elpaca-use-package, some use-package definitions, such as
;; Vertico's, may need modifications. See the following discussion for details:
;; https://github.com/jamescherti/minimal-emacs.d/issues/54
;;
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

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


