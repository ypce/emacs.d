;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;;; Performance & Native Compilation
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024 8) ; 8MB
      package-enable-at-startup nil
      frame-inhibit-implied-resize t
      native-comp-async-report-warnings-errors 'silent
      native-comp-deferred-compilation t
      native-comp-jit-compilation t)

;;; Directory Organization
(defvar emacs-var-dir (expand-file-name "var/" user-emacs-directory)
  "Directory for variable Emacs data.")

(unless (file-exists-p emacs-var-dir)
  (make-directory emacs-var-dir t))

;; Package directories
(setq package-user-dir (expand-file-name "elpa/" emacs-var-dir))

;; Elpaca directories
(defvar elpaca-directory (expand-file-name "elpaca/" emacs-var-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

;; Native compilation cache
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (defvar native-comp-eln-cache-dir (expand-file-name "eln-cache/" emacs-var-dir))
  (unless (file-exists-p native-comp-eln-cache-dir)
    (make-directory native-comp-eln-cache-dir t))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache native-comp-eln-cache-dir)))

;;; macOS Configuration
(when (eq system-type 'darwin)
  ;; Command key mappings
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none
        mac-control-modifier 'control
        mac-function-modifier 'hyper
        mac-pass-command-to-system nil
        mac-pass-control-to-system nil)
  
  ;; 1. PATH Setup - Add Homebrew paths early
  (let ((homebrew-paths '("/opt/homebrew/bin" "/opt/homebrew/sbin" "/usr/local/bin")))
    (dolist (path homebrew-paths)
      (when (and (file-directory-p path)
                 (not (member path exec-path)))
        (push path exec-path)
        (setenv "PATH" (concat path ":" (getenv "PATH"))))))
  
  ;; 2. LIBRARY_PATH Setup for GCC (native compilation)
  (defun setup-gcc-library-paths ()
    "Set up LIBRARY_PATH for native compilation."
    (let ((paths (split-string (or (getenv "LIBRARY_PATH") "") ":" t)))
      
      ;; Add CommandLineTools SDK
      (let ((clt-path "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
        (when (file-directory-p clt-path)
          (push clt-path paths)))
      
      ;; Add GCC paths if available
      (when (executable-find "brew")
        (ignore-errors
          (let ((gcc-prefix (string-trim (shell-command-to-string "brew --prefix gcc 2>/dev/null"))))
            (when (file-directory-p (concat gcc-prefix "/lib/gcc/current"))
              (push (concat gcc-prefix "/lib/gcc/current") paths))))
        (ignore-errors
          (let ((jit-prefix (string-trim (shell-command-to-string "brew --prefix libgccjit 2>/dev/null"))))
            (when (file-directory-p (concat jit-prefix "/lib/gcc/current"))
              (push (concat jit-prefix "/lib/gcc/current") paths)))))
      
      ;; Set the final LIBRARY_PATH
      (when paths
        (setenv "LIBRARY_PATH" (string-join (delete-dups paths) ":")))))
  
  (setup-gcc-library-paths)
  (message "LIBRARY_PATH set to: %s" (getenv "LIBRARY_PATH")))

;;; Frame Configuration
(setq-default
 default-frame-alist '((fullscreen . maximized)
                       (tool-bar-lines . 0)
                       (menu-bar-lines . 1) ; Keep menu bar on macOS
                       (vertical-scroll-bars . nil)
                       (horizontal-scroll-bars . nil)
                       (alpha-background . 100)
                       (ns-appearance . dark)
                       (ns-transparent-titlebar . t))
 initial-frame-alist default-frame-alist)

;; Disable unnecessary UI early
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))

;;; File Handler Optimization
(defvar original-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore after init
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist original-file-name-handler-alist
                  gc-cons-threshold (* 16 1024 1024) ; 16MB
                  gc-cons-percentage 0.1))
          100)

;;; Elpaca Bootstrap
(defvar elpaca-installer-version 0.11)
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
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
                                                  ,@(when-let ((depth (plist-get order :depth)))
                                                      (list "--depth" (number-to-string depth)
                                                            "--no-single-branch"))
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

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; Startup Message
(defun display-startup-time ()
  "Display startup time and GC info."
  (message "Emacs loaded in %.2f seconds with %d GCs | var: %s"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done
           emacs-var-dir))

(add-hook 'emacs-startup-hook #'display-startup-time 100)

(provide 'early-init)
;;; early-init.el ends here
