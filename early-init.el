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

;;; macOS Environment & PATH Setup
(when (eq system-type 'darwin)
  ;; Command key mappings
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-control-modifier 'control
        mac-function-modifier 'hyper
        mac-pass-command-to-system nil
        mac-pass-control-to-system nil)
  
  ;; Import PATH from shell to fix Homebrew binary access
  (defun import-shell-path ()
    "Import PATH from shell environment on macOS."
    (let* ((shell (or (getenv "SHELL") "/bin/zsh"))
           (command (format "%s -l -c 'echo $PATH'" shell))
           (path-from-shell (string-trim (shell-command-to-string command))))
      (when (and path-from-shell (not (string-empty-p path-from-shell)))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator t))
        (message "PATH imported from shell: %s" (getenv "PATH")))))
  
  ;; Import PATH early so packages can find tools like ripgrep
  (import-shell-path))

;; macOS Homebrew GCC paths fix for native compilation
(defun homebrew-gcc-paths ()
  "Return GCC library paths from Homebrew installations.
Detects paths for gcc and libgccjit packages to be used in LIBRARY_PATH."
  (let* ((paths '())
         (brew-bin (or (executable-find "brew")
                       (let ((arm-path "/opt/homebrew/bin/brew")
                             (intel-path "/usr/local/bin/brew"))
                         (cond
                          ((file-exists-p arm-path) arm-path)
                          ((file-exists-p intel-path) intel-path))))))
    (when brew-bin
      ;; Get gcc paths.
      (condition-case nil
        (let* ((gcc-prefix (string-trim
                            (shell-command-to-string
                             (concat brew-bin " --prefix gcc 2>/dev/null"))))
               (gcc-lib-current (expand-file-name "lib/gcc/current" gcc-prefix)))
          (when (file-directory-p gcc-lib-current)
            (push gcc-lib-current paths)
            ;; Find apple-darwin directory.
            (let* ((default-directory gcc-lib-current)
                   (arch-dirs (file-expand-wildcards "gcc/*-apple-darwin*/*[0-9]")))
              (when arch-dirs
                (push (expand-file-name
                       (car (sort arch-dirs #'string>)))
                      paths)))))
        (error nil))
      ;; Get libgccjit paths
      (condition-case nil
        (let* ((jit-prefix (string-trim
                            (shell-command-to-string
                             (concat brew-bin " --prefix libgccjit 2>/dev/null"))))
               (jit-lib-current (expand-file-name "lib/gcc/current" jit-prefix)))
          (when (file-directory-p jit-lib-current)
            (push jit-lib-current paths)))
        (error nil)))
    (nreverse paths)))

(defun setup-macos-native-comp-library-paths ()
  "Set up LIBRARY_PATH for native compilation on macOS.
Includes Homebrew GCC paths and CommandLineTools SDK libraries."
  (let* ((existing-paths (split-string (or (getenv "LIBRARY_PATH") "") ":" t))
         (gcc-paths (homebrew-gcc-paths))
         (clt-paths '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
         (unique-paths (delete-dups
                        (append existing-paths gcc-paths clt-paths))))
    (setenv "LIBRARY_PATH" (mapconcat #'identity unique-paths ":"))))

;; Set up library paths for native compilation on macOS.
(when (eq system-type 'darwin)
  (setup-macos-native-comp-library-paths)
  ;; Debug: Print the LIBRARY_PATH to verify it's set correctly
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
