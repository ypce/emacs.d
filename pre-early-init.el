;;; pre-early-init.el --- 1 -*- lexical-binding: t; -*-
;;; Set up directories
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)

; ;; Disable package.el to avoid conflicts with Elpaca
; (setq package-enable-at-startup nil)
; (setq package-quickstart nil)

;;; Disable debug
(defvar minimal-emacs-debug nil
  "Non-nil to enable debug.")
;;; Homebrew gcc paths fix
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
      (let* ((gcc-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix gcc"))))
             (gcc-lib-current (expand-file-name "lib/gcc/current" gcc-prefix)))
        (push gcc-lib-current paths)
        ;; Find apple-darwin directory.
        (let* ((default-directory gcc-lib-current)
               (arch-dirs (file-expand-wildcards "gcc/*-apple-darwin*/*[0-9]")))
          (when arch-dirs
            (push (expand-file-name
                   (car (sort arch-dirs #'string>)))
                  paths))))
      ;; Get libgccjit paths
      (let* ((jit-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix libgccjit"))))
             (jit-lib-current (expand-file-name "lib/gcc/current" jit-prefix)))
        (push jit-lib-current paths)))
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
  (setup-macos-native-comp-library-paths))
;;; Display startup time
(defun display-startup-time ()
  "Display the startup time and garbage collections."
  (message "Emacs loaded in %.2f seconds with %d GCs."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time 100)
;;; Elpaca Setup
(setq minimal-emacs-package-initialize-and-refresh nil)
