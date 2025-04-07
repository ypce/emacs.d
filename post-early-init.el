;;; post-early-init.el --- 2 -*- no-byte-compile: t; lexical-binding: t; -*-

;;;; Move native compilation cache into /var.
(defvar native-comp-eln-cache-dir (expand-file-name "eln-cache/" minimal-emacs-var-dir)
  "Directory for native compilation cache.")

;; Create the directory if it doesn't exist
(unless (file-exists-p native-comp-eln-cache-dir)
  (make-directory native-comp-eln-cache-dir t))

;; Redirect the cache
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename native-comp-eln-cache-dir)))

;; Also set this variable directly as a fallback (for older Emacs versions)
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path native-comp-eln-cache-dir))
