;;; vp-tts.el --- Text to speech via kokoro-tts -*- lexical-binding: t; -*-
;;; Commentary:
;; Streams region text to the kokoro-tts CLI. Entry points:
;; `vp/tts-speak-dwim', `vp/tts-speed-up', `vp/tts-speed-down',
;; `vp/tts-stop'. init.el autoloads them, so this file loads on
;; first use.
;;; Code:

(defvar vp/tts-speed 1.0
  "Speech speed for kokoro-tts.")

(defvar vp/tts--text nil
  "The text that kokoro-tts spoke last.")

(defvar vp/tts--process nil)

(defvar vp/tts--status nil
  "Speech state: nil, `processing', or `speaking'.")

(defvar vp/tts-model-directory "~/Git/kokoro-tts/"
  "Directory that holds kokoro-v1.0.onnx and voices-v1.0.bin.")

(defun vp/tts--speak (text)
  "Speak TEXT with kokoro-tts at `vp/tts-speed'."
  (vp/tts-stop)
  (setq vp/tts--text text
        vp/tts--status 'processing)
  (with-current-buffer (get-buffer-create " *kokoro-tts*")
    (erase-buffer))
  (setq vp/tts--process
        ;; kokoro-tts looks for its model files in the working directory.
        (let ((default-directory (expand-file-name vp/tts-model-directory)))
          (make-process
           :name "kokoro-tts"
           :buffer " *kokoro-tts*"
           :command (list (expand-file-name "~/.local/bin/kokoro-tts")
                          "-" "--stream"
                          "--speed" (number-to-string vp/tts-speed))
           :connection-type 'pipe
           :filter #'vp/tts--filter
           :sentinel #'vp/tts--sentinel)))
  (process-send-string vp/tts--process text)
  (process-send-eof vp/tts--process)
  (force-mode-line-update t)
  (message "Speaking at %.1fx..." vp/tts-speed))

(defun vp/tts--filter (proc out)
  "Flip the state to `speaking' when kokoro-tts starts the audio stream."
  (when (and (eq vp/tts--status 'processing)
             (string-match-p "Starting audio stream" out))
    (setq vp/tts--status 'speaking)
    (force-mode-line-update t))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert out))))

(defun vp/tts--sentinel (_proc _event)
  "Clear the mode-line indicator when kokoro-tts exits."
  (setq vp/tts--status nil)
  (force-mode-line-update t))

(defun vp/tts--mode-line ()
  "Mode-line indicator: TTS… while processing, TTS▶ speed while speaking."
  (pcase vp/tts--status
    ('processing " TTS…")
    ('speaking (format " TTS▶ %.1fx" vp/tts-speed))))

;; Renders via mode-line-misc-info in the hand-rolled mode line.
(unless global-mode-string (setq global-mode-string '("")))
(add-to-list 'global-mode-string '(:eval (vp/tts--mode-line)) t)

(defun vp/tts-stop ()
  "Stop speech."
  (interactive)
  (when (process-live-p vp/tts--process)
    (delete-process vp/tts--process)
    (message "Speech stopped."))
  (setq vp/tts--process nil))

(defun vp/tts-speak-dwim ()
  "Speak the region. With no region: stop speech, or replay the last text."
  (interactive)
  (cond ((use-region-p)
         (vp/tts--speak (buffer-substring-no-properties
                         (region-beginning) (region-end)))
         (deactivate-mark))
        ((process-live-p vp/tts--process) (vp/tts-stop))
        (vp/tts--text (vp/tts--speak vp/tts--text))
        (t (message "No region and nothing to replay."))))

(defun vp/tts--adjust-speed (delta)
  "Change `vp/tts-speed' by DELTA. Restart speech when it plays."
  (setq vp/tts-speed (/ (round (* 10 (max 0.5 (min 2.0 (+ vp/tts-speed delta))))) 10.0))
  (if (process-live-p vp/tts--process)
      (vp/tts--speak vp/tts--text)
    (message "Speech speed: %.1fx" vp/tts-speed)))

(defun vp/tts-speed-up ()
  "Increase speech speed."
  (interactive)
  (vp/tts--adjust-speed 0.1))

(defun vp/tts-speed-down ()
  "Decrease speech speed."
  (interactive)
  (vp/tts--adjust-speed -0.1))

;; After C-c + or C-c -, a bare + - or = repeats the speed change.
(defvar-keymap vp/tts-repeat-map
  :repeat t
  "+" #'vp/tts-speed-up
  "=" #'vp/tts-speed-up   ; unshifted +
  "-" #'vp/tts-speed-down)

(provide 'vp-tts)
;;; vp-tts.el ends here
