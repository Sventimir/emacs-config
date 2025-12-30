;;; -*- lexical-binding: t -*-
;;; Package: --- A simple frontend for ffmpeg suite.
;;; Code:
(require 'ffmpeg-recorder)

;; Define the major mode:
(defvar-keymap ffmpeg-mode-map
  :doc "Keymap for the recorder mode."
  :parent special-mode-map
  "c" 'recorder-edit-screen-capture
  "f" 'recorder-edit-filter
  "l" 'recorder-reload-stream-data
  "p" 'recorder-playback
  "r" 'recorder-start
  "s" 'recorder-stop
  "w" 'recorder-save-recording
  "SPC" 'recorder-toggle-selection
  "<f12>" 'recorder-extract-screenshot)

(define-derived-mode ffmpeg-mode special-mode "Recorder"
  "Major mode that provides an interface to audio and video recording with ffmpeg."
  :group 'ffmpeg)

(defun ffmpeg ()
  "Open the recorder interface."
  (interactive)
  (setup-buffer "*ffmpeg*"
    (pop-to-buffer "*ffmpeg*")
    (ffmpeg-mode)
    (add-hook 'kill-buffer-query-functions 'recorder-check-unsaved-streams)
    (recorder-reload-stream-data)))


(provide 'ffmpeg)
;;; ffmpeg.el ends here
