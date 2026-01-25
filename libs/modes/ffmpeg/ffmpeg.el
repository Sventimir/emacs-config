;;; -*- lexical-binding: t -*-
;;; Package: --- A simple frontend for ffmpeg suite.
;;; Code:
(require 'ffmpeg-commons)
(require 'ffmpeg-recorder)
(require 'ffmpeg-transcoder)
(require 'ffmpeg-filters)


(defun ffmpeg-toggle-table-selection ()
  "If point is inside a table, toggle selection for the current row."
  (interactive)
  (if (org-at-table-p)
      (progn
        (let ((inhibit-read-only t)
              (val (org-table-get nil 1)))
          (cond ((string= val "x") (org-table-put nil 1 "" 'realign))
                ((string= val "") (org-table-put nil 1 "x" 'realign))
                (t nil))))))

(defun ffmpeg-init-open-files ()
  "Output a table showing open files; initially empty."
  (let ((inhibit-read-only t))
    (insert "\n" (propertize "*OPEN FILES*" 'face 'bold) "\n")
    (ffmpeg-init-table "open-files" "Selected" "Filename" "Length" "Created")
    (org-table-kill-row)))

(defun ffmpeg-playback ()
  "Play the selected material back."
  (interactive)
  (save-excursion
    (org-table-goto-column 2)
    (ffmpeg-run-command ffmpeg-ffplay-binary-path (string-trim (org-table-get-field)))))

;; Define the major mode:
(defvar-keymap ffmpeg-mode-map
  :doc "Keymap for the recorder mode."
  :parent special-mode-map
  "f" 'ffmpeg-transcoder-edit-filter
  "l" 'ffmpeg-recorder-reload-stream-data
  "m" 'ffmpeg-transcoder-edit-mappings
  "o" 'ffmpeg-transcoder-open-file
  "p" 'ffmpeg-playback
  "r" 'ffmpeg-recorder-start
  "s" 'ffmpeg-recorder-stop
  "t" 'ffmpeg-transcoder-run
  "<f12>" 'ffmpeg-transcoder-take-frame
  "SPC" 'ffmpeg-toggle-table-selection)

(define-derived-mode ffmpeg-mode special-mode "ffmpeg"
  "Major mode that provides an interface to audio and video recording with ffmpeg."
  :group 'ffmpeg)

(defun ffmpeg ()
  "Open the ffmpeg interface."
  (interactive)
  (setup-buffer "*ffmpeg*"
    (pop-to-buffer "*ffmpeg*")
    (ffmpeg-mode)
    (let ((inhibit-read-only t))
      (ffmpeg-recorder-load-stream-data)
      (forward-line 1)
      (ffmpeg-init-open-files)
      (forward-line 1)
      (insert "\n#+name: transcoder-filter\n")
      (ffmpeg-transcoder-display-filter)
      (insert "\n#+name: transcoder-mappings\n")
      (ffmpeg-transcoder-display-mappings))))


(provide 'ffmpeg)
;;; ffmpeg.el ends here
