;;; -*- lexical-binding: t -*-
;;; Package --- ffmpeg streams
;;; Commentary:
;;; Detects and configures audio and video input streams available on the device.
;;; Code:
(require 'ffmpeg-commons)

(defcustom ffmpeg-recorder-desktop-stream "x11grab"
  "Desktop recording stream to use with ffmpeg."
  :type '(choice (const nil) string)
  :group 'recorder)

(defcustom ffmpeg-recorder-desktop-size '(3840 1080)
  "Desktop size."
  :type '(list number number)
  :group 'ffmpeg)

(defcustom ffmpeg-recorder-video-stream "v4l2"
  "Video recording stream to use with ffmpeg."
  :type '(choice (const nil) string)
  :group 'recorder)

(defcustom ffmpeg-recorder-video-device "/dev/video0"
  "Video recording device to use with ffmpeg."
  :type 'file
  :group 'recorder)

(defcustom ffmpeg-recorder-video-resolution '(1280 720)
  "Video capture resolution for ffmpeg."
  :type '(list natnum)
  :group 'recorder)


(defun ffmpeg-list-audio-inputs ()
  "Return a list of all input devices recognised by Pulseaudio on the machine."
  (let ((shell-command-buffer-name "*pacmd*")
        (outputs nil))
    (shell-command "pacmd list-sources")
    (with-current-buffer shell-command-buffer-name
      (goto-char (point-min))
      (while (search-forward "index:" nil t)
        (push (ffmpeg-parse-audio-device) outputs))
      outputs)))

(defun ffmpeg-parse-audio-device ()
  "Parse a single audio device parameters from pacmd output."
  (let ((default? (string-match-p "\\*" (buffer-substring (line-beginning-position) (point))))
        (index (string-trim (buffer-substring (point) (line-end-position))))
        (name (progn
                (search-forward "name:" nil t)
                (string-trim (buffer-substring (point) (line-end-position))
                             "[\t\n <]+alsa_\\(input\\|output\\)\\."
                             "\\(\\.monitor\\)?[\t\n >]+")))
        (dev (progn
               (search-forward "device.string =")
               (string-trim (buffer-substring (point) (line-end-position))
                            "[\t\n \"]+" "[\t\n \"]+"))))
    (list (if default? "x" "") "pulse" name index "")))

(defun ffmpeg-streams-add-device (dev)
  "Add a device DEV configuration to the devices table.
NOTE: this assumes the point already is over the devices table."
  (dolist (item dev)
    (insert (if (listp item)
                (format "%dx%d" (car item) (cadr item))
              item))
    (org-table-next-field)))

(defun ffmpeg-recorder-load-stream-data ()
  "Reload the stream data from the customisation variables."
  (interactive)
  (let ((inhibit-read-only t))
    (insert (propertize "*RECORDER*" 'face 'bold) "\n")
    (ffmpeg-init-table "available-streams" "Selected" "Type" "Description" "Device" "Size")
    (ffmpeg-streams-add-device (list "x" ffmpeg-recorder-desktop-stream "Desktop" ":0"
                                     ffmpeg-recorder-desktop-size))
    (ffmpeg-streams-add-device (list "x" ffmpeg-recorder-video-stream "Camera" ffmpeg-recorder-video-device
                                     ffmpeg-recorder-video-resolution))
    (dolist (dev (ffmpeg-list-audio-inputs))
      (ffmpeg-streams-add-device dev))
    (org-table-align)))

(provide 'ffmpeg-streams)
;;; ffmpeg-streams.el ends here
