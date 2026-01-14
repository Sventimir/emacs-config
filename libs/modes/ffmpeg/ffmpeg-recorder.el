;;; -*- lexical-binding: t -*-
;;; Package --- Summary
;;; Commentary:
;;; Provides an interface for audio and video recording using ffmpeg backend.
;;; Code:
(require 'buffers)
(require 'org)
(require 'range)
(require 'ffmpeg-timecode)

(defcustom ffmpeg-recorder-audio-stream "alsa"
  "Audio stream type for ffmpeg."
  :type '(choice (const nil) string)
  :group 'recorder)

(defcustom ffmpeg-recorder-microphone-device "default"
  "Audio recording device to use with ffmpeg."
  :type 'string
  :group 'recorder)

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

(defcustom ffmpeg-recoder-acodec "flac"
  "Audio codec to use for output stream.
Unlike transcoder, we want an uncompressed audio for faster transcoding and better quality."
  :type 'string
  :group 'recorder)

(defcustom ffmpeg-recorder-vcodec "zlib"
  "Video codec to use for output stream.
Unlike transcoder, we want an uncompressed video for faster transcoding and better quality."
  :type 'string
  :group 'recorder)

(defcustom ffmpeg-recorder-output-format "mkv"
  "The container format to use for recorder's output."
  :type 'string
  :group 'ffmpeg)

(defun ffmpeg-recorder-read-streams ()
  "Read input stream data from the config table."
  (save-excursion
    (ffmpeg-goto-element 'table "available-streams")
    (let ((data (cddr (org-table-to-lisp))))
      (mapcar (lambda (row)
                (append (butlast (cdr row))
                        (list (mapcar 'string-to-number (string-split (car (last row)) "x" t " ")))))
              (ffmpeg-table-raw-strings
               (seq-filter (lambda (row) (string= (car row) "x")) data))))))

(defun ffmpeg-recorder-stream-args (stream)
  "Decode sexp STREAM into a list of ffmpeg arguments.
Sexp format is (stream-type device (width height))"
  (append
   (ffmpeg-format-option "-f" (car stream))
   (ffmpeg-format-option "-s" (ffmpeg-opt-map 'ffmpeg-recorder-format-resolution (caddr stream)))
   (ffmpeg-format-option "-i" (cadr stream))))


(defun ffmpeg-recorder-mapping-args (streams)
  "Generate stream of STREAMS mappings for recording."
  (mapcan (lambda (idx)
            (list "-map" (number-to-string idx)))
          (range-uncompress (list (cons 0 (1- (length streams)))))))

(defun ffmpeg-recorder-format-resolution (coordinates)
  "Convert a list of 2 COORDINATES to a string readable for ffmpeg.
NOTE: any excess elements in COORDINATES list are ignored."
  (format "%d:%d" (car coordinates) (cadr coordinates)))

(defun ffmpeg-recorder-add-device (dev)
  "Add a device DEV configuration to the devices table.
NOTE: this assumes the point already is over the devices table."
  (dolist (item (cons "x" dev))
    (insert (if (listp item)
                (format "%dx%d" (car item) (cadr item))
              item))
    (org-table-next-field)))

;; Recorder commands:
(defun ffmpeg-recorder-load-stream-data ()
  "Reload the stream data from the customisation variables."
  (interactive)
  (let ((inhibit-read-only t))
    (insert (propertize "*RECORDER*" 'face 'bold) "\n")
    (ffmpeg-init-table "available-streams" "Selected" "Type" "Device" "Size")
    (ffmpeg-recorder-add-device (list ffmpeg-recorder-desktop-stream ":0" ffmpeg-recorder-desktop-size))
    (ffmpeg-recorder-add-device (list ffmpeg-recorder-video-stream ffmpeg-recorder-video-device
                                      ffmpeg-recorder-video-resolution))
    (ffmpeg-recorder-add-device (list ffmpeg-recorder-audio-stream ffmpeg-recorder-microphone-device))
    (org-table-align)))

(defun ffmpeg-recorder-sentinel (proc status)
  "Sentinel function for the recording process, using PROC and STATUS."
  (ffmpeg-default-process-sentinel proc status)
  (with-current-buffer "*ffmpeg*"
    (ffmpeg-goto-element 'table "open-files")
    (let ((fname (car (last (process-command proc))))
          (status (string-trim status))
          (inhibit-read-only t))
      (search-forward fname)
      (if (string= status "finished")
          (let ((length (ffmpeg-file-duration fname)))
            (org-table-put nil 1 "x")
            (org-table-put nil 3 (timecode-to-string length)))
        (org-table-put nil 1 status))
      (org-table-align))))

(defun ffmpeg-recorder-start ()
  "Start recording."
  (interactive)
  (let ((output-file (make-temp-file "emacs-recorder" nil (concat "." ffmpeg-recorder-output-format)))
        (streams (ffmpeg-recorder-read-streams))
        (ffmpeg-process-name "ffmpeg-recorder-process")
        (ffmpeg-process-sentinel 'ffmpeg-recorder-sentinel))
    (apply 'ffmpeg-run-command ffmpeg-binary-path
           (append (mapcan 'ffmpeg-recorder-stream-args streams)
                   (list "-r" "30" "-time_base" "1/1000")
                   (ffmpeg-recorder-mapping-args streams)
                   (list "-c:a" ffmpeg-recoder-acodec "-c:v" ffmpeg-recorder-vcodec
                         output-file)))
    (ffmpeg-goto-element 'table "open-files")
    (goto-char (1- (org-table-end)))
    (let ((inhibit-read-only t)
          (idx (string-to-number (org-table-get nil 2))))
      (dolist (field (list "n/a" output-file "unknown" (current-time-string)))
        (org-table-next-field)
        (insert field))
      (org-table-align))))

(defun ffmpeg-recorder-stop ()
  "Stop recording."
  (interactive)
  (process-send-string "ffmpeg-recorder-process" "q"))

(provide 'ffmpeg-recorder)
;;; ffmpeg-recorder.el ends here
