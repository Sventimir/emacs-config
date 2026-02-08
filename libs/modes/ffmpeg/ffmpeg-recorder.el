;;; -*- lexical-binding: t -*-
;;; Package --- Summary
;;; Commentary:
;;; Provides an interface for audio and video recording using ffmpeg backend.
;;; Code:
(require 'buffers)
(require 'org)
(require 'range)
(require 'ffmpeg-streams)
(require 'ffmpeg-timecode)

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
   (ffmpeg-format-option "-s" (ffmpeg-opt-map 'ffmpeg-recorder-format-resolution (cadddr stream)))
   (ffmpeg-format-option "-i" (caddr stream))))


(defun ffmpeg-recorder-mapping-args (streams)
  "Generate stream of STREAMS mappings for recording."
  (mapcan (lambda (idx)
            (list "-map" (number-to-string idx)))
          (range-uncompress (list (cons 0 (1- (length streams)))))))

(defun ffmpeg-recorder-format-resolution (coordinates)
  "Convert a list of 2 COORDINATES to a string readable for ffmpeg.
NOTE: any excess elements in COORDINATES list are ignored."
  (format "%d:%d" (car coordinates) (cadr coordinates)))

;; Recorder commands:
(defun ffmpeg-recorder-finish-hook (buffer status)
  "A function to call with BUFFER and STATUS when a recording process finishes."
  (if (string= (buffer-name buffer) "*ffmpeg-recorder*")
      (let ((fname (with-current-buffer buffer (car (last ffmpeg-command)))))
        (with-current-buffer "*ffmpeg*"
          (ffmpeg-goto-element 'table "open-files")
          (let ((status (string-trim status))
                (inhibit-read-only t))
            (search-forward fname)
            (let ((length (ffmpeg-file-duration fname)))
              (org-table-put nil 1 "x")
              (org-table-put nil 3 (timecode-to-string length)))
            (org-table-align))))))

(add-to-list 'compilation-finish-functions 'ffmpeg-recorder-finish-hook)

(defun ffmpeg-recorder-start ()
  "Start recording."
  (interactive)
  (let ((output-file (make-temp-file "emacs-recorder" nil (concat "." ffmpeg-recorder-output-format)))
        (streams (ffmpeg-recorder-read-streams))
        (ffmpeg-log-buffer "*ffmpeg-recorder*"))
    (apply 'ffmpeg-run-command ffmpeg-binary-path
           (append (mapcan 'ffmpeg-recorder-stream-args streams)
                   (list "-r" "30" "-time_base" "1/1000")
                   (ffmpeg-recorder-mapping-args streams)
                   (list "-c:a" ffmpeg-recoder-acodec "-c:v" ffmpeg-recorder-vcodec
                         output-file)))
    (ffmpeg-table-append-row "open-files" (list "n/a" output-file "unknown" (current-time-string)))))

(defun ffmpeg-recorder-stop ()
  "Stop recording."
  (interactive)
  (process-send-string (get-buffer-process "*ffmpeg-recorder*") "q"))

(provide 'ffmpeg-recorder)
;;; ffmpeg-recorder.el ends here
