;;; Package --- Summary
;;; Commentary:
;;; Provides an interface for audio and video recording using ffmpeg backend.
;;; Code:
(require 'buffers)

;; Customisation variables and internal variables:
(defcustom recorder-ffmpeg-path "ffmpeg"
  "Path to ffmpeg executable."
  :type 'string
  :group 'recorder)

(defcustom recorder-output-format "mkv"
  "Ffmpeg output format."
  :type 'string
  :group 'recorder)

(defcustom recorder-playback-program "mplayer"
  "Program used for recording playback."
  :type 'string
  :group 'recorder)

(defcustom recorder-ffmpeg-audio-stream "pulse"
  "Audio stream type for ffmpeg."
  :type '(choice (const nil) string)
  :group 'recorder)

(defcustom recorder-ffmpeg-microphone-device "alsa_input.usb-Sonix_Technology_Co.__Ltd._USB_2.0_Camera_SN0001-02.iec958-stereo"
  "Audio recording device to use with ffmpeg."
  :type 'string
  :group 'recorder)

(defcustom recorder-ffmpeg-desktop-stream "x11grab"
  "Desktop recording stream to use with ffmpeg."
  :type '(choice (const nil) string)
  :group 'recorder)

(defcustom recorder-ffmpeg-video-stream "v4l2"
  "Video recording stream to use with ffmpeg."
  :type '(choice (const nil) string)
  :group 'recorder)

(defcustom recorder-ffmpeg-video-device "/dev/video0"
  "Video recording device to use with ffmpeg."
  :type 'file
  :group 'recorder)

(defcustom recorder-ffmpeg-capture-coords '(1920 32 1920 1030)
  "Screen coordinates from which to capture.
Format: \\'(start-x, start-y, stop-x stop-y)."
  :type '(list natnum)
  :group 'recorder)

(defcustom recorder-ffmpeg-video-resolution '(1280 720)
  "Video capture resolution for ffmpeg."
  :type '(list natnum)
  :group 'recorder)

(defcustom recorder-ffmpeg-output-acodec "libvorbis"
  "Audio codec to use for output stream."
  :type 'string
  :group 'recorder)

(defcustom recorder-ffmpeg-output-vcodec "libx264"
  "Video codec to use for output stream."
  :type 'string
  :group 'recorder)

(defcustom recorder-ffmpeg-video-filter "[1:v]scale=384:216[cam], [2:v][cam]overlay=30:800[video]"
  "Video filter to combine streams with ffmpeg."
  :type 'string
  :group 'recorder)

(defcustom recorder-default-writing-dir (getenv "HOME")
  "Default directory to write recorded files."
  :type 'directory
  :group 'recorder)

(defvar recorder-ffmpeg-last-output-file nil
  "Last output file used for recording.")

(defvar recorder-ffmpeg-streams nil
  "List of streams to record.")


;; Internal functions:
(defun recorder-format-option (option value)
  "Format OPTION and VALUE for ffmpeg command.
Omit the option entirely if VALUE is nil."
  (if value
      (list option value)
    nil))

(defun recorder-opt-map (f x)
  "Map an optional argument X over a function F."
  (if x
      (funcall f x)
    nil))

(defun recorder-ffmpeg-desktop-device (&optional dev start-coords)
  "Format the desktop device DEV for ffmpeg with START-COORDS."
  (let ((coords (or start-coords recorder-ffmpeg-capture-coords)))
    (format "%s+%d,%d"
            (or dev (getenv "DISPLAY") "0.0")
            (car coords)
            (cadr coords))))

(defun recorder-show-stream (index stream)
  "Format STREAM with INDEX for display."
  (format "STREAM #%d: %s from: %s%s\n"
          index
          (propertize (car stream) 'face 'bold)
          (cadr stream)
          (recorder-opt-map
           (lambda (s) (format ", size: %s" (recorder-ffmpeg-format-resolution s)))
           (caddr stream))))

(defun recorder-find-stream (types streams)
  "Find the first stream in STREAMS that has on of TYPES as its first element.
Return the index of the stream and the stream itself."
  (let ((i 0))
    (cl-dolist (stream streams)
      (if (member (car stream) types)
        (cl-return (list i stream))
        (setq i (1+ i))))))

(defun recorder-ffmpeg-stream-args (stream)
  "Decode sexp STREAM into a list of ffmpeg arguments.
Sexp format is (stream-type device (width height))"
  (append
   (recorder-format-option "-f" (car stream))
   (recorder-format-option "-s" (recorder-opt-map 'recorder-ffmpeg-format-resolution (caddr stream)))
   (recorder-format-option "-i" (cadr stream))))

(defun recorder-ffmpeg-mapping-args (&optional streams)
  "Format ffmpeg mapping arguments for STREAMS."
  (let* ((streams (or streams recorder-ffmpeg-streams))
         (audio (recorder-find-stream '("alsa" "pulse") streams)))
    (append
     (if audio (list "-map" (format "%d:a" (car audio)) "-c:a" recorder-ffmpeg-output-acodec))
     (list "-map" "[video]" "-c:v" recorder-ffmpeg-output-vcodec))))

(defun recorder-ffmpeg-format-resolution (coordinates)
  "Conver a list of 2 COORDINATES to a string readable for ffmpeg.
NOTE: any excess elements in COORDINATES list are ignored."
  (format "%d:%d" (car coordinates) (cadr coordinates)))

(defun recorder-ffmpeg-filter-arg ()
  "Format ffmpeg arguments for complex filter."
  (if (and recorder-ffmpeg-video-stream recorder-ffmpeg-desktop-stream)
      (list "-filter_complex" recorder-ffmpeg-video-filter)
    nil))

(defun recorder-ffmpeg-command (output-file)
  "Construct the ffmpeg command writing to OUTPUT-FILE."
  (append
   (list recorder-ffmpeg-path "-y")
   (mapcan 'recorder-ffmpeg-stream-args recorder-ffmpeg-streams)
   (recorder-ffmpeg-filter-arg)
   (recorder-ffmpeg-mapping-args)
   (list output-file)))

(defun recorder-ffmpeg-sentinel (proc status)
  "Sentinel function for fmmpeg process using PROC and STATUS."
  (if (not (string= status "finished\n"))
      (pop-to-buffer "*recorder-ffmpeg*"))
  (with-current-buffer "*recorder*"
    (let ((inhibit-read-only t))
      (insert "Recording stopped: " status))))

(defun recorder-transcoding-sentinel (proc status)
  "Sentinel function for transcoder ffmpeg process using PROC and STATUS."
  (if (not (string= status "finished\n"))
      (pop-to-buffer "*recorder-ffmpeg-process*"))
  (with-current-buffer "*recorder*"
    (let ((inhibit-read-only t))
      (insert "Transcoding stopped: " status))))

(defun recorder-check-unsaved-streams ()
  "Check if there are unsaved streams and prompt the user to save them."
  (recorder-stop)
  (if recorder-ffmpeg-last-output-file
      (if (yes-or-no-p "There is an unsaved recording.  Save it?")
          (recorder-save-recording))
    t))

;; Recorder commands:
(defun recorder-playback ()
  "Play back the recorded file using RECORDER-PLAYBACK-PROGRAM."
  (interactive)
  (if recorder-ffmpeg-last-output-file
      (make-process
       :name "recorder-playback"
       :buffer "*recorder-playback*"
       :command (list recorder-playback-program recorder-ffmpeg-last-output-file))
    (message "No recorded file to play back.")))

(defun recorder-save-recording (duration)
  "Save the recording to a file, cutting everything aster DURATION."
  (interactive "sDuration: ")
  (if recorder-ffmpeg-last-output-file
      (let ((time (if (string-empty-p duration) nil duration))
            (output-file
             (expand-file-name
              (read-file-name "Output file: " recorder-default-writing-dir))))
        (make-process
         :name "recorder-ffmpeg-transcoder"
         :buffer "*recorder-ffmpeg-process*"
         :command (append
                   (list recorder-ffmpeg-path "-y" "-i" recorder-ffmpeg-last-output-file)
                   (recorder-format-option "-t" time)
                   (list output-file))
         :sentinel 'recorder-transcoding-sentinel)
        (with-current-buffer "*recorder*"
          (let ((inhibit-read-only t))
            (insert "Transcoding: '" recorder-ffmpeg-last-output-file "' -> '" output-file "'\n"))))
    (message "No recorded file to save.")))

(defun recorder-start ()
  "Start recording."
  (interactive)
  (setq recorder-ffmpeg-last-output-file
        (make-temp-file "emacs-recorder" nil (concat "." recorder-output-format)))
  (let ((cmd (recorder-ffmpeg-command recorder-ffmpeg-last-output-file))
        (inhibit-read-only t))
    (make-process
     :name "recorder-ffmpeg-process"
     :buffer "*recorder-ffmpeg*"
     :command cmd
     :sentinel 'recorder-ffmpeg-sentinel)
    (insert "Recording: '" (mapconcat 'identity cmd " ") "'\n")))

(defun recorder-stop ()
  "Stop recording."
  (interactive)
  (process-send-string "recorder-ffmpeg-process" "q"))

;; Define the major mode:
(defvar-keymap recorder-mode-map
  :doc "Keymap for the recorder mdoe."
  :parent special-mode-map
  "p" 'recorder-playback
  "r" 'recorder-start
  "s" 'recorder-stop
  "w" 'recorder-save-recording)

(define-derived-mode recorder-mode special-mode "Recorder"
  "Major mode that provides an interface to audio and video recording with ffmpeg."
  :group 'recorder)

(defun recorder ()
  "Open the recorder interface."
  (interactive)
  (setup-buffer "*recorder*"
    (pop-to-buffer "*recorder*")
    (recorder-mode)
    (add-hook 'kill-buffer-query-functions 'recorder-check-unsaved-streams)
    (if recorder-ffmpeg-desktop-stream
        (add-to-list 'recorder-ffmpeg-streams
                     (list recorder-ffmpeg-desktop-stream
                           (recorder-ffmpeg-desktop-device)
                           (cddr recorder-ffmpeg-capture-coords))))
    (if recorder-ffmpeg-video-stream
        (add-to-list 'recorder-ffmpeg-streams
                     (list recorder-ffmpeg-video-stream
                           recorder-ffmpeg-video-device
                           recorder-ffmpeg-video-resolution)))
    (if recorder-ffmpeg-audio-stream
        (add-to-list 'recorder-ffmpeg-streams
                     (list recorder-ffmpeg-audio-stream recorder-ffmpeg-microphone-device)))
    (let ((inhibit-read-only t)
          (stream-index 0))
      (insert (propertize "*RECORDER*" 'face 'bold) "\n")
      (dolist (stream recorder-ffmpeg-streams)
        (insert (recorder-show-stream (setq stream-index (1+ stream-index)) stream)))
      (insert (propertize "Filter: " 'face 'bold) recorder-ffmpeg-video-filter "\n"))))

(provide 'recorder)
;;; recorder.el ends here
