;;; Package --- Summary
;;; Commentary:
;;; Provides an interface for audio and video recording using ffmpeg backend.
;;; Code:
(defcustom recorder-ffmpeg-path "ffmpeg"
  "Path to ffmpeg executable."
  :type 'string
  :group 'recorder)

(defcustom recorder-output-file (format "%s/recording.mkv" (getenv "HOME"))
  "Ffmpeg output filename."
  :type 'file
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

(defcustom recorder-ffmpeg-video-filter "[2:v]scale=384:216[cam], [1:v][cam]overlay=30:800[video]"
  "Video filter to combine streams with ffmpeg."
  :type 'string
  :group 'recorder)

(defun recorder-playback ()
  "Play back the recorded file using RECORDER-PLAYBACK-PROGRAM."
  (interactive)
  (make-process
   :name "recorder-playback"
   :buffer "*recorder-playback*"
   :command (list recorder-playback-program recorder-output-file)))

(defun recorder-ffmpeg-format-resolution (coordinates)
  "Conver a list of 2 COORDINATES to a string readable for ffmpeg.
NOTE: any excess elements in COORDINATES list are ignored."
  (format "%d:%d" (car coordinates) (cadr coordinates)))

(defun recorder-ffmpeg-input-top-left ()
  "Format the screen capture top-left coordinate for ffmpeg."
  (format "%s+%d,%d"
          (getenv "DISPLAY")
          (car recorder-ffmpeg-capture-coords)
          (cadr recorder-ffmpeg-capture-coords)))

(defun recorder-ffmpeg-input-bottom-right ()
  "Format the screen capture bottom-right coordinate-for ffmpeg.."
  (recorder-ffmpeg-format-resolution (cddr recorder-ffmpeg-capture-coords)))

(defun recorder-ffmpeg-audio-stream-config ()
  "Format ffmpeg arguments for audio input stream."
  (if recorder-ffmpeg-audio-stream
      (list "-f" recorder-ffmpeg-audio-stream
            "-i" recorder-ffmpeg-microphone-device)
    nil))

(defun recorder-ffmpeg-video-stream-config ()
  "Format ffmpeg arguments for video input stream."
  (if recorder-ffmpeg-video-stream
      (list "-f" recorder-ffmpeg-video-stream
            "-s" (recorder-ffmpeg-format-resolution recorder-ffmpeg-video-resolution)
            "-i" recorder-ffmpeg-video-device)
    nil))

(defun recorder-ffmpeg-desktop-stream-config ()
  "Format ffmpeg arguments for desktop input stream."
  (if recorder-ffmpeg-desktop-stream
      (list "-f" recorder-ffmpeg-desktop-stream
            "-s" (recorder-ffmpeg-input-bottom-right)
            "-i" (recorder-ffmpeg-input-top-left))
    nil))

(defun recorder-ffmpeg-filter-arg ()
  "Format ffmpeg arguments for complex filter."
  (if (and recorder-ffmpeg-video-stream recorder-ffmpeg-desktop-stream)
      (list "-filter_complex" recorder-ffmpeg-video-filter)
    nil))

(defun recorder-ffmpeg-audio-codec-arg ()
  "Fomrat ffmpeg arg for audio output codec."
  (if recorder-ffmpeg-audio-stream
      (list "-acodec" recorder-ffmpeg-output-acodec)
    nil))

(defun recorder-ffmpeg-video-codec-arg ()
  "Fomrat ffmpeg arg for video output codec."
  (if (or recorder-ffmpeg-video-stream recorder-ffmpeg-desktop-stream)
      (list "-vcodec" recorder-ffmpeg-output-vcodec)
    nil))

(defun recorder-ffmpeg-output-mapping ()
  "Format ffmpeg args for output stream mapping."
  (let* ((stream-index -1)
         (audio (if recorder-ffmpeg-audio-stream
                    (format "%d:a" (setq stream-index (1+ stream-index)))
                  nil))
         (video (cond ((and recorder-ffmpeg-video-stream recorder-ffmpeg-desktop-stream) "[video]")
                      ((or recorder-ffmpeg-video-stream (format "%d:v" stream-index)))
                      (t nil))))
    (append
     (if audio (list "-map" audio) nil)
     (if video (list "-map" video) nil))))

(defun recorder-ffmpeg-command ()
  "Construct the ffmpeg command."
  (append
   (list recorder-ffmpeg-path "-y")
   (recorder-ffmpeg-audio-stream-config)
   (recorder-ffmpeg-desktop-stream-config)
   (recorder-ffmpeg-video-stream-config)
   (recorder-ffmpeg-filter-arg)
   (recorder-ffmpeg-audio-codec-arg)
   (recorder-ffmpeg-video-codec-arg)
   (recorder-ffmpeg-output-mapping)
   (list recorder-output-file)))

(defun recorder-ffmpeg-sentinel (proc status)
  "Sentinel function for fmmpeg process using PROC and STATUS."
  (with-current-buffer "*recorder*"
    (let ((inhibit-read-only t))
      (insert "Recording stopped: " status))))

(defun recorder-start ()
  "Start recording."
  (interactive)
  (let ((cmd (recorder-ffmpeg-command))
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

(defvar-keymap recorder-mode-map
  :doc "Keymap for the recorder mdoe."
  :parent special-mode-map
  "p" 'recorder-playback
  "r" 'recorder-start
  "s" 'recorder-stop)

(define-derived-mode recorder-mode special-mode "Recorder"
  "Major mode that provides an interface to audio and video recording with ffmpeg."
  :group 'recorder)

(defun recorder ()
  "Open the recorder interface."
  (interactive)
  (pop-to-buffer "*recorder*")
  (recorder-mode)
  (let ((inhibit-read-only t))
    (insert "*RECORDER*\n")
    (insert (format "Output file: %s\n" recorder-output-file))))

(provide 'recorder)
;;; recorder.el ends here
