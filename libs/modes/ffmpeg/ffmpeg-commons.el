;;; -*- lexical-binding: t -*-
;;; Package --- Ffmpeg commons
;;; Commentary:
;;; Common definitions and variables used by all ffmpeg packages.
;;; Code:
(require 'buffers)
(require 'ffmpeg-timecode)

(defcustom ffmpeg-binary-path "/usr/bin/ffmpeg"
  "Disk location of ther ffmpeg binary."
  :type 'string
  :group 'ffmpeg)

(defcustom ffmpeg-ffplay-binary-path "/usr/bin/ffplay"
  "Disk location of the ffplay binary."
  :type 'string
  :group 'ffmpeg)

(defcustom ffmpeg-ffprobe-binary-path "/usr/bin/ffprobe"
  "Disk location of the ffprobe binary."
  :type 'string
  :group 'ffmpeg)

(defcustom ffmpeg-log-level "warning"
  "The log level to pass into ffmpeg."
  :type '(choice (const "quiet")
                 (const "panic")
                 (const "fatal")
                 (const "error")
                 (const "warning")
                 (const "info")
                 (const "debug")
                 (const "trace"))
  :group 'ffmpeg)

(defcustom ffmpeg-output-format "mkv"
  "The multimedia container format for ffmpeg output."
  :type 'string
  :group 'ffmpeg)


(defvar ffmpeg-log-buffer "*ffmpeg-log*"
  "Buffer that contains standard output from ffmpeg and other tools.
Invoked commands are logged too.")

(defvar ffmpeg-process-name "ffmpeg-process"
  "Name of the ffmpeg process.")

(defvar ffmpeg-process-filter 'ffmpeg-default-process-filter
  "Filter function for the ffmpeg process.
Use local let-bindings to alter it if required.")

(defvar ffmpeg-process-sentinel 'ffmpeg-default-process-sentinel
  "Sentinel functions for the ffmpeg process.
Use local let-binding to alter it if required.")

(defun ffmpeg-format-option (option value)
  "Format OPTION with VALUE for ffmpeg command.
Return nil if VALUE is nil or a list (OPTION VALUE) otherwise"
  (if value (list option value)))

(defun ffmpeg-compose (&rest funcs)
  "Composes several FUNCS into one."
  (lambda (arg)
    (if funcs
        (funcall (car funcs) (funcall (apply 'ffmpeg-compose (cdr funcs)) arg))
        arg)))

(defun ffmpeg-opt-map (f x)
  "Map an optional argument X over function F."
  (if x (funcall f x)))

(defun ffmpeg-read-sexp (prompt &optional default)
  "Show PROMPT, read a s-expression from minibuffer, providing DEFAULT.
If entered expression starts with a quasi-quote, evaluate it."
  (let ((expr (read (read-string prompt (format "%s" default)))))
    (if (eq (car expr) '\`)
        (eval expr)
      expr)))

(defun ffmpeg-run-command (executable &rest arguments)
  "Run EXECUTABLE as an async process with ARGUMENTS."
  (let ((cmd (append (list executable "-hide_banner" "-v" ffmpeg-log-level "-y")
                     arguments)))
    (with-current-buffer (get-buffer-create ffmpeg-log-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "> " (mapconcat 'identity cmd " ") "\n\n\n")))
    (make-process
     :name ffmpeg-process-name
     :buffer ffmpeg-log-buffer
     :command cmd
     :filter 'compilation-filter
     :sentinel ffmpeg-process-sentinel)
    (display-buffer ffmpeg-log-buffer)))

(defun ffmpeg-default-process-sentinel (proc status)
  "The default process sentinel for ffmpeg PROC exiting with STATUS."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "> ffmpeg process exited: " status "\n"))))

(defun ffmpeg-table-raw-strings (table)
  "Strip all text properties of all strings inside TABLE."
  (mapcar (lambda (row)
            (mapcar (lambda (cell)
                      (set-text-properties 0 (length cell) nil cell))
                    row))
          table)
  table)

(defun ffmpeg-init-table (name &rest columns)
  "Initialize an interface table called NAME with COLUMNS."
  (insert "#+name: " name "\n")
  (org-table-create (format "%dx1" (length columns)))
  (org-table-next-field)
  (dolist (header columns)
    (insert header)
    (org-table-next-field))
  (org-table-kill-row)
  (org-table-hline-and-move)
  (org-table-align))

(defun ffmpeg-goto-element (class name)
  "Find an interface element of CLASS by its NAME so that it can be read or updated."
  (if (ffmpeg-opt-map
       (ffmpeg-compose 'goto-char 'org-element-begin)
       (car
        (org-element-map
            (org-element-parse-buffer 'greater-element t)
            class
          (lambda (el)
            (if (equal (org-element-property :name el) name) el)))))
      (forward-line 1)))

(defun ffmpeg-file-duration (filename)
  "Measure the duration of the file under FILENAME."
  (string-to-timecode
   (cadr (string-split
          (shell-command-to-string
           (format "%s -hide_banner -v warning -show_format -show_entries format=duration -print_format csv %s"
                   ffmpeg-ffprobe-binary-path
                   filename))
          ","))))

(provide 'ffmpeg-commons)
;;; ffmpeg-commons.el ends here
