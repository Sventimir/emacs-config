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

(defvar ffmpeg-process-finish-hook nil
  "A function to call when the ffmpeg process finishes.
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
  (save-excursion
    (let ((cmd (append (list executable "-hide_banner" "-v" ffmpeg-log-level)
                       (if (string= executable ffmpeg-ffplay-binary-path) nil '("-y"))
                       arguments)))
      (let* ((buf (compile (mapconcat 'shell-quote-argument cmd " ") t))
             (proc (get-buffer-process buf))\
             (old-buf (get-buffer ffmpeg-log-buffer)))
        (if old-buf (kill-buffer old-buf))
        (with-current-buffer buf
          (rename-buffer ffmpeg-log-buffer)
          (goto-char (point-max))
          (setq-local ffmpeg-command cmd)))
      (display-buffer ffmpeg-log-buffer))))

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

(defun ffmpeg-table-append-row (name data)
  "Find table NAME and append a row containing DATA to it."
  (ffmpeg-goto-element 'table name)
  (goto-char (1- (org-table-end)))
  (let ((inhibit-read-only t))
    (dolist (field data)
      (org-table-next-field)
      (insert field))
    (org-table-align)))

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
