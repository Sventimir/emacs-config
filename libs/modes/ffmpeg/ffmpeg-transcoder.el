;;; -*- lexical-binding: t -*-
;;; Package --- Ffmpeg transcoder
;;; Commentary:
;;; A set of transcoding utilities for multimedia, implemented with ffmpeg.
;;; Code:
(require 'org)
(require 'ffmpeg-commons)
(require 'ffmpeg-filter-compiler)
(require 'ffmpeg-timecode)

(defcustom ffmpeg-transcoder-vcodec "hevc"
  "The video codec to use for transcoding output.
Unlike with recorder, we want this to be a well-compressed codec."
  :type 'string
  :group 'ffmpeg)

(defcustom ffmpeg-transcoder-acodec "libvorbis"
  "The audio codec to use for transcoding output.
Unlike with recorder, we want this to be a well-compressed codec."
  :type 'string
  :group 'ffmpeg)

(defcustom ffmpeg-transcoder-filter nil
  "Sexp-encoded ffmpeg filter to use during transcoding."
  :type 'sexp
  :group 'ffmpeg)

(defcustom ffmpeg-transcoder-saved-filters nil
  "A list of pairs: name + sexp describing an ffmpeg filter stored for reuse."
  :type '(alist :key-type 'string :value-type 'sexp)
  :group 'ffmpeg)

(defcustom ffmpeg-transcoder-mappings nil
  "A list of streams to include in the output."
  :type 'sexp
  :group 'ffmpeg)

(defcustom ffmpeg-transcoder-default-output-dir (getenv "HOME")
  "The default directory for transcoding output."
  :type 'string
  :group 'ffmpeg)

(defcustom ffmpeg-saved-filters nil
  "A list of transcoding filters previously saved for reuse."
  :type '(alist :key-type string :value-type sexp)
  :group 'ffmpeg)


(defun ffmpeg-transcoder-filter-arg (arg)
  "Format ffmpeg arguments ARG for complex filter.
If the expression starts with quasi-quote, evaluate it before returning."
  (let* ((expr (if (eq (car arg) '\`)
                   (eval arg)
                 arg)))
    (if expr (list "-filter_complex" (ffmpeg-filter-codegen expr)))))

(defun ffmpeg-transcoder-display-filter ()
  "Display the contents of FFMPEG-TRANSCODER-FILTER in the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize "Filter: " 'face 'bold)
            (or (cadr (ffmpeg-transcoder-filter-arg ffmpeg-transcoder-filter))
                "")
            "\n")))

(defun ffmpeg-transcoder-display-mappings ()
  "Display the contents of FFMPEG-TRANSCODER-MAPPINGS in the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize "Mappings: " 'face 'bold)
            (mapconcat
                 'identity
                 (seq-filter (lambda (arg)
                               (not (string= arg "-map")))
                             (ffmpeg-transcoder-mapping-args ffmpeg-transcoder-mappings))
                 " ")
            "\n")))

(defun ffmpeg-transcoder-edit-filter (new-filter)
  "Edit the ffmpeg video filter, etting it to NEW-FILTER."
  (interactive (list (read (read-string "Filter: " (format "%S" ffmpeg-transcoder-filter)))))
  (setq ffmpeg-transcoder-filter new-filter)
  (with-current-buffer "*ffmpeg*"
    (ffmpeg-goto-element 'paragraph "transcoder-filter")
    (let ((inhibit-read-only t))
      (delete-region (line-beginning-position) (line-beginning-position 2))
      (ffmpeg-transcoder-display-filter))))

(defun ffmpeg-transcoder-edit-mappings (new-mappings)
  "Edit the ffmpeg mappings, etting it to NEW-MAPPINGS."
  (interactive (list (ffmpeg-read-sexp "Mappings: " ffmpeg-transcoder-mappings)))
  (setq ffmpeg-transcoder-mappings new-mappings)
  (with-current-buffer "*ffmpeg*"
    (ffmpeg-goto-element 'paragraph "transcoder-mappings")
    (let ((inhibit-read-only t))
      (delete-region (line-beginning-position) (line-beginning-position 2))
      (ffmpeg-transcoder-display-mappings))))

(defun ffmpeg-transcoder-input-files ()
  "Read selected input files from the table."
  (save-excursion
    (ffmpeg-goto-element 'table "open-files")
    (mapcar (lambda (row)
              (cadr row))
            (ffmpeg-table-raw-strings
             (seq-filter (lambda (row) (string= (car row) "x")) (cddr (org-table-to-lisp)))))))

(defun ffmpeg-transcoder-input-arguments ()
  "Prefix each input filename in FFMPEG-TANSCODER-INPUT-FILES with -i flag."
  (mapcan (lambda (fname) (list "-i" fname))
          (ffmpeg-transcoder-input-files)))

(defun ffmpeg-transcoder-mapping-args (mappings)
  "Generate mapping arguments for ffmpeg from MAPPINGS."
  (mapcan (lambda (m)
            (list "-map"
                  (cond ((or (stringp m) (symbolp m)) (format "[%s]" m))
                        ((numberp m) (number-to-string m))
                        ((listp m) (format "%d:%s" (car m) (cadr m)))
                        (t (error (format "Invalid mapping: %s." m))))))
          mappings))

(defun ffmpeg-transcoder-finish-hook (buffer status)
  "A function to call with BUFFER and STATUS when transcoding process finishes."
  (if (string= (buffer-name buffer) "*ffmpeg-transcoder*")
      (let ((fname (with-current-buffer buffer (car (last ffmpeg-command)))))
        (with-current-buffer "*ffmpeg*"
          (let ((status (string-trim status))
                (inhibit-read-only t))
            (if (string= status "finished")
                (let ((length (ffmpeg-file-duration fname)))
                  (ffmpeg-table-append-row "open-files"
                                           (list "x"
                                                 fname
                                                 (timecode-to-string length)
                                                 (current-time-string))))))))))

(add-to-list 'compilation-finish-functions 'ffmpeg-transcoder-finish-hook)

(defun ffmpeg-transcoder-run (output-file)
  "Run ffmpeg to transcode the selected files from the inputs table and save result to OUTPUT-FILE."
  (interactive (list (expand-file-name
                      (read-file-name "Output file:" ffmpeg-transcoder-default-output-dir))))
  (let ((ffmpeg-log-buffer "*ffmpeg-transcoder*"))
    (apply 'ffmpeg-run-command ffmpeg-binary-path
           (append (ffmpeg-transcoder-input-arguments)
                   (ffmpeg-transcoder-filter-arg ffmpeg-transcoder-filter)
                   (ffmpeg-transcoder-mapping-args ffmpeg-transcoder-mappings)
                   (list "-c:a" ffmpeg-transcoder-acodec "-c:v" ffmpeg-transcoder-vcodec
                         (ffmpeg-absolute-path output-file))))))

(defun ffmpeg-transcoder-open-file (filename)
  "Probe an existing video file at FILENAME and add it to thr list of open files."
  (interactive (list (expand-file-name
                      (read-file-name "Input file:" ffmpeg-transcoder-default-output-dir))))
  (with-current-buffer "*ffmpeg*"
    (let ((length (ffmpeg-file-duration (ffmpeg-absolute-path filename)))
          (inhibit-read-only t))
      (if length
          (ffmpeg-table-append-row "open-files" (list "x" filename (timecode-to-string length)))
        (error (format "Could not open %s as a video container." filename))))))

(defun ffmpeg-transcoder-take-frame (timecode fname)
  "Extract a frame from the selected video at a given TIMECODE into FNAME."
  (interactive (list (read-timecode "Timecode: ")
                     (read-file-name "Output file: " ffmpeg-transcoder-default-output-dir)))
  (apply 'ffmpeg-run-command ffmpeg-binary-path
         (append (ffmpeg-transcoder-input-arguments)
                 (append
                  (list "-ss" (number-to-string (timecode-to-seconds timecode)))
                  (ffmpeg-transcoder-filter-arg ffmpeg-transcoder-filter)
                  (ffmpeg-transcoder-mapping-args ffmpeg-transcoder-mappings)
                  (list "-vframes" "1" "-update" "1" (ffmpeg-absolute-path fname))))))

(defun ffmpeg-transcoder-cut (start end output-file)
  "Cut out a part of input material delimited by START and END; save to OUTPUT-FILE."
  (interactive (list (read-timecode "Start: ")
                     (read-timecode "End: ")
                     (read-file-name "Output file: " ffmpeg-transcoder-default-output-dir)))
  (let ((ffmpeg-log-buffer "*ffmpeg-transcoder*"))
    (apply 'ffmpeg-run-command ffmpeg-binary-path
           (append (list "-ss" (timecode-to-string start)
                         "-to" (timecode-to-string end))
                   (ffmpeg-transcoder-input-arguments)
                   (list (ffmpeg-absolute-path output-file))))))

(defun ffmpeg-transcoder-concat (output-file)
  "Concatenate selected streams into the OUTPUT-FILE."
  (interactive (list (read-file-name "Output file: " ffmpeg-transcoder-default-output-dir)))
  (let* ((ffmpeg-log-buffer "*ffmpeg-transcoder*")
         (file-list (apply 'concat
                           (mapcar (lambda (f) (format "file '%s'\n" f))
                                   (ffmpeg-transcoder-input-files))))
         (list-file-name (make-temp-file "emacs-trancoder-inputs-" nil ".lst" file-list)))
    (apply 'ffmpeg-run-command ffmpeg-binary-path
           (list "-f" "concat" "-safe" "0" "-i" list-file-name "-c" "copy" (ffmpeg-absolute-path output-file)))))

(defun ffmpeg-transcoder-load-filter (name)
  "Load previously saved transcoding filter named NAME."
  (interactive (list (completing-read "Filter: " ffmpeg-saved-filters)))
  (ffmpeg-transcoder-edit-filter (cdr (assoc name ffmpeg-saved-filters))))

(defun ffmpeg-transcoder-save-filter (name)
  "Save the current transcoding filter by NAME."
  (interactive "sName: ")
  (let ((slot (cdr (assoc name ffmpeg-saved-filters))))
    (if slot
        (setf slot ffmpeg-transcoder-filter)
      (add-to-list 'ffmpeg-saved-filters (cons name ffmpeg-transcoder-filter)))
    (customize-save-variable 'ffmpeg-saved-filters ffmpeg-saved-filters)))

(defun ffmpeg-transcoder-delete-filter (name)
  "Remove NAME from the list od saved filters."
  (interactive (list (completing-read "Filter: " (mapcar 'car ffmpeg-saved-filters))))
  (customize-save-variable 'ffmpeg-saved-filters
                           (delq (assoc name ffmpeg-saved-filters) ffmpeg-saved-filters)))


(provide 'ffmpeg-transcoder)
;;; ffmpeg-transcoder.el ends here
