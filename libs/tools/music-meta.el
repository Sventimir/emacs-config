;;; Package --- Summary
;;; Commentary:
;;; Code:

(require 'dired)

(defvar music-meta-default-artist "")

(defun music-meta-proc-sentinel (process event)
  "Sentinel for EVENTs in the music metadata PROCESS."
  (with-current-buffer (process-buffer process)
    (goto-char 0)))

(defun music-meta-show (filename)
  "Show the metadata of the music file FILENAME."
  (save-selected-window
    (pop-to-buffer "*id3v2*")
    (erase-buffer)
    (goto-char 0)
    (make-process
     :name "id3v2"
     :buffer "*id3v2*"
     :command (list "id3v2" "-l" filename)
     :sentinel 'music-meta-proc-sentinel)))

(defun music-meta-set (artist title filename)
  "Set the ARTIST and TITLE of the music file FILENAME."
  (save-selected-window
    (pop-to-buffer "*id3v2*")
    (erase-buffer)
    (make-process
     :name "id3v2"
     :buffer "*id3v2*"
     :command (list "id3v2" "-a" artist "-t" title filename)
     :sentinel 'music-meta-proc-sentinel)))


(define-prefix-command 'music-meta-map)
(define-key dired-mode-map (kbd "C-t") 'music-meta-map)

(define-key music-meta-map (kbd "m") (lambda ()
                                         (interactive)
                                         (music-meta-show (dired-get-filename))))
(define-key music-meta-map (kbd "s") (lambda ()
                                         (interactive)
                                         (let ((artist (read-string "Artist: " music-meta-default-artist))
                                               (title (read-string "Title: ")))
                                           (setq music-meta-default-artist artist)
                                           (music-meta-set artist title (dired-get-filename)))))

(provide 'music-meta)
;;; music-meta.el ends here
