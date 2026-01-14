;;; -*- lexical-binding: t -*-
;;; Package: FFMPEG timecode
;;; Commentary:
;;; Provides a data structure for storing timecodes.
;;; A time code is a triplet of (SECONDS MINUTES HOURS)
;;; representing a specific moment or a duration of a
;;; video or audio.  Nils are treated as zeros.
;;; Code:

(defun timecode-p (tc)
  "Return TC if it is a timecode; nil otherwise."
  (if (and (listp tc)
           (not (seq-empty-p tc))
           (< (length tc) 4)
           (seq-every-p 'numberp tc ))
      tc
    nil))

(defun timecode-to-seconds (tc)
  "Convert TC to a number opf seconds represented but it."
  (seq-reduce (lambda (acc n)
                (+ (* 60 acc) n))
              tc
              0))

(defun timecode-to-string (tc)
  "Convert TC to its string representation [[HH:]MM:]SS[.S...]."
  (if tc
      (let ((time (reverse tc)))
          (format "%02d:%02d:%011.8f" (or (nth 2 time) 0) (or (nth 1 time) 0) (or (nth 0 time) 0)))
    "0"))

(defun string-to-timecode (s)
  "Convert the string S representing a timecode to the corresponding timecode."
  (string-match "^\\(\\([0-9]\\{1,2\\}:\\)?\\([0-9]\\{1,2\\}:\\)\\)?\\([0-9]+\\(\\.[0-9]+\\)\\)$" s)
  (timecode-normalize
   (mapcan (lambda (idx)
             (let ((start (nth (* 2 idx) (match-data)))
                   (end (nth (1+ (* 2 idx)) (match-data))))
               (if (and start end)
                   (list (string-to-number (substring s start end))))))
           '(2 3 4))))

(defun number-to-timecode (n)
  "Convert the number N to the corresponding timecode."
  (let* ((total (floor n))
         (frac (- n total))
         (tc nil))
    (dotimes (_ 3)
      (if (> total 60)
          (progn
            (add-to-list 'tc (/ total 60))
            (setq total (% total 60)))))
    (add-to-list 'tc total)
    (setf (car tc) (+ (car tc) frac))
    (reverse tc)))

(defun timecode-normalize (tc)
  "Normalize the timecode TC."
  (number-to-timecode (timecode-to-seconds tc)))

(provide 'ffmpeg-timecode)
;;; ffmpeg-timecode.el ends here
