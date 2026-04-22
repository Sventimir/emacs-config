;;; -*- lexical-binding: t -*-
;;; Package --- Ffmpeg-filters
;;; Commentary:
;;; A set of utilities for building complex filters for ffmpeg more conveniently.
;;; Code:
(require 'ffmpeg-timecode)

(defun timecode (tc)
  "Convert string representation of TC into a number of seconds."
  (if (stringp tc)
      (timecode-to-seconds (string-to-timecode tc))
    tc))

(defmacro screen-coords (left top right bottom)
  "Return spec of a filter for a rectangle from (LEFT, TOP) to (RIGHT, BOTTOM)."
  `'(:x ,left :y ,top :w ,(- right left) :h ,(- bottom top)))

(defmacro crop (left top right bottom)
  "Return a spec of a crop filter from (LEFT, TOP) to (RIGHT, BOTTOM)."
  `'(crop :x ,left :y ,top :w ,(- right left) :h ,(- bottom top)))

(defmacro overlay (x y)
  "Return a spec of the overlay macro at (X, Y)."
  `'(overlay :x ,x :y ,y))

(defmacro trim (start end video-in audio-in video-out audio-out)
  "Return specs of trim filter from START to END for streams AUDIO-IN and VIDEO-IN into AUDIO-OUT and VIDEO-OUT."
  (let ((s (if start (list :start (timecode start))))
        (e (if end (list :end (timecode end)))))
    `'(((,video-in) ,(cons 'trim (append s e)) (,video-out))
       ((,audio-in) ,(cons 'atrim (append s e)) (,audio-out)))))

(defmacro cat (&rest streams)
  "Return spec of a filter concatenating STREAMS together; STREAMS should alternate between video and audio."
  `'(,streams (concat :n ,(/ (length streams) 2) :v 1 :a 1) (video audio)))

(defmacro sync-audio (seconds)
  "Return spec of a filter moving an audio stream by SECONDS (backwards if negative)."
  `'(asetpts ,(format "PTS+%i/TB" (timecode seconds))))

(provide 'ffmpeg-filters)
;;; ffmpeg-filters.el ends here
