;;; -*- lexical-binding: t -*-
;;; Package --- Ffmpeg-filters
;;; Commentary:
;;; A set of utilities for building complex filters for ffmpeg more conveniently.
;;; Code:

(defmacro screen-coords (left top right bottom)
  "Return spec of FILT filter for a rectangle from (LEFT, TOP) to (RIGHT, BOTTOM)."
  `'((x ,left) (y ,top)
     (w ,(- right left)) (h ,(- bottom top))))

(provide 'ffmpeg-filters)
;;; ffmpeg-filters.el ends here
