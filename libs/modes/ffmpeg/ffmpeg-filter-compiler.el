;;; -*- lexical-binding: t -*-
;;; Package --- Ffmpeg-filters
;;; Commentary:
;;; A set of utilities for building complex filters for ffmpeg more conveniently.
;;; Code:
(require 'seq)
(require 'ffmpeg-filters)

(defun ffmpeg-filter-codegen (expr)
  "Compile EXPR as a sequence of ffmpeg filters with input and output arguments."
  (mapconcat (lambda (subexpr)
               (concat (mapconcat 'ffmpeg-compile-stream (car subexpr) "")
                       (mapconcat 'ffmpeg-compile-filter-expr (butlast (cdr subexpr)) ",")
                       (mapconcat 'ffmpeg-compile-stream (car (last subexpr)) "")))
             expr
             ";"))

(defun ffmpeg-compile-stream (expr)
  "Compile EXPR into a named ffmpeg stream."
  (format "[%s]" (cond ((listp expr) (mapconcat 'ffmpeg-compile-atom expr ":"))
                       (t (ffmpeg-compile-atom expr)))))

(defun ffmpeg-compile-filter-expr (expr)
  "Compile a sexp EXPR into an ffmpeg filters."
  (cond ((listp expr) (ffmpeg-compile-filter-app expr))
        (t (ffmpeg-compile-atom expr))))

(defun ffmpeg-compile-atom (expr)
  "Compile a single atomic EXPR."
  (cond ((numberp expr) (number-to-string expr))
        ((symbolp expr) (symbol-name expr))
        ((stringp expr) expr)))

(defun ffmpeg-compile-filter-app (expr)
  "Compile EXPR as a function (filter) application."
  (let ((filter (ffmpeg-compile-filter-expr (car expr))))
    (if (cdr expr)
        (let ((arglist (seq-reduce 'ffmpeg-compile-filter-arg (cdr expr) (list nil :free))))
          (if (eq :free (cadr arglist))
              (format "%s=%s" filter (mapconcat 'identity (car arglist) ":"))
            filter)))))

(defun ffmpeg-compile-filter-arg (state arg)
  "Compile ARG as a filter argument, considering current STATE."
  (let ((output (car state)))
    (if (eq (cadr state) :free)
        (ffmpeg-compile-free-filter-arg output arg)
      (ffmpeg-compile-bounded-filter-arg output (cadr state) arg))))

(defun ffmpeg-compile-free-filter-arg (output arg)
  "Compile ARG and place it into the OUTPUT either as an argument name or a value."
  (if (symbolp arg)
      (let ((name (symbol-name arg)))
        (if (string-match "^:" name)
            (list output (substring name 1))
          (list (append output (list name)) :free)))
    (list (append output (list (ffmpeg-compile-filter-expr arg))) :free)))

(defun ffmpeg-compile-bounded-filter-arg (output varname arg)
  "Compile ARG and place it into the OUTPUT bound to VARNAME."
  (list (append output (list (format "%s=%s" varname (ffmpeg-compile-filter-expr arg)))) :free))

(defun ffmpeg-filter-eval-macros (expr)
  "Evaluate macros in EXPR before compiling."
  (if (eq (car expr) '\`)
      (eval expr)
    expr))

;;; TESTS:
;; (setq debug-on-error (not debug-on-error))

(defun ffmpeg-assert (expected expr)
  "Assert that EXPR compiled into ffmpeg filter outpus EXPECTED."
  (let ((out (ffmpeg-filter-codegen expr)))
    (unless (string= expected out)
        (error "Test failed: expected: '%s'; got: '%s'" expected out))))

(ffmpeg-assert "crop=30:30:100:200" '((nil (crop 30 30 100 200) nil)))
(ffmpeg-assert "crop=x=30:y=30:w=100:h=200" '((nil (crop :x 30 :y 30 :w 100 :h 200) nil)))
(ffmpeg-assert "[0]overlay=5:15[v]" '(((0) (overlay 5 15) (v))))
(ffmpeg-assert "[0:v:0]crop=x=50:y=50:w=250:h=150,scale=100:50[vid]"
               '((((0 v 0)) (crop :x 50 :y 50 :w 250 :h 150) (scale 100 50) (vid))))
(ffmpeg-assert "[0][1]overlay=x=50:y=50[vid];[2][vid]overlay=0:0[fin]"
               '(((0 1) (overlay :x 50 :y 50) (vid)) ((2 vid) (overlay 0 0) (fin))))
(ffmpeg-assert "[0:v:0]crop=30:30" '(((0:v:0) (crop 30 30) nil)))
(ffmpeg-assert "[0:v:0]crop=x=0:y=0:w=1920:h=1030[scr];[0:v:1]scale=384:216[cam];[scr][cam]overlay=10:804[vid]"
               '(((0:v:0) (crop (x 0) (y 0) (w 1920) (h 1030)) (scr))
                 ((0:v:1) (scale 384 216) (cam))
                 ((scr cam) (overlay 10 804) (vid))))
(ffmpeg-assert "[0:v:0]crop=x=0:y=0:w=1920:h=1030[scr];[0:v:1]scale=384:216[cam];[scr][cam]overlay=10:804[vid]"
               `(((0:v:0) (crop ,@(screen-coords 0 0 1920 1030)) (scr))
                 ((0:v:1) (scale 384 216) (cam))
                 ((scr cam) (overlay 10 804) (vid))))

(provide 'ffmpeg-filter-compiler)
;;; ffmpeg-filter-compiler.el ends here
