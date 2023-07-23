;;; ocaml-sexp --- Deserialize OCaml S-expressions into Emacs objects.
;;; Commentary:
;;; Code:

(require 'pcase)

(cl-defstruct sexp constructor data)

(defun ocaml-sexp-get (keys struct)
  "Retrieve the value of the nested key sequence KEYS from STRUCT."
  (cond ((null keys) struct)
        ((equal (length struct) 1) (ocaml-sexp-get keys (car struct)))
        (t (ocaml-sexp-get (cdr keys) (cdr (assoc (car keys) struct))))))

(defun ocaml-sexp-alist-keys (alist)
  "Retrieve a list of keys of ALIST."
  (mapcar 'car alist))

(defun ocaml-sexp-alist-data (alist)
  "Retrieve a list of values of ALIST."
  (mapcar 'cdr alist))

(defun ocaml-sexp-pair-list-items (list)
  "Combine subsequent elements of LIST into pairs."
  (if (< (length list) 2)
      list
    (cons (cons (car list) (cadr list)) (ocaml-sexp-pair-list-items (cddr list)))))

(defun ocaml-sexp-read-struct (sexp)
  "Parse SEXP as a struct."
  (mapcar
   (lambda (elem)
     (pcase elem
       (`(,key ,value) (cons key (ocaml-sexp-read value)))
       (_ elem)))
   (car sexp)))

(defun ocaml-sexp-read (sexp)
  "Parse SEXP as an OCaml S-expression."
  (pcase sexp
    (`(,variant ,data)
     (make-sexp :constructor variant :data (ocaml-sexp-read data)))
    (`((,key ,value) . kvs)
     (ocaml-sexp-read-struct sexp))
    (_ sexp)))

(defun field-descr (field)
  "Return the :FIELD symbol."
  (intern (concat ":" (symbol-name field))))

(defun parser-name (type)
  "Return the parser function name for the TYPE."
  (intern (concat "ocaml-" (symbol-name type) "-of-sexp")))

(provide 'ocaml-sexp)
;;; ocaml-sexp.el ends here
