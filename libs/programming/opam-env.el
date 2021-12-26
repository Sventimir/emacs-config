;;; opam-env --- configure opam environment
;;; Commentary:
;;; Code:

(defun intercalate (sep &rest nodes)
  "Joins `NODES' into a single `string', putting `SEP' between subsequent elements."
  (let ((acc (car nodes))
        (args (cdr nodes)))
    (if args
        (apply 'make-path (concat acc sep (car args)) (cdr args))
      acc)))

(defun make-path (&rest nodes)
  "Joins `NODES' into a proper file-system path.
Path separator is OS-dependent.  The first node is *not* preceded by
anything; it's programmers responsibility to decide whether path
should be absolute or relative."
  (apply 'intercalate (f-path-separator) nodes))

(defun path-list (&rest paths)
  "Joins `PATHS' into a single string.
Paths are separated with a proper separator (usually ':').  Useful for
manipulating variables like PATH."
  (apply 'intercalate path-separator paths))

(defun set-opam-env (switch-dir)
  "Set environment variables required by `opam'.
`SWITCH-DIR' is a file-system directory, where the opam switch to use
is located.  Roughly equivalent to calling `eval $(opam env)' in the
system shell."
  (let* ((path (getenv "PATH"))
         (manpath (getenv "MANPATH"))
         (vars (cl-pairlis
                '("OPAM_SWITCH_PREFIX"
                  "OCAML_TOPLEVEL_PATH"
                  "PKG_CONFIG_PATH"
                  "PATH"
                  "MANPATH"
                  "CAML_LD_LIBRARY_PATH")
                (list
                 switch-dir
                 (make-path switch-dir "lib" "toplevel")
                 (make-path switch-dir "lib" "pkgconfig")
                 (path-list (make-path switch-dir "bin") path)
                 (path-list (make-path switch-dir "man") manpath)
                 (path-list
                  (make-path switch-dir "lib" "stublibs")
                  (make-path switch-dir "lib" "ocaml" "stublibs")
                  (make-path switch-dir "lib" "ocaml")))
                )))
    (dolist (var vars nil) (setenv (car var) (cdr var)))))

(defun setup-lsp ()
  "Enables use of OCaml language server as installed in OPAM.
`set-opam-env' function should be called before this one is LSP for OCaml
is not installed globally for the whole system."
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ocamllsp"))
    :major-modes '(tuareg-mode)
    :server-id 'ocamllsp)))

(provide 'opam-env)
;;; opam-env.el ends here
