;;; -*- lexical-binding: t -*-
;;; Package --- Relation
;;; Commentary: A package for working with tabular data.
;;; Code:
(require 'dash)
(require 's)

(cl-defstruct relation (columns nil :type 'list :readonly t)
                       (key nil :type 'list :readonly t)
                       (data nil :type 'list :readonly t))

(defun rel-check-cols (&rest cols)
  "Return the list of COLS if there are no duplicates."
  (if (= (length (seq-uniq cols)) (length cols))
      cols
    (signal 'error '(relation duplicate-columns))))

(defun rel-empty (&rest columns)
  "Build an empty relation with given COLUMNS."
  (let ((cols (apply 'rel-check-cols columns)))
      (make-relation :columns cols :key (car cols))))

(defun rel-singleton (&rest contents)
  "Create a relation with a single row CONTENTS where keys define column names."
  (let ((last 'datum)
        (columns nil)
        (data nil))
    (dolist (item contents)
      (cond ((symbolp item) (progn
                              (push item columns)
                              (if (eq 'col last) (push nil data))
                              (setq last 'col)))
            ((eq 'col last) (progn
                              (push item data)
                              (setq last 'datum)))
            (t (error "Datum (%s) for unspecified columns!" item))))
    (let* ((cols (reverse (apply 'rel-check-cols columns)))
           (d (reverse (if (= (length data) (length cols)) data (cons nil data)))))
      (make-relation :columns cols :key (car cols) :data (list d)))))

(defun rel-unary (col &rest values)
  "Create a relation with single column COL and VALUES as singleton rows."
  (make-relation :columns (list col) :key col :data (mapcar 'list values)))

(defun rel-from-rows (columns key &rest rows)
  "Construct a relation with COLUMNS and KEY consisting of ROWS."
  (let ((l (length (apply 'rel-check-cols columns))))
    (cond ((not (cl-position key columns)) (signal 'error (list 'rel-from-rows 'invalid-key key)))
          ((-any-p (lambda (r) (/= l (length r))) rows) (signal 'error '(rel-from-rows invalid-row)))
          (t (make-relation :columns columns :key key :data rows)))))

(defun rel-to-org-table (r &optional sorting-col sorting-f)
  "Convert R to an \"org-mode\" table format, sorting on SORTING-COL with SORTING-F."
  (let* ((cols (relation-columns r))
         (sort (or sorting-col (relation-key r)))
         (idx (cl-position sort cols)))
    (cons (mapcar (lambda (c) (string-trim-left (symbol-name c) ":")) cols)
          (cons 'hline
                (if idx
                    (seq-sort-by (lambda (row) (nth idx row)) (or sorting-f 'rel<) (relation-data r))
                  (relation-data r))))))

(defun rel-columns (r)
  "Return the normalized (sorted) list of columns of R."
  (sort (relation-columns r)))

(defun rel-union (&rest rels)
  "Return the union of RELS provided they have the same set of columns and keys."
  (if (null rels) (make-relation)
    (let ((cols (rel-columns (car rels)))
          (key (relation-key (car rels))))
      (if (-all-p (lambda (r) (and (equal key (relation-key r)) (equal cols (rel-columns r)))) (cdr rels))
          (make-relation :columns cols :key key
                         :data (apply 'append (mapcar 'relation-data rels)))
        (signal 'error '('rel-union 'columns-do-not-match))))))

(defun rel-times2 (r1 r2)
  "Return the carthesian product of R1 and R2."
  (let ((cls1 (relation-columns r1))
        (cls2 (relation-columns r2)))
    (let ((cols (apply 'rel-check-cols (append cls1 cls2)))
          (data (mapcan
                 (lambda (r1)
                   (mapcar (lambda (r2) (append r1 r2)) (relation-data r2)))
                 (relation-data r1))))
      (make-relation :columns cols :key (car cols) :data data))))

(defun rel-times (&rest rs)
  "Return the carthesian product of RS."
  (seq-reduce 'rel-times2 (cdr rs) (car rs)))

(defun rel-column (col r)
  "Return just a single column COL from R as a list."
  (let ((idx (cl-position col (relation-columns r))))
    (if idx
        (mapcar (lambda (row) (nth idx row)) (relation-data r)))))

(defun rel-project (r &rest columns)
  "Remove from R all columns but COLUMNS."
  (let ((cols (seq-uniq columns))
        (data (mapcar (lambda (_) nil) (relation-data r))))
    (dolist (c cols (make-relation :columns cols :key (car cols) :data data))
      (setq data (-zip-with (lambda (row v) (append row (list v))) data (rel-column c r))))))

(defun rel-replace (r col f &rest columns)
  "Add or replace COL in R by applying F to COLUMNS on each row."
  (let* ((orig-cols (relation-columns r))
         (drop-col (cl-position col orig-cols))
         (cols (seq-uniq (cons col (relation-columns r))))
         (is (mapcar (lambda (c) (cl-position c (relation-columns r))) columns))
         (data (if drop-col
                   (mapcar (lambda (row) (append (take drop-col row) (drop (1+ drop-col) row))) (relation-data r))
                 (relation-data r))))
    (if (-all-p 'identity is)
        (make-relation :columns cols :key (relation-key r)
                       :data (mapcar
                              (lambda (row) (cons (apply f (mapcar (lambda (i) (nth i row)) is)) row))
                              data))
      (signal 'error '(rel-replace unknown-argument-col)))))

(defun rel-filter (r f &rest cols)
  "Filter R, preserving rows where F applied to COLS is non-nil."
  (let ((is (mapcar (lambda (c) (cl-position c (relation-columns r))) cols)))
    (make-relation :columns (relation-columns r) :key (relation-key r)
                   :data (seq-filter (lambda (row)
                                       (apply f (mapcar (lambda (i) (nth i row)) is)))
                                     (relation-data r)))))

(defun rel< (a b)
  "Compare A and B accordiongly to their type."
  (cond ((stringp a) (string< a b))
        ((numberp a) (< a b))
        (t (signal 'error `(invalid-argument 'comparison ,a ,b)))))

(defun rel-sql-interpret (expr)
  "The entrypoint to rel-sql interpreter, converting EXPR into a rel operation."
  (let ((cmd (intern-soft (s-concat "rel-sql-" (symbol-name (car expr))))))
    (eval (cons cmd (cdr expr)))))

(defmacro rel-sql-where (rel-expr filter-expr)
  "Generate code to filter REL-EXPR using FILTER-EXPR."
  (let* ((rel (eval rel-expr))
         (cols (rel-columns rel)))
      `(rel-filter ,rel
                   (lambda ,(mapcar (lambda (s) (intern (string-trim (symbol-name s) ":"))) cols)
                     ,(rel-sql-rec-normalize-infix-op filter-expr))
                     ,@cols)))

(defmacro rel-sql-select (&rest args)
  "Interpret the SEELCT expression consisting of ARGS as rel-sql.
Walk through ARGS until from symbol is found; then everything before from
becomes a column to project, while everything after becomes a relation
expression to evaluate."
  (let* ((from-idx (seq-position args 'from))
         (where-idx (seq-position args 'where))
         (cols (take from-idx args))
         (rel-expr (rel-sql-interpret (car (drop (1+ from-idx) args))))
         (filt-expr (if where-idx
                        `(rel-sql-where ,rel-expr ,(drop (1+ where-idx) args))
                      rel-expr)))
    `(rel-project ,filt-expr ,@(mapcar 'rel-sql-quote-column cols))))

(defmacro rel-sql-singleton (&rest args)
  "A rel-sql wrapper calling rel-singleton with columns quoted in ARGS."
  `(rel-singleton ,@(mapcar (lambda (arg) (if (symbolp arg) (rel-sql-quote-column arg) arg)) args)))

(defalias 'rel-sql-unary 'rel-unary)

(defmacro rel-sql-product (&rest exprs)
  "Translate EXPRS to relations and compute their product."
  `(rel-times ,@(mapcar 'rel-sql-interpret exprs)))

(defun rel-sql-quote-column (colname)
  "Quote the column name COLNAME so that it does nor act as a Lisp variable."
  (intern (s-concat ":" (symbol-name colname))))

(defun rel-sql-normalize-infix-op (expr)
  "Rearrange symbols in EXPR so that they form a valid Lisp function call."
  (if (and (symbolp (cadr expr)) (symbol-function (cadr expr)))
      (cons (cadr expr) (cons (car expr) (cddr expr)))
    expr))

(defun rel-sql-rec-normalize-infix-op (expr)
  "Recursively apply rel-sql-normalize-infix-op to EXPR and its subexpressions."
  (if (listp expr)
      (mapcar 'rel-sql-rec-normalize-infix-op (rel-sql-normalize-infix-op expr))
    expr))

(defun org-babel-execute:rel-sql (body params)
  "Interpret an SQL-like Lisp in BODY describing a relation, using PARAMS."
  (let ((expr (read (format "(%s)" body))))
    (rel-to-org-table (rel-sql-interpret expr))))

(provide 'relation)
;;; relation.el ends here
