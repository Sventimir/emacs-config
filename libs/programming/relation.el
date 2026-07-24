;;; -*- lexical-binding: t -*-
;;; Package --- Relation
;;; Commentary: A package for working with tabular data.
;;; Code:
(require 'dash)

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
      (cond ((keywordp item) (progn
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
    (cons (mapcar (lambda (c) (substring (symbol-name c) 1)) cols)
          (cons 'hline
                (if idx
                    (seq-sort-by (lambda (row) (nth idx row)) (or sorting-f 'string<) (relation-data r))
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

(provide 'relation)
;;; relation.el ends here
