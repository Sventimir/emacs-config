;;; nbp --- A library for getting exchange rates from NBP web pages.
;;; Commentary:
;;; Code:

(require 'request)
(require 'org-ext)

;; A table to cache NBP exchange rates in.
(setq nbp-exchange-rates (make-hash-table)
      nbp-loaded-dates nil)

(defcustom nbp-currency "USD"
  "The currency to donwload exchange rates for."
  :type 'string
  :group 'nbp)

(defun nbp-put-rate (rate-obj)
  "Extract exchange rate from RATE-OBJ and store it in cache."
  (let ((date (assoc-default 'effectiveDate rate-obj)))
    (progn
      (puthash date (assoc-default 'mid rate-obj) nbp-exchange-rates)
      date)))

(defun unconses-intern (f args list fs)
  "Call F from FS with ARGS from LIST."
  (if (and (car list) (car fs))
      (unconses-intern (car fs) (cons (car list) args) (cdr list) (cdr fs))
      (funcall f args)))

(defun unconses (list &rest fs)
  "Call a function from FS on the LIST.
Nth function in FS should accept N+1 arguments and subsequent list elements
will be fed to the appropriate function with the tail as the last argument."
  (unconses-intern (car fs) nil list (cdr fs)))

(defun nbp-greatest (cmp list)
  "Select the greatest element from LIST according to CMP function."
  (if (cdr list)
      (let ((fst (car list))
            (snd (car (cdr list)))
            (rem (cdr (cdr list))))
        (if (funcall cmp fst snd)
            (nbp-greatest cmp (cons fst rem))
            (nbp-greatest cmp (cons snd rem))))
      (car list)))

(defun nbp-load-data (start-date end-date)
  "Download exchange rates for NBP-CURRENCY between START-DATE and END-DATE and put them in the cache."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (url (format "http://api.nbp.pl/api/exchangerates/rates/A/%s/%s/%s"
                      nbp-currency
                      (nbp-greatest 'string< (list start-date today))
                      (nbp-greatest 'string< (list end-date today)))))
    (progn
      (message "Loading NBP exchange rates from %s to %s..." start-date end-date)
      (request url
        :parser 'json-read
        :complete (cl-function
                   (lambda (&key response &allow-other-keys)
                     (let* ((updated (mapcar
                                      'nbp-put-rate
                                      (assoc-default 'rates (request-response-data response))))
                            (sorted (sort updated 'string<)))
                       (progn
                         (setq nbp-loaded-dates
                               (cons
                                (nbp-greatest
                                 'string<
                                 (seq-filter 'identity (list (car sorted) (car nbp-loaded-dates))))
                                (nbp-greatest
                                 'string>
                                 (seq-filter 'identity (list (car (last sorted)) (cdr nbp-loaded-dates))))))
                         (message "Succesfully loaded NBP rates for %s from %s to %s."
                                  nbp-currency
                                  (car nbp-loaded-dates)
                                  (cdr nbp-loaded-dates)))))))))
)

(defun nbp-read-org-timestamp (ts)
  "Return date represented by TS as NBP-acceptable string."
  (cond ((stringp ts) (substring ts 1 11))
        ((arrayp ts) (substring (dbus-byte-array-to-string ts) 1 11))
        (t nil)))

(defun nbp-date-gte (d1 d2)
  "Check if D1 is later or equal to D2."
  (or (string= d1 d2) (string> d1 d2)))

(defun nbp-date-lte (d1 d2)
  "Check if D1 is earlier or equal to D2."
  (or (string= d1 d2) (string< d1 d2)))

(defun nbp-format-date (date)
  "Format the DATE as %Y-%m-%d."
  (format "%04d-%02d-%02d"
          (decoded-time-year date)
          (decoded-time-month date)
          (decoded-time-day date)))

(defun nbp-add-weeks (date &optional weeks)
  "Add WEEKS weeks to DATE."
  (nbp-format-date
   (decoded-time-add
    (parse-time-string date)
    (make-decoded-time :day (* 7 (or weeks 1))))))

(defun nbp-find-latest-before (date dates)
  "Find the latest date before DATE among DATES.
Assume DATES are sorted in ascending order."
  (let ((current (car dates))
        (next (car (cdr dates)))
        (rest (cdr dates)))
    (cond ((string< next date) (nbp-find-latest-before date rest))
          ((string> current date) nil)
          (t current))))

(defun nbp-find-exchange-rate (date)
  "Find the exchange rate at DATE.
If not published, check the latest exchange rate published before given date."
  (let ((dates (sort (hash-table-keys nbp-exchange-rates) 'string<)))
    (gethash (nbp-find-latest-before date dates) nbp-exchange-rates)))

(defun nbp-extreme-dates ()
  "Read the table at point and find earliest and latest date."
  (let* ((tbl (flatten-list (org-table-to-lisp)))
         (dates (sort
                 (mapcar 'dbus-byte-array-to-string
                         (seq-filter (lambda (s)
                                       (and
                                        (stringp s)
                                        (numberp (string-match "<[0-9]+-[0-9]+-[0-9]+ [A-Z][a-z]+>" s))))
                                     tbl))
                 'string<)))
    (list
     (nbp-read-org-timestamp (car dates))
     (nbp-read-org-timestamp (car (last dates))))))

(defun nbp-load-data-for-table ()
  "Load NBP data for date range present in the table at point."
  (interactive)
  (apply 'nbp-load-data (nbp-extreme-dates)))

(define-key org-mode-map (kbd "C-c C-n") 'nbp-load-data-for-table)

(provide 'nbp)
;;; nbp.el ends here
