;;; nbp --- A library for getting exchange rates from NBP web pages.
;;; Commentary:
;;; Code:

(require 'request)
(require 'org-ext)

;; A table to cache NBP exchange rates in.
(setq nbp-exchange-rates (make-hash-table))

(defun nbp-put-rate (rate-obj)
  "Extract exchange rate from RATE-OBJ and store it in cache."
  (puthash
   (assoc-default 'effectiveDate rate-obj)
   (assoc-default 'mid rate-obj)
   nbp-exchange-rates))

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

(defun nbp-load-data (currency start-date end-date)
  "Download exchange rates for CURRENCY between START-DATE and END-DATE and put them in the cache."
  (let ((url (format "http://api.nbp.pl/api/exchangerates/rates/A/%s/%s/%s" currency start-date end-date)))
    (request url
      :parser 'json-read
      :complete (cl-function
                 (lambda (&key response &allow-other-keys)
                   (seq-do
                    'nbp-put-rate
                    (assoc-default 'rates (request-response-data response))))))))


(provide 'nbp)
;;; nbp.el ends here
