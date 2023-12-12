;;; bridge --- A library for computing bridge scores.
;;; Commentary:
;;; Code:

(cl-defstruct bridge-contract level suit declarer double)

(cl-defstruct bridge-result vulnerable contract tricks)

(defun bridge--score-contract-made (contract vulnerable overtricks)
  "Compute the score for CONTRACT made at VULNERABLE with OVERTRICKS."
  (let* ((level (bridge-contract-level contract))
         (dbl (bridge-contract-double contract))
         (dbl-multiplier (pcase dbl
                           ('() 1)
                           ('dbl 2)
                           ('rdbl 4)))
         (trick-score (pcase (bridge-contract-suit contract)
                        ('nt 30)
                        ('spades 30)
                        ('hearts 30)
                        ('diamonds 20)
                        ('clubs 20)))
         (base (* dbl-multiplier (+ (pcase (bridge-contract-suit contract) ('nt 10) (_ 0)) (* trick-score level))))
         (game-score (if (>= base 100) (if vulnerable 500 300) 50))
         (slam-score (pcase level
                       (6 (if vulnerable 750 500))
                       (7 (if vulnerable 1500 1000))
                       (_ 0)))
         (dbl-score (pcase dbl
                      ('() 0)
                      ('dbl 50)
                      ('rdbl 100)))
         (overtrick-score (pcase dbl
                            ('() (* overtricks trick-score))
                            ('dbl (* overtricks 100))
                            ('rdbl (* overtricks 200)))))
    (+ base game-score slam-score dbl-score overtrick-score)))

(defun bridge--score-undertricks-doubled (vulnerable tricks)
  "Compute the score for VULNERABLE undertricks with TRICKS doubled.
Assume TRICKS is greater(!) than 0."
  (if vulnerable
      (- (* 300 tricks) 100)
    (- (* 300 tricks) 100 (* (min tricks 3) 100))))

(defun bridge--score-contract-down (vulnerability tricks double)
  "Compute undertricks score for TRICKS, VULNERABILITY and DOUBLE."
  (pcase double
    ('() (* tricks (if vulnerability 50 100)))
    ('dbl (bridge--score-undertricks-doubled vulnerability tricks))
    ('rdbl (* 2 (bridge--score-undertricks-doubled vulnerability tricks)))))

(defun bridge-score (result)
  "Compute the score for NS pair for RESULT."
  (let* ((tricks (bridge-result-tricks result))
        (scr (if (>= tricks 0)
                 (bridge--score-contract-made
                  (bridge-result-contract result)
                  (bridge-result-vulnerable result)
                  (bridge-result-tricks result))
               (- (bridge--score-contract-down
                   (bridge-result-vulnerable result)
                   (- tricks)
                   (bridge-contract-double (bridge-result-contract result)))))))
    (pcase (bridge-contract-declarer (bridge-result-contract result))
      ('north scr)
      ('south scr)
      ('east (- scr))
      ('west (- scr)))))

(defmacro bridge--bind-parser (state &rest parsers)
  "Sequentially bind PARSERS to STATE."
  (if (null parsers)
      state
    `(let ((arg (car ,state))
           (str (cadr ,state))
           (p (car ,parsers)))
       (bridge--bind-parser
        (funcall p arg str)
        ,@(cdr parsers)))))

(defun bridge--parse-level (contract str)
  "Parse the level of CONTRACT from STR."
  (let ((level (string-to-number (substring str 0 1))))
    (list
     (setf (bridge-contract-level contract) level)
     (substring str 1))))

(defun bridge--parse-suit (contract str)
  "Parse the suit of CONTRACT from STR."
  (let ((suit (substring str 0 1)))
    (list
     (setf (bridge-contract-suit contract) suit)
     (substring str 1))))

(defun bridge-parse-contract (contract-str)
  "Parse CONTRACT-STR into a bridge-contract struct."
  (bridge--bind-parser
   (list
    (make-bridge-contract)
    contract-str)
   '(bridge--parse-level
     bridge--parse-suit
     bridge--parse-declarer
     bridge--parse-double)))

(provide 'bridge)
;;; bridge.el ends here
