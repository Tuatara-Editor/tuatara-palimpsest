;;;; figures.lisp --- Numbers in a page's text that carry their own check, and totals that must add up.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A figure changed by hand seldom comes with every number that depends on
;;;; it changed too. Some numbers carry a check digit worked out from the
;;;; others: a Swedish organisation or personal identity number, a bankgiro
;;;; number and an OCR payment reference (the modulus 10 method), an IBAN
;;;; (ISO 13616, modulus 97). And an invoice's net and VAT add up to its total.
;;;; A number is taken only where its label stands right before it and it is
;;;; written as such numbers are, so that a number that merely looks like one
;;;; is passed over. Sums of an invoice's lines are not added up.

(in-package #:palimpsest)

(defparameter *check-number-labels*
  '((:organisation "organisationsnummer" "organisationsnr" "organisation number" "organization number"
     "organisation no" "organization no" "org.nummer" "org nummer" "org.nr" "org. nr" "org nr" "org-nr" "orgnr"
     "org.no" "org. no" "org no")
    (:personal "personnummer" "personnr" "pers.nr" "pers. nr" "personal identity number" "samordningsnummer")
    (:bankgiro "bankgironummer" "bankgironr" "bankgiro" "bg-nr" "bg.nr" "bg nr" "bg")
    (:ocr "ocr-referens" "ocr-nummer" "ocrnummer" "ocr-nr" "ocr nr" "ocr")
    (:iban "iban"))
  "The labels a number carrying a check digit is taken after, by kind, lower
case, the longest first: :ORGANISATION, a Swedish organisation number;
:PERSONAL, a Swedish personal identity or coordination number; :BANKGIRO, a
bankgiro number; :OCR, an OCR payment reference; :IBAN, an international
bank account number.")

(defparameter *check-number-fillers* '("nummer" "number" "referens" "reference" "nr" "no" "ref")
  "Words that may stand between a label of *CHECK-NUMBER-LABELS* and its
number (\"OCR-referens nr: 1234\"), beside spaces and the marks : . # - ( ) /.")

(defparameter *iban-lengths*
  '(("AD" . 24) ("AE" . 23) ("AL" . 28) ("AT" . 20) ("AZ" . 28) ("BA" . 20) ("BE" . 16) ("BG" . 22) ("BH" . 22)
    ("BR" . 29) ("CH" . 21) ("CR" . 22) ("CY" . 28) ("CZ" . 24) ("DE" . 22) ("DK" . 18) ("DO" . 28) ("EE" . 20)
    ("ES" . 24) ("FI" . 18) ("FO" . 18) ("FR" . 27) ("GB" . 22) ("GE" . 22) ("GI" . 23) ("GL" . 18) ("GR" . 27)
    ("GT" . 28) ("HR" . 21) ("HU" . 28) ("IE" . 22) ("IL" . 23) ("IS" . 26) ("IT" . 27) ("JO" . 30) ("KW" . 30)
    ("KZ" . 20) ("LB" . 28) ("LI" . 21) ("LT" . 20) ("LU" . 20) ("LV" . 21) ("MC" . 27) ("MD" . 24) ("ME" . 22)
    ("MK" . 19) ("MR" . 27) ("MT" . 31) ("MU" . 30) ("NL" . 18) ("NO" . 15) ("PK" . 24) ("PL" . 28) ("PS" . 29)
    ("PT" . 25) ("QA" . 29) ("RO" . 24) ("RS" . 22) ("SA" . 24) ("SE" . 24) ("SI" . 19) ("SK" . 24) ("SM" . 27)
    ("TN" . 24) ("TR" . 26) ("UA" . 29) ("VG" . 24) ("XK" . 20))
  "The length of an IBAN in each country that uses one, by its two letters
\(ISO 13616's register). An IBAN of a country not listed is passed over.")

(defun figure-digit-p (char)
  "True when CHAR is one of the digits 0 to 9."
  (char<= #\0 char #\9))

(defun luhn-valid-p (digits)
  "True when DIGITS, a string of digits, ends in the right check digit by the
modulus 10 method (Luhn's): from the right, every second digit before the
check digit doubled, the digits of the products added, and the sum with the
check digit a multiple of 10."
  (zerop (mod (loop for i from (1- (length digits)) downto 0
                    for double = nil then (not double)
                    for d = (digit-char-p (char digits i))
                    sum (if double (multiple-value-bind (tens ones) (floor (* 2 d) 10) (+ tens ones)) d))
              10)))

(defun luhn-check-digit (digits)
  "The check digit DIGITS, a string of digits ending in one, should end in by
the modulus 10 method (LUHN-VALID-P), as a character."
  (loop for c across "0123456789"
        when (luhn-valid-p (concatenate 'string (subseq digits 0 (1- (length digits))) (string c)))
          return c))

(defun iban-valid-p (iban)
  "True when IBAN, its letters upper case and no spaces, gives remainder 1
when its first four characters are moved to its end, each letter written as
a number from 10 (A) to 35 (Z), and the whole taken modulo 97 (ISO 13616)."
  (let ((rest 0))
    (loop for c across (concatenate 'string (subseq iban 4) (subseq iban 0 4))
          do (let ((value (digit-char-p c 36)))
               (setf rest (mod (+ (* rest (if (< value 10) 10 100)) value) 97))))
    (= rest 1)))

(defun personal-date-p (digits)
  "True when the first six of DIGITS, a Swedish personal identity number's ten
digits, are a date that can be: year, month and day, the day raised by 60
for a coordination number."
  (let ((month (parse-integer digits :start 2 :end 4)) (day (parse-integer digits :start 4 :end 6)))
    (when (> day 60) (decf day 60))
    (and (<= 1 month 12) (<= 1 day (nth (1- month) '(31 29 31 30 31 30 31 31 30 31 30 31))))))

(defun digits-run (text start)
  "The digits of TEXT from START on, and where they end."
  (let ((end (or (position-if-not #'figure-digit-p text :start start) (length text))))
    (values (subseq text start end) end)))

(defun figure-ends-p (text end)
  "True when a number of TEXT ending at END ends there: what follows is not a
digit, nor a mark that runs it on into a date or an amount."
  (or (>= end (length text))
      (not (or (figure-digit-p (char text end))
               (and (find (char text end) "-/.,:") (< (1+ end) (length text))
                    (figure-digit-p (char text (1+ end))))))))

(defun dashed-number (text start before after &optional (marks "-"))
  "The digits of a number written at START of TEXT as BEFORE digits, one of
MARKS and AFTER digits, or as BEFORE + AFTER digits together, as two values:
the digits and where the number ends; NIL when it is not so written."
  (multiple-value-bind (first end) (digits-run text start)
    (cond ((and (= (length first) (+ before after)) (figure-ends-p text end))
           (values first end))
          ((and (= (length first) before) (< end (length text)) (find (char text end) marks))
           (multiple-value-bind (second last) (digits-run text (1+ end))
             (and (= (length second) after) (figure-ends-p text last)
                  (values (concatenate 'string first second) last)))))))

(defun grouped-digits (text start most)
  "The digits written at START of TEXT in groups a single space apart (not
a tab, which stands for a gap between columns, FIGURE-LINE), at most MOST
of them, as two values: the list of groups and where they end; NIL when
the last group does not end the number (FIGURE-ENDS-P), or is followed by
a gap between columns and more digits, which may be the number's rest."
  (let ((groups '()) (at start) (count 0))
    (loop (multiple-value-bind (group end) (digits-run text at)
            (when (or (zerop (length group)) (> (+ count (length group)) most)) (return))
            (push group groups)
            (incf count (length group))
            (setf at end)
            (unless (and (< (1+ at) (length text)) (char= (char text at) #\Space)
                         (figure-digit-p (char text (1+ at))))
              (return))
            (incf at)))
    (let ((end (+ start (reduce #'+ groups :key #'length) (max 0 (1- (length groups))))))
      (and groups (figure-ends-p text end)
           (not (and (< (1+ end) (length text)) (char= (char text end) #\Tab)
                     (figure-digit-p (char text (1+ end)))))
           (values (nreverse groups) end)))))

(defun check-number-at (kind text start)
  "The number of KIND (*CHECK-NUMBER-LABELS*) written at START of TEXT, the
page's line as written, as three values: its digits (or characters, for an
IBAN), whether its check holds, and where it ends; NIL when what is written
there is not such a number. A grouped OCR reference is taken as one only
when no run of its first groups checks alone, which would be a reference
followed by another number."
  (ecase kind
    (:organisation
     (multiple-value-bind (digits end) (dashed-number text start 6 4)
       (and digits (values digits (luhn-valid-p digits) end))))
    (:personal
     (multiple-value-bind (digits end) (dashed-number text start 6 4 "-+")
       (unless digits
         (multiple-value-setq (digits end) (dashed-number text start 8 4 "-+"))
         (when digits (setf digits (subseq digits 2))))
       (and digits (personal-date-p digits) (values digits (luhn-valid-p digits) end))))
    (:bankgiro
     (multiple-value-bind (digits end) (dashed-number text start 3 4)
       (unless digits (multiple-value-setq (digits end) (dashed-number text start 4 4)))
       (and digits (values digits (luhn-valid-p digits) end))))
    (:ocr
     (multiple-value-bind (groups end) (grouped-digits text start 25)
       (let ((digits (and groups (format nil "~{~a~}" groups))))
         (when (and digits (<= 4 (length digits)))
           (if (luhn-valid-p digits)
               (values digits t end)
               (unless (loop for k from 1 below (length groups)
                             thereis (luhn-valid-p (format nil "~{~a~}" (subseq groups 0 k))))
                 (values digits nil end)))))))
    (:iban
     (let* ((country (and (<= (+ start 4) (length text)) (subseq text start (+ start 2))))
            (length (cdr (assoc country *iban-lengths* :test #'string=))))
       (when (and length (every #'figure-digit-p (subseq text (+ start 2) (+ start 4))))
         (let ((out (make-string-output-stream)) (count 0) (at start))
           (loop while (and (< at (length text)) (< count length))
                 do (let ((c (char text at)))
                      (cond ((or (figure-digit-p c) (char<= #\A c #\Z)) (write-char c out) (incf count))
                            ((not (and (char= c #\Space) (< (1+ at) (length text))
                                       (alphanumericp (char text (1+ at)))))
                             (return))))
                    (incf at))
           (when (and (= count length) (or (>= at (length text)) (not (alphanumericp (char text at)))))
             (let ((iban (get-output-stream-string out)))
               (values iban (iban-valid-p iban) at)))))))))

(defun label-end (lower at label &optional (fillers *check-number-fillers*))
  "Where the number after LABEL, found at AT of LOWER (a line in lower case),
may begin: past the label, the marks and the words of FILLERS
\(*CHECK-NUMBER-FILLERS* when not given) after it; NIL when LABEL is part
of a longer word."
  (let ((end (+ at (length label))))
    (unless (or (and (plusp at) (alphanumericp (char lower (1- at))))
                (and (< end (length lower)) (alpha-char-p (char lower end))))
      (loop repeat 6
            do (setf end (or (position-if-not (lambda (c) (find c #.(format nil " ~c:.#-()/" #\Tab))) lower :start end)
                             (length lower)))
               (let ((word (find-if (lambda (w)
                                      (let ((w-end (+ end (length w))))
                                        (and (<= w-end (length lower)) (string= w lower :start2 end :end2 w-end)
                                             (or (= w-end (length lower)) (not (alpha-char-p (char lower w-end)))))))
                                    fillers)))
                 (if word (incf end (length word)) (return))))
      end)))

(defun check-numbers-in (text &optional below)
  "The numbers carrying a check digit in TEXT, a page's line as FIGURE-LINE
makes it, each (KIND NUMBER VALID START BELOW-P): KIND of
*CHECK-NUMBER-LABELS*, NUMBER as CHECK-NUMBER-AT gives it, VALID true when
its check holds, START where its label begins, and BELOW-P true when the
number was read from the next line. BELOW, given the place of a label that
ends TEXT, gives the next line and where in it the number may begin
\(TEXT-BELOW), or NIL. A Swedish VAT number (SE, the ten digits of an organisation number,
01) is taken without a label."
  (let ((lower (string-downcase text)) (out '()))
    (loop for (kind . labels) in *check-number-labels*
          do (dolist (label labels)
               (loop for at = (search label lower) then (search label lower :start2 (1+ at))
                     while at
                     do (let* ((from (label-end lower at label))
                               (next nil) (start nil))
                          (when (and from (= from (length text)) below)
                            (multiple-value-setq (next start) (funcall below at)))
                          (multiple-value-bind (number valid)
                              (cond (start (check-number-at kind next start))
                                    ((and from (< from (length text))) (check-number-at kind text from)))
                            (when (and number (not (find from out :key #'sixth)))
                              (push (list kind number valid at (and start t) from) out)))))))
    (loop for at = (search "SE" text) then (search "SE" text :start2 (1+ at))
          while at
          do (multiple-value-bind (digits end) (digits-run text (+ at 2))
               (when (and (= (length digits) 12) (uiop:string-suffix-p digits "01") (figure-ends-p text end)
                          (or (zerop at) (not (alphanumericp (char text (1- at))))))
                 (let ((number (subseq digits 0 10)))
                   (unless (find number out :key #'second :test #'equal)
                     (push (list :organisation number (luhn-valid-p number) at nil (+ at 2)) out))))))
    (mapcar (lambda (entry) (subseq entry 0 5)) (sort out #'< :key #'fourth))))

(defun check-number-words (kind number)
  "A number of KIND whose check fails, NUMBER as CHECK-NUMBER-AT gives it,
in words with what its check gives."
  (ecase kind
    (:organisation (format nil "the organisation number ~a-~a, whose check digit would be ~a"
                           (subseq number 0 6) (subseq number 6) (luhn-check-digit number)))
    (:personal (format nil "the personal identity number ~a-~a, whose check digit would be ~a"
                       (subseq number 0 6) (subseq number 6) (luhn-check-digit number)))
    (:bankgiro (format nil "the bankgiro number ~a-~a, whose check digit would be ~a"
                       (subseq number 0 (- (length number) 4)) (subseq number (- (length number) 4))
                       (luhn-check-digit number)))
    (:ocr (format nil "the OCR reference ~a, whose check digit would be ~a" number (luhn-check-digit number)))
    (:iban (format nil "the IBAN ~a, which does not give remainder 1 modulo 97" number))))

;;; Totals

(defparameter *amount-roles*
  '((:rounding "öresavrundning" "öresutjämning" "avrundning" "rounding")
    (:vat "ingår med" "varav moms" "därav moms")
    (:net "momsunderlag" "underlag" "exkl" "excl" "excluding" "ex moms" "ex. moms" "före moms" "ex vat" "ex. vat"
     "before vat")
    (:total "inkl" "incl" "including" "ingår" "efter moms")
    (:vat "mervärdesskatt" "moms" "vat")
    (:net "netto" "net amount" "net total")
    (:total "att betala" "amount due" "balance due" "to pay" "totalbelopp" "slutsumma" "totalt" "total" "summa"))
  "The words a line's label is read by, each group in turn, the first group
whose word stands in the label giving what its amount is: :NET, the amount
before VAT; :VAT, the VAT; :TOTAL, the amount with VAT; :ROUNDING, the öre
rounded away. \"Net\" alone is :NET too. A subtotal is not taken for the
net, as fees may follow it.")

(defparameter *amount-not-money* '("vikt" "weight" "antal" "qty" "quantity" "timmar" "hours" "kg" "pris/st" "per st"
                                   "rabatt" "discount" "betalt" "paid" "tidigare" "previous")
  "Words in a line's label that say its amount is not the net, VAT or total
of the page: a weight, a count, hours, a discount, a sum paid before.")

(defparameter *currency-words* '("kr" "kr." "sek" "eur" "usd" "gbp" "nok" "dkk" "€" "$" "£")
  "What may stand before or after an amount: its currency.")

(defun amount-at-end (text)
  "The amount TEXT, a line as written, ends in, as three values: the amount
in hundredths, as written, and where it begins; NIL when it ends in no
amount. An amount has two decimals (12 400,00, 12,400.00, 1.250,50) or
ends in :- (1 250:-), its thousands a single space, point, comma or
apostrophe apart, a minus before it for a credit, its currency before or
after. Spaces of every width count as spaces; an amount with a figure
before it across a space is not taken."
  (let* ((text (figure-spaces text))
         (words (uiop:split-string (string-trim " " text) :separator '(#\Space #\Tab)))
         (end (length (string-right-trim " " text))))
    (loop while (and words (member (string-downcase (car (last words))) *currency-words* :test #'string=))
          do (decf end (1+ (length (car (last words)))))
             (setf words (butlast words)))
    (when (<= end 3) (return-from amount-at-end nil))
    (let* ((whole (string-right-trim " " (subseq text 0 end)))
           (n (length whole))
           (decimal (cond ((and (>= n 3) (uiop:string-suffix-p whole ":-")) :none)
                          ((and (>= n 4) (find (char whole (- n 3)) ".,")
                                (figure-digit-p (char whole (- n 2))) (figure-digit-p (char whole (- n 1))))
                           (char whole (- n 3)))))
           (at (and decimal (if (eq decimal :none) (- n 2) (- n 3))))
           (groups '()) (thousands nil))
      (unless (and decimal (plusp at) (figure-digit-p (char whole (1- at))))
        (return-from amount-at-end nil))
      (loop (let ((start (or (position-if-not #'figure-digit-p whole :end at :from-end t) -1)))
              (push (subseq whole (1+ start) at) groups)
              (when (or (> (length (first groups)) 3) (< start 1)) (return))
              (let ((mark (char whole start)))
                (unless (and (find mark (if thousands (string thousands) " .,'"))
                             (not (eql mark decimal))
                             (= (length (first groups)) 3)
                             (figure-digit-p (char whole (1- start))))
                  (return))
                (setf thousands mark at start))))
      (let* ((digits (format nil "~{~a~}" groups))
             (begin (- (length whole) (length digits) (max 0 (1- (length groups))) (if (eq decimal :none) 2 3)))
             (minus (and (plusp begin) (find (char whole (1- begin)) "-−"))))
        (when (or (and (rest groups) (or (> (length (first groups)) 3) (char= (char (first groups) 0) #\0)))
                  (and (plusp begin) (find (char whole (1- begin)) "0123456789.,"))
                  (let ((before (position-if-not (lambda (c) (find c '(#\Space #\Tab))) whole :end begin :from-end t)))
                    (and before (< before (1- begin)) (figure-digit-p (char whole before)))))
          (return-from amount-at-end nil))
        (when (and (plusp begin) (not minus) (alphanumericp (char whole (1- begin)))
                   (not (member (string-downcase
                                 (subseq whole (let ((space (position #\Space whole :end begin :from-end t)))
                                                 (if space (1+ space) 0))
                                         begin))
                                *currency-words* :test #'string=)))
          (return-from amount-at-end nil))
        (when minus (decf begin))
        (let ((value (+ (* 100 (parse-integer digits))
                        (if (eq decimal :none) 0 (parse-integer whole :start (- (length whole) 2))))))
          (values (if minus (- value) value) (subseq text begin (length whole)) begin))))))

(defun amount-role (label)
  "What the amount after LABEL, a line's text before it in lower case, is
\(*AMOUNT-ROLES*): :NET, :VAT, :TOTAL or :ROUNDING, or NIL. A label holding
other figures than a rate (25 %), longer than 40 characters, not beginning
with a letter or naming what is not money (*AMOUNT-NOT-MONEY*) gives NIL."
  (let ((bare (string-trim " :." (remove-rates label))))
    (when (and (< 0 (length bare) 40) (alpha-char-p (char bare 0)) (notany #'figure-digit-p bare)
               (notany (lambda (w) (search w bare)) *amount-not-money*))
      (flet ((word-p (word)
               (loop for at = (search word bare) then (search word bare :start2 (1+ at))
                     while at
                     thereis (and (or (zerop at) (not (alpha-char-p (char bare (1- at)))))
                                  (let ((end (+ at (length word))))
                                    (or (= end (length bare)) (not (alpha-char-p (char bare end)))
                                        (not (alpha-char-p (char word (1- (length word)))))))))))
        (or (loop for (role . words) in *amount-roles* when (some #'word-p words) return role)
            (and (word-p "net") :net))))))

(defun remove-rates (label)
  "LABEL with each rate in it (25 %, 12%, 6,00 %) taken out."
  (let ((out label))
    (loop for at = (position #\% out)
          while at
          do (let* ((start (or (position-if-not (lambda (c) (or (figure-digit-p c) (find c " ,."))) out
                                                :end at :from-end t)
                               -1)))
               (setf out (concatenate 'string (subseq out 0 (1+ start)) " " (subseq out (1+ at))))))
    out))

(defun page-amounts (lines)
  "The amounts LINES, a page's lines as written, end in, each (ROLE VALUE
WRITTEN LINE): ROLE as AMOUNT-ROLE gives it, or :OTHER for an amount whose
label names none of them, VALUE in hundredths, WRITTEN as on the page, LINE
the index of its line."
  (loop for text in lines for index from 0
        append (multiple-value-bind (value written begin) (amount-at-end text)
                 (and value (list (list (or (amount-role (string-downcase (subseq text 0 begin))) :other)
                                        value written index))))))

(defun unbalanced-sums (amounts)
  "The net, VAT and totals of AMOUNTS (PAGE-AMOUNTS) when they do not add
up, as (NET VAT TOTALS), each an entry of AMOUNTS (TOTALS a list); NIL when
they add up or cannot be told. They are told only on a page stating one
net and one VAT (each perhaps more than once alike), of one sign, the VAT
no more than 27 % of the net, and a total or more, no other amount (a fee,
a sum paid) standing between the net and VAT and the last total. They add up when some
total is the net and VAT together within 51 öre (öre rounded to whole
kronor, or a hundredth lost rounding the VAT line by line)."
  (flet ((one (role)
           (let ((these (remove role amounts :key #'first :test-not #'eq)))
             (and these (every (lambda (a) (= (second a) (second (first these)))) these) (first these)))))
    (let ((net (one :net)) (vat (one :vat))
          (totals (remove :total amounts :key #'first :test-not #'eq)))
      (when (and net vat totals
                 (notany (lambda (a) (and (eq (first a) :other)
                                          (< (min (fourth net) (fourth vat)) (fourth a)
                                             (reduce #'max totals :key #'fourth))))
                         amounts)
                 (not (zerop (second net)))
                 (<= 0 (* (second net) (second vat)))
                 (<= (abs (second vat)) (+ 100 (* 27/100 (abs (second net))))))
        (unless (some (lambda (total) (<= (abs (- (second total) (second net) (second vat))) 51)) totals)
          (list net vat totals))))))

(defun amount-words (hundredths)
  "HUNDREDTHS, an amount, in words as a Swedish invoice writes it: 12 400,00."
  (multiple-value-bind (whole part) (floor (abs hundredths) 100)
    (let ((digits (format nil "~d" whole)))
      (format nil "~:[~;-~]~{~a~^ ~},~2,'0d" (minusp hundredths)
              (loop for end from (length digits) above 0 by 3
                    collect (subseq digits (max 0 (- end 3)) end) into groups
                    finally (return (reverse groups)))
              part))))

;;; The check

;;; The check

(defun figure-spaces (text)
  "TEXT with every space of another width (no-break, narrow no-break, thin,
figure space) made a plain space, as long as it was."
  (substitute-if #\Space (lambda (c) (member c '(#\No-break_space #\Narrow_no-break_space #\Thin_space
                                                  #\Figure_space)))
                 text))

(defun figure-line (line &optional drawn)
  "The text LINE, runs on one baseline, shows as the figures are read from
it (LINE-TEXT, as its glyphs draw it with DRAWN), and the vector of the run
of each character: spaces of every width plain spaces, and a gap between
runs wider than half their size, as between the columns of a table, a tab."
  (multiple-value-bind (text runs) (line-text line drawn)
    (let ((text (figure-spaces text)))
      (loop for i below (length text)
            for run = (aref runs i)
            when (and (consp run) (> (car run) (* 0.5 (cdr run))))
              do (setf (char text i) #\Tab))
      (values text runs))))

(defun line-xs (runs)
  "Where each character of a line lies across the page, RUNS the run of each
of its characters (FIGURE-LINE): a vector of its run's left edge and the
part of the run's width before it, NIL for a space between runs. One pass
over the line."
  (let ((xs (make-array (length runs) :initial-element nil)) (first -1))
    (dotimes (at (length runs) xs)
      (let ((run (aref runs at)))
        (when (and (plusp at) (not (eq run (aref runs (1- at))))) (setf first (1- at)))
        (when (mark-p run)
          (setf (aref xs at)
                (+ (mark-x0 run)
                   (* (- (mark-x1 run) (mark-x0 run)) (/ (- at first 1) (max 1 (length (mark-text run))))))))))))

(defun text-below (runs next next-runs)
  "A function that, given AT, where a label begins in a line whose
characters' runs are RUNS, gives NEXT, the next line (NEXT-RUNS its runs,
FIGURE-LINE), and where in it the number or date of that label may begin,
as two values: past its spaces when NEXT is one word, else the first word
that begins under the label or after it; NIL for none. The places of both lines' characters
and NEXT's words are worked out once, at the first call, so that each
call takes a few steps however long the lines."
  (let ((xs nil) (one-word nil) (starts nil) (most nil))
    (flet ((prepare ()
             (setf xs (line-xs runs))
             (let ((from (position-if-not (lambda (c) (member c '(#\Space #\Tab))) next)))
               (when (and from (not (find-if (lambda (c) (member c '(#\Space #\Tab)))
                                             (string-right-trim '(#\Space #\Tab) next) :start from)))
                 (setf one-word from)))
             ;; NEXT's words in order, and the furthest across any of them up to each
             (let ((next-xs (line-xs next-runs)) (found '()) (furthest nil))
               (dotimes (j (length next))
                 (let ((jx (aref next-xs j)))
                   (when (and jx (or (zerop j) (member (char next (1- j)) '(#\Space #\Tab))))
                     (setf furthest (if furthest (max furthest jx) jx))
                     (push (cons j furthest) found))))
               (setf starts (coerce (nreverse (mapcar #'car found)) 'vector)
                     most (coerce (nreverse (mapcar #'cdr found)) 'vector)))))
      (lambda (at)
        (unless xs (prepare))
        (let ((x (aref xs at)))
          (cond (one-word (values next one-word))
                (x (let ((least (- x (* 0.5 (max 1 (mark-size (aref runs at))))))
                         (low 0) (high (length most)))
                     ;; the first word whose furthest up to it reaches LEAST is the first that does
                     (loop while (< low high)
                           do (let ((middle (floor (+ low high) 2)))
                                (if (>= (aref most middle) least) (setf high middle) (setf low (1+ middle)))))
                     (and (< low (length starts)) (values next (aref starts low)))))))))))

(defun recognised-line-p (check line)
  "True when LINE, marks of CHECK's page in one line, is all invisible text
over a scanned page: its text recognition, which may misread a digit."
  (and (page-check-scan check) (every (lambda (m) (member (mark-mode m) '(3 7))) line)))

(defun figure-found (check strength failed shown)
  "Add to CHECK a finding of STRENGTH of FAILED, numbers whose check fails,
each (KIND NUMBER TEXT), TEXT the line it stands in, the marks SHOWN shown:
worth a look for text drawn, for the record for a scan's text recognition."
  (page-found check strength
              (format nil "Page ~d has ~:[a number~;numbers~] whose check fails: ~{~a~^; ~}.~:[ A figure changed by ~
                           hand, or a slip where the number was written.~; As read by the page's text recognition, ~
                           which may have misread or dropped a digit; the picture was not compared.~]"
                      (page-check-number check) (rest failed)
                      (loop for (kind number) in failed collect (check-number-words kind number))
                      (eq strength :record))
              #.(concatenate 'string "Organisation, personal identity and bankgiro numbers and OCR references end "
                                     "in a check digit by the modulus 10 method, an IBAN checks modulo 97; one "
                                     "changed without its check digit fails. Only numbers right after their "
                                     "label and written as such numbers are, are taken.")
              (sort (copy-list shown) #'> :key #'mark-y0)
              :details (loop for (nil nil text) in failed collect (format nil "~s" text))
              :method :figure-unchecked))

(defun check-figures (check)
  "Add to CHECK the numbers of its page's text whose check digit fails
\(CHECK-NUMBERS-IN), a label ending a line read with the number under it or
after it on the next (TEXT-BELOW); and a net and VAT that do not add up to
the page's total (UNBALANCED-SUMS). Worth a look for text the page draws;
for the record for a scan's text recognition, which may misread a digit.
Where a font's glyphs draw other figures than copying gives (MARK-DRAWN),
the numbers are checked as drawn too, as a reader sees them."
  (let* ((lines (page-lines-of (page-check-texts check)))
         (read (mapcar (lambda (line) (multiple-value-list (figure-line line))) lines))
         (texts (mapcar #'first read))
         (seen '()) (drawn '()) (drawn-marks '()) (recognised '()) (recognised-marks '()))
    (flet ((check-numbers (read)
             (loop for ((here runs) . rest) on read for (line . more) on lines
                   do (loop with below = (and rest (text-below runs (first (first rest)) (second (first rest))))
                            for (kind number valid nil below-p) in (check-numbers-in here below)
                            for marks = (if below-p (append line (first more)) line)
                            for text = (shorten-string (if below-p (format nil "~a ~a" here (first (first rest))) here)
                                                       100 :one-line t)
                            unless (or valid (member number seen :test #'equal))
                              do (push number seen)
                                 (if (recognised-line-p check marks)
                                     (progn (push (list kind number text) recognised)
                                            (setf recognised-marks (union recognised-marks marks)))
                                     (progn (push (list kind number text) drawn)
                                            (setf drawn-marks (union drawn-marks marks))))))))
      (check-numbers read)
      (when (find-if #'mark-drawn (page-check-texts check))
        (check-numbers (mapcar (lambda (line) (multiple-value-list (figure-line line t))) lines))))
    (when drawn (figure-found check :look (nreverse drawn) drawn-marks))
    (when recognised (figure-found check :record (nreverse recognised) recognised-marks))
    (let ((sums (unbalanced-sums (page-amounts texts))))
      (when sums
        (destructuring-bind (net vat totals) sums
          (let* ((entries (list* net vat totals))
                 (marks (loop for entry in entries append (nth (fourth entry) lines)))
                 (read (some (lambda (entry) (recognised-line-p check (nth (fourth entry) lines))) entries)))
            (page-found check (if read :record :look)
                        (format nil "Page ~d states a net of ~a and VAT of ~a, which make ~a, but its total~:[ is~;s ~
                                     are~] ~{~a~^, ~}.~:[ A figure changed by hand without those that add up to it, ~
                                     or a total of more than this net and VAT.~; As read by the page's text ~
                                     recognition, which may have misread or dropped a digit; the picture was not ~
                                     compared.~]"
                                (page-check-number check) (third net) (third vat)
                                (amount-words (+ (second net) (second vat))) (rest totals) (mapcar #'third totals)
                                read)
                        #.(concatenate 'string "An invoice's net and VAT add up to its total, öre rounded to whole "
                                               "kronor at most; a page naming one net and one VAT, with no other "
                                               "amount between them and the total, is checked.")
                        (sort (copy-list marks) #'> :key #'mark-y0)
                        :details (loop for entry in entries collect (format nil "~s" (nth (fourth entry) texts)))
                        :method :figure-unchecked)))))))
