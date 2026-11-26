;;;; figures.lisp --- Numbers in a page's text that carry their own check, and totals that must add up.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A figure changed by hand seldom comes with every number that depends on
;;;; it changed too. Some numbers carry a check digit worked out from the
;;;; others: a Swedish organisation or personal identity number, a bankgiro
;;;; number and an OCR payment reference (the modulus 10 method), an IBAN
;;;; (ISO 13616, modulus 97), a card number, a plusgiro number, a creditor
;;;; reference (ISO 11649) and a Norwegian KID. And an invoice's net and VAT
;;;; add up to its total, a column adds up to the total under it, a
;;;; statement's balance follows from the row before, a payslip's net pay is
;;;; its gross less its deductions, a figure stated twice is the same both
;;;; times, and a weekday written beside a date is that date's. A number is
;;;; taken only where its label stands right before it and it is written as
;;;; such numbers are, so that a number that merely looks like one is passed
;;;; over. Only text the file holds is read: a scanned page without a text
;;;; layer has none.

(in-package #:palimpsest)

(defparameter *check-number-labels*
  '((:organisation "organisationsnummer" "organisationsnr" "organisation number" "organization number"
     "organisation no" "organization no" "org.nummer" "org nummer" "org.nr" "org. nr" "org nr" "org-nr" "orgnr"
     "org.no" "org. no" "org no")
    (:personal "personnummer" "personnr" "pers.nr" "pers. nr" "personal identity number" "samordningsnummer")
    (:bankgiro "bankgironummer" "bankgironr" "bankgiro" "bg-nr" "bg.nr" "bg nr" "bg")
    (:plusgiro "plusgironummer" "plusgironr" "plusgiro" "pg-nr" "pg.nr")
    (:ocr "ocr-referens" "ocr-nummer" "ocrnummer" "ocr-nr" "ocr nr" "ocr")
    (:kid "kid-nummer" "kidnummer" "kid-nr")
    (:card "card number" "kortnummer" "kortnr")
    (:creditor "creditor reference" "payment reference" "betalningsreferens" "referensnummer" "reference" "referens")
    (:iban "iban" "account number" "account no" "account" "kontonummer" "konto"))
  "The labels a number carrying a check digit is taken after, by kind, lower
case, the longest first: :ORGANISATION, a Swedish organisation number;
:PERSONAL, a Swedish personal identity or coordination number; :BANKGIRO, a
bankgiro number; :PLUSGIRO, a plusgiro number; :OCR, an OCR payment
reference; :KID, a Norwegian customer identification number (KID); :CARD, a
payment card's number; :CREDITOR, a creditor reference of ISO 11649 (RF and
two check digits); :IBAN, an international bank account number (IBAN). A
creditor reference and an IBAN are taken after a general label, reference
or account, only when written as one: any other reference or account
number there is passed over.")

(defparameter *check-number-fillers* '("nummer" "number" "referens" "reference" "nr" "no" "ref")
  "Words that may stand between a label of *CHECK-NUMBER-LABELS* and its
number (\"OCR-referens nr: 1234\"), beside spaces and the marks : . # - ( ) /.")

(defun figure-digit-p (char)
  "True when CHAR is one of the digits 0 to 9."
  (char<= #\0 char #\9))

(defun luhn-check-digit (digits)
  "The check digit DIGITS, a string of digits ending in one, should end in by
the Luhn check (LUHN-P), as a character."
  (loop for c across "0123456789"
        when (luhn-p (concatenate 'string (subseq digits 0 (1- (length digits))) (string c)))
          return c))

(defun modulus-11-p (digits)
  "True when DIGITS, a string of digits ending in a check digit, pass the
modulus 11 check Norwegian KID and account numbers use: the digits before
the last weighted 2 to 7 from the right, over again, their sum taken from
the next multiple of 11 giving the last; a sum that would need 10 has no
valid number."
  (let* ((n (length digits))
         (sum (loop for i from (- n 2) downto 0
                    for weight = 2 then (if (= weight 7) 2 (1+ weight))
                    sum (* weight (- (char-code (char digits i)) 48))))
         (check (mod (- 11 (mod sum 11)) 11)))
    (and (> n 1) (< check 10) (= check (- (char-code (char digits (1- n))) 48)))))

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
followed by another number, and an OCR reference is not taken in brackets
of its own, as a year cited after the Office for Civil Rights or text
recognition (OCR) is: \"OCR (2023)\"."
  (ecase kind
    (:organisation
     (multiple-value-bind (digits end) (dashed-number text start 6 4)
       (and digits (values digits (luhn-p digits) end))))
    (:personal
     (multiple-value-bind (digits end) (dashed-number text start 6 4 "-+")
       (unless digits
         (multiple-value-setq (digits end) (dashed-number text start 8 4 "-+"))
         (when digits (setf digits (subseq digits 2))))
       (and digits (personal-date-p digits) (values digits (luhn-p digits) end))))
    (:bankgiro
     (multiple-value-bind (digits end) (dashed-number text start 3 4)
       (unless digits (multiple-value-setq (digits end) (dashed-number text start 4 4)))
       (and digits (values digits (luhn-p digits) end))))
    (:ocr
     (multiple-value-bind (groups end) (grouped-digits text start 25)
       (let ((digits (and groups (format nil "~{~a~}" groups))))
         (when (and digits (<= 4 (length digits))
                    (not (and (plusp start) (char= (char text (1- start)) #\()
                              (< end (length text)) (char= (char text end) #\)))))
           (if (luhn-p digits)
               (values digits t end)
               (unless (loop for k from 1 below (length groups)
                             thereis (luhn-p (format nil "~{~a~}" (subseq groups 0 k))))
                 (values digits nil end)))))))
    (:plusgiro
     (let ((digits (make-string-output-stream)) (at start) (count 0) (n (length text)))
       (loop while (and (< at n) (< count 8))
             do (let ((c (char text at)))
                  (cond ((figure-digit-p c) (write-char c digits) (incf count))
                        ((not (and (find c " -") (plusp count) (< (1+ at) n) (figure-digit-p (char text (1+ at)))))
                         (return))))
                (incf at))
       (let ((number (get-output-stream-string digits)))
         (and (<= 2 (length number) 8) (figure-ends-p text at) (values number (luhn-p number) at)))))
    (:kid
     (multiple-value-bind (digits end) (digits-run text start)
       (and (<= 4 (length digits) 25) (figure-ends-p text end)
            (values digits (or (luhn-p digits) (modulus-11-p digits)) end))))
    (:card
     (multiple-value-bind (groups end) (grouped-digits text start 19)
       (let ((digits (and groups (format nil "~{~a~}" groups))))
         (and digits (<= 13 (length digits) 19) (values digits (luhn-p digits) end)))))
    (:creditor
     (when (and (<= (+ start 4) (length text)) (string= "RF" text :start2 start :end2 (+ start 2))
                (figure-digit-p (char text (+ start 2))) (figure-digit-p (char text (+ start 3))))
       (let ((out (make-string-output-stream)) (count 0) (at start))
         (loop while (and (< at (length text)) (< count 25))
               do (let ((c (char text at)))
                    (cond ((or (figure-digit-p c) (char<= #\A c #\Z)) (write-char c out) (incf count))
                          ((not (and (char= c #\Space) (< (1+ at) (length text))
                                     (or (figure-digit-p (char text (1+ at))) (char<= #\A (char text (1+ at)) #\Z))))
                           (return))))
                  (incf at))
         (when (and (<= 5 count) (or (>= at (length text)) (not (alphanumericp (char text at)))))
           (let ((reference (get-output-stream-string out)))
             (values reference (iban-check-p reference) at))))))
    (:iban
     (let* ((country (and (<= (+ start 4) (length text)) (subseq text start (+ start 2))))
            (length (and country (iban-length country))))
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
               (values iban (iban-check-p iban) at)))))))))

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
                     (push (list :organisation number (luhn-p number) at nil (+ at 2)) out))))))
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
    (:plusgiro (format nil "the plusgiro number ~a-~a, whose check digit would be ~a"
                       (subseq number 0 (1- (length number))) (subseq number (1- (length number)))
                       (luhn-check-digit number)))
    (:ocr (format nil "the OCR reference ~a, whose check digit would be ~a" number (luhn-check-digit number)))
    (:kid (format nil "the KID ~a, whose check digit fits neither modulus 10 nor modulus 11" number))
    (:card (format nil "the card number ~a, whose check digit would be ~a" number (luhn-check-digit number)))
    (:creditor (format nil "the creditor reference ~a, which does not give remainder 1 modulo 97" number))
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

;;; The figures of a line

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
                   (* (mark-width run) (/ (- at first 1) (max 1 (length (mark-text run))))))))))))

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
  (and (page-check-scan check) (every #'invisible-text-p line)))

;;; Totals of a column and rows multiplied out

(defparameter *sum-labels* '("total" "summa" "sum" "gross pay" "bruttolön")
  "Words in a line's label that say its amount adds up those above it:
total, subtotal and total due; summa and delsumma; sum; a payslip's gross
pay, which adds up its payments.")

(defparameter *sum-slack* 51
  "How far, in hundredths, figures may be from adding or multiplying up and
still agree: a rounding to whole units and no more, as the öre rounding of
UNBALANCED-SUMS.")

(defun plain-number (word)
  "WORD, a number written with a decimal point or comma and thousands a
comma or point apart (3, 2.5, 118.00, 1,250.00, 1.250,00), a currency sign
before it or after, as two values: its value in hundredths, and true when it
is written with two decimals, as an amount is; NIL for anything else."
  (let* ((word (string-trim "£$€" word))
         (n (length word))
         (minus (and (plusp n) (find (char word 0) "-−")))
         (digits (if minus (subseq word 1) word))
         (n (length digits)))
    (when (and (plusp n) (figure-digit-p (char digits 0)) (figure-digit-p (char digits (1- n)))
               (every (lambda (c) (or (figure-digit-p c) (find c ".,"))) digits))
      (let* ((mark (position-if (lambda (c) (find c ".,")) digits :from-end t))
             (decimals (if mark (- n mark 1) 0))
             (whole (if (and mark (<= decimals 2)) (subseq digits 0 mark) digits))
             (grouped (and (find-if (lambda (c) (find c ".,")) whole) t)))
        (when (and (or (not mark) (<= decimals 2) (= decimals 3))
                   (or (not grouped)
                       (every (lambda (group) (= (length group) 3))
                              (rest (uiop:split-string whole :separator ".,")))))
          (let ((value (if (and mark (<= decimals 2))
                           (+ (* 100 (parse-integer (remove-if-not #'figure-digit-p whole)))
                              (* (if (= decimals 1) 10 1) (parse-integer digits :start (1+ mark))))
                           (* 100 (parse-integer (remove-if-not #'figure-digit-p digits))))))
            (values (if minus (- value) value) (and mark (= decimals 2)))))))))

(defun line-numbers (text)
  "The numbers TEXT, a line as FIGURE-LINE makes it, ends in, from the left,
each (VALUE AMOUNT-P) as PLAIN-NUMBER reads it, and as a second value the
line's label, what stands before them, in lower case."
  (let* ((words (remove "" (uiop:split-string (substitute #\Space #\Tab text) :separator " ") :test #'string=))
         (numbers '()))
    (loop for word in (reverse words)
          do (multiple-value-bind (value amount) (plain-number word)
               (if value (push (list value amount) numbers) (return))))
    (values numbers
            (string-downcase (format nil "~{~a~^ ~}" (subseq words 0 (- (length words) (length numbers))))))))

(defun sum-label-p (label)
  "True when LABEL, a line's text before its figures in lower case, says the
line adds up those above it (*SUM-LABELS*)."
  (some (lambda (word) (search word label)) *sum-labels*))

(defun one-digit-apart-p (a b)
  "True when A and B, amounts in hundredths, are written with as many digits
and differ in one of them only: 27743 and 37743."
  (let ((a (format nil "~d" (abs a))) (b (format nil "~d" (abs b))))
    (and (= (length a) (length b)) (= 1 (count nil (map 'list #'char= a b))))))

(defun unsummed-rows (rows)
  "The totals among ROWS that the amounts above them do not add up to, each
\(TOTAL-LINE SUM ITEM-LINES): ROWS are the amounts of one column from the
top, each (LINE VALUE TOTAL-P), LINE the index of its line, VALUE in
hundredths, TOTAL-P true for one labelled a total (SUM-LABEL-P); NIL among
them is passed over. Each total found gives its line, what the amounts
above it make, and their lines. A total is set against the amounts above
it, back to the total before or to the top, each above nothing and smaller
than it. It adds up when it is the sum of the last of them, however many,
with or without the total before, or the total before less them, or the
total before alone. A total over three amounts or more that does not add up
is named. A total that follows another with one or two amounts between (a
subtotal, delivery, the total) is named only when it is one digit from what
they make (ONE-DIGIT-APART-P): totals of other things stand so too, hours
and pay. Only the first such total of a column is named."
  (let ((items '()) (before nil) (out '()))
    (flet ((agree (a b) (<= (abs (- a b)) *sum-slack*)))
      (dolist (row rows (nreverse out))
        (when row
          (destructuring-bind (index value total-p) row
            (if (not total-p)
                (push (list index value) items)
                (let ((all (reduce #'+ items :key #'second)))
                  (when (and items (null out)
                             (every (lambda (item) (< 0 (second item) value)) items)
                             (not (and before (agree (second before) value)))
                             (notany (lambda (count)
                                       (let ((sum (reduce #'+ (subseq items 0 count) :key #'second)))
                                         (or (agree sum value)
                                             (and before (or (agree (+ (second before) sum) value)
                                                             (agree (- (second before) sum) value))))))
                                     (loop for count from 1 to (length items) collect count)))
                    (cond ((>= (length items) 3)
                           (push (list index all (reverse (mapcar #'first items))) out))
                          ((and before (one-digit-apart-p (+ (second before) all) value))
                           (push (list index (+ (second before) all)
                                       (cons (first before) (reverse (mapcar #'first items))))
                                 out))))
                  (setf before (list index value) items '())))))))))

(defun unsummed-totals (texts)
  "The totals among TEXTS, a page's lines as FIGURE-LINE makes them, that the
amounts above them do not add up to (UNSUMMED-ROWS), each (TOTAL-LINE SUM
ITEM-LINES): the column read is the amount each line ends in, a line
labelled a total (SUM-LABEL-P) being one."
  (unsummed-rows (loop for text in texts for index from 0
                       collect (multiple-value-bind (numbers label) (line-numbers text)
                                 (let ((last (car (last numbers))))
                                   (and last (second last) (list index (first last) (sum-label-p label))))))))

(defun unmultiplied-rows (texts)
  "The rows among TEXTS, a page's lines as FIGURE-LINE makes them, whose
amount is not their quantity times their price, each (LINE PRODUCT): the
index of the line and what quantity and price make, in hundredths. A row
ends in a quantity, a price and an amount; a page is judged when three of
its rows or more multiply out (within *SUM-SLACK*) and fewer than half as
many do not."
  (let ((fit 0) (out '()))
    (loop for text in texts for index from 0
          do (let ((numbers (line-numbers text)))
               (when (>= (length numbers) 3)
                 (destructuring-bind ((quantity quantity-p) (price price-p) (amount amount-p)) (last numbers 3)
                   (declare (ignore quantity-p))
                   (when (and price-p amount-p (plusp quantity) (plusp price) (plusp amount))
                     (let ((product (round (* quantity price) 100)))
                       (if (<= (abs (- product amount)) *sum-slack*)
                           (incf fit)
                           (push (list index product) out))))))))
    (and (>= fit 3) out (< (* 2 (length out)) fit) (nreverse out))))

;;; Amounts with their labels

(defun line-cells (text runs)
  "The cells of TEXT, a line as FIGURE-LINE makes it, RUNS the run of each of
its characters: what stands between its tabs, the gaps between a table's
columns, each (WORDS MARK), its text without the spaces around it and the
run its last character is of."
  (let ((cells '()) (start 0))
    (loop for tab = (position #\Tab text :start start)
          for end = (or tab (length text))
          for last = (position-if-not (lambda (c) (char= c #\Space)) text :start start :end end :from-end t)
          do (when last
               (push (list (string-trim " " (subseq text start end))
                           (let ((run (aref runs last))) (and (mark-p run) run)))
                     cells))
             (if tab (setf start (1+ tab)) (return)))
    (nreverse cells)))

(defun cell-amounts (words)
  "The amounts written with two decimals (PLAIN-NUMBER) among WORDS, a cell's
text, each (LABEL VALUE): VALUE in hundredths and LABEL the words of the
cell between the amount before it and itself, in lower case without the
marks around them; and as a second value the words after the last amount,
so given, the empty string when the cell ends in an amount."
  (let ((label '()) (out '()))
    (flet ((words-so-far ()
             (string-downcase (string-trim " :.-" (format nil "~{~a~^ ~}" (reverse label))))))
      (dolist (word (uiop:split-string words :separator " "))
        (multiple-value-bind (value amount-p) (plain-number word)
          (cond ((and value amount-p)
                 (push (list (words-so-far) value) out)
                 (setf label '()))
                ((plusp (length word)) (push word label)))))
      (values (nreverse out) (words-so-far)))))

(defun labelled-amounts (cells)
  "The amounts among CELLS, a line's cells (LINE-CELLS), each (LABEL VALUE
MARK LAST-P): VALUE in hundredths, MARK the run its cell ends in, LAST-P
true for the amount ending the line, and LABEL what names it, in lower
case: the words before it in its own cell, back to the amount before
\(\"Closing balance: 933.60\"), else the nearest words before it that no
amount follows in their cell, else the empty string."
  (let ((label "") (out '()))
    (loop for (cell . rest) on cells
          do (multiple-value-bind (amounts after) (cell-amounts (first cell))
               (loop for ((own value) . more) on amounts
                     do (push (list (if (plusp (length own)) own label) value (second cell)
                                    (and (null rest) (null more) (zerop (length after))))
                              out))
               (when (plusp (length after)) (setf label after))))
    (nreverse out)))

(defun figure-place-words (mark &optional value)
  "Where within MARK, a run of text, the amount VALUE (in hundredths) is
written, in the words findings give places in, to follow the run's own
place: \", the figure at x 139, y 804\". Without VALUE, the last amount
the run holds. The empty string when the run begins with the amount, so
that its own place is the amount's, or holds none."
  (let* ((text (mark-text mark))
         (at (loop with found = nil
                   for start = 0 then (1+ end)
                   for end = (or (position #\Space text :start (min start (length text))) (length text))
                   do (when (< start end)
                        (multiple-value-bind (read amount-p) (plain-number (subseq text start end))
                          (when (and read amount-p (or (null value) (= read value))) (setf found start))))
                   when (>= end (length text)) return found)))
    (if (and at (plusp at) (mark-frame mark))
        (format nil ", the figure at x ~d, y ~d" (round (mark-character-x mark at)) (round (mark-y0 mark)))
        "")))

(defun label-says-p (label phrases)
  "True when LABEL, in lower case, holds one of PHRASES as whole words; the
phrase it holds, the first of PHRASES that fits."
  (find-if (lambda (phrase)
             (loop for at = (search phrase label) then (search phrase label :start2 (1+ at))
                   while at
                   thereis (and (or (zerop at) (not (alpha-char-p (char label (1- at)))))
                                (let ((end (+ at (length phrase))))
                                  (or (= end (length label)) (not (alpha-char-p (char label end))))))))
           phrases))

;;; Columns inside a line

(defparameter *column-amounts-least* 4
  "The fewest amounts a column inside a table must hold, its total among
them, for the total to be set against those above it (INNER-COLUMNS).")

(defun inner-columns (amounts)
  "The columns of amounts that do not end their lines, among AMOUNTS, each
line's labelled amounts from the top of the page (LABELLED-AMOUNTS, NIL for
a line with none): a list of columns, each a list of (LINE VALUE TOTAL-P
MARK) from the top, as UNSUMMED-ROWS reads them with the run each amount is
in. Amounts are of one column when their right edges are within a point of
each other, as a table sets figures. In a line written as one run, as a
printer joins it, the edges are the run's, and its amounts are of the
columns of their order in the line instead: the first with the first of
other such lines. Columns of fewer than *COLUMN-AMOUNTS-LEAST* are left
out."
  (let ((columns '()))
    (loop for line in amounts for index from 0
          for joined = (and (rest line) (every (lambda (amount) (eq (third amount) (third (first line)))) line))
          do (loop for (label value mark last-p) in line for nth from 0
                   when (and mark (not last-p))
                     do (let ((column (find-if (lambda (c)
                                                 (if joined
                                                     (eql (car c) (- -1 nth))
                                                     (and (plusp (car c)) (< (abs (- (car c) (mark-x1 mark))) 1))))
                                               columns)))
                          (unless column
                            ;; a joined line's columns are named below nothing, by their order
                            (setf column (list (if joined (- -1 nth) (max 1 (mark-x1 mark)))))
                            (push column columns))
                          (push (list index value (and (sum-label-p label) t) mark) (cdr column)))))
    (loop for (nil . rows) in (reverse columns)
          when (>= (length rows) *column-amounts-least*)
            collect (reverse rows))))

;;; Net pay

(defparameter *pay-labels*
  '((:gross "total gross pay" "gross pay" "total gross" "bruttolön" "summa brutto")
    (:deductions "total deductions" "deductions total" "summa avdrag")
    (:net "net pay" "take home pay" "nettolön" "att utbetala" "utbetalas"))
  "The labels a payslip's three sums are read by, lower case, the longest of
a kind first: :GROSS, the pay before deductions; :DEDUCTIONS, what is taken
off; :NET, what is paid out.")

(defun unbalanced-pay (amounts)
  "The gross pay, deductions and net pay among AMOUNTS, a page's labelled
amounts line by line (LABELLED-AMOUNTS), when the net is not the gross less
the deductions, as (GROSS DEDUCTIONS NET), each (LINE VALUE MARK); NIL when
it is, within a hundredth, or when the page does not state each of the
three once (or more than once alike)."
  (flet ((one (kind)
           (let ((found (loop for line in amounts for index from 0
                              append (loop for (label value mark) in line
                                           when (label-says-p label (rest (assoc kind *pay-labels*)))
                                             collect (list index value mark)))))
             (and found (every (lambda (f) (= (second f) (second (first found)))) found) (first found)))))
    (let ((gross (one :gross)) (deductions (one :deductions)) (net (one :net)))
      (and gross deductions net
           (> (abs (- (second net) (- (second gross) (second deductions)))) 1)
           (list gross deductions net)))))

;;; Running balances

(defparameter *opening-labels* '("brought forward" "opening balance" "previous balance" "b/f" "ingående saldo"
                                 "ingående balans")
  "Words in a line's label that say its one amount is the balance a
statement begins with.")

(defparameter *closing-labels* '("closing balance" "carried forward" "new balance" "c/f" "utgående saldo"
                                 "utgående balans")
  "Words in a line's label that say its one amount is the balance a
statement ends with.")

(defparameter *balance-rows-least* 4
  "The fewest rows of a page whose balance must follow from the row before
for the page to be read as a statement with a running balance
\(BROKEN-BALANCES).")

(defun broken-balances (texts)
  "The rows among TEXTS, a page's lines as FIGURE-LINE makes them, where a
running balance breaks, each (LINE KIND BEFORE AMOUNT): the index of the
line, the balance before it and the row's amount in hundredths, and KIND:
:ROW for a row whose balance is not the balance before it with its amount
added or taken off, while the rows after follow from it (an amount changed,
or a balance and every one after it); :BALANCE for a balance that alone
does not follow, the row after following from what it should be; :CLOSING
for a line labelled the closing balance (*CLOSING-LABELS*) that is not the
last row's balance. A row ends in its amount and its balance, each with two
decimals; a line ending in one such amount is a balance alone. A row is
judged only after a row that followed or a line labelled the opening
balance (*OPENING-LABELS*), and a line that neither follows nor is followed
is passed over as no part of the table. Nothing is named unless
*BALANCE-ROWS-LEAST* rows follow, nor when more than two break or as many
as a third of those that follow."
  (let* ((rows (coerce (loop for text in texts for index from 0
                             append (multiple-value-bind (numbers label) (line-numbers text)
                                      (let ((balance (car (last numbers))) (amount (car (last numbers 2))))
                                        (and balance (second balance)
                                             (list (list index
                                                         (and (rest numbers) (second amount) (first amount))
                                                         (first balance) label))))))
                       'vector))
         (before nil) (trusted nil) (follow 0) (out '()))
    (flet ((step-p (from amount to) (or (= to (+ from amount)) (= to (- from amount)))))
      (loop for k from 0 below (length rows)
            do (destructuring-bind (index amount balance label) (aref rows k)
                 (cond ((null amount)
                        (when (and trusted (>= follow *balance-rows-least*) (/= balance before)
                                   (label-says-p label *closing-labels*))
                          (push (list index :closing before nil) out))
                        (setf before balance trusted (and (label-says-p label *opening-labels*) t)))
                       ((null before) (setf before balance))
                       ((step-p before amount balance) (incf follow) (setf before balance trusted t))
                       (t (let* ((next (and (< (1+ k) (length rows)) (aref rows (1+ k))))
                                 (next-amount (and next (second next))) (next-balance (and next (third next))))
                            (cond ((null next-amount) (setf before balance trusted nil))
                                  ((step-p balance next-amount next-balance)
                                   (when trusted (push (list index :row before amount) out))
                                   (setf before balance))
                                  ((step-p (+ before amount) next-amount next-balance)
                                   (when trusted (push (list index :balance before amount) out))
                                   (setf before (+ before amount)))
                                  ((step-p (- before amount) next-amount next-balance)
                                   (when trusted (push (list index :balance before amount) out))
                                   (setf before (- before amount)))
                                  ((step-p before next-amount next-balance))
                                  (t (setf before balance trusted nil)))))))))
    (and (>= follow *balance-rows-least*) out (<= (length out) 2) (< (* 3 (length out)) follow)
         (nreverse out))))

(defun balance-break-words (page kind line before amount)
  "What a break of a running balance says (BROKEN-BALANCES), as two values:
the sentence naming it, on page PAGE at LINE, the line as written, of KIND
with the balance BEFORE it and the row's AMOUNT in hundredths; and the
sentence saying what explains it."
  (ecase kind
    (:row (values (format nil "On page ~d the running balance breaks at ~s: the balance before it, ~a, and its ~
                               amount, ~a, do not make the balance it states, while the rows after follow from ~
                               that balance."
                          page line (amount-words before) (amount-words amount))
                  #.(concatenate 'string " An amount changed without its balance, or this balance and every one "
                                         "after it changed together; or a row left out above it.")))
    (:balance (values (format nil "On page ~d the running balance breaks at ~s: the balance before it, ~a, and its ~
                                   amount, ~a, do not make the balance it states, and the row after follows from ~
                                   what they do make."
                              page line (amount-words before) (amount-words amount))
                      " This one balance was changed."))
    (:closing (values (format nil "On page ~d the closing balance stated, ~s, is not the last row's balance, ~a."
                              page line (amount-words before))
                      " The closing balance changed without the rows, or a row taken out above it."))))

(defparameter *balance-rule*
  #.(concatenate 'string "In a statement each row's balance is the balance before it with the row's amount added or "
                         "taken off, and the closing balance is the last row's. A page is read so when four rows or "
                         "more follow one another; a row is judged after a row that followed or the opening balance.")
  "The rule a finding of a running balance that breaks states.")

;;; A weekday beside a date

(defparameter *weekday-words*
  '((0 "söndagen" "söndag" "sunday" "sön" "sun") (1 "måndagen" "måndag" "monday" "mån" "mon")
    (2 "tisdagen" "tisdag" "tuesday" "tues" "tis" "tue") (3 "onsdagen" "onsdag" "wednesday" "ons" "wed")
    (4 "torsdagen" "torsdag" "thursday" "thurs" "thur" "tors" "thu") (5 "fredagen" "fredag" "friday" "fre" "fri")
    (6 "lördagen" "lördag" "saturday" "lör" "sat"))
  "The names of the days of the week a page may write beside a date, in
Swedish and English, whole or short, lower case, each list beginning with
the day's number as WEEKDAY gives it, 0 for Sunday.")

(defun weekday-word-at (text start)
  "The day of the week whose name (*WEEKDAY-WORDS*, case not counting) is
written whole at START of TEXT, as two values: its number, 0 for Sunday,
and where the name ends, past a full stop after a short name; NIL when none
is there."
  (let* ((end (or (position-if-not #'alpha-char-p text :start start) (length text)))
         (entry (and (< start end (+ start 10))
                     (or (zerop start) (not (alpha-char-p (char text (1- start)))))
                     (find-if (lambda (e) (member-if (lambda (name) (string-equal name text :start2 start :end2 end))
                                                     (rest e)))
                              *weekday-words*))))
    (when entry
      (values (first entry)
              (if (and (< end (length text)) (char= (char text end) #\.) (<= (- end start) 5)) (1+ end) end)))))

(defun wrong-weekdays (text)
  "The weekdays written beside a date in TEXT, a line as written, that are
not that date's, each (WRITTEN DAY START END): the weekday written and the
date's day number (DAY-NUMBER), from START to END of TEXT. A weekday is
beside a date when it stands right before it, a comma, \"the\" or \"den\"
at most between (Monday, 14 March 2026; fredag den 15 mars 2024), or right
after it, a comma or an opening bracket at most between (2024-03-15,
fredag; 14 March 2026 (Saturday)). Only dates TEXT-DATE-AT reads are
taken: one whose day and month could be swapped is passed over."
  (let ((out '()) (n (length text)))
    (flet ((past (at words)
             ;; past spaces, a comma or bracket and one of WORDS standing whole
             (let ((at (past-chars text at " ,(")))
               (dolist (word words at)
                 (let ((end (+ at (length word))))
                   (when (and (< end n) (string-equal word text :start2 at :end2 end) (char= (char text end) #\Space))
                     (return (past-chars text end " "))))))))
      (loop for at from 0 below n
            do (when (or (zerop at) (not (alphanumericp (char text (1- at)))))
                 (multiple-value-bind (written end) (weekday-word-at text at)
                   (if written
                       (let ((from (past end '("the" "den"))))
                         (when (and (> from end) (< from n) (date-begins-p text from))
                           (multiple-value-bind (day date-end) (text-date-at text from)
                             (when (and day (/= written (weekday day)))
                               (push (list written day at date-end) out)))))
                       (when (date-begins-p text at)
                         (multiple-value-bind (day date-end) (text-date-at text at)
                           (when day
                             (let ((from (past date-end '())))
                               (when (and (> from date-end) (< from n))
                                 (multiple-value-bind (after after-end) (weekday-word-at text from)
                                   (when (and after (/= after (weekday day)))
                                     (push (list after day at after-end) out))))))))))))
      (remove-duplicates (nreverse out) :key #'third))))

(defun weekday-name (number)
  "The day of the week NUMBER, 0 for Sunday, in English."
  (aref *weekday-names* number))

;;; The check

(defparameter *misread-words*
  #.(concatenate 'string " As read by the page's text recognition, which may have misread or dropped a digit; the "
                         "picture was not compared.")
  "What a finding of figures read by a scan's text recognition says in place
of how they came to disagree.")

(defun figure-finding (check read words cause rule marks details &key place (misread *misread-words*))
  "Add to CHECK a finding of figures of its page that do not agree, shown by
MARKS: WORDS, then CAUSE, how they came to disagree, by RULE, with the lines
DETAILS. When READ, the figures are as the page's text recognition read
them, and the finding is for the record, MISREAD said in place of CAUSE;
else it is worth a look. PLACE as FOUND takes it."
  (page-found check (if read :record :look) (concatenate 'string words (if read misread cause)) rule marks
              :details details :method :figure-unchecked :place place))

(defun figure-line-words (text)
  "TEXT, a line as FIGURE-LINE makes it, as a finding names it: its tabs as
spaces, without the spaces around it."
  (string-trim " " (substitute #\Space #\Tab text)))

(defun lines-recognised-p (check lines indexes)
  "True when one of LINES, the runs of each line of CHECK's page, at INDEXES
is the page's text recognition (RECOGNISED-LINE-P)."
  (some (lambda (index) (recognised-line-p check (nth index lines))) indexes))

(defun figure-line-place (check texts lines index &optional (mark (car (last (nth index lines)))) value)
  "The line INDEX of TEXTS, lines of CHECK's page as FIGURE-LINE makes them,
LINES their runs, as a finding's details name it: its words, where MARK
stands on the page, its last run unless given, and where VALUE stands in it
\(FIGURE-PLACE-WORDS)."
  (format nil "~s, ~a~a" (figure-line-words (nth index texts)) (mark-words (page-check-page check) mark)
          (figure-place-words mark value)))

(defun check-number-findings (check lines read)
  "Add to CHECK the numbers of LINES, the runs of each line of its page, READ
as FIGURE-LINE reads them, whose check digit fails (CHECK-NUMBERS-IN), a
label ending a line read with the number under it or after it on the next
\(TEXT-BELOW). Where a font's glyphs draw other figures than copying gives
\(MARK-DRAWN), the numbers are checked as drawn too, as a reader sees them.
Worth a look for text drawn, for the record for a scan's text recognition."
  (let ((seen '()) (drawn '()) (drawn-marks '()) (recognised '()) (recognised-marks '()))
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
    (flet ((found (read failed marks)
             (figure-finding check read
                             (format nil "Page ~d has ~:[a number~;numbers~] whose check fails: ~{~a~^; ~}."
                                     (page-check-number check) (rest failed)
                                     (loop for (kind number) in failed collect (check-number-words kind number)))
                             " A figure changed by hand, or a slip where the number was written."
                             #.(concatenate 'string "Organisation, personal identity, bankgiro, plusgiro and card "
                                                    "numbers and OCR references end in a check digit by the modulus "
                                                    "10 method, a KID by modulus 10 or 11, an IBAN and a creditor "
                                                    "reference (RF) check modulo 97; one changed without its check "
                                                    "digit fails. Only numbers right after their label and written "
                                                    "as such numbers are, are taken.")
                             (sort (copy-list marks) #'> :key #'mark-y0)
                             (loop for (nil nil text) in failed collect (format nil "~s" text)))))
      (when drawn (found nil (nreverse drawn) drawn-marks))
      (when recognised (found t (nreverse recognised) recognised-marks)))))

(defun net-and-vat-finding (check lines texts)
  "Add to CHECK a net and VAT of its page that do not add up to its total
\(UNBALANCED-SUMS): TEXTS its lines as FIGURE-LINE makes them, LINES their
runs."
  (let ((sums (unbalanced-sums (page-amounts texts))))
    (when sums
      (destructuring-bind (net vat totals) sums
        (let ((entries (list* net vat totals)))
          (figure-finding check (lines-recognised-p check lines (mapcar #'fourth entries))
                          (format nil "Page ~d states a net of ~a and VAT of ~a, which make ~a, but its total~:[ is~;s ~
                                       are~] ~{~a~^, ~}."
                                  (page-check-number check) (third net) (third vat)
                                  (amount-words (+ (second net) (second vat))) (rest totals) (mapcar #'third totals))
                          #.(concatenate 'string " A figure changed by hand without those that add up to it, or "
                                                 "a total of more than this net and VAT.")
                          #.(concatenate 'string "An invoice's net and VAT add up to its total, öre rounded to whole "
                                                 "kronor at most; a page naming one net and one VAT, with no other "
                                                 "amount between them and the total, is checked.")
                          (sort (loop for entry in entries append (copy-list (nth (fourth entry) lines))) #'>
                                :key #'mark-y0)
                          (loop for entry in entries collect (format nil "~s" (nth (fourth entry) texts)))))))))

(defun sum-findings (check lines texts rows mark-of &optional value-of)
  "Add to CHECK a finding for each of ROWS, (TOTAL SUM ITEMS) as
UNSUMMED-TOTALS gives them, a total the amounts above it do not add up to:
TEXTS the lines of its page as FIGURE-LINE makes them, LINES their runs,
TOTAL and ITEMS indexes into both. MARK-OF gives the run of a line's
amount; VALUE-OF, given for a column of a table (INNER-COLUMNS), its
amount."
  (loop for (total sum items) in rows
        do (figure-finding check (lines-recognised-p check lines (cons total items))
                           (format nil "On page ~d the ~:d amounts above ~s~:[~; in its column~] add up to ~a, not to ~
                                        the amount it states."
                                   (page-check-number check) (length items) (figure-line-words (nth total texts))
                                   value-of (amount-words sum))
                           #.(concatenate 'string " A figure changed without the total that adds it up, or a "
                                                  "total of other figures than those above it.")
                           #.(concatenate 'string "A line labelled a total adds up the amounts listed above it, in "
                                                  "the column it stands in. A forger changes the figure that matters "
                                                  "and cannot change every figure that depends on it without forging "
                                                  "more. A total is judged against three amounts or more, and agrees "
                                                  "when it adds up the last of them, however many, with or without "
                                                  "the total before.")
                           (mapcar mark-of (cons total items))
                           (loop for index in (cons total items)
                                 collect (figure-line-place check texts lines index (funcall mark-of index)
                                                            (and value-of (funcall value-of index))))
                           :place :field)))

(defun unmultiplied-finding (check lines texts)
  "Add to CHECK the rows of a table on its page whose amount is not their
quantity times their price (UNMULTIPLIED-ROWS): TEXTS its lines as
FIGURE-LINE makes them, LINES their runs."
  (let ((rows (unmultiplied-rows texts)))
    (when rows
      (figure-finding check (lines-recognised-p check lines (mapcar #'first rows))
                      (format nil "On page ~d, ~:d row~:p of a table ~:*~[~;does~:;do~] not multiply out as its other ~
                                   rows do: ~{~s, where quantity and price make ~a~^; ~}."
                              (page-check-number check) (length rows)
                              (loop for (index product) in rows
                                    append (list (figure-line-words (nth index texts)) (amount-words product))))
                      " A price, a quantity or an amount changed without the others."
                      #.(concatenate 'string "Where three rows or more of a table end in a quantity, a price and "
                                             "their product, a row that does not multiply out was changed in one "
                                             "figure and not the others.")
                      (loop for (index) in rows append (last (nth index lines)))
                      (mapcar (lambda (row) (figure-line-place check texts lines (first row))) rows)
                      :place :field
                      :misread " As read by the page's text recognition, which may have misread a digit."))))

(defun pay-finding (check lines texts amounts)
  "Add to CHECK a net pay on its page that is not the gross pay less the
deductions (UNBALANCED-PAY): TEXTS its lines as FIGURE-LINE makes them,
LINES their runs, AMOUNTS their labelled amounts (LABELLED-AMOUNTS)."
  (let ((pay (unbalanced-pay amounts)))
    (when pay
      (destructuring-bind (gross deductions net) pay
        (figure-finding check (lines-recognised-p check lines (mapcar #'first pay))
                        (format nil "On page ~d the net pay stated, ~a, is not the gross pay of ~a less the deductions ~
                                     of ~a, which make ~a."
                                (page-check-number check) (amount-words (second net)) (amount-words (second gross))
                                (amount-words (second deductions))
                                (amount-words (- (second gross) (second deductions))))
                        #.(concatenate 'string " One of the three changed without the others; or pay that is not "
                                               "taxed, expenses paid back, added after the deductions.")
                        #.(concatenate 'string "A payslip's net pay is its gross pay less its deductions. A page "
                                               "stating each of the three once is checked, to the hundredth.")
                        (list (third net) (third gross) (third deductions))
                        (loop for (index value mark) in (list net gross deductions)
                              collect (figure-line-place check texts lines index mark value))
                        :place :field)))))

(defun balance-findings (check lines texts)
  "Add to CHECK a running balance on its page that breaks from one row to the
next, and a closing balance that is not the last row's (BROKEN-BALANCES):
TEXTS its lines as FIGURE-LINE makes them, LINES their runs."
  (loop for (index kind before amount) in (broken-balances texts)
        do (multiple-value-bind (words cause)
               (balance-break-words (page-check-number check) kind (figure-line-words (nth index texts)) before amount)
             (figure-finding check (lines-recognised-p check lines (list index)) words cause *balance-rule*
                             (last (nth index lines)) (list (figure-line-place check texts lines index))
                             :place :field))))

(defun weekday-findings (check lines texts)
  "Add to CHECK each weekday written beside a date on its page that is not
that date's (WRONG-WEEKDAYS): TEXTS its lines as FIGURE-LINE makes them,
LINES their runs."
  (loop for text in texts for line in lines
        do (loop for (written day start end) in (wrong-weekdays text)
                 do (figure-finding check (recognised-line-p check line)
                                    (format nil "Page ~d writes ~s: ~a was a ~a, not a ~a."
                                            (page-check-number check) (subseq text start end) (day-words day)
                                            (weekday-name (weekday day)) (weekday-name written))
                                    " A date changed without the weekday beside it, or a slip where it was written."
                                    #.(concatenate 'string "A weekday written right beside a date is that date's day "
                                                           "of the week. Only a date whose day and month cannot be "
                                                           "swapped is checked.")
                                    line
                                    (list (format nil "~s, ~a" (figure-line-words text)
                                                  (mark-words (page-check-page check) (first line))))
                                    :place :field))))

(defun check-figures (check)
  "Add to CHECK the numbers of its page's text whose check digit fails
\(CHECK-NUMBER-FINDINGS); a net and VAT that do not add up to the page's
total (NET-AND-VAT-FINDING); a total the amounts above it do not add up to,
and a total inside a table that the amounts above it in its column do not
add up to (SUM-FINDINGS); rows of a table whose amount is not their
quantity times their price (UNMULTIPLIED-FINDING); a net pay that is not the
gross less the deductions (PAY-FINDING); a running balance that breaks from
one row to the next, and a closing balance that is not the last row's
\(BALANCE-FINDINGS); and a weekday written beside a date that is not that
date's (WEEKDAY-FINDINGS). Worth a look for text the page draws; for the
record for a scan's text recognition, which may misread a digit."
  (let* ((lines (page-lines-of (page-check-texts check)))
         (read (mapcar (lambda (line) (multiple-value-list (figure-line line))) lines))
         (texts (mapcar #'first read))
         (amounts (mapcar (lambda (entry) (labelled-amounts (line-cells (first entry) (second entry)))) read)))
    (check-number-findings check lines read)
    (net-and-vat-finding check lines texts)
    (sum-findings check lines texts (unsummed-totals texts) (lambda (index) (car (last (nth index lines)))))
    (dolist (column (inner-columns amounts))
      (sum-findings check lines texts (unsummed-rows (mapcar (lambda (row) (subseq row 0 3)) column))
                    (lambda (index) (fourth (find index column :key #'first)))
                    (lambda (index) (second (find index column :key #'first)))))
    (unmultiplied-finding check lines texts)
    (pay-finding check lines texts amounts)
    (balance-findings check lines texts)
    (weekday-findings check lines texts)))

;;; Figures of the whole document

(defparameter *carried-labels* '("carried forward" "carried over" "carried down" "c/f" "transport" "att överföra")
  "Words in a line's label that say its amount is carried to the next page.")

(defparameter *brought-labels* '("brought forward" "brought over" "brought down" "b/f" "transport"
                                 "från föregående sida")
  "Words in a line's label that say its amount is brought from the page before.")

(defparameter *single-figure-labels*
  '("total amount due" "amount due" "total due" "balance due" "total to pay" "amount to pay" "invoice total"
    "order total" "grand total" "net pay" "closing balance" "summa att betala" "att betala" "slutsumma"
    "totalbelopp" "fakturabelopp" "utgående saldo" "nettolön")
  "Labels of a figure a document states as its one result, lower case, the
longer first where one holds another: what is to pay, the net pay, the
closing balance. Stated twice, it is the same both times.")

(defparameter *payment-phrases* '("direct debit" "will collect" "will be collected" "will be taken" "will take"
                                  "please pay" "autogiro")
  "Words that say a sentence names the sum to be paid, lower case.")

(defparameter *document-figure-pages* 50
  "The most pages of a document whose figures are set against each other
from page to page (CHECK-DOCUMENT-FIGURES).")

(defun sentence-amounts (text)
  "The amounts written with two decimals (PLAIN-NUMBER) among the words of
TEXT, a line of running text, in hundredths: a full stop, comma or bracket
after a word is not part of it."
  (loop for word in (uiop:split-string (substitute #\Space #\Tab text) :separator " ")
        for (value amount-p) = (multiple-value-list (plain-number (string-trim "().,;:" word)))
        when (and value amount-p) collect value))

(defun document-figures (pdf revision)
  "The figures of REVISION's first *DOCUMENT-FIGURE-PAGES* pages, one entry a
page: a list of (TEXT MARKS AMOUNTS) for each line from the top, TEXT as
FIGURE-LINE makes it, MARKS its runs and AMOUNTS its labelled amounts
\(LABELLED-AMOUNTS)."
  (loop for page in (pages-of pdf revision) repeat *document-figure-pages*
        collect (loop for line in (page-text-lines pdf revision page)
                      collect (multiple-value-bind (text runs) (figure-line line)
                                (list text line (labelled-amounts (line-cells text runs)))))))

(defparameter *prose-words-least* 5
  "The fewest words (PROSE-WORDS) a line must hold to be running text: a
line without a figure or a field's label to be boilerplate
\(PLACE-WORTH-CHANGING), a sentence to state an amount (CHECK-DOCUMENT-FIGURES).
A short line may be a name.")

(defun prose-words (text)
  "How many words TEXT holds, a word being a run between spaces or tabs
holding a letter."
  (count-if (lambda (w) (some #'alpha-char-p w)) (uiop:split-string text :separator '(#\Space #\Tab))))

(defun check-document-figures (pdf revision)
  "The findings of figures of REVISION that disagree from one place of the
document to another: a total carried to the next page (*CARRIED-LABELS*)
unlike the one brought from the page before (*BROUGHT-LABELS*); a running
balance that breaks where the balance before stands on the page before
\(BROKEN-BALANCES over the pages together, less what each page shows
alone); a figure
the document states as its one result (*SINGLE-FIGURE-LABELS*) stated with
two amounts; and a sentence naming the sum to be paid (*PAYMENT-PHRASES*)
with an amount that no other line of the document states, while the
document labels what is to pay. A sentence is a line of running text: no
gap between columns in it, more than *PROSE-WORDS-LEAST* words, no amount
ending it, so that a statement's row naming a direct debit is none. Each names the lines and where they are. Worth a
look for text the pages draw; for the record where every line it rests on
is a scan's text recognition."
  (let* ((pages (pages-of pdf revision))
         (figures (document-figures pdf revision))
         (out '()))
    (labels ((entry-words (number entry mark)
               (format nil "page ~d: ~s, ~a~a" number
                       (string-trim " " (substitute #\Space #\Tab (first entry)))
                       (mark-words (nth (1- number) pages) mark) (figure-place-words mark)))
             (found-here (words rule places)
               ;; PLACES: each (PAGE-NUMBER ENTRY MARK), the first the place the finding is put at
               (let ((read (every (lambda (place)
                                    (every #'invisible-text-p (second (second place))))
                                  places)))
                 (push (found (if read :record :look)
                              (if read
                                  (format nil "~a As read by the pages' text recognition, which may have misread ~
                                               or dropped a digit; the pictures were not compared."
                                          words)
                                  words)
                              :rule rule :revision (revision-index revision) :page (first (first places))
                              :details (loop for (number entry mark) in places
                                             collect (entry-words number entry mark))
                              :evidence (loop for (number nil mark) in places repeat 4
                                              append (mark-evidence pdf revision mark
                                                                    (mark-words (nth (1- number) pages) mark)))
                              :objects (remove-duplicates
                                        (remove nil (loop for (nil nil mark) in places collect (mark-source mark))))
                              :method :figure-unchecked :place :field)
                       out)))
             (labelled (number phrases &key from-end)
               ;; the first (or last) amount of page NUMBER whose label says one of PHRASES: (ENTRY VALUE MARK)
               (let ((all (loop for entry in (nth (1- number) figures)
                                append (loop for (label value mark) in (third entry)
                                             when (and mark (label-says-p label phrases))
                                               collect (list entry value mark)))))
                 (if from-end (car (last all)) (first all)))))
      ;; carried from page to page
      (loop for number from 1 below (length figures)
            do (let ((carried (labelled number *carried-labels* :from-end t))
                     (brought (labelled (1+ number) *brought-labels*)))
                 (when (and carried brought (/= (second carried) (second brought)))
                   (found-here (format nil "Page ~d carries ~a forward and page ~d brings ~a forward: the two ~
                                            disagree. A figure changed on one of the pages, or a page put in from ~
                                            another copy of the document."
                                       number (amount-words (second carried)) (1+ number)
                                       (amount-words (second brought)))
                               #.(concatenate 'string "A total carried forward at the foot of a page is the total "
                                                      "brought forward at the head of the next.")
                               (list (list (1+ number) (first brought) (third brought))
                                     (list number (first carried) (third carried)))))))
      ;; a running balance from one page to the next
      (when (rest figures)
        (let ((lines (loop for page-figures in figures for number from 1
                           append (mapcar (lambda (entry) (cons number entry)) page-figures)))
              (own (loop for page-figures in figures
                         append (loop for (index) in (broken-balances (mapcar #'first page-figures))
                                      collect (nth index page-figures)))))
          (loop for (index kind before amount) in (broken-balances (mapcar #'second lines))
                for (number . entry) = (nth index lines)
                unless (member entry own :test #'eq)
                  do (multiple-value-bind (words cause)
                         (balance-break-words number kind (string-trim " " (substitute #\Space #\Tab (first entry)))
                                              before amount)
                       (found-here (format nil "~a~a The balance before it stands on the page before: a page put ~
                                                in from another copy does this too, and so does another account ~
                                                beginning on this page."
                                           words cause)
                                   *balance-rule*
                                   (list (list number entry (car (last (second entry))))))))))
      ;; one figure stated twice
      (let ((stated (make-hash-table :test 'equal)) (every-amount (make-hash-table)))
        (loop for page-figures in figures for number from 1
              do (dolist (entry page-figures)
                   (loop for (label value mark) in (third entry)
                         do (pushnew entry (gethash value every-amount) :test #'eq)
                            (let ((phrase (and mark (label-says-p label *single-figure-labels*))))
                              (when phrase (push (list number entry mark value) (gethash phrase stated)))))))
        (loop for phrase in *single-figure-labels*
              for places = (reverse (gethash phrase stated))
              for amounts = (remove-duplicates (mapcar #'fourth places))
              when (and (= 2 (length amounts)) (<= (length places) 6))
                do (found-here (format nil "The document states its ~a as ~a and as ~a. One of them was changed ~
                                            without the other; or the file holds two documents, each with its own."
                                       phrase (amount-words (first amounts)) (amount-words (second amounts)))
                               #.(concatenate 'string "A figure a document states as its one result, what is to "
                                                      "pay, the net pay or the closing balance, is the same "
                                                      "wherever it is stated. A label stated with exactly two "
                                                      "amounts is named.")
                               (mapcar (lambda (place) (subseq place 0 3)) places)))
        ;; a sentence naming the sum to pay
        (let ((results (loop for phrase in *single-figure-labels*
                             append (mapcar (lambda (place) (cons phrase place)) (reverse (gethash phrase stated))))))
          (when results
            (loop for page-figures in figures for number from 1
                  do (dolist (entry page-figures)
                       (let ((lower (string-downcase (first entry))))
                         ;; running text: one cell, no gap between columns, more words than a label, no
                         ;; amount ending it as a table's row ends
                         (when (and (label-says-p lower *payment-phrases*)
                                    (not (find #\Tab (string-trim '(#\Space #\Tab) lower)))
                                    (>= (prose-words lower) *prose-words-least*)
                                    (notany #'fourth (third entry)))
                           (let ((other (find-if-not (lambda (value)
                                                       ;; stated by a line other than the sentence itself
                                                       (remove entry (gethash value every-amount) :test #'eq))
                                                     (sentence-amounts (first entry)))))
                             (when other
                               (found-here
                                (format nil "A sentence on page ~d names ~a to be paid, an amount no line of the ~
                                             document states, while it states its ~a as ~a. The figure was ~
                                             changed in one place and not in the sentence; or the sentence names ~
                                             an instalment."
                                        number (amount-words other) (car (first results))
                                        (amount-words (fourth (cdr (first results)))))
                                #.(concatenate 'string "Where a document labels what is to pay and a sentence "
                                                       "names a sum to be collected or paid, the sentence's "
                                                       "amount is one the document states.")
                                (cons (list number entry (first (second entry)))
                                      (mapcar (lambda (result) (subseq (cdr result) 0 3)) results)))))))))))))
    (nreverse out)))

;;; Places worth changing

(defparameter *field-words* '("namn" "name" "konto" "account" "kontonummer" "referens" "reference" "betalare"
                              "mottagare" "payee" "underskrift" "signature" "signed" "belopp" "amount")
  "Words that name a field a forger would change and that the labels of
*AMOUNT-ROLES*, *CHECK-NUMBER-LABELS* and *OWN-DATE-LABELS* leave out:
names, accounts, references, signatures and amounts, lower case.")

(defun field-label-p (lower)
  "True when LOWER, a line in lower case, holds the label of a field worth
changing: of a total, net or VAT (*AMOUNT-ROLES*), a number with a check
digit (*CHECK-NUMBER-LABELS*), the document's own date (*OWN-DATE-LABELS*)
or a name, account, reference, signature or amount (*FIELD-WORDS*)."
  (flet ((holds (word) (search word lower)))
    (or (some (lambda (group) (some #'holds (rest group))) *amount-roles*)
        (some (lambda (group) (some #'holds (rest group))) *check-number-labels*)
        (some #'holds *own-date-labels*)
        (some #'holds *field-words*))))

(defun place-worth-changing (text)
  "What the line TEXT, where a sign lies, is to a forger: :FIELD when a
change there would serve one, a line holding a figure (an amount, a total,
a date, an account, reference or check number, an IBAN all hold figures)
or a field's label (FIELD-LABEL-P); :PROSE when it is running text or
boilerplate, *PROSE-WORDS-LEAST* words or more without either (terms and
conditions, a letter's body, a printed notice); NIL when it cannot be told,
a short line without figures, which may be a name or a heading, or no text
at all. A sign of a kind honest files show (*HONEST-KINDS*) on :PROSE is
noted for the record, not named worth a look."
  (cond ((null text) nil)
        ((or (some #'digit-char-p text) (field-label-p (string-downcase text))) :field)
        ((>= (prose-words text) *prose-words-least*) :prose)))

(defun page-text-lines (pdf revision page)
  "The lines of text PAGE of REVISION of PDF draws, its invisible text (a
scan's text recognition) among them (PAGE-LINES-OF)."
  (page-lines-of (remove-if-not (lambda (m) (eq (mark-kind m) :text)) (coerce (marks-of pdf revision page) 'list))))

(defun line-at-box (lines box)
  "The text of the first of LINES (PAGE-TEXT-LINES) one of whose runs
overlaps BOX, (X0 Y0 X1 Y1) on the page, Y0 the lower; NIL for none."
  (destructuring-bind (x0 y0 x1 y1) box
    (let ((line (find-if (lambda (line)
                           (some (lambda (m) (and (< (mark-x0 m) x1) (< x0 (mark-x1 m))
                                                  (< (mark-y0 m) y1) (< y0 (mark-y1 m))))
                                 line))
                         lines)))
      (and line (values (line-text line))))))
