;;;; dates.lisp --- The dates a document states against the dates its file gives.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A program writes a document's date at or before it saves the file, so
;;;; the date an invoice or letter gives as its own cannot be later than
;;;; every date the file gives of itself: its creation and change dates in
;;;; the document information and the XMP, in every save. Dates are read
;;;; from the page's text only where they are plainly the document's own
;;;; (right after Fakturadatum, Invoice date, Datum or Date, or a letter's
;;;; date line), written so that the day and month cannot be swapped, and
;;;; compared with the latest day any time zone had at the file's dates. A
;;;; picture or scan dated after the file, and the file's own dates out of
;;;; order between saves, are given for the record.

(in-package #:palimpsest)

;;; Dates as a page writes them

(defparameter *text-month-names*
  '((1 "januari" "january" "jan") (2 "februari" "february" "feb") (3 "mars" "march" "mar") (4 "april" "apr")
    (5 "maj" "may") (6 "juni" "june" "jun") (7 "juli" "july" "jul") (8 "augusti" "august" "aug")
    (9 "september" "sept" "sep") (10 "oktober" "october" "okt" "oct") (11 "november" "nov") (12 "december" "dec"))
  "The names of the months a page's date may be written with, in Swedish and
English, whole or short, lower case, each list beginning with its month's
number.")

(defparameter *text-date-years* '(1980 . 2099)
  "The first and last year a date in a page's text is taken with: a figure
of four digits outside them is not read as a year.")

(defun month-word-at (text start)
  "The month whose name (*TEXT-MONTH-NAMES*, case not counting) is written
whole at START of TEXT, as two values: the month, 1 to 12, and where its
name ends, past a full stop after a short name; NIL when no month's name is
there."
  (let* ((end (or (position-if-not #'alpha-char-p text :start start) (length text)))
         (entry (and (< start end (+ start 10))
                     (find-if (lambda (e) (member-if (lambda (name) (string-equal name text :start2 start :end2 end))
                                                     (rest e)))
                              *text-month-names*))))
    (when entry
      (values (first entry)
              (if (and (< end (length text)) (char= (char text end) #\.) (<= (- end start) 4)) (1+ end) end)))))

(defun date-year-at (text start)
  "The year written in four digits at START of TEXT, within
*TEXT-DATE-YEARS*, and where it ends; NIL for none."
  (multiple-value-bind (digits end) (digits-run text start)
    (when (= (length digits) 4)
      (let ((year (parse-integer digits)))
        (and (<= (car *text-date-years*) year (cdr *text-date-years*)) (values year end))))))

(defun past-chars (text start chars)
  "Where the run of CHARS in TEXT from START ends."
  (or (position-if-not (lambda (c) (find c chars)) text :start start) (length text)))

(defun ordinal-end (text end)
  "Where a day written up to END of TEXT ends, past the ordinal's letters
after it (15th, 1st, 15:e) and a full stop (15.)."
  (let ((lower (string-downcase (subseq text end (min (length text) (+ end 3))))))
    (let ((end (+ end (or (loop for suffix in '("st" "nd" "rd" "th" ":e" ":a")
                                when (and (uiop:string-prefix-p suffix lower)
                                          (or (= (length lower) 2) (not (alpha-char-p (char lower 2)))))
                                  return 2)
                          0))))
      (if (and (< end (length text)) (char= (char text end) #\.)) (1+ end) end))))

(defun date-ends-p (text end)
  "True when a date of TEXT ending at END ends there: what follows is no
letter or digit, nor a mark running it on into more digits, unless it is a
T and a digit, the time ISO 8601 writes after a date (2024-03-15T10:00)."
  (let ((n (length text)))
    (or (>= end n)
        (and (char= (char text end) #\T) (< (1+ end) n) (digit-char-p (char text (1+ end))))
        (not (or (alphanumericp (char text end))
                 (and (find (char text end) "./-") (< (1+ end) n) (digit-char-p (char text (1+ end)))))))))

(defun text-date-at (text start)
  "The day of a date written at START of TEXT, as two values: its day number
\(DAY-NUMBER) and where the date ends; NIL when no date that is in the
calendar is written there, or one whose day and month could be swapped.
Read: 2024-03-15; 15 mars 2024, 15th March 2024, 15. mars 2024; March 15,
2024; 15/3/2024, 15.03.2024, 15-03-2024 and 3/15/2024, where only one of
the two numbers can be the month, or both are the same. A year has four
digits (*TEXT-DATE-YEARS*)."
  (let ((n (length text)))
    (flet ((dated (year month day end)
             (and year (valid-date-p year month day) (date-ends-p text end) (values (day-number year month day) end)))
          (year-after (at)
            ;; a year after a comma or spaces, at least one of them
            (let* ((comma (if (and (< at n) (char= (char text at) #\,)) (1+ at) at))
                   (from (past-chars text comma " ")))
              (and (> from at) (date-year-at text from)))))
      (multiple-value-bind (first after) (digits-run text start)
        (case (length first)
          (0 (multiple-value-bind (month end) (month-word-at text start)
               (let ((at (and month (past-chars text end " "))))
                 (when (and at (> at end))
                   (multiple-value-bind (day day-end) (digits-run text at)
                     (when (<= 1 (length day) 2)
                       (multiple-value-bind (year year-end) (year-after (ordinal-end text day-end))
                         (dated year month (parse-integer day) year-end))))))))
          (4 (when (and (< (+ after 5) n) (char= (char text after) #\-) (char= (char text (+ after 3)) #\-))
               (multiple-value-bind (month month-end) (digits-run text (1+ after))
                 (multiple-value-bind (day day-end) (digits-run text (1+ month-end))
                   (when (and (= (length month) 2) (= (length day) 2))
                     (dated (date-year-at text start) (parse-integer month) (parse-integer day) day-end))))))
          ((1 2)
           (let ((a (parse-integer first)))
             (if (and (< after n) (find (char text after) "./-") (< (1+ after) n) (digit-char-p (char text (1+ after))))
                 ;; numbers apart by one mark, the same twice, the year last
                 (multiple-value-bind (second second-end) (digits-run text (1+ after))
                   (when (and (<= 1 (length second) 2) (< second-end n)
                              (char= (char text second-end) (char text after)))
                     (multiple-value-bind (year end) (date-year-at text (1+ second-end))
                       (let ((b (parse-integer second)))
                         (cond ((= a b) (dated year a a end))
                               ((and (> a 12) (<= b 12)) (dated year b a end))
                               ((and (> b 12) (<= a 12) (char= (char text after) #\/)) (dated year a b end)))))))
                 ;; a day, then its month's name and the year
                 (let* ((day-end (ordinal-end text after))
                        (at (past-chars text day-end " ")))
                   (when (> at day-end)
                     (multiple-value-bind (month end) (month-word-at text at)
                       (when month
                         (multiple-value-bind (year year-end) (year-after end)
                           (dated year month a year-end))))))))))))))

(defparameter *date-range-words* '("-" "–" "—" "till" "to" "until" "t.o.m." "tom" "och" "and")
  "What may stand between two dates that make a range or a pair (2024-03-01
– 2024-03-31, 1 mars till 31 mars 2024): neither is a document's own date.")

(defun range-end-p (text start)
  "True when what TEXT holds before START ends a range's first part: a word
or mark of *DATE-RANGE-WORDS* after a figure or a month's name (1 mars till,
March 1 -)."
  (flet ((blank-end (end)
           ;; where the text before END ends, spaces and tabs before END passed over
           (1+ (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text :end end :from-end t) -1))))
    (let* ((before (blank-end start))
           (word (find-if (lambda (w)
                            (let ((at (- before (length w))))
                              (and (>= at 0) (string-equal w text :start2 at :end2 before)
                                   (or (not (alpha-char-p (char w 0))) (zerop at)
                                       (not (alpha-char-p (char text (1- at))))))))
                          *date-range-words*)))
      (when word
        (let* ((rest (blank-end (- before (length word))))
               (from (or (position-if-not #'alpha-char-p text :end rest :from-end t) -1)))
          (and (plusp rest)
               (or (digit-char-p (char text (1- rest)))
                   (and (< (1+ from) rest) (month-word-at text (1+ from))))))))))

(defun date-begins-p (text at)
  "True when a date may begin at AT of TEXT: a letter or digit there, after
no letter or digit, nor after a digit and a mark."
  (and (alphanumericp (char text at))
       (or (zerop at)
           (and (not (alphanumericp (char text (1- at))))
                (not (and (find (char text (1- at)) "./-:") (> at 1) (digit-char-p (char text (- at 2)))))))))

(defun range-follows-p (text end)
  "True when a date of TEXT ending at END begins a range or a pair: a word
or mark of *DATE-RANGE-WORDS* follows it, then another date, with no more
than a few spaces or tabs on either side of the word. Only the characters
right after END are read."
  (flet ((past-blanks (at)
           ;; past up to four spaces or tabs from AT; NIL when more follow
           (let ((stop (min (length text) (+ at 5))))
             (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text :start at :end stop)
                 (and (< stop (+ at 5)) stop)))))
    (let ((at (past-blanks end)))
      (some (lambda (w)
              (let* ((word-end (+ at (length w)))
                     (next (and (<= word-end (length text)) (string-equal w text :start2 at :end2 word-end)
                                (past-blanks word-end))))
                (and next (< next (length text)) (date-begins-p text next) (text-date-at text next))))
            (and at *date-range-words*)))))

(defun text-dates-in (text)
  "The dates written in TEXT (TEXT-DATE-AT), each (DAY START END): DAY its
day number, from START to END of TEXT. A date begins where DATE-BEGINS-P
says one may; two dates with only a word or mark of *DATE-RANGE-WORDS*
between them, a range or a pair (RANGE-FOLLOWS-P), are left out, and so is
a date ending a range whose first part is no whole date (RANGE-END-P)."
  (let ((out '()) (at 0) (n (length text)))
    (loop while (< at n)
          do (if (date-begins-p text at)
                 (multiple-value-bind (day end) (text-date-at text at)
                   (if day
                       (progn (push (list day at end) out) (setf at end))
                       (setf at (or (position-if-not #'alphanumericp text :start at) n))))
                 (incf at)))
    (loop with follows = nil
          for d in (nreverse out)
          for ends-pair = follows
          do (setf follows (range-follows-p text (third d)))
          unless (or ends-pair follows (range-end-p text (second d)))
            collect d)))

(defun day-words (day)
  "The day number DAY as a date is written in ISO 8601: 2024-03-15."
  (multiple-value-bind (year month date) (day-date day)
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

;;; A document's own dates

(defparameter *own-date-labels*
  '("utfärdandedatum" "dokumentdatum" "fakturadatum" "faktura datum" "kvittodatum" "date of invoice"
    "invoice date" "receipt date" "document date" "date of issue" "issue date" "utfärdad" "utfärdat" "utställd"
    "issued" "dated" "datum" "date")
  "The labels after which a date is the document's own: the date an invoice,
receipt or letter was issued, lower case, the longest of a kind first. A
due date, a delivery date, a period or a date the text announces has other
labels and is never taken.")

(defparameter *own-date-plain-labels* '("utfärdad" "utfärdat" "utställd" "issued" "dated" "datum" "date")
  "The labels of *OWN-DATE-LABELS* that are taken only where no word stands
right before them: Due date, Delivery date or Datum för leverans are not
the document's own.")

(defparameter *own-date-general-labels* '("datum" "date")
  "The labels of *OWN-DATE-LABELS* that say no more than that a date
follows. Agendas, invitations, tickets and notices are often dated after
their file was made, so a date after one of these labels is for the
record.")

(defparameter *date-label-fillers* '("den" "on")
  "Words that may stand between a label of *OWN-DATE-LABELS* and its date
\(Utfärdad den 15 mars 2024, Issued on 15 March 2024).")

(defparameter *not-own-date-words*
  '("dat" "förfall" "due" "period" "giltig" "valid" "expir" "leverans" "deliver" "betala" "pay" "senast" "from"
    "från" "start" "slut" "end" "till")
  "Parts of words that tell a date is not a letter's own date line when they
stand before it on its line or on the line above: a label of some date
\(Förfallodatum, Due date) or of a period.")

(defun word-before-p (text at)
  "True when a word ends right before AT of TEXT, only spaces between: not
a tab, which stands for a gap between columns."
  (let ((before (position #\Space text :end at :from-end t :test-not #'char=)))
    (and before (alpha-char-p (char text before)))))

(defun own-dates-in (text &optional below)
  "The document's own dates in TEXT, a page's line as FIGURE-LINE makes it,
each (DAY WRITTEN BELOW-P GENERAL-P): DAY its day number, WRITTEN the date
as the line writes it, BELOW-P true when it was read from the next line,
GENERAL-P true when its label is one of *OWN-DATE-GENERAL-LABELS*. A date
is taken right after a label of *OWN-DATE-LABELS*, a plain one with no word
before it (*OWN-DATE-PLAIN-LABELS*), also when text recognition joined the
two (Datum2025-03-10), and read where it begins only (TEXT-DATE-AT), a
range or pair beginning there left out (RANGE-FOLLOWS-P). A label ending
TEXT, or followed by a gap between columns and no date (a row of labels
over a row of values), has its date read from the next line: BELOW, given
the label's place, gives that line and where in it the date may begin
\(TEXT-BELOW), or NIL."
  (let ((lower (string-downcase text)) (taken (make-hash-table)) (out '()))
    (flet ((date-at (line from)
             (multiple-value-bind (day end) (text-date-at line from)
               (and day (not (range-follows-p line end)) (list day (subseq line from end))))))
      (dolist (label *own-date-labels*)
        (loop for at = (search label lower) then (search label lower :start2 (1+ at))
              while at
              unless (and (member label *own-date-plain-labels* :test #'string=) (word-before-p text at))
                do (let* ((from (label-end lower at label *date-label-fillers*))
                          (here (and from (< from (length text)) (date-at text from)))
                          (there (and from (not here) below
                                      (or (= from (length text))
                                          (find #\Tab text :start (+ at (length label)) :end from))
                                      (multiple-value-bind (next start) (funcall below at)
                                        (and start (< start (length next)) (date-at next start)))))
                          (date (or here there)))
                     (when (and date (not (gethash at taken)))
                       (setf (gethash at taken) t)
                       (push (list (first date) (second date) (and there t)
                                   (and (member label *own-date-general-labels* :test #'string=) t))
                             out))))))
    (nreverse out)))

(defun not-own-date-text-p (text)
  "True when TEXT holds a word of *NOT-OWN-DATE-WORDS*."
  (let ((lower (string-downcase text)))
    (some (lambda (word) (search word lower)) *not-own-date-words*)))

(defun date-line-day (text &optional above)
  "The day TEXT, a page's line, states when it is a letter's date line: one
date with its month's name, alone or after the name of a place
\(Stockholm den 15 mars 2024, London, 15 March 2024); NIL otherwise, and
when the place's words or ABOVE, the line above, hold a word of
*NOT-OWN-DATE-WORDS*."
  (let* ((text (string-right-trim "." (string-trim '(#\Space #\Tab) text)))
         (dates (text-dates-in text)))
    (when (= 1 (length dates))
      (destructuring-bind (day start end) (first dates)
        (let* ((prefix (string-trim " ," (subseq text 0 start)))
               (prefix (if (uiop:string-suffix-p (string-downcase prefix) " den")
                           (string-right-trim " ," (subseq prefix 0 (- (length prefix) 4)))
                           prefix))
               (words (and (plusp (length prefix)) (uiop:split-string prefix :separator " "))))
          (and (= end (length text))
               (find-if #'alpha-char-p text :start start :end end)
               (<= (length words) 2)
               (every (lambda (w) (and (plusp (length w)) (upper-case-p (char w 0))
                                       (every (lambda (c) (or (alpha-char-p c) (char= c #\-))) w)))
                      words)
               (not (not-own-date-text-p prefix))
               (not (and above (not-own-date-text-p above)))
               day))))))

(defun page-own-dates (check)
  "The document's own dates CHECK's page states, each (DAY WRITTEN TEXT
MARKS RECOGNISED GENERAL): DAY its day number, WRITTEN as the page writes
it, TEXT the line (two lines, for a date under its label) it stands in,
MARKS the runs of that text, RECOGNISED true when they are a scan's text
recognition, GENERAL true when only a label of *OWN-DATE-GENERAL-LABELS*
gives it. Dates after their label (OWN-DATES-IN), and on the first page's
upper half a letter's date line (DATE-LINE-DAY); each day once, as a label
other than a general one gives it when one does."
  (let* ((lines (page-lines-of (page-check-texts check)))
         (read (mapcar (lambda (line) (multiple-value-list (figure-line line))) lines))
         (media (page-media (page-check-page check)))
         (middle (/ (+ (second media) (fourth media)) 2))
         (days (make-hash-table))
         (out '()))
    (flet ((add (day written text marks general)
             (let ((entry (list day written (shorten-string text 100 :one-line t) marks
                                (recognised-line-p check marks) general))
                   (had (gethash day days)))
               (cond ((null had) (push (setf (gethash day days) entry) out))
                     ((and (sixth had) (not general)) (replace had entry))))))
      (loop for ((here runs) . rest) on read
            for previous = nil then line-text
            for line-text = here
            for (line . more) on lines
            do (loop with below = (and rest (text-below runs (first (first rest)) (second (first rest))))
                     for (day written below-p general) in (own-dates-in here below)
                     do (if below-p
                            (add day written (format nil "~a ~a" here (first (first rest))) (append line (first more))
                                 general)
                            (add day written here line general)))
               (when (and (= 1 (page-check-number check)) (> (mark-y0 (first line)) middle))
                 (let ((day (date-line-day here previous)))
                   (when day (add day (string-trim '(#\Space #\Tab) here) here line nil))))))
    (nreverse out)))

;;; The dates the file gives

(defparameter *earliest-file-date* (encode-universal-time 0 0 0 15 6 1993 0)
  "The earliest date the file's own dates are believed from, the day the
first PDF specification was published: an earlier one is a clock never set.")

(defun file-dates (pdf revision)
  "The dates PDF gives of itself as REVISION has it, each (KIND PLACE TEXT
TIME ZONE): KIND :made or :changed, PLACE the field (/CreationDate,
xmp:ModifyDate), TEXT as written, TIME its universal time and ZONE its zone
in minutes or NIL (DATE-OF). The document information of every save up to
REVISION, and REVISION's XMP CreateDate, ModifyDate and MetadataDate; each
text once, none before *EARLIEST-FILE-DATE*."
  (let ((out '()))
    (flet ((add (kind place text)
             (when (and text (not (find text out :key #'third :test #'string=)))
               (multiple-value-bind (time zone) (date-of text)
                 (when (and time (>= time *earliest-file-date*))
                   (push (list kind place text time zone) out))))))
      (loop for r across (pdf-revisions pdf)
            do (add :made "/CreationDate" (info-text pdf r "/CreationDate"))
               (add :changed "/ModDate" (info-text pdf r "/ModDate"))
            until (eq r revision))
      (let ((fields (xmp-of pdf revision)))
        (add :made "xmp:CreateDate" (xmp-get fields "CreateDate"))
        (add :changed "xmp:ModifyDate" (xmp-get fields "ModifyDate"))
        (add :changed "xmp:MetadataDate" (xmp-get fields "MetadataDate"))))
    (nreverse out)))

(defun latest-day (time zone)
  "The latest day any time zone had at TIME, a universal time written with
ZONE minutes from UTC: the day 14 hours ahead of it, as far ahead as a zone
goes; when ZONE is NIL, TIME is read as its writer's local time in a zone
unknown, and the day 26 hours ahead of it is taken. A day number
\(DAY-NUMBER)."
  (floor (+ time (* 3600 (if zone 14 26))) 86400))

(defun file-date-words (entry)
  "ENTRY of FILE-DATES in words: its field and its date as written, a PDF's
date in its own zone (2024-03-01 12:00:00 +13:00)."
  (destructuring-bind (kind place text &rest rest) entry
    (declare (ignore kind rest))
    (format nil "~a ~a" place (if (char= (char text 0) #\D) (pdf-date-words text) text))))

(defun latest-file-date (dates &optional kind)
  "The entry of DATES (FILE-DATES) of KIND (any, for NIL) whose LATEST-DAY
is the latest, or NIL."
  (let ((these (if kind (remove kind dates :key #'first :test-not #'eq) dates)))
    (and these (reduce (lambda (a b) (if (>= (latest-day (fourth a) (fifth a)) (latest-day (fourth b) (fifth b))) a b))
                       these))))

;;; Pictures' own dates

(defun picture-made (sections)
  "When SECTIONS, what a picture says of itself (as JPEG-METADATA gives
them), say it was made, as four values: its universal time, its zone in
minutes or NIL, the field's label and its value as written; NIL for none."
  (let ((made (first (dates-among (picture-fields sections) *picture-made-labels*))))
    (when made
      (multiple-value-bind (time zone) (date-of (picture-date (second made)))
        (and time (values time zone (first made) (second made)))))))

(defun picture-dated-later (pdf revision mark page sections)
  "A finding for the record when SECTIONS, what the JPEG picture MARK on PAGE
of REVISION says of itself, say it was made after every date PDF gives of
itself (FILE-DATES): more than a minute after when both name their zone,
else more than *PICTURE-ZONES-APART*; NIL otherwise."
  (multiple-value-bind (time zone label value) (picture-made sections)
    (let ((latest (and time (latest-file-date (file-dates pdf revision)))))
      (when (and latest (> (- time (fourth latest)) (if (and zone (fifth latest)) 60 *picture-zones-apart*)))
        (found :record (format nil "The JPEG picture ~a (object ~d) on page ~d says it was made ~a (~a), after every ~
                                    date the file gives (the latest ~a): put in later, or a camera's or scanner's ~
                                    clock set wrong."
                               (mark-text mark) (mark-object mark) (1+ (page-index page)) value label
                               (file-date-words latest))
               :rule (format nil "A picture is made before the file it is put in is saved. Dates that both name their ~
                                  zone are compared to the minute; others, as EXIF's name none, within ~d hours."
                             (round *picture-zones-apart* 3600))
               :revision (revision-index revision) :page (1+ (page-index page)) :objects (list (mark-object mark))
               :evidence (nth-value 0 (object-evidence pdf revision (mark-object mark))))))))

;;; The check of a page

(defun dates-found (check strength days words rule)
  "Add to CHECK a finding of STRENGTH of DAYS, entries of PAGE-OWN-DATES,
saying WORDS (a format string given the page's number and the dates and
lines as words) by RULE."
  (page-found check strength
              (format nil words (page-check-number check)
                      (loop for (day written text) in days collect (format nil "~a (~s)" (day-words day) written)))
              rule
              (sort (remove-duplicates (loop for entry in days append (copy-list (fourth entry)))) #'> :key #'mark-y0)
              :details (loop for entry in days collect (format nil "~s" (third entry)))
              :method :text-dated-later))

(defun check-text-dates (check)
  "Add to CHECK the document's own dates its page states (PAGE-OWN-DATES)
later than the latest day any time zone had at every date the file gives
\(FILE-DATES, LATEST-DAY): worth a look for text the page draws, for the
record for a scan's text recognition, which may misread a figure, and for
a date only a general label gives (*OWN-DATE-GENERAL-LABELS*). Dates after
the file was made but not after its last change, and dates a scan's
text recognition reads after the day its scanner says it scanned the
page, are for the record."
  (let* ((pdf (page-check-pdf check)) (revision (page-check-revision check))
         (dates (file-dates pdf revision))
         (made (latest-file-date dates :made))
         (latest (latest-file-date dates))
         (scan (page-check-scan check))
         (scanned (and scan (mark-object scan)
                       (multiple-value-list (picture-made (picture-jpeg-sections pdf revision scan))))))
    (when (or made (first scanned))
      (let* ((own (page-own-dates check))
             (all-day (and latest (latest-day (fourth latest) (fifth latest))))
             (made-day (and made (latest-day (fourth made) (fifth made))))
             (after-all (and made (remove-if-not (lambda (d) (> (first d) all-day)) own)))
             (after-made (and made (remove-if-not (lambda (d) (< made-day (first d) (1+ all-day))) own)))
             (drawn (remove-if (lambda (d) (or (fifth d) (sixth d))) after-all))
             (general (remove-if (lambda (d) (or (fifth d) (not (sixth d)))) after-all))
             (recognised (remove-if-not #'fifth after-all))
             (file-words (and made (format nil "made ~a~:[~;, the latest date ~:*~a~]" (file-date-words made)
                                           (and (not (eq made latest)) (file-date-words latest)))))
             (rule #.(concatenate 'string "A program writes a document's date at or before it saves the file; a date "
                                          "the document gives as its own (after Fakturadatum, Invoice date, Datum, "
                                          "Date, or a letter's date line) later than the latest day any time zone "
                                          "had at every date the file gives was not in it when it was saved. Only "
                                          "dates whose day and month cannot be swapped are read.")))
        (when drawn
          (dates-found check :look drawn
                       (format nil "Page ~~d is dated ~~{~~a~~^, ~~}, after every date the file gives of itself (~a): ~
                                    the date was changed by hand, or the file's dates set back. A document dated ~
                                    ahead on purpose gives the same."
                               file-words)
                       rule))
        (when general
          (dates-found check :record general
                       (format nil "Page ~~d is dated ~~{~~a~~^, ~~}, after every date the file gives of itself (~a), ~
                                    by a label that says only that a date follows: agendas, invitations and tickets ~
                                    are often dated ahead of the day they are written."
                               file-words)
                       rule))
        (when recognised
          (dates-found check :record recognised
                       (format nil "Page ~~d is dated ~~{~~a~~^, ~~} as its text recognition reads it, after every ~
                                    date the file gives of itself (~a). Text recognition may misread a figure; the ~
                                    picture was not compared."
                               file-words)
                       rule))
        (when after-made
          (dates-found check :record after-made
                       (format nil "Page ~~d is dated ~~{~~a~~^, ~~}, after the file was made (~a) but not after ~
                                    its last change (~a): the date was written in a later change, or the file ~
                                    saved again."
                               (file-date-words made) (file-date-words latest))
                       rule))
        (destructuring-bind (&optional time zone label value) scanned
          (when time
            (let ((later (remove-if-not (lambda (d) (and (fifth d) (> (first d) (latest-day time zone)))) own)))
              (when later
                (dates-found check :record later
                             (format nil "Page ~~d is dated ~~{~~a~~^, ~~} as its text recognition reads it, after ~
                                          its scanned picture says it was made (~a ~a): the paper was dated after ~
                                          it was scanned, a scanner's clock set wrong, or a figure misread."
                                     label value)
                             (concatenate 'string "A scanner dates the picture as it scans the paper, and the paper's "
                                                  "own date is no later. The picture's date names no zone, so the "
                                                  "latest day any zone had then is taken."))))))))))

;;; The file's dates among themselves

(defun file-date-order (reading)
  "The findings, for the record, of the dates READING's file gives out of
order: the XMP's MetadataDate more than a minute before its CreateDate, a
save giving another creation date than the save before (more than a minute
apart), or a last change more than a minute before the save before's. An
honest program's clock may slip so, or a program set the creation date
anew."
  (let* ((pdf (metadata-reading-pdf reading)) (last (metadata-reading-last reading))
         (fields (metadata-reading-fields reading))
         (out '()))
    (let ((created (xmp-get fields "CreateDate")) (metadata (xmp-get fields "MetadataDate")))
      (multiple-value-bind (ct cz) (and created (date-of created))
        (multiple-value-bind (mt mz) (and metadata (date-of metadata))
          (when (and ct mt (< mt (- ct 60)))
            (push (found :record (format nil "The XMP metadata says its metadata was last changed (~a) before the ~
                                              file was made (~a)~:[~;, by a gap a time zone written wrong leaves~]."
                                         metadata created (clock-slip-p (- ct mt) cz mz))
                         :rule "Metadata is written as the file is made or after."
                         :revision (revision-index last) :objects (list (metadata-reading-xmp-number reading))
                         :evidence (append (xmp-evidence reading "CreateDate") (xmp-evidence reading "MetadataDate")))
                  out)))))
    (loop for r across (pdf-revisions pdf)
          for before = nil then previous
          for previous = r
          when before
            do (let ((was (info-text pdf before "/CreationDate")) (now (info-text pdf r "/CreationDate")))
                 (let ((apart (and was now (dates-apart was now))))
                   (when (and apart (> apart 60))
                     (push (found :record (format nil "~@(~a~) gives another creation date than the save before: ~
                                                       ~a, before ~a."
                                                  (revision-name r) (pdf-date-words now) (pdf-date-words was))
                                  :rule "A file is made once; its later saves keep the creation date of the first."
                                  :revision (revision-index r)
                                  :evidence (let ((n (nth-value 1 (info-of pdf r))))
                                              (and n (key-evidence pdf r n "/CreationDate"))))
                           out))))
               (multiple-value-bind (wt wz) (date-of (info-text pdf before "/ModDate"))
                 (multiple-value-bind (nt nz) (date-of (info-text pdf r "/ModDate"))
                   (when (and wt nt (< nt (- wt 60)))
                     (push (found :record (format nil "~@(~a~) says the file was last changed ~a, before the save ~
                                                       before it (~a)~:[~;, by a gap a clock or a time zone written ~
                                                       wrong leaves~]."
                                                  (revision-name r) (utc-time-words nt) (utc-time-words wt)
                                                  (clock-slip-p (- wt nt) wz nz))
                                  :rule "Each save is made after the one before it."
                                  :revision (revision-index r)
                                  :evidence (let ((n (nth-value 1 (info-of pdf r))))
                                              (and n (key-evidence pdf r n "/ModDate"))))
                           out)))))
    (nreverse out)))
