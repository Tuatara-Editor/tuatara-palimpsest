;;;; widths.lisp --- Figures placed unlike their font's widths, one font with two sets of widths, odd table cells.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A program that writes a PDF places each character where the one before it
;;;; ends, by the widths its font gives, and moves the text on between
;;;; characters only to kern or justify. Text set again by another tool keeps
;;;; positions worked out for other widths: a few characters of a line placed
;;;; unlike their own font. A font of one name embedded twice with other widths
;;;; was written from another program of that font. In a table, the figures of
;;;; a column are set in one font, size and ink; a cell set otherwise than its
;;;; column and its row was set apart.

(in-package #:palimpsest)

;;; Characters placed against their font's widths

(defparameter *pitch-off-least* 30
  "The least, in thousandths of the size, the text must be moved on between
two characters of a figure beyond what their font's widths give for the
step to count as placed unlike the font. Kerning moves single pairs by 10
to 120 and justifying moves the spaces; this counts only the steps of a
stretch, several in a row (*PITCH-STRETCH-LEAST*). Figures set again for a
font a few per cent wider or narrower are moved on by 20 to 60 at each step.")

(defparameter *pitch-stretch-least* 3
  "The fewest steps between characters in a row, within one figure, each
moved on by *PITCH-OFF-LEAST* or more and all the same way, that make a
stretch placed unlike its font: four characters (1,250 or 12.50).")

(defparameter *pitch-rest-least* 12
  "The fewest steps between characters within words the rest of a line must
have for a stretch to be set beside it.")

(defparameter *pitch-rest-agreeing* 0.9
  "The share of the rest of a line's steps within words that must be placed
by their font's widths, give or take half of *PITCH-OFF-LEAST*, for a
stretch unlike them to be named. Word processors move most characters of a
line on by a few thousandths as they round to the screen's pixels.")

(defun level-text-p (mark)
  "True when MARK is a run of visible text set level, left to right, unturned
and unslanted, in a font whose widths are known."
  (let ((f (mark-frame mark)) (font (mark-font mark)))
    (and f font (plusp (hash-table-count (font-widths font))) (not (member (mark-mode mark) '(3 7)))
         (plusp (mark-size mark)) (plusp (mark-squeeze mark)) (plusp (aref f 2))
         (<= (abs (aref f 3)) (* 0.01 (aref f 2))) (<= (abs (aref f 4)) (* 0.01 (mark-size mark))))))

(defun mark-em (mark)
  "The width on the page, in points, of one size of MARK's text along its
baseline: its size as drawn, its horizontal scale taken in."
  (* (mark-size mark) (mark-squeeze mark)))

(defun line-steps (line)
  "The steps from one character to the next along LINE (PAGE-LINES-OF), each
\(LEFT RIGHT NUDGE MARK), LEFT and RIGHT the characters, NUDGE how far the
text was moved on between them beyond their font's widths, in thousandths of
the size, MARK the run of RIGHT. Between runs of the same size it is the
space from where the first ends to where the next begins; NIL stands where
the line cannot be followed: a run not level (LEVEL-TEXT-P), of another
size, or drawn over the one before."
  (let ((out '()) (previous nil))
    (dolist (m line (nreverse out))
      (if (not (level-text-p m))
          (progn (push nil out) (setf previous nil))
          (let* ((text (mark-text m)) (nudges (mark-nudges m)) (n (length text)))
            (when (and previous (plusp n))
              (let* ((p-text (mark-text previous)) (p-nudges (mark-nudges previous))
                     (f (mark-frame previous)) (g (mark-frame m))
                     (gap (- (aref g 0) (+ (aref f 0) (aref f 2))))
                     (nudge (+ (/ (* 1000 gap) (mark-em previous))
                               (if p-nudges (aref p-nudges (1- (length p-nudges))) 0))))
                (push (if (and (<= (abs (- (mark-size m) (mark-size previous))) (* 0.01 (mark-size m)))
                               (> nudge -300))
                          (list (char p-text (1- (length p-text))) (char text 0) nudge m)
                          nil)
                      out)))
            (loop for k from 0 below (1- n)
                  do (push (list (char text k) (char text (1+ k)) (if nudges (aref nudges k) 0) m) out))
            (setf previous (and (plusp n) m)))))))

(defun within-word-p (step)
  "True when STEP (LINE-STEPS) is from one character of a word or figure to
the next: neither is a space, and the text was not moved on as far as a
space between words (a fifth of the size)."
  (and step
       (not (find (first step) '(#\Space #\Tab #\No-break_space)))
       (not (find (second step) '(#\Space #\Tab #\No-break_space)))
       (< (third step) 200)))

(defun pitch-agrees-p (step)
  "True when STEP (LINE-STEPS) moves the text on by less than half of
*PITCH-OFF-LEAST*: placed by its font's widths."
  (< (abs (third step)) (* 1/2 *pitch-off-least*)))

(defun line-stretches (steps)
  "The stretches among STEPS (LINE-STEPS) each moved on by *PITCH-OFF-LEAST*
or more, the same way, within one figure (FIGURE-TEXT-P) of
*PITCH-STRETCH-LEAST* steps or more between two pairs of characters or more,
each a list of its steps. A stretch of one pair again and again (1111) is
how a font kerns that pair, not a figure set for other widths."
  (let ((stretches '()) (run '()))
    (flet ((off-p (step) (and (within-word-p step) (>= (abs (third step)) *pitch-off-least*)))
           (close-run ()
             (let ((steps (reverse run)))
               (when (and (>= (length steps) *pitch-stretch-least*)
                          (figure-text-p (coerce (cons (first (first steps)) (mapcar #'second steps)) 'string))
                          (remove (cons (first (first steps)) (second (first steps))) steps
                                  :key (lambda (step) (cons (first step) (second step))) :test #'equal))
                 (push steps stretches)))
             (setf run '())))
      (dolist (step steps)
        (cond ((and (off-p step) (or (null run) (= (signum (third step)) (signum (third (first run))))))
               (push step run))
              (t (close-run)
                 (when (off-p step) (push step run)))))
      (close-run))
    (nreverse stretches)))

(defparameter *pitch-font-agreeing* 0.5
  "The share of a font's steps within words elsewhere on the page that must
be placed by its widths for a stretch in it to be named: below that, the
font's widths are not those it is drawn with (an old printer's font, a
font of pictures), wherever it is.")

(defun line-words-steps (steps)
  "STEPS (LINE-STEPS) within words, in lists, one for each word."
  (let ((words '()) (word '()))
    (dolist (step steps)
      (if (within-word-p step)
          (push step word)
          (when word (push (nreverse word) words) (setf word '()))))
    (when word (push (nreverse word) words))
    (nreverse words)))

(defun page-pitch-stretches (lines)
  "The figures of LINES (PAGE-LINES-OF) placed unlike their font's widths
where the rest of their line is placed by them, each (MARKS TEXT NUDGES REST
AGREEING): the runs drawing it, its characters, how far the text was moved
on at each step in thousandths of the size, how many steps within the other
words of the line there are and how many of those agree with the widths. A
font placed unlike its widths elsewhere on the page as well
\(*PITCH-FONT-AGREEING*) is left alone. See *PITCH-OFF-LEAST* and the
parameters after it."
  (let* ((steps (mapcar #'line-steps lines))
         (fonts (make-hash-table :test 'eq)))
    (flet ((tally (some)
             ;; how many of SOME there are, and how many agree with their font's widths
             (cons (length some) (count-if #'pitch-agrees-p some))))
      ;; for each font, how many steps within words the page sets in it and how many agree with its widths
      (dolist (line steps)
        (dolist (step line)
          (when (within-word-p step)
            (let ((entry (or (gethash (mark-font (fourth step)) fonts)
                             (setf (gethash (mark-font (fourth step)) fonts) (cons 0 0)))))
              (incf (car entry))
              (when (pitch-agrees-p step) (incf (cdr entry)))))))
      (loop for line in steps
            for words = (line-words-steps line)
            for all = (tally (apply #'append words))
            append (loop for stretch in (line-stretches line)
                         for word = (tally (find-if (lambda (w) (member (first stretch) w :test #'eq)) words))
                         for rest = (- (car all) (car word))
                         for agreeing = (- (cdr all) (cdr word))
                         for font = (gethash (mark-font (fourth (first stretch))) fonts)
                         for font-rest = (- (car font) (car word))
                         when (and (>= rest *pitch-rest-least*) (>= agreeing (* *pitch-rest-agreeing* rest))
                                   (or (< font-rest *pitch-rest-least*)
                                       (>= (- (cdr font) (cdr word)) (* *pitch-font-agreeing* font-rest))))
                           collect (list (remove-duplicates (mapcar #'fourth stretch))
                                         (coerce (cons (first (first stretch)) (mapcar #'second stretch)) 'string)
                                         (mapcar #'third stretch) rest agreeing))))))

(defun check-drawn-pitch (check)
  "Add to CHECK the stretches of text its page draws placed unlike their own
font's widths, where the rest of their line is placed by them
\(PAGE-PITCH-STRETCHES)."
  (let ((found (page-pitch-stretches (page-lines-of (page-check-texts check)))))
    (when found
      (page-found check :look
                  (format nil "On page ~d, ~:d stretch~:*~[es~;~:;es~] of text ~:*~[are~;is~:;are~] placed unlike ~
                               ~:*~[their~;its~:;their~] own font's widths, where the rest of the line keeps to ~
                               them: set again with another font's widths." (page-check-number check) (length found))
                  (format nil "A program places each character where the one before ends by its font's widths, and ~
                               moves the text on only to kern a pair or spread a line; several characters in a row ~
                               each moved the same way were placed for other widths than their font's.")
                  (loop for (marks) in found append marks)
                  :details (loop for (marks text nudges rest agreeing) in found
                                 collect (format nil "~s moved on ~{~d~^, ~} thousandths of its size between ~
                                                      characters; ~d of the line's ~d other steps within words keep ~
                                                      to the widths~@[ (~a)~]"
                                                 text (mapcar #'round nudges) agreeing rest (mark-place (first marks))))
                  :method :fitted :kind :layout))))

;;; One font name, two sets of widths

(defun font-glyph-widths (font)
  "A table of the characters FONT draws to their widths, in thousandths of
the size, by its ToUnicode map or its encoding; characters of no width, or
one code given two widths, left out. NIL for a Type 3 font, whose widths are
its own drawing's."
  (unless (equal (font-subtype font) "Type3")
    (let ((table (make-hash-table :test 'equal)) (twice (make-hash-table :test 'equal)))
      (loop for code being the hash-keys of (font-widths font) using (hash-value width)
            for text = (or (and (font-unicode font) (gethash code (font-unicode font)))
                           (and (font-table font) (< code 256) (let ((c (aref (font-table font) code)))
                                                                 (and c (string c)))))
            when (and text (plusp (length text)) (plusp width) (not (gethash text twice)))
              do (let ((before (gethash text table)))
                   (if (and before (/= before width))
                       (progn (remhash text table) (setf (gethash text twice) t))
                       (setf (gethash text table) width))))
      table)))

(defparameter *widths-unlike-least* 5
  "The least, in thousandths of the size, two widths of one character must be
apart to differ: programs round widths to whole thousandths or to a tenth of
one.")

(defun widths-unlike (a b)
  "How the widths of two fonts' characters differ, A and B tables of
FONT-GLYPH-WIDTHS: how many characters both give widths for, and a list of
those whose widths differ by *WIDTHS-UNLIKE-LEAST* or more, each (TEXT
WIDTH-A WIDTH-B), as two values."
  (let ((shared 0) (unlike '()))
    (loop for text being the hash-keys of a using (hash-value wa)
          for wb = (gethash text b)
          when wb
            do (incf shared)
               (when (>= (abs (- wa wb)) *widths-unlike-least*) (push (list text wa wb) unlike)))
    (values shared (sort unlike #'string< :key #'first))))

(defparameter *widths-shared-least* 6
  "The fewest characters two fonts of one name must both give widths for to
be set beside each other.")

(defun font-uses (pdf revision)
  "Each font the pages of REVISION of PDF name, once, with the pages naming
it: a list of (FONT NUMBER PAGES) in the order first named, NUMBER its
object number or NIL."
  (let ((seen (make-hash-table :test 'eq)) (out '()))
    (dolist (page (pages-of pdf revision))
      (loop for (nil . value) in (page-font-entries pdf revision page)
            do (let* ((font (font-of pdf revision value)) (entry (gethash font seen)))
                 (if entry
                     (pushnew (1+ (page-index page)) (third entry))
                     (let ((new (list font (and (pdf-ref-p value) (second value)) (list (1+ (page-index page))))))
                       (setf (gethash font seen) new)
                       (push new out))))))
    (mapcar (lambda (e) (list (first e) (second e) (reverse (third e)))) (nreverse out))))

(defun font-embedding (font)
  "What tells FONT's embedded program from another's of its name: its subset
tag, else the object number of its font file, else NIL for a font not
embedded."
  (or (font-subset font) (and (font-file font) (second (font-file font)))))

(defun same-name-unlike-widths (pdf revision)
  "The fonts of one name in REVISION of PDF, each embedded, from two
embeddings (FONT-EMBEDDING), whose widths for the same characters differ: a
list of (BASE USE-A USE-B SHARED UNLIKE), USE-A and USE-B as FONT-USES gives
them, SHARED and UNLIKE as WIDTHS-UNLIKE, the two written in different
saves of the file. Left alone: one embedding written with two encodings
(small capitals, old-style figures), which draws other glyphs for the same
characters; two embeddings written in one save, as a program splits a font
or keeps its alternates (tabular and proportional figures), or joins two
documents; differences in figures alone, which are such alternates; and
differences in fewer than two characters."
  (let ((by-name (make-hash-table :test 'equal)) (out '()))
    (dolist (use (font-uses pdf revision))
      (when (plusp (length (font-base (first use))))
        (push use (gethash (font-base (first use)) by-name))))
    (loop for base in (sort (loop for k being the hash-keys of by-name collect k) #'string<)
          for uses = (reverse (gethash base by-name))
          when (cdr uses)
            do (let ((tables (mapcar (lambda (u) (font-glyph-widths (first u))) uses)))
                 (loop for (a . more) on uses for ta in tables for k from 1
                       do (loop for b in more for tb in (nthcdr k tables)
                                do (when (and ta tb (font-embedded (first a)) (font-embedded (first b))
                                              (font-embedding (first a)) (font-embedding (first b))
                                              (not (equal (font-embedding (first a)) (font-embedding (first b)))))
                                     (multiple-value-bind (shared unlike) (widths-unlike ta tb)
                                       (when (and (cdr unlike) (>= shared *widths-shared-least*)
                                                  (notevery (lambda (u) (every #'digit-char-p (first u))) unlike)
                                                  (second a) (second b)
                                                  (not (eq (written-in pdf revision (second a))
                                                           (written-in pdf revision (second b)))))
                                         (push (list base a b shared unlike) out))))))))
    (nreverse out)))

(defun font-drawn-text (pdf revision font pages)
  "The text drawn in FONT on PAGES (numbers from 1) of REVISION of PDF: how
many characters, and the first three runs' text, as two values."
  (let ((count 0) (runs '()) (all (pages-of pdf revision)))
    (dolist (number pages)
      (let ((page (nth (1- number) all)))
        (when page
          (loop for m across (marks-of pdf revision page)
                when (and (eq (mark-kind m) :text) (eq (mark-font m) font))
                  do (incf count (mark-characters m))
                     (when (< (length runs) 3) (push (shorten-string (mark-text m) 40 :one-line t) runs))))))
    (values count (nreverse runs))))

(defun check-font-widths (pdf revision)
  "The fonts of one name in REVISION of PDF embedded with other widths for
the same characters (SAME-NAME-UNLIKE-WIDTHS)."
  (loop for (base a b shared unlike) in (same-name-unlike-widths pdf revision)
        collect (found :look
                       (format nil "The font ~a is in the file twice with other widths for ~:d of the ~:d characters ~
                                    both draw: two versions of the font, or one stood in for the other, as when text ~
                                    is set again on another machine."
                               base (length unlike) shared)
                       :rule (concatenate 'string "A program writes each character of a font with the width its font "
                                                  "program gives; one name with two widths for the same characters is "
                                                  "two font programs, from two machines or programs, or pages joined "
                                                  "from two documents.")
                       :revision (revision-index revision) :page (first (third b))
                       :details (append (loop for (font nil pages) in (list a b)
                                              collect (multiple-value-bind (count runs)
                                                          (font-drawn-text pdf revision font pages)
                                                        (format nil "~a on page~p ~{~d~^, ~}: ~:d character~:p~
                                                                     ~@[, ~{~s~^, ~}~]"
                                                                (setting-words :font font) (length pages) pages count
                                                                runs)))
                                        (loop for (text wa wb) in unlike repeat 8
                                              collect (format nil "~s ~d, ~d" text (round wa) (round wb))))
                       :objects (remove nil (list (second a) (second b)))
                       :evidence (loop for number in (remove nil (list (second a) (second b)))
                                       append (or (key-evidence pdf revision number "/Widths")
                                                  (key-evidence pdf revision number "/BaseFont")))
                       :method :retyped)))

;;; Figures of a table's columns

(defun figure-text-p (text)
  "True when TEXT is a figure as a table sets one: digits, at least one, with
only separators, signs, brackets and currency or per cent signs beside."
  (and (plusp (length text)) (<= (length text) 20)
       (some #'digit-char-p text)
       (every (lambda (c) (or (digit-char-p c) (find c ".,'-+()%$€£¥ "))) text)
       (>= (count-if #'digit-char-p text) (* 0.5 (length text)))))

(defun mark-character-along (mark k &optional end)
  "How far along the baseline of MARK, a run of text, its Kth character
begins, or with END where it ends, as a part of the run's length, 0 at its
start and 1 at its end: from where its characters lie (MARK-STOPS), else
shared out evenly among its characters."
  (let ((stops (mark-stops mark)))
    (if stops
        (aref stops (+ (* 2 k) (if end 1 0)))
        (/ (if end (1+ k) k) (max 1 (length (mark-text mark)))))))

(defun mark-character-x (mark k &optional end)
  "Where on the page the Kth character of MARK, a run of level text, begins
along its baseline, or with END where it ends (MARK-CHARACTER-ALONG); the
run's own ends when where its characters lie is not known."
  (let ((f (mark-frame mark)))
    (+ (aref f 0) (* (aref f 2) (cond ((mark-stops mark) (mark-character-along mark k end)) (end 1) (t 0))))))

(defun mark-placed-words (mark &optional shared)
  "The words MARK, a run of text, shows, each (TEXT START END): where in its
text the word begins and ends, words parted by spaces, tabs, line ends and
no-break spaces. A run whose characters' places are not known gives its
text as one word, or none when it holds several; with SHARED, its words
still, their places then shared out among its characters
\(MARK-CHARACTER-ALONG)."
  (let ((text (mark-text mark)) (out '()) (start nil))
    (loop for k from 0 to (length text)
          do (if (and (< k (length text)) (not (member (char text k) '(#\Space #\Tab #\Newline #\No-break_space))))
                 (unless start (setf start k))
                 (when start
                   (push (list (subseq text start k) start k) out)
                   (setf start nil))))
    (if (or shared (mark-stops mark) (null (cdr out))) (nreverse out) '())))

(defun line-figure-cells (line)
  "The figures of LINE (PAGE-LINES-OF) that stand as cells of a table, each
\(TEXT LEFT RIGHT POINT MARK): where on the page it begins and ends along the
baseline, where its decimal point is (a full stop or comma before its last
one or two digits) or NIL, and the run drawing it. A cell is a figure
\(FIGURE-TEXT-P) set at least a size apart from the words beside it; groups
of three digits set a space apart (1 250 000) are one figure."
  (let ((words '()))
    (dolist (m line)
      (when (level-text-p m)
        (dolist (w (mark-placed-words m))
          (let ((previous (first words)))
            (if (and previous (eq (fifth previous) m) (figure-text-p (first previous))
                     (= (length (first w)) 3) (every #'digit-char-p (first w))
                     (every (lambda (c) (or (digit-char-p c) (find c "- "))) (first previous))
                     (= (third previous) (1- (second w))))
                (setf (first words) (list (format nil "~a ~a" (first previous) (first w)) (second previous) (third w)
                                          (mark-character-x m (1- (third w)) t) m (fifth (rest previous))))
                (push (list (first w) (mark-character-x m (second w)) (third w) (mark-character-x m (1- (third w)) t)
                            m (second w))
                      words))))))
    ;; WORDS holds each word as (TEXT LEFT END RIGHT MARK START), the last first
    (let ((placed (sort (map 'vector (lambda (w) (list (first w) (second w) (fourth w) (fifth w) (sixth w))) words)
                        #'< :key #'second))
          (out '()))
      (dotimes (k (length placed) (nreverse out))
        (destructuring-bind (text left right mark start) (aref placed k)
          (let ((em (mark-em mark)))
            (when (and (figure-text-p text)
                       (or (= k 0) (>= (- left (third (aref placed (1- k)))) em))
                       (or (= k (1- (length placed))) (>= (- (second (aref placed (1+ k))) right) em)))
              (let ((point (let ((p (position-if (lambda (c) (find c ".,")) text :from-end t)))
                             (and p (< 0 (- (length text) p 1) 3) (every #'digit-char-p (subseq text (1+ p)))
                                  (mark-character-x mark (+ start p))))))
                (push (list text left right point mark) out)))))))))

(defparameter *column-edge-slack* 0.3
  "How far apart, in points, the right edges or decimal points of figures may
be for them to stand in one column: programs align them to a hundredth.")

(defparameter *column-rows-least* 4
  "The fewest rows a column of figures must have for a cell unlike the rest
of it to be named: three set alike and the one set apart.")

(defun edge-columns (figures key)
  "FIGURES (LINE-FIGURE-CELLS) grouped into columns by the place KEY gives of
each (their right edge, or decimal point), within *COLUMN-EDGE-SLACK*, one
figure to a row: lists of *COLUMN-ROWS-LEAST* figures or more."
  (let ((sorted (sort (remove-if-not key figures) #'< :key key)) (groups '()) (group '()))
    (dolist (fig sorted)
      (if (and group (<= (- (funcall key fig) (funcall key (car (last group)))) *column-edge-slack*))
          (push fig group)
          (progn (when group (push group groups)) (setf group (list fig)))))
    (when group (push group groups))
    (loop for g in groups
          for rows = (remove-duplicates g :test (lambda (a b) (<= (abs (- (mark-y0 (fifth a)) (mark-y0 (fifth b))))
                                                                  (* 0.5 (mark-size (fifth a))))))
          when (and (>= (length rows) *column-rows-least*) (= (length rows) (length g)))
            collect g)))

(defun mark-setting (mark kind)
  "How MARK is set, in the way KIND: :font its font object, :size its size,
:ink its colour."
  (ecase kind
    (:font (mark-font mark))
    (:size (mark-size mark))
    (:ink (mark-colour mark))))

(defun settings-alike-p (kind a b)
  "True when A and B, two settings of the way KIND (MARK-SETTING), are the
same: fonts of one name and one embedding (FONT-PROGRAM), sizes within 1 %,
colours within 0.02 in each part."
  (ecase kind
    (:font (and a b (equal (font-base a) (font-base b)) (equal (font-embedding a) (font-embedding b))))
    (:size (and a b (<= (abs (- a b)) (* 0.01 (max a b)))))
    (:ink (or (and (null a) (null b))
              (and a b (every (lambda (x y) (<= (abs (- x y)) 0.02)) a b))))))

(defun cell-unlike-p (kind own usual)
  "True when a cell set OWN way of KIND (MARK-SETTING) stands apart from a
column set the USUAL way as an edit would, not as a table is styled: a
font of another family, or of the column's own name from another embedding,
not the column's family in bold or italic; a size within 30 % of the
column's; a black or dark grey, not a red or a highlight."
  (ecase kind
    (:font (or (string/= (font-family own) (font-family usual)) (string= (font-base own) (font-base usual))))
    (:size (< 0.7 (/ own usual) 1.43))
    (:ink (and (dark-p own) (dark-p usual)))))

(defun column-odd-cells (column line-of)
  "The figures of COLUMN (EDGE-COLUMNS) set apart from the rest of it, each
\(KIND FIGURE OWN USUAL): one figure whose font, size or colour (KIND :font,
:size, :ink) differs from every other figure of the column, those all
alike, and from every other run of text in its row, some of which are set
as the column is, in a way an edit sets it (CELL-UNLIKE-P); LINE-OF gives
the runs of a figure's row. A row set apart as a whole, a bold total or a
header, is left alone, as is a row of samples each set its own way, a cell
whose characters are spaced apart unlike the column's (a font's table of
glyphs), and a column of single figures (numbers of rows, labels of a
chart)."
  (let ((out '()))
    (dolist (kind '(:font :size :ink) (nreverse out))
      (let ((odd (remove-if (lambda (fig)
                              (let ((own (mark-setting (fifth fig) kind)))
                                (> (count-if (lambda (g) (settings-alike-p kind own (mark-setting (fifth g) kind)))
                                             column)
                                   1)))
                            column)))
        (when (= (length odd) 1)
          (let* ((fig (first odd)) (own (mark-setting (fifth fig) kind))
                 (rest (remove fig column))
                 (usual (mark-setting (fifth (first rest)) kind))
                 (row (remove (fifth fig) (funcall line-of (fifth fig)))))
            (when (and (every (lambda (g) (settings-alike-p kind usual (mark-setting (fifth g) kind))) rest)
                       (>= (count-if (lambda (g) (>= (count-if #'digit-char-p (first g)) 2)) rest) 3)
                       (<= (abs (- (mark-spacing (fifth fig)) (mark-spacing (fifth (first rest))))) 0.03)
                       own usual (cell-unlike-p kind own usual)
                       (some (lambda (m) (settings-alike-p kind usual (mark-setting m kind))) row)
                       (notany (lambda (m) (settings-alike-p kind own (mark-setting m kind))) row))
              (push (list kind fig own usual) out))))))))

(defun page-odd-cells (texts)
  "The cells of the columns of figures among TEXTS, a page's runs of text,
set apart from their column and their row (COLUMN-ODD-CELLS), each figure
named once, as (KIND FIGURE OWN USUAL)."
  (let* ((lines (page-lines-of texts))
         (line-of (let ((table (make-hash-table :test 'eq)))
                    (dolist (line lines) (dolist (m line) (setf (gethash m table) line)))
                    (lambda (m) (gethash m table))))
         (figures (loop for line in lines append (line-figure-cells line)))
         (out '()))
    (dolist (column (append (edge-columns figures #'third) (edge-columns figures #'fourth)))
      (dolist (odd (column-odd-cells column line-of))
        (unless (find-if (lambda (o) (and (eq (first o) (first odd)) (eq (second o) (second odd)))) out)
          (push odd out))))
    (nreverse out)))

(defun setting-words (kind value)
  "VALUE, a setting of the way KIND (MARK-SETTING), in words."
  (ecase kind
    (:font (format nil "~:[~a~;~:*~a+~a~]~@[ (object ~d)~]" (font-subset value) (font-base value) (font-number value)))
    (:size (format nil "~,1f pt" value))
    (:ink (if value (colour-figures value) "a colour not known"))))

(defun check-column-fonts (check)
  "Add to CHECK the cells of a table of figures its page draws set in another
font object, size or colour than the rest of their column and their row
\(PAGE-ODD-CELLS)."
  (let ((odd (page-odd-cells (page-check-texts check))))
    (when odd
      (page-found check :look
                  (format nil "On page ~d, ~:d figure~:p of a table ~:*~[are~;is~:;are~] set in another font, size or ~
                               ink than the rest of ~:*~[their~;its~:;their~] column and ~:*~[their~;its~:;their~] ~
                               row: a cell set again." (page-check-number check) (length odd))
                  (format nil "A program sets the figures of a table's column in one font, size and colour; one cell ~
                               set otherwise than its column and the rest of its row was set apart from them.")
                  (mapcar (lambda (o) (fifth (second o))) odd)
                  :details (loop for (kind fig own usual) in odd
                                 collect (format nil "~s in ~a, the rest of its column in ~a~@[ (~a)~]"
                                                 (first fig) (setting-words kind own) (setting-words kind usual)
                                                 (mark-place (fifth fig))))
                  :method :retyped :kind :layout :place :field))))
