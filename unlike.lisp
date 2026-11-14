;;;; unlike.lisp --- Scanned words unlike the rest, bands of paper set apart, scans read.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A scanned page's words are set beside their neighbours, in their line and
;;;; in the lines about it, for their ink, edges, paper and where they sit; a word
;;;; unlike all of them is named. Bands of rows whose paper stands apart are
;;;; found and read again with their paper lifted. Each scanned picture of a page
;;;; is read once into a SCAN-PICTURE.

(in-package #:palimpsest)

;;; Words unlike the rest

(declaim (inline nth-least))
(defun nth-least (values count k)
  "The Kth least (from 0) of the first COUNT numbers of VALUES, a vector,
found by setting them in order in place as far as it needs: VALUES's first
COUNT are left in another order."
  (declare (type vector values) (type fixnum count k))
  (let ((low 0) (high (1- count)))
    (declare (type fixnum low high))
    (loop
      (when (>= low high) (return (aref values k)))
      (let ((pivot (aref values (ash (+ low high) -1))) (i low) (j high))
        (declare (type fixnum i j))
        (loop while (<= i j)
              do (loop while (< (aref values i) pivot) do (incf i))
                 (loop while (> (aref values j) pivot) do (decf j))
                 (when (<= i j)
                   (rotatef (aref values i) (aref values j))
                   (incf i) (decf j)))
        (cond ((<= k j) (setf high j))
              ((>= k i) (setf low i))
              (t (return (aref values k))))))))

(defun median-of (values &key (even :upper))
  "The middle of VALUES, a list of numbers: of an even count, the upper of
the two middle ones, or with EVEN :mean, halfway between them; NIL for
none."
  (let ((count (length values)))
    (declare (type fixnum count))
    (flet ((middle (vector)
             (declare (type simple-vector vector))
             (let* ((k (floor count 2)) (upper (nth-least vector count k)))
               (if (or (oddp count) (eq even :upper))
                   upper
                   ;; the lower middle is the greatest of those the selection left before the upper one
                   (/ (+ upper (loop for i below k maximize (svref vector i))) 2)))))
      (cond ((zerop count) nil)
            ((<= count 4096)
             (let ((vector (make-array count)))
               (declare (dynamic-extent vector))
               (middle (replace vector values))))
            (t (middle (replace (make-array count) values)))))))

(defparameter *scan-measures*
  '((:offset word-box-baseline 1.5 5 "a ~a baseline" "higher" "lower")
    (:turn word-box-slope 1.5 4 "turned ~a" "anticlockwise" "clockwise")
    (:ink word-box-ink 30 6 "~a ink" "darker" "lighter")
    (:edges word-box-edges 0.25 6 "~a edges" "sharper" "softer")
    (:noise word-box-noise 4 6 "~a paper" "cleaner" "noisier"))
  "What is measured of each word of a scan, and how it is judged
\(UNLIKE-WORDS). It holds its key, and what is measured (its baseline, set
beside its neighbours' to tell how far it sits off its line; its slope, set
beside its line's to tell how far it is turned; its ink, edges and paper).
It holds the least difference from what its neighbours say that counts, in
grey levels, pixels or degrees. It holds how many times as far from it as
the page's words typically are from theirs a word must be. And it holds its
words: how a word unlike the rest is said to be, ~a standing for what a
word with less, and with more, of it is. :OFFSET and :TURN tell where a
word sits, the others how it looks (*SCAN-LOOKS*).")

(defparameter *scan-neighbours* 3
  "How many words on each side of a word, the nearest first, it is set
beside in its line (UNLIKE-WORDS).")

(defparameter *scan-lines-about* 2
  "How many lines above a word's line, and how many below, give the shape
of its line between it and its neighbours, where it sits (UNLIKE-WORDS).")

(declaim (inline word-centre))
(defun word-centre (word)
  "The column across the middle of WORD."
  (* 0.5d0 (+ (word-box-x0 word) (word-box-x1 word))))

(defun scan-lines (words)
  "WORDS (WORD-BOXes) in their lines, as a vector of lines from the top of
the page down, each a vector of its words from left to right: the words of
one column of the page and one top, so that the lines of two columns are
never one."
  (let ((lines (make-hash-table :test 'equal)))
    (dolist (word words) (push word (gethash (cons (word-box-column word) (word-box-y0 word)) lines)))
    (sort (coerce (loop for line being the hash-values of lines
                        collect (sort (coerce line 'simple-vector) #'< :key #'word-centre))
                  'simple-vector)
          (lambda (a b)
            (let ((a (svref a 0)) (b (svref b 0)))
              (or (< (word-box-y0 a) (word-box-y0 b))
                  (and (= (word-box-y0 a) (word-box-y0 b)) (< (word-box-column a) (word-box-column b)))))))))

(defun line-points (line value usable)
  "The points of LINE (a vector of words from left to right) that tell its
shape by VALUE (a function of a word): each (CENTRE . VALUE) of its words
that are USABLE (a function of a word), from left to right."
  (coerce (loop for word across line
                when (funcall usable word) collect (cons (word-centre word) (float (funcall value word) 1d0)))
          'simple-vector))

(declaim (inline line-at))
(defun line-at (points x)
  "What a line of POINTS (from LINE-POINTS) gives at column X: read in a
straight line between the two points about it; NIL left of the first or
right of the last."
  (declare (type simple-vector points))
  (let ((n (length points)) (x (float x 1d0)))
    (declare (type fixnum n) (type double-float x))
    (flet ((at (i) (the double-float (car (svref points i))))
           (value (i) (the double-float (cdr (svref points i)))))
      (declare (inline at value))
      (when (and (plusp n) (<= (at 0) x (at (1- n))))
        (let ((lo 0) (hi (1- n)))
          (declare (type fixnum lo hi))
          ;; the last point at or left of X, LO
          (loop while (< lo hi)
                do (let ((mid (ceiling (+ lo hi) 2)))
                     (if (<= (at mid) x) (setf lo mid) (setf hi (1- mid)))))
          (if (or (= lo (1- n)) (= (at lo) x))
              (value lo)
              (+ (value lo) (* (- (value (1+ lo)) (value lo)) (/ (- x (at lo)) (- (at (1+ lo)) (at lo)))))))))))

(defun lines-about (all at)
  "The lines of ALL (a vector of LINE-POINTS, a line's each) nearest line
AT that tell a shape, two points at least: twice *SCAN-LINES-ABOUT* of
them, as many above as below where the page has them, the rest from the
other side."
  (let ((above '()) (below '()) (wanted (* 2 *scan-lines-about*)))
    (loop for i from (1- at) downto 0
          while (< (length above) wanted)
          when (>= (length (svref all i)) 2) do (push (svref all i) above))
    (loop for i from (1+ at) below (length all)
          while (< (length below) wanted)
          when (>= (length (svref all i)) 2) do (push (svref all i) below))
    (setf above (nreverse above) below (nreverse below))
    (let* ((up (min (length above) (max *scan-lines-about* (- wanted (length below)))))
           (down (min (length below) (- wanted up))))
      (append (subseq above 0 up) (subseq below 0 down)))))

(defun side-residual (word value mates about fallback)
  "How far WORD's VALUE (a function of a word) is from what MATES, words of
its line on one side of it, say it should be. For each mate and each of the
lines ABOUT it (LINE-POINTS each) that reaches both columns, take the
mate's value with the difference that line makes from the mate's column to
WORD's. So a line bowed, turned or seen at a slant as the lines about it
are sets no word apart. Where none reaches both, take the mate's value with
FALLBACK for each column between them. The middle of them, NIL for no
MATES."
  (when mates
    (let* ((x (word-centre word)) (n 0)
           (said (make-array (* (length mates) (max 1 (length about))) :element-type 'double-float)))
      (declare (dynamic-extent said) (type fixnum n) (type double-float x))
      (flet ((say (v)
               ;; kept in order as it is said, the least first
               (let ((v (float v 1d0)) (i n))
                 (declare (type fixnum i))
                 (loop while (and (plusp i) (> (aref said (1- i)) v))
                       do (setf (aref said i) (aref said (1- i))) (decf i))
                 (setf (aref said i) v)
                 (incf n))))
        (declare (inline say))
        (dolist (mate mates)
          (let ((from (word-centre mate)) (counted nil) (own (float (funcall value mate) 1d0)))
            (declare (type double-float from own))
            (dolist (points about)
              (let ((here (line-at points x)) (there (line-at points from)))
                (when (and here there)
                  (say (+ own (- (the double-float here) (the double-float there))))
                  (setf counted t))))
            (unless counted (say (+ own (* fallback (- x from))))))))
      (- (funcall value word) (aref said (floor n 2))))))

(defun line-residuals (word line value usable about fallback &optional without)
  "How far WORD's VALUE is from what the words of its LINE (a vector, from
left to right) say, as two values: from those on its left and from those on
its right (SIDE-RESIDUAL), *SCAN-NEIGHBOURS* of each that are USABLE, the
nearest first, WITHOUT left out; NIL for a side with none."
  (let ((at (position word line)) (left '()) (right '()))
    (loop for i from (1- at) downto 0
          for mate = (svref line i)
          while (< (length left) *scan-neighbours*)
          when (and (not (eq mate without)) (funcall usable mate)) do (push mate left))
    (loop for i from (1+ at) below (length line)
          for mate = (svref line i)
          while (< (length right) *scan-neighbours*)
          when (and (not (eq mate without)) (funcall usable mate)) do (push mate right))
    (values (side-residual word value (nreverse left) about fallback)
            (side-residual word value (nreverse right) about fallback))))

(defun both-sides (left right)
  "How far a word stands from its line, from LEFT and RIGHT, how far it
stands from what its words on each side say (NIL for a side with none): a
step away from both the same way, the smaller of them; NIL from both,
none; from one side alone, that one."
  (cond ((and left right) (if (plusp (* left right)) (if (< (abs left) (abs right)) left right) 0))
        (t (or left right))))

(defun turn-residual (word about tilt)
  "How far, in degrees, WORD is turned clockwise against its line: its
slope set beside TILT, the page's, and beside the slope the lines ABOUT it
\(LINE-POINTS of baselines) fall by across its columns, the middle of them,
where any reaches across it; turned against both the same way, the
smaller of the two, else 0. A line of a few words set at different
heights, as a form's label and value are, falls steeply between them,
where the page does not."
  (let ((x0 (word-box-x0 word)) (x1 (word-box-x1 word)) (slopes '()))
    (dolist (points about)
      (let ((a (line-at points x0)) (b (line-at points x1)))
        (when (and a b) (push (/ (- b a) (- x1 x0)) slopes))))
    (flet ((against (slope) (float (* (/ 180 pi) (atan (- (word-box-slope word) slope))))))
      (let ((page (against tilt)))
        (if slopes
            (let ((line (against (median-of slopes))))
              (cond ((not (plusp (* page line))) 0.0)
                    ((< (abs line) (abs page)) line)
                    (t page)))
            page)))))

(defparameter *scan-column-mates* 6
  "How many words over a word at the end of its line, and how many under
it, it is set beside, from twice as many lines either side (COLUMN-MATES).")

(defun column-mates (word lines at have)
  "Words in the lines LINES (from SCAN-LINES) above and below line AT that
stand over or under WORD, across half of the narrower of the two at least,
and are in HAVE (a table): *SCAN-COLUMN-MATES* above it and as many below
at most, from twice as many lines either side."
  (let ((most *scan-column-mates*))
    (let ((found '()) (taken 0))
      (declare (type fixnum taken))
      (flet ((across-p (other)
               (>= (* 2 (- (min (word-box-x1 other) (word-box-x1 word)) (max (word-box-x0 other) (word-box-x0 word))))
                   (min (- (word-box-x1 other) (word-box-x0 other)) (- (word-box-x1 word) (word-box-x0 word))))))
        (dolist (step '(-1 1) found)
          (setf taken 0)
          (loop for i = (+ at step) then (+ i step)
                repeat (* 2 most)
                while (and (< -1 i (length lines)) (< taken most))
                do (loop for o across (svref lines i)
                         while (< taken most)
                         when (and (gethash o have) (across-p o))
                           do (push o found) (incf taken))))))))

(defparameter *scan-widest-word* 5
  "How many times as wide as the page's words typically are a word may be
and still be judged (UNLIKE-WORDS): one wider is lines run together, a
rule or a mark across the page, not a word.")

(defparameter *scan-column-rows* 5
  "The fewest words one under another that make a column set apart
together (COLUMNS-UNLIKE).")

(defun middles-leaving-out (values)
  "For each of VALUES, a vector of numbers, the middle of the others as
MEDIAN-OF finds it with EVEN :mean (halfway between the two middle ones),
as a vector: the values sorted once, each one's left out by its place."
  (let* ((n (length values))
         (order (sort (let ((v (make-array n))) (dotimes (i n v) (setf (svref v i) i)))
                      #'<
                      :key (lambda (i) (elt values i))))
         (sorted (map 'vector (lambda (i) (elt values i)) order))
         (out (make-array n)))
    (flet ((kept (k skip) (svref sorted (if (< k skip) k (1+ k)))))
      (dotimes (place n out)
        (let ((c (1- n)))
          (setf (svref out (svref order place))
                (cond ((<= c 0) nil)
                      ((oddp c) (kept (floor c 2) place))
                      (t (/ (+ (kept (1- (floor c 2)) place) (kept (floor c 2) place)) 2)))))))))

(declaim (inline middle-skipping))
(defun middle-skipping (sorted first second &optional (count (length sorted)))
  "The middle of the first COUNT of SORTED, a vector of numbers from the
least, as MEDIAN-OF finds it, the values at the places FIRST and SECOND
left out (either NIL for none): read by place, nothing copied. NIL when
nothing is left."
  (declare (type (simple-array double-float (*)) sorted) (type fixnum count))
  (let ((left (- count (if first 1 0) (if (and second (not (eql second first))) 1 0))))
    (when (plusp left)
      ;; the (floor left 2)th of those kept
      (let ((wanted (floor left 2)) (seen -1))
        (declare (type fixnum wanted seen))
        (dotimes (i count)
          (unless (or (eql i first) (eql i second))
            (incf seen)
            (when (= seen wanted) (return (aref sorted i)))))))))

(defun page-trend (lines value usable)
  "How far, by VALUE (a function of a word), the lines of LINES (from
SCAN-LINES) rise or fall together across the page, as a function of a
column. It is a curve of the second degree fitted to the words of lines of
five USABLE words or more (lines of text), each line's own height left
out. The words further from the middle of their line than three times as
far as words typically are (and than half a pixel) are left out, except where
nine lines in ten or more hold such words. The fit is by least squares. It
is then made again without the words more than three times as far from it
as the words typically are (and more than half a pixel), as words moved or
pasted are. It is 0 everywhere where there are no such lines. A page lying
bowed in a scanner, or seen at a slant, bends all its lines alike, and the
lines of text show it."
  (let* ((texts (loop for line across lines
                      for count = (count-if usable line)
                      when (>= count 5)
                        collect (let ((us (make-array count :element-type 'double-float))
                                      (ys (make-array count :element-type 'double-float))
                                      (k 0))
                                  (loop for w across line
                                        when (funcall usable w)
                                          do (setf (aref us k) (/ (word-centre w) 1000d0)
                                                   (aref ys k) (float (funcall value w) 1d0))
                                             (incf k))
                                  (cons us ys))))
         ;; a word further from the middle of its line than three times as far as words typically stand
         ;; from theirs, as a column moved or pasted in a table's row is, is left out from the first
         (texts (let* ((typical (median-of (loop for (nil . ys) in texts
                                                 append (let ((m (median-of (coerce ys 'list))))
                                                          (map 'list (lambda (y) (abs (- y m))) ys)))))
                       (bar (if typical (max 0.5d0 (* 3 typical)) 0d0))
                       ;; words far off their lines in nearly every line are the page's own shape, and kept
                       (bar (if (>= (count-if (lambda (text) (let ((m (median-of (coerce (cdr text) 'list))))
                                                                   (some (lambda (y) (> (abs (- y m)) bar))
                                                                         (cdr text))))
                                                  texts)
                                   (* 9/10 (length texts)))
                                most-positive-double-float
                                bar)))
                  (loop for (us . ys) in texts
                        for m = (median-of (coerce ys 'list))
                        for keep = (loop for y across ys collect (<= (abs (- y m)) bar))
                        for n = (count t keep)
                        when (>= n 3)
                          collect (let ((ku (make-array n :element-type 'double-float))
                                        (ky (make-array n :element-type 'double-float))
                                        (k 0))
                                    (loop for u across us for y across ys for kept in keep
                                          when kept do (setf (aref ku k) u (aref ky k) y) (incf k))
                                    (cons ku ky)))))
         (total (loop for (us) in texts sum (length us)))
         (off (make-array total :element-type 'double-float))
         (b 0d0) (c 0d0))
    (declare (type double-float b c) (type fixnum total))
    (labels ((away (us ys k)
               ;; how far word K of a line stands off the curve, the line's own middle by it left out
               (declare (type (simple-array double-float (*)) us ys) (type fixnum k))
               (let ((u (aref us k))) (- (aref ys k) (* b u) (* c u u))))
             (fit (bar)
               ;; the curve through the words no further off it than BAR, each line about its own middle;
               ;; how far each word stands off the curve it was fitted from, in OFF
               (declare (type double-float bar))
               (let ((xx 0d0) (xz 0d0) (zz 0d0) (xy 0d0) (zy 0d0) (at 0))
                 (declare (type double-float xx xz zz xy zy) (type fixnum at))
                 (loop for (us . ys) in texts
                       do (let* ((n (length us))
                                 (level (progn (dotimes (k n) (setf (aref off (+ at k)) (away us ys k)))
                                               ;; the line's middle by the curve so far
                                               (let ((scratch (make-array n :element-type 'double-float)))
                                                 (declare (dynamic-extent scratch))
                                                 (replace scratch off :start2 at)
                                                 (nth-least scratch n (floor n 2)))))
                                 (mu 0d0) (mz 0d0) (my 0d0) (m 0))
                            (declare (type double-float level mu mz my) (type fixnum n m)
                                     (type (simple-array double-float (*)) us ys))
                            (dotimes (k n)
                              (let ((d (- (aref off (+ at k)) level)))
                                (setf (aref off (+ at k)) (abs d))
                                (when (<= (abs d) bar)
                                  (let ((u (aref us k)))
                                    (incf mu u) (incf mz (* u u)) (incf my (aref ys k)) (incf m)))))
                            (when (>= m 3)
                              (setf mu (/ mu m) mz (/ mz m) my (/ my m))
                              (dotimes (k n)
                                (when (<= (aref off (+ at k)) bar)
                                  (let* ((u (aref us k)) (du (- u mu)) (dz (- (* u u) mz)) (dy (- (aref ys k) my)))
                                    (incf xx (* du du)) (incf xz (* du dz)) (incf zz (* dz dz))
                                    (incf xy (* du dy)) (incf zy (* dz dy))))))
                            (incf at n)))
                 (let ((det (- (* xx zz) (* xz xz))))
                   (when (> det 1d-12)
                     (setf b (/ (- (* xy zz) (* zy xz)) det)
                           c (/ (- (* zy xx) (* xy xz)) det)))))))
      (when (plusp total)
        (fit most-positive-double-float)
        ;; again, the words far off the first curve left out
        (fit most-positive-double-float)
        (fit (max 0.5d0 (* 3 (nth-least off total (floor total 2)))))))
    (lambda (x) (let ((u (/ x 1000d0))) (+ (* b u) (* c u u))))))

(defun columns-unlike (lines value usable times least &optional trend)
  "The words of LINES (from SCAN-LINES) in columns set off their lines together,
each (WORD . HOW-FAR). Words one under another, *SCAN-COLUMN-ROWS* or more, in
lines of two USABLE words or more, make a column; the columns and lines of such
words are a table, of each word's VALUE (a function of a word: where it sits,
the page's tilt allowed for), the page's TREND at its place (PAGE-TREND, found
when not given) taken out. Each line's middle and each column's middle are taken
out of it in turn, twice, so that every column's own offset is told apart from
every line's, however many columns are moved. Words one under another in a
column, *SCAN-COLUMN-ROWS* or more, set off the same way by more than LEAST and
more than TIMES as far as words of lines of text typically stand from the middle
of theirs, alike within a third of their middle, are set off together. Columns
next to each other moved together each so. Runs set off opposite ways in the
same lines, as moving some columns of a table draws the others the other way,
are both given, EITHER true, as one of them moved. Not when every column of the
table is so: the page itself is turned or curled. Those words are given, each
\(WORD HOW-FAR EITHER)."
  (let* ((count (length lines)) (texts '())
         (trend (or trend (page-trend lines value usable)))
         ;; each word's value, the page's trend at its place taken out
         (level (lambda (w) (- (funcall value w) (funcall trend (word-centre w)))))
         (columns '()) (out '()))
    ;; how far the words of lines of text stand from the middle of their lines, which a column moved in
    ;; a table's row does not shift
    (loop for line across lines
          for others = (remove-if-not usable line)
          when (>= (length others) 5)
            do (let ((m (median-of (map 'list level others))))
                 (loop for w across others do (push (abs (- (funcall level w) m)) texts))))
    (flet ((over-p (a b)
             (>= (- (min (word-box-x1 a) (word-box-x1 b)) (max (word-box-x0 a) (word-box-x0 b)))
                 (* 1/2 (min (- (word-box-x1 a) (word-box-x0 a)) (- (word-box-x1 b) (word-box-x0 b))))))
           (row-p (line) (>= (count-if usable line) 2)))
      ;; the columns: words one under another in lines of two words or more, a word of each line at most
      (let ((taken (make-hash-table :test 'eq)))
        (loop for at below count
              when (row-p (svref lines at))
                do (loop for word across (svref lines at)
                         when (and (funcall usable word) (not (gethash word taken)))
                           do (let ((column (list (cons at word))))
                                (loop for i from (1+ at) below count
                                      for below = (and (row-p (svref lines i))
                                                       (find-if (lambda (o)
                                                                  (and (funcall usable o)
                                                                       (not (gethash o taken))
                                                                       (over-p word o)))
                                                                (svref lines i)))
                                      when below do (push (cons i below) column))
                                (dolist (c column) (setf (gethash (cdr c) taken) t))
                                (when (>= (length column) *scan-column-rows*) (push (nreverse column) columns))))))
      (when (cdr columns)
        (let* ((columns (coerce (sort columns #'< :key (lambda (c) (word-centre (cdr (first c))))) 'simple-vector))
               (n (length columns))
               ;; the table: each column's words by line, their values, less the line's and the column's middles
               (cells (make-hash-table :test 'eq))
               (row-of (make-hash-table))
               (column-offset (make-array n :initial-element 0)))
          (loop for c across columns
                do (loop for (at . w) in c do (setf (gethash w cells) (funcall level w)) (push w (gethash at row-of))))
          (dotimes (pass 2)
            (loop for words being the hash-values of row-of
                  do (when (cdr words)
                       (let ((m (median-of (mapcar (lambda (w) (gethash w cells)) words) :even :mean)))
                         (dolist (w words) (decf (gethash w cells) m)))))
            (loop for c across columns for k from 0
                  do (let ((m (median-of (mapcar (lambda (cell) (gethash (cdr cell) cells)) c) :even :mean)))
                       (incf (aref column-offset k) m)
                       (dolist (cell c) (decf (gethash (cdr cell) cells) m)))))
          (let* ((spread (if texts (median-of texts :even :mean) 0))
                 (runs (make-array n :initial-element nil)))
            ;; in each column, the runs of words one under another set off the same way, by more than the least
            ;; and the multiple of the lines of text, and set off alike, within a third of their middle
            (dotimes (k n)
              (let* ((c (svref columns k))
                     (offsets (mapcar (lambda (cell) (+ (aref column-offset k) (gethash (cdr cell) cells))) c))
                     (run '()))
                (flet ((close-run ()
                         (when (>= (length run) *scan-column-rows*)
                           (let* ((ds (mapcar #'cdr run)) (middle (median-of ds :even :mean)))
                             (when (<= (median-of (mapcar (lambda (d) (abs (- d middle))) ds)
                                                  :even :mean) (/ (abs middle) 3))
                               (push run (aref runs k)))))
                         (setf run '())))
                  (loop for cell in c for d in offsets
                        do (if (and (> (abs d) least) (> (abs d) (* times spread))
                                    (or (null run) (plusp (* d (cdr (first run))))))
                               (push (cons (cdr cell) d) run)
                               (progn (close-run)
                                      (when (and (> (abs d) least) (> (abs d) (* times spread)))
                                        (push (cons (cdr cell) d) run)))))
                  (close-run))))
            ;; every column of the table set off the same way: the page, not a column
            (unless (let ((signs (loop for k below n
                                       collect (and (aref runs k) (signum (cdr (first (first (aref runs k)))))))))
                      (and (every #'identity signs) (every (lambda (s) (= s (first signs))) signs)))
              ;; columns moved in the lines of a table draw the others of those lines the other way: runs set off
              ;; opposite ways in the same lines are named both, one of them moved
              (let ((line-of (make-hash-table :test 'eq)))
                (loop for at below count do (loop for w across (svref lines at) do (setf (gethash w line-of) at)))
                (dotimes (k n)
                  (dolist (run (aref runs k))
                    (let* ((sign (signum (cdr (first run)))) (rows (mapcar (lambda (p) (gethash (car p) line-of)) run))
                           (either (loop for j below n
                                         thereis (and (/= j k)
                                                      (some (lambda (other)
                                                              (and (/= (signum (cdr (first other))) sign)
                                                                   (intersection rows
                                                                                 (mapcar (lambda (p)
                                                                                           (gethash (car p) line-of))
                                                                                           other))))
                                                            (aref runs j))))))
                      (loop for (word . d) in run do (push (list word d either) out))))))))))
      out)))

(defparameter *scan-region-lines* 6
  "The most lines a region of words set apart together may span
\(REGIONS-UNLIKE).")

(defun regions-unlike (lines measure bar &optional (aside (constantly nil)))
  "The blocks of words of LINES (from SCAN-LINES) set apart together by MEASURE
\(a function of a word, how it looks), each a list of (WORD . HOW-FAR), a word in
one block at most. A block is a number of lines next to each other,
*SCAN-REGION-LINES* at most, over the same columns, whose words all stand apart
the same way, by more than BAR. They stand apart both from the words over the
block (in the three lines above it) and from those under it (in the three lines
below). Each of those is set beside the words over or under it, the block itself
left out. At the foot of the page, or over or under a blank gap three lines
tall, it is judged from the side that has lines alone. A block at the top of the
page, where headings and forms stand apart honestly, is not judged so. Four
words at least; a block of one line three quarters of it at least, or the rest
of the line no further from the lines about it than half of BAR. Words ASIDE
says (a function of a word) are no words over or under a block for others, as
the words of a band of paper set apart are not (PAPER-BANDS). A page darkening
or lightening down its length stands apart from the lines on one side one way
and from those on the other the other way, and is not so."
  (let* ((out '()) (count (length lines)) (taken (make-hash-table :test 'eq)) (values (make-hash-table :test 'eq))
         (tops (map 'vector (lambda (line) (word-box-y0 (svref line 0))) lines))
         (pitch (if (> count 1)
                    (median-of (loop for i from 1 below count collect (- (aref tops i) (aref tops (1- i)))))
                    0)))
    (labels ((value (word)
               ;; MEASURE of WORD, taken once
               (or (gethash word values) (setf (gethash word values) (funcall measure word))))
             (over-p (a b) (and (< (word-box-x0 b) (word-box-x1 a)) (> (word-box-x1 b) (word-box-x0 a))))
             (open-p (from to)
               ;; no lines of the page from FROM to TO: the top or the foot of the page, or a blank gap
               (or (< to 0) (>= from count)
                   (> (abs (- (aref tops (max 0 (min (1- count) to))) (aref tops (max 0 (min (1- count) from)))))
                      (* 3 pitch))))
             (beside (word from to)
               ;; the middle of the words over or under WORD in lines FROM to TO, NIL for none
               (let ((n (loop for i from (max 0 from) to (min (1- count) to)
                              sum (count-if (lambda (o) (and (over-p word o) (not (funcall aside o))))
                                            (svref lines i)))))
                 (declare (type fixnum n))
                 (when (plusp n)
                   (let ((found (make-array n)) (k 0))
                     (declare (dynamic-extent found) (type fixnum k))
                     (loop for i from (max 0 from) to (min (1- count) to)
                           do (loop for o across (svref lines i)
                                    when (and (over-p word o) (not (funcall aside o)))
                                      do (setf (svref found k) (value o)) (incf k)))
                     (nth-least found n (floor n 2))))))
             (apart (word top bottom)
               ;; how far WORD stands from the words over the block from TOP to BOTTOM and under it, both
               ;; the same way: the nearer of the two; where the page has no lines on one side, from the
               ;; other alone
               (let* ((v (value word))
                      (up (let ((m (beside word (- top 3) (1- top)))) (and m (- v m))))
                      (down (let ((m (beside word (1+ bottom) (+ bottom 3)))) (and m (- v m)))))
                 (cond ((and up down) (and (plusp (* up down)) (if (< (abs up) (abs down)) up down)))
                       ((and up (or (= bottom (1- count)) (open-p bottom (1+ bottom)))) up)
                       ((and down (plusp top) (open-p (1- top) top)) down))))
             (plain-p (word top bottom near)
               ;; WORD no further than NEAR from the words over the block from TOP to BOTTOM, nor from
               ;; those under it, with one side at least to tell
               (let* ((v (value word))
                      (up (beside word (- top 3) (1- top)))
                      (down (beside word (1+ bottom) (+ bottom 3))))
                 (and (or up down)
                      (or (null up) (<= (abs (- v up)) near))
                      (or (null down) (<= (abs (- v down)) near))))))
      (loop for top below count
            do (loop for word across (svref lines top)
                     ;; the top of a block: unlike the words over it, or none over it
                     do (let ((up (let ((m (beside word (- top 3) (1- top)))) (and m (- (value word) m)))))
                          (when (if up (> (abs up) bar) (and (plusp top) (open-p (1- top) top)))
                            (loop for bottom from top below (min count (+ top *scan-region-lines*))
                                  do (let* ((sign nil) (block '()) (whole t))
                                       ;; in each line of the block, the run of words next to each other set
                                       ;; apart the same way that reaches over WORD
                                       (loop for i from top to bottom while whole
                                             do (let* ((line (svref lines i))
                                                       (ways (map 'vector (lambda (o) (apart o top bottom)) line))
                                                       (at (position-if (lambda (o) (over-p word o)) line)))
                                                  (flet ((good (k) (let ((d (svref ways k)))
                                                                     (and d
                                                                          (> (abs d) bar)
                                                                          (or (null sign) (= (signum d) sign))))))
                                                    (if (and at (good at))
                                                        (progn
                                                          (setf sign (signum (svref ways at)))
                                                          (let ((from at) (to at))
                                                            (loop while (and (plusp from) (good (1- from)))
                                                                  do (decf from))
                                                            (loop while (and (< (1+ to) (length line)) (good (1+ to)))
                                                                  do (incf to))
                                                            (loop for k from from to to
                                                                  do (push (cons (svref line k) (svref ways k))
                                                                           block))))
                                                        (setf whole nil)))))
                                       (when (and whole (>= (length block) 4)
                                                  ;; a block of one line is the whole line, near enough, or the rest
                                                  ;; of the line is like the lines about it
                                                  (or (< top bottom)
                                                      (>= (* 4 (length block)) (* 3 (length (svref lines top))))
                                                      (loop for o across (svref lines top)
                                                            always (or (assoc o block)
                                                                       (plain-p o top bottom (/ bar 2)))))
                                                  ;; a side to judge it by
                                                  (or (beside word (- top 3) (1- top))
                                                      (beside word (1+ bottom) (+ bottom 3))))
                                         (let ((new (remove-if (lambda (pair) (gethash (car pair) taken)) block)))
                                           (when new
                                             (dolist (pair new) (setf (gethash (car pair) taken) t))
                                             (push (nreverse new) out))))))))))
      (nreverse out))))

(defun bow-p (bend from to)
  "True when BEND (a function of a column, PAGE-TREND) across the columns
FROM to TO is a bow, however deep: its ends standing apart by no more than
twice how far it strays from the straight line between them, read at ten
steps. A bend that mostly turns the lines is the page's tilt misread, as
a photograph taken at a slant to the page shows."
  (let* ((a (funcall bend from)) (b (funcall bend to))
         (sag (loop for k from 1 below 10
                    for x = (+ from (* k 1/10 (- to from)))
                    maximize (abs (- (funcall bend x) (+ a (* k 1/10 (- b a))))))))
    (<= (abs (- b a)) (* 2 sag))))

(defparameter *scan-bow-lines* 6
  "The fewest lines of text that tell a page's bow is shared by all its
lines (UNLIKE-WORDS).")

(defun flat-page-p (lines tilt &optional (bend (constantly 0)) (fewest 1))
  "True when the lines of LINES (from SCAN-LINES) keep straight as a flat page's
do. Take each line of five words or more, a line of text rather than a table's
row, a column of which may be moved. Take how far its words' baselines stand
from the middle of the others, the page's TILT and its BEND allowed for (a
function of a column, what all its lines rise or fall by there, PAGE-TREND), the
lower middle of them. The middle of those over the page is a third of a pixel at
most. Where FEWEST lines at least are asked for, as to tell a bend all the lines
share, it is over that many, and three quarters of them half a pixel at most.
Where FEWEST is one and words typically stand a third of a pixel or less from
the middle of their lines, as on a scanner's glass, a word standing further from
the middle of its line than three times that (and than half a pixel) is left out
of its line. A column moved or pasted in a table's row does so. Leaving it out
means that what is changed does not hide itself, but where nine lines in ten or
more hold such words. Then a bend, a page photographed, or one whose every line
droops at its end, is told from all the words. A page lying in a scanner bowed
bends all its lines alike, and the bend taken out leaves them straight; a page
photographed curls and slants each of its lines its own way."
  (let* ((rows (loop for line across lines
                     for others = (remove-if #'word-box-cut line)
                     when (>= (length others) 5)
                       collect (map 'vector
                                    (lambda (o)
                                      (- (word-box-baseline o) (* tilt (word-centre o)) (funcall bend (word-centre o))))
                                    others)))
         ;; how far words typically stand from the middle of their lines; a word further than three times
         ;; that, as a column moved or pasted in a table's row is, is left out of its line
         (typical (median-of (loop for at in rows
                                   append (let ((m (median-of (coerce at 'list))))
                                            (map 'list (lambda (v) (abs (- v m))) at)))))
         (bar (and typical (max 1/2 (* 3 typical))))
         ;; words far off their lines in nearly every line are the page's own shape, not a column changed
         (few (and bar (< (count-if (lambda (at) (let ((m (median-of (coerce at 'list))))
                                                        (some (lambda (v) (> (abs (- v m)) bar)) at)))
                                          rows)
                           (* 9/10 (length rows)))))
         (straight (loop for at in rows
                         for m = (median-of (coerce at 'list))
                         for kept = (if (and (= fewest 1) (<= typical 1/3) few)
                                        (remove-if (lambda (v) (> (abs (- v m)) bar)) at)
                                        at)
                         when (>= (length kept) 3)
                           collect (let ((ds (sort (map 'list
                                                        (lambda (v m) (abs (- v m)))
                                                        kept
                                                        (middles-leaving-out kept)) #'<)))
                                     (nth (floor (1- (length ds)) 2) ds)))))
    (and straight (>= (length straight) fewest)
         (let ((sorted (sort straight #'<)))
           (and (<= (nth (floor (length sorted) 2) sorted) 1/3)
                (or (= fewest 1) (<= (nth (floor (* 3 (length sorted)) 4) sorted) 1/2)))))))

(defun light-trend (words)
  "How the light falls across the page of WORDS (WORD-BOXes), as a function of a
column and a row, the grey of their paper there. It is a smooth surface of the
second degree fitted to each word's paper at its middle by least squares, three
times. Each time it is fitted without the words more than three times as far
from the surface so far as the words typically are (and more than a grey level
and a half), the first time from the middle paper. Words pasted on paper of
their own, or a band printed again, are left out. A lamp or a window lights a
page unevenly but smoothly; a scanner nearly evenly."
  (let* ((n (length words))
         (xs (make-array n :element-type 'double-float)) (ys (make-array n :element-type 'double-float))
         (zs (make-array n :element-type 'double-float)) (offs (make-array n :element-type 'double-float))
         (keep (make-array n :element-type 'bit :initial-element 1))
         (c (make-array 6 :element-type 'double-float :initial-element 0d0))
         (a (make-array '(6 7) :element-type 'double-float))
         (terms (make-array 6 :element-type 'double-float)))
    (declare (type fixnum n))
    (loop for w in words for k from 0
          do (setf (aref xs k) (/ (word-centre w) 1000d0)
                   (aref ys k) (/ (+ (word-box-y0 w) (word-box-y1 w)) 2000d0)
                   (aref zs k) (float (word-box-paper w) 1d0)))
    (labels ((at (x y)
               (declare (type double-float x y))
               (+ (aref c 0) (* (aref c 1) x) (* (aref c 2) y) (* (aref c 3) x x) (* (aref c 4) x y)
                  (* (aref c 5) y y)))
             (fit ()
               ;; the normal equations of the kept points, solved by elimination
               (fill (make-array 42 :element-type 'double-float :displaced-to a) 0d0)
               (dotimes (k n)
                 (when (= 1 (aref keep k))
                   (let ((x (aref xs k)) (y (aref ys k)))
                     (setf (aref terms 0) 1d0 (aref terms 1) x (aref terms 2) y
                           (aref terms 3) (* x x) (aref terms 4) (* x y) (aref terms 5) (* y y))
                     (dotimes (i 6)
                       (dotimes (j 6) (incf (aref a i j) (* (aref terms i) (aref terms j))))
                       (incf (aref a i 6) (* (aref terms i) (aref zs k)))))))
               (dotimes (i 6)
                 (let ((pivot i))
                   (loop for r from i below 6 when (> (abs (aref a r i)) (abs (aref a pivot i))) do (setf pivot r))
                   (when (< (abs (aref a pivot i)) 1d-9) (return-from fit nil))
                   (dotimes (j 7) (rotatef (aref a i j) (aref a pivot j)))
                   (dotimes (r 6)
                     (unless (= r i)
                       (let ((f (/ (aref a r i) (aref a i i))))
                         (dotimes (j 7) (decf (aref a r j) (* f (aref a i j)))))))))
               (dotimes (i 6) (setf (aref c i) (/ (aref a i 6) (aref a i i))))
               t))
      (let ((middle (if words (float (median-of (mapcar #'word-box-paper words)) 1d0) 0d0)))
        (setf (aref c 0) middle)
        (when (>= n 12)
          ;; from the middle paper, three times: the words near enough the surface so far fitted
          (dotimes (pass 3)
            (dotimes (k n) (setf (aref offs k) (abs (- (aref zs k) (at (aref xs k) (aref ys k))))))
            (let ((bar (max 1.5d0
                            (* 3 (nth-least (replace (make-array n :element-type 'double-float) offs) n (floor n 2)))))
                  (was (copy-seq c)))
              (dotimes (k n) (setf (aref keep k) (if (<= (aref offs k) bar) 1 0)))
              (unless (fit) (replace c was) (return)))))
        (lambda (x y) (at (/ (float x 1d0) 1000d0) (/ (float y 1d0) 1000d0)))))))

(defun even-light-p (words light)
  "True when the paper of WORDS (WORD-BOXes) is lit evenly once the smooth
fall of the light across the page, LIGHT (LIGHT-TREND), is taken out: the
middle of how far each word's paper stands from it two grey levels at
most. A photograph's light falls unevenly beyond a smooth trend, where
the paper curls or a shadow falls, by six grey levels and more."
  (and words
       (<= (median-of (mapcar (lambda (w) (abs (- (word-box-paper w)
                                                   (funcall light
                                                            (word-centre w)
                                                            (/ (+ (word-box-y0 w) (word-box-y1 w)) 2)))))
                              words))
           2)))

;;; Bands of paper set apart

(defstruct (paper-band (:copier nil) (:predicate nil))
  "A band of a scanned picture's rows whose paper stands apart from the
paper above and below it (PAPER-BANDS). TOP and BOTTOM are its rows in
the picture made level (the last excluded), X0 and X1 the columns of the
text it crosses. PAPER is the grey of its blank paper, ABOVE that of the
blank paper within a line above it and BELOW within a line below it.
WORDS is how many of the page's words read it holds, LINES how many lines
of text its rows span."
  top bottom x0 x1 paper above below (words 0) (lines 0))

(defparameter *band-least* 6
  "The fewest grey levels the paper of a band of rows must stand apart
from the paper above and below it to be named (PAPER-BANDS).")

(defun row-papers (grey width height slope x0 x1 top rows)
  "Three vectors of ROWS fixnums, for each row of the picture GREY (WIDTH
by HEIGHT) from TOP, followed along SLOPE (how far the lines fall for
each pixel across) over the columns X0 below X1. The first is its paper,
the middle of the grey levels *BAND-PAPER-SHARE* of the pixels of each
fifth of it reach or stay under, so that a patch across less than half
the row moves it not. The second is its ink, the darkest fiftieth of it.
The third is its lightest hundredth."
  (declare (type grey-pixels grey) (type fixnum width height x0 x1 top rows))
  (let ((paper (make-array rows :element-type 'fixnum))
        (dark (make-array rows :element-type 'fixnum))
        (light (make-array rows :element-type 'fixnum))
        (counts (make-array 256 :element-type 'fixnum))
        (part (make-array 256 :element-type 'fixnum))
        (fifths (make-array 5 :element-type 'fixnum))
        ;; how far each column's rows fall, worked out once
        (fall (let ((fall (make-array (- x1 x0) :element-type 'fixnum)))
                (dotimes (i (- x1 x0) fall) (setf (aref fall i) (round (* slope (+ x0 i))))))))
    (dotimes (r rows (values paper dark light))
      (fill counts 0)
      (let ((n 0))
        (declare (type fixnum n))
        (dotimes (k 5)
          (fill part 0)
          (let ((in 0))
            (declare (type fixnum in))
            (loop for x from (+ x0 (floor (* k (- x1 x0)) 5)) below (+ x0 (floor (* (1+ k) (- x1 x0)) 5))
                  for y of-type fixnum = (+ top r (aref fall (- x x0)))
                  when (< -1 y height)
                    do (let ((g (aref grey (+ (* y width) x))))
                         (incf (aref part g)) (incf (aref counts g)) (incf in)))
            (incf n in)
            (setf (aref fifths k) (counted-level part (max 1 (ceiling (* *band-paper-share* in)))))))
        (flet ((at (share) (counted-level counts (max 1 (ceiling (* share n))))))
          (setf (aref paper r) (median-of (coerce fifths 'list))
                (aref dark r) (at 1/50)
                (aref light r) (at 99/100)))))))

(defun rows-away (paper blank reach)
  "How far the paper of each row (PAPER, a vector of fixnums) stands from
the middle of the rows of its kind within REACH rows of it, a vector as
long: a row of blank paper (its bit in BLANK 1) beside such rows, a row
crossing letters beside such rows. A window of them is counted as it
slides down the page."
  (declare (type (simple-array fixnum (*)) paper) (type simple-bit-vector blank) (type fixnum reach))
  (let* ((rows (length paper))
         (away (make-array rows :element-type 'fixnum))
         (counts (make-array 256 :element-type 'fixnum)))
    (dolist (kind '(0 1) away)
      (fill counts 0)
      (let ((in 0) (from 0) (to 0))
        (declare (type fixnum in from to))
        (dotimes (r rows)
          (loop while (< to (min rows (+ r reach 1)))
                do (when (= kind (sbit blank to))
                     (incf (aref counts (aref paper to)))
                     (incf in))
                   (incf to))
          (loop while (< from (- r reach))
                do (when (= kind (sbit blank from))
                     (decf (aref counts (aref paper from)))
                     (decf in))
                   (incf from))
          (when (= kind (sbit blank r))
            (setf (aref away r) (- (aref paper r) (counted-level counts (ceiling in 2))))))))))

(defun paper-bands (grey width height tilt words)
  "The bands of rows of the scanned picture GREY (WIDTH by HEIGHT, its
lines falling TILT for each pixel across) whose paper stands apart from
the paper above and below them, as PAPER-BANDs, from WORDS, its words
\(WORD-BOXes), which tell where its text is. Each row's paper is read
across the columns the text spans, followed along the page's tilt
\(ROW-PAPERS). It is set beside the middle of the rows of its kind about
it, as far as seven lines either way (ROWS-AWAY). A band is a run of rows
standing apart the same way, from one line of text tall to
*SCAN-REGION-LINES* lines. It stands apart by more than four times as far
as the page's rows typically stray and by *BAND-LEAST* grey levels at
least. Its own paper is read in its rows of blank paper, between its lines
or about them, which a hatching or a bar across a line has none of. The
blank paper within a line above it, and that within a line below it, each
stand the other way from it by *BAND-LEAST* at least: a step or a slope of
the light, or a tinted panel closed by a rule, is on one side only. Its
edges are sharp. A quarter of a line's height at most of the blank rows
within a line of each edge stand between a quarter and three quarters of
the way towards it, as no shadow, curl of the paper or fall of the light
leaves them. It has ink in it, as dark beside its paper as half the page's
ink is beside the page's, so that a box of one grey is not one and thin
light type scanned at 200 dots an inch is. And, on darker paper, it has no
marks lighter than halfway back to the paper about it, as type set in
reverse, light on a dark bar, has. Only on a page whose light is even once
its smooth fall is taken out (LIGHT-TREND, EVEN-LIGHT-P), as for the
blocks of words of REGIONS-UNLIKE. Runs of one band its noise parted, a
line apart at most, are one, judged whole. However much darker or lighter
the band is, so long as ink still shows in it, it is found: its words need
not be read, and those of a darker band are read again with its paper
lifted to the page's (BANDS-LIFTED). A band of ink alone printed darker on
the page's own paper is not one."
  (declare (type grey-pixels grey) (type fixnum width height))
  (let* ((text (remove-if #'word-box-lone words))
         (tall (and text (median-of (mapcar (lambda (w) (- (word-box-y1 w) (word-box-y0 w))) text))))
         (tops (and text (sort (remove-duplicates (mapcar #'word-box-y0 text)) #'<)))
         (pitch (if (cdr tops) (median-of (loop for (a b) on tops while b collect (- b a))) tall)))
    (when (and text (cddr tops) (>= tall 6) (even-light-p text (light-trend text)))
      (let* ((x0 (reduce #'min text :key #'word-box-x0)) (x1 (reduce #'max text :key #'word-box-x1))
             (top (reduce #'min text :key #'word-box-y0)) (bottom (reduce #'max text :key #'word-box-y1))
             (rows (- bottom top)) (ramp (max 2 (floor tall 4))) (out '()))
        (multiple-value-bind (paper dark light)
            (row-papers grey width height (if (< (abs tilt) 0.0003) 0 tilt) x0 x1 top rows)
          ;; a row of blank paper between lines, or one crossing letters, whose paper reads a little darker for
          ;; the ink about it
          (let* ((blank (let ((bits (make-array rows :element-type 'bit)))
                          (dotimes (r rows bits)
                            (setf (sbit bits r) (if (>= (* 4 (aref dark r)) (* 3 (aref paper r))) 1 0)))))
                 (away (rows-away paper blank (* 7 pitch)))
                 (bar (max *band-least* (* 4 (median-of (map 'list #'abs away)))))
                 ;; how dark the page's ink is beside its paper, in the rows crossing letters
                 (inked (median-of (loop for k below rows when (zerop (sbit blank k))
                                         collect (/ (aref dark k) (max 1 (aref paper k))))))
                 (r 0))
            (labels ((apart-p (k sign) (and (> (abs (aref away k)) bar) (= (signum (aref away k)) sign)))
                     (over (from to key) (loop for k from (max 0 from) below (min rows to) collect (aref key k)))
                     (sharp-p (from to d)
                       ;; few rows of blank paper near an edge stand part of the way towards the band's, the
                       ;; ringing a JPEG leaves the other way beside an edge not counted
                       (<= (loop for k from (max 0 from) below (min rows to)
                                 count (and (= 1 (sbit blank k))
                                            (< (/ (abs d) 4) (* (signum d) (aref away k)) (* 3/4 (abs d)))))
                           ramp))
                     (side (from to)
                       ;; the middle paper of the blank rows from FROM below TO, NIL for none
                       (median-of (loop for k from (max 0 from) below (min rows to)
                                        when (= 1 (sbit blank k)) collect (aref paper k))))
                     (band-p (r end d own above below)
                       (and (>= (- end r) tall) (<= (- end r) (* *scan-region-lines* pitch)) (> (abs d) bar)
                            ;; blank paper within a line above it and within a line below, each standing the
                            ;; other way from it by the least at least: no step or slope of the light, which
                            ;; is on one side alone
                            above below
                            (>= (* (signum d) (- own above)) *band-least*)
                            (>= (* (signum d) (- own below)) *band-least*)
                            (sharp-p (- r tall) (+ r tall) d) (sharp-p (- end tall) (+ end tall) d)
                            ;; ink in it, as dark beside its paper as half the page's is at least, and on darker
                            ;; paper no marks lighter than it, as type set in reverse, light on a dark bar, has
                            (let ((own-ink (median-of (loop for k from r below end when (zerop (sbit blank k))
                                                            collect (/ (aref dark k) own)))))
                              (and own-ink inked (< own-ink (/ (+ 1 inked) 2))))
                            (or (plusp d) (<= (median-of (over r end light)) (- own (/ d 2)))))))
              (loop while (< r rows)
                    do (if (<= (abs (aref away r)) bar)
                           (incf r)
                           (let ((end r) (sign (signum (aref away r))))
                             ;; the run, across rows nearer the paper about it, a line of them at most, as a row
                             ;; of heavy ink or the band's own noise makes: its runs so parted are one band,
                             ;; judged whole
                             (loop while (and (< end rows) (apart-p end sign))
                                   do (incf end)
                                      (let ((next (loop for k from end below (min rows (+ end pitch 1))
                                                        when (apart-p k sign) return k)))
                                        (when next (setf end next))))
                             (let ((d (median-of (over r end away)))
                                   (own (median-of (loop for k from r below end
                                                         when (= 1 (sbit blank k)) collect (aref paper k)))))
                               ;; blank paper between its lines or about them, as a hatching or a bar has none
                               (let ((above (side (- r pitch) (- r ramp))) (below (side (+ end ramp) (+ end pitch))))
                                 (when (and own (plusp own) (band-p r end d own above below))
                                   (push (make-paper-band :top (+ top r) :bottom (+ top end) :x0 x0 :x1 x1
                                                          :paper own :above above :below below
                                                          :words (count-if
                                                                  (lambda (w)
                                                                    (<= (+ top r)
                                                                        (* 1/2 (+ (word-box-y0 w) (word-box-y1 w)))
                                                                        (+ top end)))
                                                                  text)
                                                          :lines (max 1 (round (- end r) pitch)))
                                         out))))
                             (setf r end)))))))
        (nreverse out)))))

(deftype doubles () "A simple vector of double-floats." '(simple-array double-float (*)))

(deftype fixnums () "A simple vector of fixnums." '(simple-array fixnum (*)))

(defstruct (unlike-reading (:constructor %make-unlike-reading) (:copier nil) (:predicate nil))
  "One reading of a page's words by WORDS-UNLIKE: its LINES (from SCAN-LINES)
falling TILT for each pixel across, the words KEPT to set beside others, FLAT,
BEND, LIGHT and ASIDE as WORDS-UNLIKE takes them. OUT, the words found
unlike the rest so far, each (WORD WAY...) the latest first; INKED, the
blocks set apart by their ink alone, kept until the others are known.
MIDDLE, the paper's middle light, FACTORS each word's light by it, WIDEST
the widest a word set beside others may be. The rest is scratch, made once:
OTHERS, ORDER, PLACES, VALUES, SORTED, MIDDLES and SCRATCH for one line's
words, as long as the longest line; SIDES and FARS for every word's
distances; and the tables of each measure (NEAR, TWO-SIDED, FAR, LINE-NEAR,
LINE-FAR)."
  lines tilt kept flat bend light aside (out '()) (inked '()) middle (factors (make-hash-table :test 'eq)) widest
  (others #() :type simple-vector) (order (make-array 0 :element-type 'fixnum) :type fixnums)
  (places (make-array 0 :element-type 'fixnum) :type fixnums)
  (values (make-array 0 :element-type 'double-float) :type doubles)
  (sorted (make-array 0 :element-type 'double-float) :type doubles)
  (middles (make-array 0 :element-type 'double-float) :type doubles)
  (scratch (make-array 0 :element-type 'double-float) :type doubles)
  (sides (make-array 0 :element-type 'double-float) :type doubles)
  (fars (make-array 0 :element-type 'double-float) :type doubles)
  (near (make-hash-table :test 'eq)) (two-sided (make-hash-table :test 'eq)) (far (make-hash-table :test 'eq))
  (line-near (make-hash-table :test 'eq)) (line-far (make-hash-table :test 'eq)))

(defun make-unlike-reading (lines tilt kept flat light bend aside)
  "A new UNLIKE-READING of LINES, its scratch as long as the longest line and
the page's words."
  (let ((longest (reduce #'max lines :key #'length :initial-value 0))
        (count (loop for line across lines sum (length line))))
    (flet ((doubles (n) (make-array n :element-type 'double-float))
           (fixnums (n) (make-array n :element-type 'fixnum)))
      (%make-unlike-reading
       :lines lines :tilt tilt :kept kept :flat flat :bend bend :light light :aside aside
       :middle (and light (median-of (loop for line across lines append (map 'list #'word-box-paper line))))
       :widest (* *scan-widest-word*
                  (median-of (loop for line across lines
                                   append (loop for w across line collect (- (word-box-x1 w) (word-box-x0 w))))))
       :others (make-array longest) :order (fixnums longest) :places (fixnums longest)
       :values (doubles longest) :sorted (doubles longest) :middles (doubles longest) :scratch (doubles longest)
       :sides (doubles (* 2 count)) :fars (doubles count)))))

(defun unlike-lit (reading w v)
  "V, a value of the word W, as it would be under READING's page's middle
light."
  (let ((factors (unlike-reading-factors reading)))
    (* v (or (gethash w factors)
             (setf (gethash w factors)
                   (let ((here (funcall (unlike-reading-light reading)
                                        (word-centre w) (/ (+ (word-box-y0 w) (word-box-y1 w)) 2))))
                     (if (plusp here) (/ (unlike-reading-middle reading) here) 1d0)))))))

(defstruct (unlike-pass (:constructor %make-unlike-pass) (:copier nil) (:predicate nil))
  "One measure of *SCAN-MEASURES* read over the words of an UNLIKE-READING:
its KEY, MEASURE (a function of a word), LEAST, TIMES, FEWER and MORE as
that table gives them. SITS, true for where a word sits (:offset, :turn);
VALUE, what is set beside the line's shape; USABLE, the words set beside
others; JUDGED, those that may be named; FALLBACK, a line's slope where its
shape cannot be told; POINTS, each line's LINE-POINTS; STRAIGHTNESS, how
straight and even each line keeps. N-SIDES and N-FARS count the distances
kept in the reading's SIDES and FARS; BETWEEN, the measure of the words
with neighbours on both sides."
  key measure least times fewer more sits value usable judged fallback points straightness
  (n-sides 0 :type fixnum) (n-fars 0 :type fixnum) (between '()))

(defun make-unlike-pass (reading entry)
  "A new UNLIKE-PASS of READING for ENTRY of *SCAN-MEASURES*, the tables of
its distances emptied."
  (destructuring-bind (key measure least times words fewer more) entry
    (declare (ignore words))
    (let* ((lines (unlike-reading-lines reading)) (widest (unlike-reading-widest reading))
           (kept (unlike-reading-kept reading)) (bend (unlike-reading-bend reading))
           (sits (member key '(:offset :turn)))
           (value (if sits (lambda (w) (- (word-box-baseline w) (funcall bend (word-centre w)))) measure))
           (usable (lambda (w) (and (funcall kept w) (<= (- (word-box-x1 w) (word-box-x0 w)) widest)
                                    (not (and sits (word-box-cut w)))))))
      (dolist (table (list (unlike-reading-near reading) (unlike-reading-two-sided reading)
                           (unlike-reading-far reading) (unlike-reading-line-near reading)
                           (unlike-reading-line-far reading)))
        (clrhash table))
      (%make-unlike-pass
       :key key :measure measure :least least :times times :fewer fewer :more more :sits sits :value value
       :usable usable
       :judged (case key
                 (:offset (lambda (w)
                            (and (<= (- (word-box-x1 w) (word-box-x0 w)) widest) (not (word-box-cut w))
                                 (wide-enough-p w))))
                 (:turn (lambda (w)
                          (and (<= (- (word-box-x1 w) (word-box-x0 w)) widest) (not (word-box-cut w))
                               (word-box-slope w))))
                 (t (lambda (w) (<= (- (word-box-x1 w) (word-box-x0 w)) widest))))
       :fallback (if sits (unlike-reading-tilt reading) 0)
       ;; where a word sits is set beside its line's shape; how it looks, beside its neighbours alone; on a page
       ;; lying flat, where a word sits is set beside a line of the page's tilt
       :points (if (and sits (not (unlike-reading-flat reading)))
                   (map 'simple-vector (lambda (line) (line-points line value usable)) lines)
                   (make-array (length lines) :initial-element #()))
       :straightness (make-array (length lines) :initial-element nil)))))

(defun pass-straight (reading pass w)
  "What places the word W on a line of the page's tilt, for a PASS of where
words sit, or how it looks."
  (if (unlike-pass-sits pass)
      (- (funcall (unlike-pass-value pass) w) (* (unlike-reading-tilt reading) (word-centre w)))
      (funcall (unlike-pass-measure pass) w)))

(defun line-sorted (reading pass line)
  "Set in READING's scratch the words of LINE that PASS sets beside others
\(OTHERS), their values (VALUES), sorted (SORTED), where each stands among
them (PLACES) and the middle of the others (MIDDLES). Returns how many."
  (let ((others (unlike-reading-others reading)) (values (unlike-reading-values reading))
        (order (unlike-reading-order reading)) (sorted (unlike-reading-sorted reading))
        (places (unlike-reading-places reading)) (middles (unlike-reading-middles reading))
        (n 0))
    (declare (type fixnum n))
    (loop for w across line
          when (funcall (unlike-pass-usable pass) w)
            do (setf (svref others n) w (aref values n) (float (pass-straight reading pass w) 1d0))
               (incf n))
    (dotimes (i n)
      (let ((v (aref values i)) (j i))
        (declare (type fixnum j))
        (loop while (and (plusp j) (> (aref values (aref order (1- j))) v))
              do (setf (aref order j) (aref order (1- j))) (decf j))
        (setf (aref order j) i)))
    (dotimes (k n)
      (setf (aref sorted k) (aref values (aref order k)) (aref places (aref order k)) k))
    (when (>= n 2)
      (dotimes (i n)
        (setf (aref middles i) (the double-float (middle-skipping sorted (aref places i) nil n)))))
    n))

(defun word-near-distance (reading pass word line about nears)
  "Note in READING how far WORD stands from its neighbours on LINE by PASS,
ABOUT being the lines about it: its distance (NEAR), whether it has
neighbours on both sides (TWO-SIDED), each side's distance (SIDES). Returns
NEARS, with the distance pushed when it has both."
  (multiple-value-bind (left right)
      (if (eq (unlike-pass-key pass) :turn)
          (let ((turn (turn-residual word about (unlike-reading-tilt reading)))) (values turn turn))
          (line-residuals word line (unlike-pass-value pass) (unlike-pass-usable pass) about
                          (unlike-pass-fallback pass)))
    (let ((both (both-sides left right)) (sides (unlike-reading-sides reading)))
      (when both
        (setf (gethash word (unlike-reading-near reading)) both
              (gethash word (unlike-reading-two-sided reading)) (and left right t))
        (when left
          (setf (aref sides (unlike-pass-n-sides pass)) (abs (float left 1d0)))
          (incf (unlike-pass-n-sides pass)))
        (when (and right (not (eq (unlike-pass-key pass) :turn)))
          (setf (aref sides (unlike-pass-n-sides pass)) (abs (float right 1d0)))
          (incf (unlike-pass-n-sides pass)))
        (when (and left right)
          (push (abs both) nears)
          (push (funcall (unlike-pass-measure pass) word) (unlike-pass-between pass)))))
    nears))

(defun word-far-distance (reading pass word n)
  "Note in READING how far WORD stands from the rest of its line by PASS, the
N words of the line in its scratch (LINE-SORTED), and how far the others
stand from the rest, both left out, the lower middle of them (LINE-FAR)."
  (let* ((others (unlike-reading-others reading)) (values (unlike-reading-values reading))
         (sorted (unlike-reading-sorted reading)) (places (unlike-reading-places reading))
         (scratch (unlike-reading-scratch reading))
         (at (position word others :end n)))
    (setf (gethash word (unlike-reading-far reading))
          (- (pass-straight reading pass word)
             (if at (aref (unlike-reading-middles reading) at) (middle-skipping sorted nil nil n)))
          (gethash word (unlike-reading-line-far reading))
          (let ((m 0) (skip (and at (aref places at))))
            (declare (type fixnum m))
            (dotimes (j n)
              (unless (eql j at)
                (setf (aref scratch m) (abs (- (aref values j) (middle-skipping sorted skip (aref places j) n))))
                (incf m)))
            (and (> m 1) (nth-least scratch m (floor (1- m) 2)))))))

(defun measure-line (reading pass line at)
  "Note in READING, by PASS, how far each word of LINE, the line AT, stands
from its neighbours and from the rest of its line, how straight and even
the line keeps, and how far its words typically stand from their
neighbours (LINE-NEAR)."
  (let* ((key (unlike-pass-key pass)) (usable (unlike-pass-usable pass))
         (about (lines-about (unlike-pass-points pass) at))
         (counted (count-if usable line))
         (n (line-sorted reading pass line))
         (nears '()))
    (loop for word across line
          when (and (funcall (unlike-pass-judged pass) word)
                    ;; the left one of a line of two is what the right one is set beside
                    (not (and (= counted 2) (not (eq key :turn)) (eq word (find-if usable line)))))
            do (setf nears (word-near-distance reading pass word line about nears))
               ;; beside the rest of its line, two words of it at least besides it
               (when (and (>= n 2) (not (eq key :turn))
                          (>= (- n (if (find word (unlike-reading-others reading) :end n) 1 0)) 2))
                 (word-far-distance reading pass word n)))
    ;; how far the words of the page stand from the rest of their lines, and how straight and even each line keeps,
    ;; the lower middle of its words' distances
    (when (and (not (eq key :turn)) (>= n 3))
      (let ((values (unlike-reading-values reading)) (middles (unlike-reading-middles reading))
            (fars (unlike-reading-fars reading)) (scratch (unlike-reading-scratch reading)))
        (dotimes (i n)
          (let ((d (abs (- (aref values i) (aref middles i)))))
            (setf (aref fars (unlike-pass-n-fars pass)) d)
            (incf (unlike-pass-n-fars pass))
            (setf (aref scratch i) d)))
        (setf (svref (unlike-pass-straightness pass) at) (nth-least scratch n (floor (1- n) 2)))))
    (let ((jitter (if (>= (length nears) 3) (median-of nears) 0)))
      (loop for word across line do (setf (gethash word (unlike-reading-line-near reading)) jitter)))))

(defun record-unlike (reading pass word off &optional block)
  "Note in READING that WORD is unlike the rest by PASS's measure, by OFF,
with BLOCK, the block of lines it is set apart with or :pair; kept on the
word too for where it sits."
  (let ((key (unlike-pass-key pass)))
    (when (eq key :offset) (setf (word-box-offset word) off))
    (when (eq key :turn) (setf (word-box-turn word) off))
    (let ((entry (or (assoc word (unlike-reading-out reading)) (car (push (list word) (unlike-reading-out reading))))))
      (unless (assoc key (cdr entry))
        (push (if (unlike-pass-sits pass)
                  (list key off 0 (if (minusp off) (unlike-pass-fewer pass) (unlike-pass-more pass)) block)
                  (let ((v (funcall (unlike-pass-measure pass) word)))
                    (list key v (- v off) (if (minusp off) (unlike-pass-fewer pass) (unlike-pass-more pass)) block)))
              (cdr entry))))))

(defun near-off (reading pass word line at spread usual)
  "How far WORD of LINE, the line AT, stands from its neighbours by PASS
when that is far enough to name, else NIL: at the end of its line, beside
the words over and under it instead; SPREAD, how far words typically stand
from their neighbours, USUAL the measure of the words between neighbours."
  (let* ((near (unlike-reading-near reading)) (own (gethash word near))
         (bar (gethash word (unlike-reading-line-near reading))) (off own)
         (sits (unlike-pass-sits pass)) (measure (unlike-pass-measure pass)) (times (unlike-pass-times pass)))
    ;; at the end of its line: beside the words over and under it; the right one of a line of two on a flat page,
    ;; beside the left one alone
    (unless (or (gethash word (unlike-reading-two-sided reading))
                (and (unlike-reading-flat reading)
                     (= (count-if (lambda (w) (not (and sits (word-box-cut w)))) line) 2)))
      (let ((column (column-mates word (unlike-reading-lines reading) at near)))
        (if (>= (length column) 2)
            (let ((middle (median-of (mapcar (lambda (o) (gethash o near)) column))))
              (setf off (- own middle))
              (when (>= (length column) 3)
                (setf bar (max bar (median-of (mapcar (lambda (o) (abs (- (gethash o near) middle))) column)))))
              ;; how it looks, unlike the words over and under it, or the page's words between neighbours, the same
              ;; way
              (unless sits
                (let ((v (funcall measure word)))
                  (unless (some (lambda (apart)
                                  (and apart (plusp (* apart off)) (> (abs apart) (* times (max spread bar)))))
                                (list (- v (median-of (mapcar measure column))) (and usual (- v usual))))
                    (setf off nil)))))
            (setf off nil))))
    (and off (> (abs off) (* times (max spread bar))) (> (abs off) (unlike-pass-least pass)) off)))

(defun far-off (reading pass word at far-spread)
  "How far WORD of the line AT stands from the rest of its line by PASS,
when that is further than it and the lines about it keep straight and even,
two of those at least, else NIL; FAR-SPREAD, how far words typically stand
from the rest of their lines."
  (let* ((d (gethash word (unlike-reading-far reading))) (line (gethash word (unlike-reading-line-far reading)))
         (straightness (unlike-pass-straightness pass))
         (kept (loop for i from (max 0 (- at *scan-lines-about*))
                       to (min (1- (length (unlike-reading-lines reading))) (+ at *scan-lines-about*))
                     unless (= i at) when (svref straightness i) collect it))
         (bar (and d line (cdr kept) (* (unlike-pass-times pass) (max far-spread line (median-of kept))))))
    (and bar (> (abs d) bar) (> (abs d) (unlike-pass-least pass)) d)))

(defun line-partner (reading pass word line off)
  "The other word of LINE, a line of two on a flat page, when WORD is unlike
by OFF only as set beside it, so that which of them moved cannot be told;
else NIL."
  (let ((sits (unlike-pass-sits pass)) (own (gethash word (unlike-reading-near reading))))
    (and (unlike-reading-flat reading)
         (not (eq (unlike-pass-key pass) :turn))
         own
         (eql off own)
         (not (gethash word (unlike-reading-two-sided reading)))
         (let ((pair (remove-if (lambda (w) (and sits (word-box-cut w))) line)))
           (and (= (length pair) 2) (find word pair) (find-if-not (lambda (w) (eq w word)) pair))))))

(defun read-measure (reading pass)
  "Read the words of READING by PASS: each word's distances first, then
each word unlike the rest, beside its neighbours or else beside the rest of
its line, and, for how a word looks, the blocks of lines set apart together
where the paper's light is even."
  (let ((lines (unlike-reading-lines reading)) (least (unlike-pass-least pass)) (times (unlike-pass-times pass)))
    (loop for line across lines for at from 0 do (measure-line reading pass line at))
    (let* ((n-sides (unlike-pass-n-sides pass)) (n-fars (unlike-pass-n-fars pass))
           (spread (max (if (plusp n-sides) (nth-least (unlike-reading-sides reading) n-sides (floor n-sides 2)) 0)
                        (/ least 6)))
           (far-spread (max (if (plusp n-fars) (nth-least (unlike-reading-fars reading) n-fars (floor n-fars 2)) 0)
                            (/ least 6)))
           (usual (and (unlike-pass-between pass) (median-of (unlike-pass-between pass)))))
      (loop for line across lines for at from 0
            do (loop for word across line
                     for off = (or (and (gethash word (unlike-reading-near reading))
                                        (near-off reading pass word line at spread usual))
                                   (far-off reading pass word at far-spread))
                     when off
                       do (let ((partner (line-partner reading pass word line off)))
                            (if partner
                                (progn (record-unlike reading pass word off :pair)
                                       (record-unlike reading pass partner (- off) :pair))
                                (record-unlike reading pass word off)))))
      ;; how it looks: blocks of lines set apart together, their ink set beside the paper's light; a block of ink
      ;; alone is kept until the others are known
      (when (and (unlike-reading-light reading) (not (unlike-pass-sits pass)))
        (let* ((key (unlike-pass-key pass)) (aside (unlike-reading-aside reading))
               (blocks (if (eq key :ink)
                           (regions-unlike lines (lambda (w) (unlike-lit reading w (word-box-ink w)))
                                           (max (/ least 2) (* times 1/2 far-spread)) aside)
                           (regions-unlike lines (unlike-pass-measure pass) (max (/ least 2) (* times far-spread))
                                           aside))))
          (dolist (block blocks)
            (if (eq key :ink)
                (push block (unlike-reading-inked reading))
                (loop for (word . d) in block
                      unless (assoc word (unlike-reading-out reading))
                        do (record-unlike reading pass word d block)))))))))

(defun name-inked-blocks (reading)
  "Name in READING the blocks set apart by their ink alone where their paper,
edges or grain stand apart too."
  (destructuring-bind (key measure least times words fewer more) (assoc :ink *scan-measures*)
    (declare (ignore least times words))
    (loop for block in (reverse (unlike-reading-inked reading))
          when (or (some (lambda (pair)
                           (some (lambda (way) (and (member (first way) '(:edges :noise)) (consp (fifth way))))
                                 (cdr (assoc (car pair) (unlike-reading-out reading)))))
                         block)
                   (paper-apart-p block (unlike-reading-lines reading)
                                  (lambda (w) (unlike-lit reading w (word-box-paper w)))))
            do (loop for (word . d) in block
                     unless (assoc word (unlike-reading-out reading))
                       do (let ((v (funcall measure word)))
                            (push (list word (list key v (- v d) (if (minusp d) fewer more) block))
                                  (unlike-reading-out reading)))))))

(defun words-unlike (lines tilt kept &optional flat light (bend (constantly 0)) (aside (constantly nil)))
  "One reading of the words of LINES (from SCAN-LINES), the page's lines
falling TILT for each pixel across, by each measure of *SCAN-MEASURES*, as
UNLIKE-WORDS gives them. Only the words KEPT says (a function of a word)
are set beside others. On a FLAT page the line's shape is not taken from
the lines about it, but from the page's TILT and its BEND (a function of a
column, what all its lines rise or fall by there, PAGE-TREND). Blocks of
lines set apart together (REGIONS-UNLIKE) are found only where the paper's
light is even, LIGHT then being how it falls across the page
\(LIGHT-TREND), else NIL. Their ink is set beside the paper's light there.
A block set apart by its ink alone is named only where its paper
\(PAPER-APART-P), edges or grain stand apart too. Words ASIDE says (a
function of a word) are set beside no block of others (REGIONS-UNLIKE)."
  (let ((reading (make-unlike-reading lines tilt kept flat light bend aside)))
    (dolist (entry *scan-measures*)
      (job-check-stop)
      (read-measure reading (make-unlike-pass reading entry)))
    (name-inked-blocks reading)
    (let ((out (unlike-reading-out reading)))
      (dolist (entry out) (setf (cdr entry) (nreverse (cdr entry))))
      (nreverse out))))

(defparameter *scan-region-paper* 2
  "How many grey levels the paper of a block of lines set apart by its ink
must stand apart from the paper of the lines about it, the same way, for
the block to be named (PAPER-APART-P).")

(defun paper-apart-p (block lines paper)
  "True when the words of BLOCK, each (WORD . HOW-FAR), set apart together
by their ink, stand apart by PAPER (a function of a word) too, the same
way, by *SCAN-REGION-PAPER* or more: the middle of theirs set beside the
middle of the words over or under the block in the three lines above it
and below it. A band printed again or a block pasted brings its paper
with it; a line printed in a second pass of the press, or in bold, has
the page's paper."
  (let* ((at (lambda (word) (position-if (lambda (line) (find word line)) lines)))
         (rows (mapcar (lambda (pair) (funcall at (car pair))) block))
         (top (reduce #'min rows)) (bottom (reduce #'max rows))
         (x0 (reduce #'min block :key (lambda (pair) (word-box-x0 (car pair)))))
         (x1 (reduce #'max block :key (lambda (pair) (word-box-x1 (car pair)))))
         (about (loop for i from (max 0 (- top 3)) to (min (1- (length lines)) (+ bottom 3))
                      unless (<= top i bottom)
                        append (loop for o across (svref lines i)
                                     when (and (< (word-box-x0 o) x1) (> (word-box-x1 o) x0))
                                     collect (funcall paper o))))
         (way (median-of (mapcar #'cdr block))))
    (and about
         (let ((d (- (median-of (mapcar (lambda (pair) (funcall paper (car pair))) block)) (median-of about))))
           (and (>= (abs d) *scan-region-paper*) (plusp (* d way)))))))

;;; How a word is set apart
;;;
;;; A word is set beside its neighbours along its line, *SCAN-NEIGHBOURS* on each
;;; side. First, where it sits, with the shape of its line between them taken from
;;; the lines about it (SIDE-RESIDUAL), so that lines bowed, turned or seen at a
;;; slant set no word apart. On a page that lies flat (FLAT-PAGE-P) it is taken
;;; along the page's tilt and the bow all its lines share (PAGE-TREND), so that
;;; words moved together in the lines about it do not hide it. Second, how it looks,
;;; beside its neighbours' alone, so that light and focus, which change little from
;;; word to word, set none apart either. A word between neighbours on both sides
;;; stands apart only by a step away from both the same way (BOTH-SIDES). A word at
;;; the end of its line, the right one of a line of two, is set beside its neighbour
;;; as the words at the end of the lines above and below are beside theirs, two of
;;; them at least (COLUMN-MATES). It stands apart by how far that differs from
;;; theirs. It stands apart by how it looks only when it is also unlike those words
;;; themselves, or the page's words between neighbours, the same way. With none, it
;;; is not judged so. On a page that lies flat (FLAT-PAGE-P), the right word of a
;;; line of two, a label and its value, is set beside the left one alone, along the
;;; page's tilt. It must stand further off than the least the measure counts. It
;;; must stand further than its multiple of how far the page's words typically stand
;;; from what one side of their neighbours says. It must stand further than its
;;; multiple of how far the words of its line between neighbours stand from theirs
;;; (three at least). And it must stand further than its multiple of how far the
;;; words over and under it differ from each other (three at least). Whichever is
;;; most holds.
;;;
;;; A word is also set beside the rest of its line as a whole, two other words
;;; of it at least: where it sits, beside the line of the page's tilt through
;;; them; how it looks, beside the middle of them. It stands apart so by its
;;; multiple of the most of three measures. The first is how far the page's
;;; words typically stand from the rest of their lines. The second is how far
;;; the other words of its line stand from one another, the lower middle of
;;; them. The third is how straight and even the lines about it keep,
;;; *SCAN-LINES-ABOUT* either side, the middle of them. Two of those must be
;;; measured at least, else it is not set beside its line so. A line that bows
;;; or darkens along itself, or among lines that do, keeps its words from
;;; being set apart so.
;;;
;;; Words that look unlike the words over and under them (COLUMN-MATES, four at
;;; least) by half the least a word alone needs, and by the multiple of how far
;;; the page's words stand from the rest of their lines, the same way, form a
;;; region. It needs four of them at least in lines next to each other and over
;;; or under one another, with words not so over and under them. Such a region
;;; is set apart together, as a picture editor's smoothing or a patch leaves
;;; one. It is named where the paper's light is even once its smooth fall
;;; across the page is taken out (LIGHT-TREND, EVEN-LIGHT-P), as a scanner's is
;;; (REGIONS-UNLIKE). The words of BANDS, the page's bands of paper set apart
;;; (PAPER-BANDS), are over or under no block of others, so that the lines next
;;; to a band are not set apart the other way by it. Their ink is set beside
;;; the light where they stand; a block set apart by its ink alone is named
;;; only where its paper, edges or grain stand apart too (PAPER-APART-P), and
;;; each block is one finding.
;;;
;;; Its baseline is judged only for a word three times as wide as its line is
;;; tall (WIDE-ENOUGH-P), its turn for one whose slope is told; neither for a
;;; word of a line cut from lines that touch, nor is that word a neighbour for
;;; them, its baseline being where the cut fell. A word more than
;;; *SCAN-WIDEST-WORD* times as wide as the page's words typically are is not
;;; judged, nor set beside others. Words of a line less than half as tall as
;;; the page's lines typically are (dots, a rule's remains, the paper's edge)
;;; are not judged, nor set beside others.

(defun unlike-words (words &optional bands)
  "Those of WORDS (WORD-BOXes) that stand apart from the words beside them,
each (WORD (KEY OWN USUAL MORE-OR-LESS-WORDS)...), by each measure of
*SCAN-MEASURES*, read twice: the second time the words the first found are
no one's neighbours, so that two or three words pasted side by side or in
a column do not hide each other. Then, on a page that lies flat
\(FLAT-PAGE-P), columns set off their lines together are added
\(WITH-COLUMNS).

OWN is how far the word's baseline is from its line's, in pixels, or how far
it is turned, in degrees, its USUAL 0; for the others, its own measure, and
USUAL what it is set beside. NIL for a page of fewer than 12 words."
  (when (>= (length words) 12)
    (let* ((tall (median-of (mapcar (lambda (w) (- (word-box-y1 w) (word-box-y0 w))) words)))
           (lines (scan-lines (remove-if (lambda (w) (< (* 2 (- (word-box-y1 w) (word-box-y0 w))) tall)) words)))
           (slopes (remove nil (mapcar #'word-box-slope words)))
           (tilt (if slopes (median-of slopes) 0))
           ;; what all the lines rise or fall by across the page, as a page bowed in a scanner bends them
           (trend (page-trend lines (lambda (w) (- (word-box-baseline w) (* tilt (word-centre w))))
                              (lambda (w) (not (word-box-cut w)))))
           (bow (bow-p trend (reduce #'min words :key #'word-box-x0) (reduce #'max words :key #'word-box-x1)))
           (flat (or (flat-page-p lines tilt) (and bow (flat-page-p lines tilt trend *scan-bow-lines*))))
           (bend (if (and flat bow) trend (constantly 0)))
           ;; the light across the page, where it falls evenly once its smooth trend is taken out
           (light (let ((light (light-trend words))) (and (even-light-p words light) light)))
           (aside (lambda (w) (let ((y (* 1/2 (+ (word-box-y0 w) (word-box-y1 w)))))
                                (some (lambda (b) (<= (paper-band-top b) y (paper-band-bottom b))) bands))))
           (first (words-unlike lines tilt (constantly t) flat light bend aside)))
      (if first
          (let* ((found (let ((found (make-hash-table :test 'eq)))
                          (dolist (entry first found) (setf (gethash (car entry) found) t))))
                 (second (words-unlike lines tilt (lambda (w) (not (gethash w found))) flat light bend aside))
                 ;; two words set beside each other alone, found together, are no one's neighbours the
                 ;; second time: kept as the first reading found them
                 (both (append second
                               (remove-if (lambda (entry) (or (assoc (car entry) second)
                                                              (notany (lambda (way) (eq (fifth way) :pair))
                                                                      (cdr entry))))
                                          first))))
            (if flat (with-columns both lines tilt trend) both))
          (if flat (with-columns first lines tilt trend) first)))))

(defun with-columns (found lines tilt trend)
  "FOUND, the words UNLIKE-WORDS found unlike, each (WORD WAY...), with the
words of LINES (from SCAN-LINES, the page's lines falling TILT for each
pixel across and rising or falling together by TREND, PAGE-TREND) in
columns set off their lines together (COLUMNS-UNLIKE)
given a lower or a higher baseline too, as the measure :OFFSET of
*SCAN-MEASURES* says."
  (destructuring-bind (key measure least times words fewer more) (assoc :offset *scan-measures*)
    (declare (ignore measure words))
    (dolist (pair (columns-unlike lines (lambda (w) (- (word-box-baseline w) (* tilt (word-centre w))))
                                  (lambda (w) (not (word-box-cut w))) times least trend)
                  found)
      (destructuring-bind (word d either) pair
        (let ((entry (or (assoc word found) (car (push (list word) found)))))
          (unless (assoc key (cdr entry))
            (setf (word-box-offset word) d)
            (setf (cdr entry)
                  (append (cdr entry) (list (list key d 0 (if (minusp d) fewer more) (and either :pair)))))))))))

(defstruct (scan-picture (:copier nil) (:predicate nil))
  "A scanned picture of a page, read. PAGE is the page and MARK the picture as
it draws it. WIDTH and HEIGHT are its pixels. TILT is how far its lines fall for
each pixel across. ODD is its words unlike the rest (UNLIKE-WORDS), each (WORD
\(KEY OWN USUAL WORDS)...). GLYPHS is its characters (WORD-GLYPHS). PAPER is how
far its blank paper strays (BLANK-PAPER-NOISE). RULES is how many pixels of
rules were taken out of it (WITHOUT-RULES) and INK how many of ink were left.
TALL-BANDS is how many bands of rows too tall to be lines were left unread
\(PICTURE-WORDS). CHARACTERS is about how many characters its ink holds, its ink
once the rules are taken out over its characters' middle ink (NIL when none is
found). UNREAD is true when no words could be read from it at all. JUDGED is
true when its grain was judged (SCANS-READ), that is, its characters differ as
prints do, and it was not drawn by a program. TWIN is the SCAN-PICTURE read
before it whose picture is kept in the file the same octet for octet and drawn
alike (SCAN-PICTURES), its characters left to that one's. DRAWN is true when it
was drawn by a program, not scanned (SCANS-READ). BANDS is its bands of rows
whose paper stands apart from the paper above and below them (PAPER-BANDS)."
  page mark width height tilt odd glyphs paper (rules 0) (ink 0)
  (tall-bands 0) characters unread judged twin drawn bands)

(defun bands-lifted (grey width height tilt bands)
  "A copy of the scanned picture GREY (WIDTH by HEIGHT, its lines
falling TILT for each pixel across) with the rows of each of BANDS
\(PAPER-BANDS) whose paper is darker than the paper about it lifted to
it, each grey times the paper about it over its own. So a band printed
far darker than the page, whose paper lies under the level its ink is
told by, is read at the page's level as the rest is. NIL when none is
darker."
  (declare (type grey-pixels grey) (type fixnum width height))
  (let ((darker (remove-if-not (lambda (b) (< (paper-band-paper b) (min (paper-band-above b) (paper-band-below b))))
                               bands)))
    (when darker
      (let ((out (copy-seq grey))
            (fall (let ((slope (if (< (abs tilt) 0.0003) 0 tilt)) (fall (make-array width :element-type 'fixnum)))
                    (dotimes (x width fall) (setf (aref fall x) (round (* slope x)))))))
        (dolist (band darker out)
          (let ((times (/ (* 1/2 (+ (paper-band-above band) (paper-band-below band))) (max 1 (paper-band-paper band)))))
            (loop for r from (paper-band-top band) below (paper-band-bottom band)
                  do (dotimes (x width)
                       (let ((y (+ r (aref fall x))))
                         (when (< -1 y height)
                           (let ((i (+ (* y width) x)))
                             (setf (aref out i) (min 255 (round (* times (aref grey i))))))))))))))))

(defun scan-picture-read (pdf revision mark page)
  "The scanned picture MARK draws on PAGE of REVISION, read
\(PICTURE-WORDS) as a SCAN-PICTURE, its characters each told its picture
and page; UNREAD, with no characters, when no words could be read from it;
NIL when it cannot be read, and why not, in words. A picture with bands of
darker paper (PAPER-BANDS) is read again with them lifted to the page's
paper (BANDS-LIFTED). That reading is the one kept, its words, characters,
tilt and ink. So the words and characters of such a band, read near or
under the page's level of ink as they are, are read as the rest. They are
set beside one another and the rest and, its characters, searched for
copies on one scale. But how far the blank paper strays, which the lifting
would stretch, is kept from the picture as it is."
  (multiple-value-bind (grey width height colours) (picture-grey pdf revision (mark-object mark))
    (if (null grey)
        ;; the second value says why it was not read
        (values nil width)
        (multiple-value-bind (level skipped counts) (page-level grey width height)
          (let* ((reading (picture-words grey width height level skipped counts))
                 (words (and reading (picture-reading-words reading)))
                 (tilt (and reading (picture-reading-tilt reading)))
                 (glyphs (and reading (picture-reading-glyphs reading)))
                 (paper (and reading (picture-reading-paper reading)))
                 (rules (and reading (picture-reading-rules reading)))
                 (ink (and reading (picture-reading-ink reading)))
                 (tall-bands (and reading (picture-reading-tall-bands reading))))
            (if words
                (let* ((bands (paper-bands grey width height tilt words))
                       (lifted (bands-lifted grey width height tilt bands)))
                  ;; the words of a band of darker paper, read near or under the page's level of ink as they
                  ;; are, read with its paper lifted to the page's, and the page's words with them
                  (when lifted
                    (multiple-value-bind (level skipped counts) (page-level lifted width height)
                      (let ((again (picture-words lifted width height level skipped counts)))
                        (when (and again (picture-reading-words again))
                          (setf words (picture-reading-words again) tilt (picture-reading-tilt again)
                                glyphs (picture-reading-glyphs again) rules (picture-reading-rules again)
                                ink (picture-reading-ink again) tall-bands (picture-reading-tall-bands again))))))
                  (dolist (band bands)
                    (setf (paper-band-words band)
                          (count-if (lambda (w) (<= (paper-band-top band) (* 1/2 (+ (word-box-y0 w) (word-box-y1 w)))
                                                    (paper-band-bottom band)))
                                    (remove-if #'word-box-lone words))))
                  (dolist (g glyphs)
                    (setf (glyph-picture g) (mark-object mark) (glyph-page g) (page-index page))
                    (when colours
                      (setf (glyph-colour g) (ink-colour grey width level colours g))))
                  (let ((middle (median-of (mapcar #'glyph-ink glyphs))))
                    (make-scan-picture :page page :mark mark :width width :height height :tilt tilt
                                       :odd (unlike-words words bands) :glyphs glyphs :paper paper :rules rules
                                       :bands bands
                                       :ink ink :tall-bands tall-bands
                                       :characters (and middle (plusp middle) (round ink middle)))))
                ;; a picture of a page from which no words could be read: said so, not passed over
                (make-scan-picture :page page :mark mark :width width :height height :unread t
                                   :rules (or rules 0) :ink (or ink 0) :tall-bands (or tall-bands 0))))))))

(defun ink-colour (grey width level colours glyph)
  "The colour of the core of GLYPH's ink: the mean of the darkest quarter of
its pixels darker than LEVEL in the picture whose grey levels are GREY
\(WIDTH wide) and whose COLOURS are (OCTETS . CHANNELS), as (RED GREEN
BLUE), each 0 to 255, alike in thin and bold type as a word's ink is
\(MEASURE-WORD); NIL for none."
  (declare (type grey-pixels grey) (type fixnum width level))
  (destructuring-bind (octets . channels) colours
    (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum channels))
    (let ((ink '()))
      (loop for y from (glyph-y0 glyph) below (glyph-y1 glyph)
            do (loop for x from (glyph-x0 glyph) below (glyph-x1 glyph)
                     for at = (+ (* y width) x)
                     when (< (aref grey at) level) do (push at ink)))
      (when ink
        (let* ((darkest (sort (coerce ink 'vector) #'< :key (lambda (at) (aref grey at))))
               (n (max 1 (floor (length darkest) 4)))
               (r 0) (g 0) (b 0))
          (dotimes (k n (list (round r n) (round g n) (round b n)))
            (let ((c (* (aref darkest k) channels)))
              (incf r (aref octets c)) (incf g (aref octets (+ c 1))) (incf b (aref octets (+ c 2))))))))))

(defun glyph-ink-colour (glyph)
  "The colour of GLYPH's ink, (RED GREEN BLUE) each 0 to 255: as measured
on a picture in colours (GLYPH-COLOUR), else the grey of the core of its
word's strokes."
  (or (glyph-colour glyph)
      (and (glyph-word glyph) (let ((k (round (word-box-ink (glyph-word glyph))))) (list k k k)))))

(defun characters-kept (scan counted)
  "How many characters the scanned pictures read so far keep, COUNTED
before SCAN, once SCAN's are counted too: SCAN's characters let go when
they would take them past *SCAN-CHARACTERS-IN-ALL*. As a second value, true
when they were let go."
  (let ((now (+ counted (length (scan-picture-glyphs scan)))))
    (if (> now *scan-characters-in-all*)
        (progn (setf (scan-picture-glyphs scan) '()) (values now t))
        (values now nil))))

(defparameter *picture-drawn-by*
  '("/Width" "/Height" "/BitsPerComponent" "/ColorSpace" "/Filter" "/DecodeParms" "/Decode"
    "/ImageMask" "/Mask" "/SMask" "/Intent")
  "The keys of a picture's dictionary that, with its octets, make what it
draws: two pictures alike in these and in their octets draw the same.")

(defparameter *drawn-by-most-parts* 10000
  "The most parts (numbers, names, strings and the pairs holding them) the
entries saying how a picture is drawn may make with their references
followed; a picture whose entries make more is told apart from every other.")

(defun drawn-by (pdf revision number dict)
  "The entries of DICT, the dictionary of the picture object NUMBER of
REVISION, that say how it is drawn (*PICTURE-DRAWN-BY*), every reference
within them followed six deep at most: an object with a stream given as
\(:STREAM HASH COUNT VALUE), the hash and count of its octets as
PICTURE-OCTETS gives them, so that a palette or a mask kept as a stream is
told by its octets. Each object is followed once; past six references, or
past *DRAWN-BY-MOST-PARTS* parts, (:OBJECT NUMBER), which no other picture
shares."
  (let ((followed (make-hash-table)) (left *drawn-by-most-parts*))
    (labels ((give-up ()
               (return-from drawn-by (list (cons :object number))))
             (object-value (n depth)
               (multiple-value-bind (kept found) (gethash n followed)
                 (if found
                     (progn (when (minusp (decf left (cdr kept))) (give-up)) (car kept))
                     (let* ((before left)
                            (object (revision-object pdf revision n))
                            (value (cond ((null object) nil)
                                         ((pdf-indirect-get object :stream-start)
                                          (cons :stream (append (nth-value 1 (picture-octets pdf revision n nil))
                                                                (list (walk (pdf-indirect-get object :value)
                                                                            (1- depth))))))
                                         (t (walk (pdf-indirect-get object :value) (1- depth))))))
                       (setf (gethash n followed) (cons value (- before left)))
                       value))))
             (walk (value depth)
               (when (or (minusp depth) (minusp (decf left)))
                 (give-up))
               (cond ((pdf-ref-p value) (object-value (second value) depth))
                     ((consp value) (cons (walk (car value) depth) (walk (cdr value) depth)))
                     (t value))))
      (loop for key in *picture-drawn-by* collect (cons key (walk (pdf-get dict key) 6))))))

(defun picture-octets (pdf revision number &optional (drawn t))
  "Where the stream of the picture object NUMBER of REVISION lies in the
file, undecoded, as (START . END), and a key for what it draws, the same
for pictures that draw the same. The key is the 64-bit FNV-1a hash and the
count of its octets, and with DRAWN the entries of its dictionary that say
how they are drawn (DRAWN-BY). So the same octets with another palette, mask
or decoding are told apart. It is NIL for none."
  (let ((object (revision-object pdf revision number)))
    (when (and object (pdf-indirect-get object :stream-start))
      (let ((start (pdf-indirect-get object :stream-start)) (end (pdf-indirect-get object :stream-end)))
        (values (cons start end)
                (list* (octets-hash (pdf-octets pdf) :start start :end end) (- end start)
                       (and drawn (drawn-by pdf revision number (pdf-indirect-get object :value)))))))))

(defun scan-pictures (pdf revision)
  "The scanned pictures of the pages of REVISION (SCAN-PICTURE-P), each read
once however many pages draw it, as SCAN-PICTUREs (SCAN-PICTURE-READ). A picture
kept twice in the file, its stream the same octet for octet as one read already
and drawn alike (its size, colours, palette, decoding and masks the same,
PICTURE-OCTETS), is read with its characters left out, its TWIN that one. A
document showing one picture twice would otherwise make every character of it a
copy. One that cannot be read is left out, and given as a second value, each
\(PAGE MARK WHY), WHY in words. The characters of those past
*SCAN-CHARACTERS-IN-ALL* are not kept."
  (let ((out '()) (not-read '()) (characters 0) (kept (make-hash-table :test 'equal)) (octets (pdf-octets pdf))
        (*trace-scratch* (list nil)))
    (loop for (m page) in (pictures-of pdf revision :keep #'scan-picture-p)
          for (place key) = (multiple-value-list (picture-octets pdf revision (mark-object m)))
          do (job-check-stop)
             (let ((twin (and place
                              (cdr (find-if (lambda (seen)
                                              (not (mismatch octets
                                                             octets
                                                             :start1 (car (car seen))
                                                             :end1 (cdr (car seen))
                                                                    :start2 (car place)
                                                                    :end2 (cdr place))))
                                            (gethash key kept)))))
                   (scan (multiple-value-bind (scan why) (scan-picture-read pdf revision m page)
                           (unless scan (push (list page m why) not-read))
                           scan)))
               (when scan
                 (if twin
                     (setf (scan-picture-twin scan) twin (scan-picture-glyphs scan) '())
                     (when place (push (cons place scan) (gethash key kept)))))
               (when scan
                 (setf characters (characters-kept scan characters))
                 (push scan out))))
    (values (nreverse out) (nreverse not-read))))

(defun scan-picture-p (page mark)
  "True when MARK, a picture PAGE draws, covers a third of the page or more, as a scan of it does."
  (and (eq (mark-kind mark) :image) (mark-object mark)
       (>= (* (- (mark-x1 mark) (mark-x0 mark)) (- (mark-y1 mark) (mark-y0 mark)))
           (* 0.3
              (- (third (page-media page)) (first (page-media page)))
              (- (fourth (page-media page)) (second (page-media page)))))))

(defun unlike-words-description (ways)
  "How a word unlike the rest is, from its WAYS ((KEY OWN USUAL WORDS)...), in
words: darker ink, a lower baseline."
  (remove-duplicates (mapcar (lambda (w) (format nil (fifth (assoc (first w) *scan-measures*)) (fourth w))) ways)
                     :test #'string=))

(defparameter *scan-looks* '(:ink :edges :noise)
  "The measures of *SCAN-MEASURES* that tell how a word looks; the others
tell where it sits.")

(defun looks-and-sits-unlike-p (ways &optional paper)
  "True when the WAYS ((KEY OWN USUAL WORDS)...) a word is unlike the rest
hold both a way it looks and a way it sits. The way it looks is edges
sharper than its line's, or paper straying half as far as the page's blank
PAPER or less (BLANK-PAPER-NOISE). Then two measures apart from each other
agree, and the finding says so, still worth a look at most. Its ink alone
is not enough: a stamp, a form filled in or a line printed in another pass
has another ink and sits apart for one cause; and narrow strokes read as
softer edges and, within a word's box, as cleaner paper."
  (and (some (lambda (w) (destructuring-bind (key own usual &rest more) w
                           (declare (ignore more))
                           (case key
                             (:edges (< own usual))
                             (:noise (and paper (< own usual) (<= own (/ paper 2)))))))
             ways)
       (some (lambda (w) (not (member (first w) *scan-looks*))) ways)))
