;;;; shapes.lisp --- The characters of a scanned page set side by side by their shapes, for the examiner to look at.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; For when a scan shows no copies but something still feels off: each
;;;; scanned page's characters of one size, as palimpsest cuts them (the
;;;; same characters the search for copies sets beside each other), made
;;;; black and white, scaled to their lines and grouped by their shape,
;;;; those that match no other shape on their page last, each with its
;;;; place. No reading of the text and no finding: honest type differs too
;;;; (bold, italic, headings, figures, forms filled in, logos), and the
;;;; examiner looks and decides. Written as a copy of the revision with one
;;;; save more, its pages of shapes first, each character a link to its
;;;; place on the scanned page that follows.

(in-package #:palimpsest)

;;; A character's shape

(defparameter *shape-rows* 32
  "How many rows a character's shape is scaled to for the height of its
line's taller characters (LINE-SIZE): the capitals and tall letters of
any size of type come out this tall.")

(defparameter *shape-widest* 48
  "The most columns a character's shape keeps once scaled; a mark wider than
that (letters run together) is cut at its right.")

(defparameter *shape-unlike* 0.3
  "How unlike two characters' shapes may be in their weight and still be
grouped as one: the part of the pixels inked in either that are inked in
one only (SHAPE-DISTANCE). The same letter set bold comes over it.")

(defparameter *shape-strays* 0.025
  "How unlike two characters' shapes may be in their form and still be
grouped as one: the part of the pixels inked in either that lie more than a
pixel of the scan from any ink of the other (SHAPE-DISTANCE). Two prints of
a letter of one typeface stray at their edges by a pixel, within it; the
same letter in another typeface or in italic has serifs, bowls or strokes
elsewhere, over it.")

(defparameter *shape-candidates-most* 24
  "The most shapes one is set beside when it is grouped (SHAPE-CANDIDATES):
those nearest it in ink of the sizes it could be, so that a page of
characters all unlike costs as much a character as a page of print.")

(defstruct (shape (:copier nil) (:predicate nil))
  "A character of a scanned page as it is set beside the others: GLYPH, the
character. LINE, its line on the page counted from the top, and POSITION,
its place in that line from the left, both from 1, among the characters
palimpsest reads (WORD-GLYPHS). SERIAL, its place among the characters of
its size in reading order, from 0. ROWS, its pixels made black and white and
scaled (GLYPH-SHAPE), each row a whole number whose bits are its pixels,
the leftmost the lowest. ACROSS, how many pixels wide ROWS are. INK, how
many of them are ink. SCALE, what it was scaled by. REACH, a pixel of the
scan in the scaled pixels, one at least. GROWN, ROWS with each pixel of ink
grown by REACH every way (GROWN-ROWS). DROP, how far below its line's
baseline its foot is, in the scan's pixels. UNLIKE, how unlike the first
of its group it is (SHAPE-DISTANCE)."
  glyph (line 0) (position 0) (serial 0) rows grown (across 0) (ink 0) (scale 1) (reach 1) (drop 0) (unlike 0))

(defun glyph-shape (glyph scale)
  "GLYPH's ink made black and white and scaled by SCALE, as two values: its
rows, a vector of whole numbers whose bits are the pixels of each, the
leftmost the lowest, and how many pixels wide they are (no more than
*SHAPE-WIDEST*). A pixel is ink when it is darker than halfway from the
lightest to the darkest of GLYPH's box and a pixel round it; a scaled
pixel is ink when most of the nine points spread over it fall on ink."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph)))
         (level (car (svref (glyph-inks glyph) 1)))
         (wide (max 1 (min *shape-widest* (round (* w scale)))))
         (tall (max 1 (round (* h scale))))
         (rows (make-array tall)))
    (declare (type grey-pixels pixels) (type fixnum across w h level wide tall))
    (dotimes (j tall (values rows wide))
      (let ((row 0))
        (dotimes (i wide)
          (let ((inked 0))
            (dotimes (b 3)
              (dotimes (a 3)
                (let ((x (min (1- w) (floor (/ (+ i (/ (+ a 0.5) 3)) scale))))
                      (y (min (1- h) (floor (/ (+ j (/ (+ b 0.5) 3)) scale)))))
                  ;; within its cut (GLYPH-CUT) the box begins two pixels in
                  (when (< (aref pixels (+ (* (+ 2 y) across) 2 x)) level) (incf inked)))))
            (when (>= inked 5) (setf row (logior row (ash 1 i))))))
        (setf (svref rows j) row)))))

(defun pixel-reach (scale)
  "How many scaled pixels a pixel of the scan is, scaled by SCALE, a quarter
of one counted as one: one at least, three at most."
  (max 1 (min 3 (floor (+ scale 1/4)))))

(defun grown-rows (rows reach)
  "ROWS (GLYPH-SHAPE) with each pixel of ink grown by REACH pixels every way:
a vector longer by twice REACH, its first row REACH rows above ROWS' first."
  (let* ((tall (length rows)) (grown (make-array (+ tall (* 2 reach)) :initial-element 0)))
    (dotimes (j tall grown)
      (let* ((r (svref rows j)) (wide r))
        (loop for k from 1 to reach do (setf wide (logior wide (ash r k) (ash r (- k)))))
        (loop for k from j to (+ j (* 2 reach)) do (setf (svref grown k) (logior (svref grown k) wide)))))))

(defun rows-ink (rows)
  "How many pixels of ROWS (GLYPH-SHAPE) are ink."
  (loop for r across rows sum (logcount r)))

(defun shapes-sizes-near-p (ha wa hb wb)
  "True when shapes HA rows by WA and HB by WB are near enough in size to be
one character: their heights within a sixth of the taller (2 pixels at
least), their widths within a fifth of the wider."
  (and (<= (abs (- ha hb)) (max 2 (floor (max ha hb) 6)))
       (<= (abs (- wa wb)) (max 2 (floor (max wa wb) 5)))))

(defun shape-distance (a b)
  "How unlike the shapes A and B (SHAPEs) are, 1 or more for two that are not
grouped as one. It is the greater of two parts of the pixels inked in
either, each over the most it may be, set together the way that makes it
least, their middles together or as much as a pixel of the scan apart
across and down. The part inked in one only, over *SHAPE-UNLIKE*, tells
their weight apart. The part lying more than a pixel of the scan from any
ink of the other, over *SHAPE-STRAYS*, tells their form. It is 2 when their
sizes are too far apart for them to be one character (SHAPES-SIZES-NEAR-P),
or their ink is (the one less than seven tenths of the other: a part inked
in one only of more than 0.3)."
  (let* ((ra (shape-rows a)) (rb (shape-rows b)) (ga (shape-grown a)) (gb (shape-grown b))
         (ha (length ra)) (hb (length rb)) (wa (shape-across a)) (wb (shape-across b))
         (reach (max (shape-reach a) (shape-reach b)))
         (inks (float (+ (shape-ink a) (shape-ink b))))
         (unlike (float *shape-unlike*)) (strays (float *shape-strays*))
         (best 2.0))
    (declare (type simple-vector ra rb ga gb) (type fixnum ha hb wa wb reach)
             (type single-float unlike strays best inks) (optimize speed))
    (when (or (not (shapes-sizes-near-p ha wa hb wb))
              (< (* 10 (min (shape-ink a) (shape-ink b))) (* 7 (max (shape-ink a) (shape-ink b))))
              (/= (shape-reach a) (shape-reach b)))
      (return-from shape-distance 2.0))
    (let ((ox (floor (- wb wa) 2)) (oy (floor (- hb ha) 2)))
      (declare (type fixnum ox oy))
      (loop for dy fixnum from (- reach) to reach
            do (loop for dx fixnum from (- reach) to reach
                     do (let* ((down (+ oy dy)) (shift (+ ox dx)) (only 0) (either 0) (away 0)
                               ;; past these, this way of setting them together cannot come under the best so far
                               (most-only (floor (* best unlike inks))) (most-away (floor (* best strays inks))))
                          (declare (type fixnum down shift only either away most-only most-away))
                          ;; A's row J lies on B's row J+DOWN, its pixels SHIFT to the right
                          (flet ((placed (row) (declare (type (unsigned-byte 62) row))
                                         (if (>= shift 0) (ash row shift) row))
                                 (other (row) (declare (type (unsigned-byte 62) row))
                                        (if (>= shift 0) row (ash row (- shift)))))
                            (declare (inline placed other))
                            (loop for j fixnum from (min (- reach) (- down reach))
                                    below (max (+ hb reach) (+ ha down reach))
                                  until (or (> only most-only) (> away most-away))
                                  do (let* ((k (- j down))
                                            (p (placed (if (< -1 k ha) (svref ra k) 0)))
                                            (q (other (if (< -1 j hb) (svref rb j) 0)))
                                            (pg (placed (if (< -1 (+ k reach) (+ ha reach reach)) (svref ga
                                                                                                         (+ k reach))
                                                                                                         0)))
                                            (qg (other (if (< -1 (+ j reach) (+ hb reach reach)) (svref gb
                                                                                                        (+ j reach))
                                                                                                        0))))
                                       (declare (type (unsigned-byte 62) p q pg qg))
                                       (incf only (logcount (logxor p q)))
                                       (incf either (logcount (logior p q)))
                                       (incf away (+ (logcount (logandc2 p qg)) (logcount (logandc2 q pg)))))))
                          (when (plusp either)
                            (setf best
                                  (min best
                                       (max (/ (/ only (float either)) unlike)
                                            (/ (/ away (float either)) strays)))))))))
    best))

;;; Shapes found by their size and ink

(defun shape-key (rows across)
  "One number for a shape's size, ROWS tall and ACROSS wide."
  (+ (* rows 64) across))

(defun shape-index (shapes)
  "SHAPES kept by size, each size's from the least ink to the most (of as
much, in reading order), for SHAPE-CANDIDATES: a hash table of vectors with
fill pointers."
  (let ((index (make-hash-table)))
    (dolist (s shapes) (shape-index-add index s))
    index))

(defun shape-index-add (index s)
  "Keep the shape S in INDEX (SHAPE-INDEX), in its place by its ink and reading order."
  (let* ((key (shape-key (length (shape-rows s)) (shape-across s)))
         (kept (or (gethash key index) (setf (gethash key index) (make-array 4 :adjustable t :fill-pointer 0))))
         (low 0) (high (fill-pointer kept)))
    ;; the first kept after S: more ink, or as much and later in reading order
    (loop while (< low high)
          do (let* ((middle (ash (+ low high) -1)) (m (aref kept middle)))
               (if (or (< (shape-ink m) (shape-ink s))
                       (and (= (shape-ink m) (shape-ink s)) (< (shape-serial m) (shape-serial s))))
                   (setf low (1+ middle))
                   (setf high middle))))
    (vector-push-extend s kept)
    (replace kept kept :start1 (1+ low) :start2 low :end2 (1- (fill-pointer kept)))
    (setf (aref kept low) s)))

(defun shape-candidates (index s)
  "The shapes of INDEX (SHAPE-INDEX) that S could be grouped with, but S:
of the sizes near enough to its own (SHAPES-SIZES-NEAR-P), of ink within
seven tenths of its own either way, the *SHAPE-CANDIDATES-MOST* nearest it
in ink (of as near, the first in reading order), in reading order."
  (let* ((h (length (shape-rows s))) (w (shape-across s)) (ink (shape-ink s))
         (most *shape-candidates-most*) (found '()))
    (loop for hh from (- h (max 2 (ceiling (+ h 2) 5))) to (+ h (max 2 (ceiling (+ h 2) 5)))
          do (loop for ww from (- w (max 2 (ceiling (+ w 2) 4))) to (+ w (max 2 (ceiling (+ w 2) 4)))
                   for kept = (and (plusp hh)
                                   (plusp ww)
                                   (shapes-sizes-near-p h w hh ww)
                                   (gethash (shape-key hh ww) index))
                   when kept
                     do (let ((low 0) (high (fill-pointer kept)) (taken 0))
                          (loop while (< low high)
                                do (let ((middle (ash (+ low high) -1)))
                                     (if (< (shape-ink (aref kept middle)) ink)
                                         (setf low (1+ middle))
                                         (setf high middle))))
                          ;; outward from the first of as much ink, the nearer first
                          (let ((down (1- low)) (up low))
                            (loop while (< taken most)
                                  do (let* ((below (and (>= down 0) (aref kept down)))
                                            (above (and (< up (fill-pointer kept)) (aref kept up)))
                                            (db (and below (- ink (shape-ink below))))
                                            (da (and above (- (shape-ink above) ink))))
                                       (when (and db (< (* 10 (shape-ink below)) (* 7 ink))) (setf db nil))
                                       (when (and da (< (* 10 ink) (* 7 (shape-ink above)))) (setf da nil))
                                       (cond ((and db (or (null da) (<= db da))) (push (cons db below) found)
                                              (decf down))
                                             (da (push (cons da above) found) (incf up))
                                             (t (return)))
                                       (incf taken)))))))
    (let ((nearest (stable-sort (sort (remove s found :key #'cdr) #'< :key (lambda (f) (shape-serial (cdr f))))
                                #'<
                                :key #'car)))
      (sort (mapcar #'cdr (subseq nearest 0 (min most (length nearest)))) #'< :key #'shape-serial))))

;;; A page's characters, by size and shape

(defun line-baseline (words)
  "The baseline of a line of WORDS (WORD-BOXes) on the picture made level:
the middle of their baselines."
  (median-of (mapcar #'word-box-baseline words)))

(defun line-size (glyphs)
  "How tall the taller characters of a line of GLYPHS are: the height a
tenth of them reach or pass, the tallest of a line of fewer than ten, so
that a line of few capitals or tall letters, or a short one, is still told
by them."
  (let ((heights (sort (mapcar (lambda (g) (- (glyph-y1 g) (glyph-y0 g))) glyphs) #'>)))
    (nth (floor (length heights) 10) heights)))

(defun page-shapes (scan)
  "The characters of SCAN, a SCAN-PICTURE read (SCAN-PICTURE-READ), as
groups of one size: a list of (SIZE . SHAPES), SIZE how tall the taller
characters of their lines are in the scan's pixels (LINE-SIZE), SHAPES
in reading order, each made black and white and scaled so that SIZE comes
to *SHAPE-ROWS* rows (GLYPH-SHAPE), with its line and its place in it. A
line's size is that of its own characters; the size most characters are
of, with the lines within a fifth of it, is the first, then the same of the
lines left, and so on."
  (let* ((glyphs (scan-picture-glyphs scan))
         (tilt (or (scan-picture-tilt scan) 0))
         (slope (if (< (abs tilt) 0.0003) 0 tilt))
         (by-line (make-hash-table :test 'equal)))
    ;; a line is the words of one band of rows (PICTURE-WORDS)
    (dolist (g glyphs)
      (let ((word (glyph-word g)))
        (push g (gethash (cons (word-box-y0 word) (word-box-y1 word)) by-line))))
    (let* ((lines (sort (loop for key being the hash-keys of by-line using (hash-value members)
                              collect (list key (sort (copy-list members) #'< :key #'glyph-x0)))
                        (lambda (a b) (or (< (car (first a)) (car (first b)))
                                          (and (= (car (first a)) (car (first b)))
                                               (< (cdr (first a)) (cdr (first b))))))))
           (numbered (loop for (nil members) in lines for number from 1
                           collect (list number (line-size members) members)))
           (out '()))
      ;; the size most characters are of, and the lines within a fifth of it, then the same of those left
      (loop while numbered
            do (let* ((counts (let ((table (make-hash-table)))
                                (loop for (nil size members) in numbered
                                      do (incf (gethash size table 0) (length members)))
                                table))
                      (size (loop with best = nil
                                  for size being the hash-keys of counts using (hash-value count)
                                  when (or (null best)
                                           (> count (gethash best counts))
                                           (and (= count (gethash best counts)) (< size best)))
                                    do (setf best size)
                                  finally (return best)))
                      (scale (/ *shape-rows* (float (max 1 size))))
                      (shapes '()))
                 (flet ((member-p (line) (<= (/ size 1.2) (second line) (* size 1.2))))
                   (loop for (number nil line-glyphs) in (remove-if-not #'member-p numbered)
                         do (let ((baseline (line-baseline (remove-duplicates (mapcar #'glyph-word line-glyphs)))))
                              (loop for g in line-glyphs for position from 1
                                    do (multiple-value-bind (rows across) (glyph-shape g scale)
                                         (push (make-shape :glyph g :line number :position position :rows rows
                                                           :grown (grown-rows rows (pixel-reach scale))
                                                           :reach (pixel-reach scale)
                                                           :ink (rows-ink rows) :across across :scale scale
                                                           :drop (- (- (glyph-y1 g)
                                                                       (* slope (/ (+ (glyph-x0 g) (glyph-x1 g)) 2)))
                                                                       baseline))
                                               shapes)))))
                   (setf numbered (remove-if #'member-p numbered)))
                 (setf shapes (nreverse shapes))
                 (loop for shape in shapes for serial from 0 do (setf (shape-serial shape) serial))
                 (push (cons size shapes) out)))
      (nreverse out))))

(defun shape-groups (shapes)
  "SHAPES (of one size, in reading order, PAGE-SHAPES) grouped by their
shape, as two values: the groups, each a list of shapes, the largest first
(of as many, the first met first), its characters the least like its first
ones first; and the shapes that match no other, in reading order. Each
shape joins the group whose first shape it is nearest of those it is set
beside (SHAPE-CANDIDATES), when under 1 (SHAPE-DISTANCE), else begins one;
then each shape left alone joins the group of any shape it is set beside
and comes under 1 of. The same shapes give the same groups, and each
shape is set beside a bounded number of others, however many there are."
  (let ((groups (make-array 0 :adjustable t :fill-pointer 0))
        (heads (make-hash-table)) (group-of (make-hash-table :test 'eq)))
    ;; each group (FIRST . MEMBERS), MEMBERS backwards
    (dolist (s shapes)
      (job-check-stop)
      (let ((best nil) (nearest 1.0))
        (dolist (head (shape-candidates heads s))
          (let ((d (shape-distance s head)))
            (when (< d nearest) (setf best (gethash head group-of) nearest d))))
        (if best
            (progn (setf (shape-unlike s) nearest) (push s (cdr best)))
            (let ((group (list s s)))
              (setf (shape-unlike s) 0 (gethash s group-of) group)
              (vector-push-extend group groups)
              (shape-index-add heads s)))))
    ;; a shape alone joins the group of any shape it is like
    (let ((owner (make-hash-table :test 'eq)) (all (shape-index shapes)))
      (loop for group across groups do (dolist (s (cdr group)) (setf (gethash s owner) group)))
      (loop for group across groups
            when (null (cddr group))
              do (let ((s (car group)))
                   (job-check-stop)
                   (dolist (other (shape-candidates all s))
                     (let ((d (shape-distance s other)))
                       (when (< d 1.0)
                         (let ((into (gethash other owner)))
                           (setf (shape-unlike s) d (cdr group) '() (gethash s owner) into)
                           (push s (cdr into)))
                         (return))))))
      (let* ((kept (remove-if (lambda (group) (null (cdr group))) (coerce groups 'list)))
             (alone (remove-if-not (lambda (group) (null (cddr group))) kept))
             (together (remove-if (lambda (group) (null (cddr group))) kept)))
        (values (mapcar (lambda (group) (stable-sort (reverse (cdr group)) #'> :key #'shape-unlike))
                        (stable-sort together #'> :key (lambda (group) (length (cdr group)))))
                (sort (mapcar #'car alone) #'< :key #'shape-serial))))))

;;; The pages of shapes

(defparameter *shapes-shown-most* 32
  "The most characters of one group drawn; how many more it holds is said.")

(defparameter *shapes-groups-shown-most* 150
  "The most groups of one size of a page drawn; how many more there are, and
of how many characters, is said.")

(defparameter *shapes-alone-shown-most* 120
  "The most characters of one size of a page matching no other shape drawn
and framed on their page; how many more there are is said.")

(defparameter *shapes-page-most* 8000
  "The most characters of one scanned page set side by side: those of its
lines from the top; how many more it holds is said. Each character is set
beside a bounded number of others (*SHAPE-CANDIDATES-MOST*), so that a page
of them takes two seconds or so, however unlike they are.")

(defparameter *shape-sheet* '(595 842 40)
  "The pages of shapes: their width and height and the margin round them, in points.")

(defparameter *shape-line-points* 14
  "How tall, in points, a line's taller characters are drawn on the pages of shapes.")

(defstruct (shape-sheet (:constructor make-shape-sheet (save next)) (:copier nil) (:predicate nil))
  "The pages of shapes being laid out and written: SAVE, the COPY-SAVE they
are written into, each page's operators as soon as it is full. NEXT, the
next object number free. PAGES, those written, each (CONTENTS LINKS
HEADINGS), the latest first, being the number of its content stream, those of its
links, and its headings, each (X Y FUNCTION), FUNCTION giving the heading's
text from the number of pages of shapes. LINKS and HEADINGS, those of the
page being laid out, its links each (X0 Y0 X1 Y1 PAGE GENERATION X Y), a
box of it leading to the place X Y of the scanned page PAGE. OUT, where its
operators are being written. Y, how far down it has been filled, from its
foot."
  save next (pages '()) (links '()) (headings '()) (out (make-string-output-stream)) (y 0))

(defun sheet-number (sheet)
  "A new object number for SHEET's copy."
  (prog1 (shape-sheet-next sheet) (incf (shape-sheet-next sheet))))

(defun sheet-new-page (sheet)
  "Write SHEET's page, when it has one begun, its links with it, and begin another."
  (let ((text (get-output-stream-string (shape-sheet-out sheet))) (save (shape-sheet-save sheet)))
    (when (or (plusp (length text)) (shape-sheet-headings sheet))
      (let ((contents (sheet-number sheet)))
        (save-stream save contents text)
        (push (list contents
                    (loop for (x0 y0 x1 y1 to generation x y) in (reverse (shape-sheet-links sheet))
                          collect (let ((link (sheet-number sheet)))
                                    (save-object save
                                                 link
                                                 (pdf-value-text (list (cons "/Type" "/Annot") (cons "/Subtype" "/Link")
                                                                       (cons "/Rect" (list x0 y0 x1 y1))
                                                                       (cons "/Border" (list 0 0 0))
                                                                       (cons "/Dest"
                                                                             (list (list :ref to generation)
                                                                                   "/XYZ"
                                                                                   x
                                                                                   y
                                                                                   "null")))))
                                    link))
                    (reverse (shape-sheet-headings sheet)))
              (shape-sheet-pages sheet)))))
  (setf (shape-sheet-links sheet) '() (shape-sheet-headings sheet) '()
        (shape-sheet-y sheet) (- (second *shape-sheet*) (third *shape-sheet*))))

(defun sheet-room (sheet height)
  "Make room for HEIGHT points more on SHEET's page, beginning another when it has not got them."
  (when (< (- (shape-sheet-y sheet) height) (third *shape-sheet*))
    (sheet-new-page sheet)))

(defun sheet-text (sheet x y size text &key (grey 0))
  "Write TEXT on SHEET's page at X Y, SIZE points tall, in GREY (0 black)."
  (format (shape-sheet-out sheet) "BT ~,2f g /PalimpsestLabel ~,1f Tf ~,2f ~,2f Td ~a Tj ET~%"
          grey size x y (pdf-literal-text text)))

(defun text-width (text size)
  "About how wide TEXT is set in Helvetica SIZE points tall, a little more rather than less."
  (* 0.55 size (length text)))

(defun wrapped-lines (text size width)
  "TEXT broken into lines of words no wider than WIDTH points, set SIZE points tall."
  (let ((lines '()) (line nil))
    (dolist (word (uiop:split-string text :separator " ") (nreverse (if line (cons line lines) lines)))
      (cond ((null line) (setf line word))
            ((<= (text-width (format nil "~a ~a" line word) size) width) (setf line (format nil "~a ~a" line word)))
            (t (push line lines) (setf line word))))))

(defun sheet-paragraph (sheet text size &key (grey 0) (after 0))
  "Write TEXT on SHEET as a paragraph of lines SIZE points tall in GREY,
across the page between its margins, leaving AFTER points below it."
  (destructuring-bind (width height margin) *shape-sheet*
    (declare (ignore height))
    (dolist (line (wrapped-lines text size (- width (* 2 margin))))
      (sheet-room sheet (* 1.3 size))
      (decf (shape-sheet-y sheet) (* 1.3 size))
      (sheet-text sheet margin (shape-sheet-y sheet) size line :grey grey))
    (decf (shape-sheet-y sheet) after)))

(defun write-glyph-picture (glyph x y width height out)
  "Write to OUT the operators drawing GLYPH's grey pixels, its box and a
pixel round it, as a picture of its own in the page's content (sixteen
greys, written in hex), filling the box X Y WIDTH HEIGHT."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (w (+ 2 (- (glyph-x1 glyph) (glyph-x0 glyph)))) (h (+ 2 (- (glyph-y1 glyph) (glyph-y0 glyph)))))
    (format out "q ~,3f 0 0 ~,3f ~,2f ~,2f cm BI /W ~d /H ~d /BPC 4 /CS /G /F /AHx ID~%" width height x y w h)
    (dotimes (j h)
      (dotimes (i w)
        ;; the box and a pixel round it begin a pixel into the cut (GLYPH-CUT)
        (write-char (char "0123456789ABCDEF" (ash (aref pixels (+ (* (+ 1 j) across) 1 i)) -4)) out))
      (when (oddp w) (write-char #\0 out))
      (terpri out))
    (format out ">~%EI Q~%")))

(defun shape-size (shape)
  "How wide and how tall SHAPE's character is drawn on a page of shapes, in
points, its box and a pixel round it, as two values."
  (let* ((g (shape-glyph shape))
         (points (* (shape-scale shape) (/ *shape-line-points* *shape-rows*))))
    (values (* points (+ 2 (- (glyph-x1 g) (glyph-x0 g)))) (* points (+ 2 (- (glyph-y1 g) (glyph-y0 g)))))))

(defun shape-label (shape)
  "SHAPE's line and its place in it, as its label says them: 12:4."
  (format nil "~d:~d" (shape-line shape) (shape-position shape)))

(defun shape-rows-laid (shapes labelled more)
  "SHAPES laid out in rows across a page of shapes, as two values: the rows,
each a list of (SHAPE . X); and how many of SHAPES are left out, with MORE
of them not given, so that the words saying so (and N more) end the last
row rather than take one of their own. With LABELLED, each is as wide as
its label too."
  (destructuring-bind (width height margin) *shape-sheet*
    (declare (ignore height))
    (let* ((left (+ margin 36)) (right (- width margin)) (rows '()) (row '()) (x left))
      (flet ((cell (s) (max (shape-size s) (if labelled (text-width (shape-label s) 5) 0))))
        (dolist (s shapes)
          (let ((cell (cell s)))
            (when (and row (> (+ x cell) right))
              (push (nreverse row) rows) (setf row '() x left))
            (push (cons s x) row)
            (incf x (+ cell (if labelled 5 3)))))
        ;; room for the words saying how many are not drawn: the last row's last ones give way
        (let ((left-out 0))
          (loop while (and (plusp (+ more left-out)) (rest row)
                           (> (+ x (text-width (format nil "and ~:d more" (+ more left-out 1)) 6)) right))
                do (let ((last (pop row)))
                     (incf left-out)
                     (setf x (cdr last))))
          (when row (push (nreverse row) rows))
          (values (nreverse rows) left-out))))))

(defun sheet-shapes (sheet shapes place &key labelled (more 0) count)
  "Draw SHAPES on SHEET side by side, standing on their baselines, on as
many rows as they take, each a link to its place: PLACE, a function of a
shape giving its scanned page, that page's generation and the place there
\(PAGE GENERATION X Y). With LABELLED, its line and place in it under each
\(12:4). MORE, how many of the group are not drawn, said after them; COUNT,
how many it holds, said at the left of its first row. Returns the shapes
drawn."
  (multiple-value-bind (rows left-out) (shape-rows-laid shapes labelled more)
    (let* ((points (/ *shape-line-points* *shape-rows*))
           (above (* 1.3 *shape-line-points*)) (below (if labelled 12 7))
           (left (+ (third *shape-sheet*) 36)) (drawn '()) (baseline 0) (x left))
      (loop for row in rows for first = t then nil
            do (sheet-room sheet (+ above below))
               (setf baseline (- (shape-sheet-y sheet) above))
               (decf (shape-sheet-y sheet) (+ above below))
               (when (and first count)
                 (let ((text (format nil "~:d" count)))
                   (sheet-text sheet (- (- left 6) (text-width text 6)) baseline 6 text :grey *shape-note-grey*)))
               (loop for (s . at) in row
                     do (multiple-value-bind (w h) (shape-size s)
                          (let ((foot (- baseline (* points (shape-scale s) (+ 1 (shape-drop s))))))
                            (write-glyph-picture (shape-glyph s) at foot w h (shape-sheet-out sheet))
                            (let ((to (funcall place s)))
                              (when to (push (list* at foot (+ at w) (+ foot h) to) (shape-sheet-links sheet))))
                            (when labelled
                              (sheet-text sheet at (- baseline 9) 5 (shape-label s) :grey *shape-note-grey*))
                            (push s drawn)
                            (setf x (+ at (max w (if labelled (text-width (shape-label s) 5) 0)) (if labelled 5 3)))))))
      (when (plusp (+ more left-out))
        (sheet-text sheet x baseline 6 (format nil "and ~:d more" (+ more left-out)) :grey *shape-note-grey*))
      (nreverse drawn))))

;;; The copy with its pages of shapes

(defun page-tree-refusal (pdf revision)
  "Why no pages can be set before REVISION's own, in words; NIL when they can:
its catalogue or the top of its page tree cannot be read, or the
catalogue's /Pages names a page, not a tree of pages."
  (let* ((root (pdf-get (revision-trailer revision) "/Root"))
         (catalogue (catalogue pdf revision))
         (tree (and catalogue (pdf-get catalogue "/Pages")))
         (top (and (pdf-ref-p tree) (deref pdf revision tree))))
    (cond ((not (and (pdf-ref-p root) (pdf-dictionary-p top)))
           "its page tree cannot be read: no pages could be set before its own")
          ((or (name-p (pdf-get top "/Type") "/Page") (not (listp (pdf-get top "/Kids"))))
           "its catalogue's /Pages names a page, not a tree of pages: no pages could be set before it")
          (t
           (multiple-value-bind (catalogue-text twice) (object-text-with pdf revision (second root) '(("/Pages" . "")))
             (multiple-value-bind (top-text top-twice) (object-text-with pdf revision (second tree) '(("/Parent" . "")))
               (cond ((or twice top-twice)
                      (format nil "its ~:[top of the page tree~;catalogue~] names ~a twice, as no honest program ~
                                   writes it: which a reader takes cannot be told, and no pages are set before its own"
                              twice (or twice top-twice)))
                     ((not (and catalogue-text top-text))
                      (format nil "its catalogue or page tree cannot be read as written: no pages could be set before ~
                                   its own")))))))))

(defun page-heading-words (scan cut)
  "What the heading of a scanned page says of SCAN, its picture read, after
its number; NIL when it has characters to set side by side. CUT, true when
its characters were let go for those of the file's pages before it
\(CHARACTERS-KEPT)."
  (cond ((null scan) "its picture could not be read")
        ((scan-picture-unread scan) "no lines of characters could be read from it")
        (cut (format nil "not set side by side: the scanned pages before it hold ~:d characters, as many as are read ~
                          in all (*scan-characters-in-all*)"
                     *scan-characters-in-all*))
        ((and (null (scan-picture-glyphs scan))
              (scan-picture-characters scan)
              (> (scan-picture-characters scan) *scan-characters-most*))
         (format nil "not set side by side: it holds more than ~:d characters, as many as are read on a page ~
                      (*scan-characters-most*)"
                 *scan-characters-most*))
        ((null (scan-picture-glyphs scan)) "no characters could be read from it")))

(defun shapes-introduction (sheet analysis revision)
  "Set on SHEET the title of the pages of shapes of REVISION of ANALYSIS's PDF
and the words saying what they are and are not."
  (sheet-paragraph sheet
                   (format nil "The shapes of the characters of ~a, ~a" (analysis-name analysis)
                           (revision-name revision))
                   12 :after 4)
  (sheet-paragraph sheet
                   (format nil "No finding and no judgement: palimpsest claims nothing here, and reads no text. ~
                                Each scanned page's characters of one size are set side by side, grouped by their ~
                                shape made black and white and scaled to their lines; each group's least like its ~
                                first come first; those that match no other shape on their page come last, with ~
                                their line and place in it under each (12:4: line 12, the 4th character, as ~
                                palimpsest reads them; dots and commas are too small to be read). Honest type ~
                                differs too: bold, italic, headings, figures, forms filled in, logos, letters run ~
                                together. Look, and decide. Each character leads to its place on its scanned page, ~
                                which follow these pages, those that match nothing framed there.")
                   8 :grey *shape-text-grey* :after 6))

(defun shapes-of-size (sheet size place number framer)
  "Set on SHEET the characters of SIZE, (TALLEST . CHARACTERS) as PAGE-SHAPES
gives it, of the scanned page NUMBER: a line counting them, their groups
of one shape, and those matching no other, labelled. PLACE gives a shape's
place on its page (SHEET-SHAPES). FRAMER, a function of a shape, frames on
the scanned page each of those matching no other that is drawn."
  (multiple-value-bind (groups alone) (shape-groups (cdr size))
    (sheet-room sheet 40)
    (sheet-paragraph sheet (format nil "~:d character~:p of one size (their lines' taller characters ~d pixels ~
                                        tall): ~:d shape~:p more than one shares, ~:d character~:p matching no other"
                                   (length (cdr size)) (car size) (length groups) (length alone))
                     8 :after 2)
    (dolist (group (subseq groups 0 (min (length groups) *shapes-groups-shown-most*)))
      (job-check-stop)
      (sheet-shapes sheet (subseq group 0 (min (length group) *shapes-shown-most*)) place
                    :more (max 0 (- (length group) *shapes-shown-most*))
                    :count (length group)))
    (when (> (length groups) *shapes-groups-shown-most*)
      (let ((rest (nthcdr *shapes-groups-shown-most* groups)))
        (sheet-paragraph sheet (format nil "and ~:d more shape~:p, of ~:d character~:p, not drawn ~
                                            (*shapes-groups-shown-most*)"
                                       (length rest) (reduce #'+ rest :key #'length))
                         6 :grey *shape-note-grey* :after 2)))
    (when alone
      (sheet-room sheet 40)
      (sheet-paragraph sheet (format nil "Matching no other shape of this size on page ~d: ~:d~@[, ~:d of them drawn ~
                                          and framed (*shapes-alone-shown-most*)~]"
                                     number (length alone)
                                     (and (> (length alone) *shapes-alone-shown-most*) *shapes-alone-shown-most*))
                       8 :after 2)
      (mapc framer (sheet-shapes sheet (subseq alone 0 (min (length alone) *shapes-alone-shown-most*)) place
                                 :labelled t
                                 :more (max 0 (- (length alone) *shapes-alone-shown-most*)))))))

(defun shapes-of-page (sheet scan mark page cut overlays)
  "Set on SHEET the heading of the scanned PAGE, whose picture MARK draws, and
when SCAN, its picture read, has characters to set, its characters of each
size (SHAPES-OF-SIZE). CUT, true when its characters were let go for those
of the pages before it. Those matching no other shape are framed on the
page in a drawing kept in OVERLAYS, a table by page of (FRAMER . STREAM)."
  (let* ((number (1+ (page-index page)))
         (words (page-heading-words scan cut))
         (held (and scan (length (scan-picture-glyphs scan))))
         (margin (third *shape-sheet*)))
    ;; a page of more characters than are set side by side: those of its lines from the top
    (when (and (not words) (> held *shapes-page-most*))
      (setf (scan-picture-glyphs scan)
            (subseq (stable-sort (copy-list (scan-picture-glyphs scan)) #'<
                                 :key (lambda (g) (word-box-y0 (glyph-word g))))
                    0 *shapes-page-most*)))
    (sheet-room sheet 60)
    (decf (shape-sheet-y sheet) 14)
    ;; its page in this copy is known once the pages of shapes are counted
    (push (list margin (shape-sheet-y sheet)
                (lambda (shape-pages)
                  (format nil "Page ~d (page ~d of this copy)~@[: ~a~]~@[: the first ~:d of its ~:d characters, from ~
                               the top (*shapes-page-most*)~]"
                          number (+ shape-pages number) words
                          (and (not words) (> held *shapes-page-most*) *shapes-page-most*) held)))
          (shape-sheet-headings sheet))
    (decf (shape-sheet-y sheet) 4)
    (unless words
      (let ((width-px (scan-picture-width scan)) (height-px (scan-picture-height scan)))
        (flet ((place (s)
                 (and (page-number page)
                      (destructuring-bind (x0 y0 x1 y1) (picture-page-box mark (shape-glyph s) width-px height-px)
                        (declare (ignore y0 x1))
                        (list (page-number page) (page-generation page) x0 y1))))
               (frame (s)
                 (let ((framed (or (gethash page overlays)
                                   (setf (gethash page overlays)
                                         (let ((out (make-string-output-stream)))
                                           (cons (page-framer page out :label-size 4) out))))))
                   (destructuring-bind (x0 y0 x1 y1) (picture-page-box mark (shape-glyph s) width-px height-px)
                     (funcall (car framed) (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5) (shape-label s)
                              *shape-frame-colour*)))))
          (dolist (size (page-shapes scan))
            (shapes-of-size sheet size #'place number #'frame)
            (decf (shape-sheet-y sheet) 6)))))))

(defun write-shape-pages (save sheet font parent)
  "Write into SAVE the pages SHEET laid, their headings now that their count
is known, each set in the font object FONT under the page tree node
PARENT. Returns references to them, in order."
  (let* ((sheets (reverse (shape-sheet-pages sheet))) (count (length sheets)))
    (loop for (contents links headings) in sheets
          collect (let ((heading-streams
                          (loop for (x y words) in headings
                                collect (let ((n (sheet-number sheet)))
                                          (save-stream save n (format nil "BT 0 g /PalimpsestLabel 10 Tf ~,2f ~,2f ~
                                                                           Td ~a Tj ET~%"
                                                                      x y (pdf-literal-text (funcall words count))))
                                          n)))
                        (page-object (sheet-number sheet)))
                    (save-object save page-object
                                 (pdf-value-text
                                  (list (cons "/Type" "/Page") (cons "/Parent" (list :ref parent 0))
                                        (cons "/MediaBox" (list 0 0 (first *shape-sheet*) (second *shape-sheet*)))
                                        (cons "/Resources" (list (cons "/Font" (list (cons "/PalimpsestLabel"
                                                                                           (list :ref font 0))))))
                                        (cons "/Contents" (mapcar (lambda (n) (list :ref n 0))
                                                                  (cons contents heading-streams)))
                                        (cons "/Annots" (mapcar (lambda (n) (list :ref n 0)) links)))))
                    (list :ref page-object 0)))))

(defun write-shape-frames (save sheet pdf revision overlays font open-state close-state)
  "Write into SAVE the drawings of OVERLAYS (SHAPES-OF-PAGE) over their
scanned pages of REVISION, each page written again with its drawing over
its own, kept apart by the streams OPEN-STATE and CLOSE-STATE, its labels
in the font object FONT."
  (when (plusp (hash-table-count overlays))
    (save-stream save open-state "q")
    (save-stream save close-state "Q")
    (dolist (page (pages-of pdf revision))
      (let ((framed (gethash page overlays)))
        (when (and framed (page-number page))
          (let ((overlay (sheet-number sheet)))
            (save-stream save overlay (get-output-stream-string (cdr framed)))
            (save-object save (page-number page)
                         (overlaid-page-text pdf revision page font open-state close-state overlay)
                         :generation (page-generation page))))))))

(defun shapes-copy (analysis revision target &key (cancel (list nil)) (progress (constantly nil)))
  "Write to TARGET the revision REVISION of ANALYSIS's PDF with one save more
setting pages of shapes before its own. For each scanned page, its
characters of one size are set side by side (PAGE-SHAPES), grouped by shape
\(SHAPE-GROUPS), each a link to its place, those that match no other last,
each with its line and place in it. On the scanned page, those are framed
and labelled so. What is not drawn, past *SHAPES-SHOWN-MOST*,
*SHAPES-GROUPS-SHOWN-MOST* and *SHAPES-ALONE-SHOWN-MOST*, or not read, past
*SCAN-CHARACTERS-MOST* and *SCAN-CHARACTERS-IN-ALL*, is said. PROGRESS, a
function of the part done from 0 to 1. Returns TARGET; :NONE when REVISION
has no scanned page; NIL when stopped by CANCEL, nothing written; an error
saying why when no copy can be made."
  (let ((pdf (analysis-pdf analysis)))
    (let ((refusal (or (copy-refusal revision) (page-tree-refusal pdf revision))))
      (when refusal (error refusal)))
    (let ((pictures (pictures-of pdf revision :keep #'scan-picture-p)))
      (when (null pictures) (return-from shapes-copy :none))
      (let* ((root (pdf-get (revision-trailer revision) "/Root"))
             (tree (pdf-get (catalogue pdf revision) "/Pages"))
             (next (first-free-number pdf))
             (save (make-copy-save pdf revision))
             (font next) (open-state (+ next 1)) (close-state (+ next 2)) (info (+ next 3)) (new-top (+ next 4))
             (sheet (make-shape-sheet save (+ next 5))) (overlays (make-hash-table :test 'eq)) (characters 0))
        (save-object save font *label-font*)
        (save-object save info (copy-information analysis revision
                                                 "its scanned pages' characters set side by side by their shapes"))
        (sheet-new-page sheet)
        (shapes-introduction sheet analysis revision)
        (loop for (mark page) in pictures for k from 1
              do (when (car cancel) (return-from shapes-copy nil))
                 (job-check-stop)
                 (funcall progress (/ (1- k) (length pictures)))
                 (let* ((scan (scan-picture-read pdf revision mark page))
                        (cut (and scan (multiple-value-bind (now cut) (characters-kept scan characters)
                                         (setf characters now)
                                         cut))))
                   (shapes-of-page sheet scan mark page cut overlays)))
        (sheet-new-page sheet)
        (funcall progress 1)
        (when (car cancel) (return-from shapes-copy nil))
        ;; the pages of shapes before the document's own, under a new top of the page tree; its catalogue and the
        ;; old top written again as they were, but for where they point
        (let ((kids (write-shape-pages save sheet font new-top))
              (reference (format nil "~d 0 R" new-top)))
          (save-object save new-top (pdf-value-text (list (cons "/Type" "/Pages")
                                                          (cons "/Kids" (append kids (list tree)))
                                                          (cons "/Count" (+ (length kids)
                                                                            (length (pages-of pdf revision)))))))
          (save-object save (second tree)
                       (object-text-with pdf revision (second tree) (list (cons "/Parent" reference)))
                       :generation (or (third tree) 0))
          (save-object save (second root) (object-text-with pdf revision (second root) (list (cons "/Pages" reference)))
                       :generation (or (third root) 0)))
        (write-shape-frames save sheet pdf revision overlays font open-state close-state)
        (finish-copy-save save revision target (shape-sheet-next sheet) info)))))

;;; The commands

(define-command palimpsest-show-shapes ()
  "Show beside the report, in a window of its own that \\`q' there closes, the
shapes of the characters of the PDF's scanned pages. Each page's characters
of one size are side by side, grouped by their shape (made black and white
and scaled to their lines), the least like their group's first first, and
last those that match no other shape on their page, each with its line and
its place in it. For when a scan shows no copies but something still feels
off. No finding and no judgement: palimpsest reads no text and claims
nothing of them; honest type differs too (bold, italic, headings, figures,
forms filled in, logos), and you look and decide. Each character leads to
its place on its scanned page, which follow the pages of shapes, those
matching nothing framed there. The revision whose part of the report point
is in, else the latest: that revision with one save more, made in the
background and written to the report's own directory for you alone, never
beside the PDF examined; the PDF itself is never changed. Shown already,
\\<palimpsest-mode>\\[palimpsest-show-shapes] closes it again; the report
stays selected throughout."
  (show-copy t))

(define-command palimpsest-write-shapes ()
  "Write the shapes of the characters of the PDF's scanned pages
\(\\<palimpsest-mode>\\[palimpsest-show-shapes]) to a PDF of its own, to keep or show to others: of the
revision whose part of the report point is in, else the latest. Asked
where, offered in the directory last saved to, else in the home directory; never
beside the PDF examined nor in the package's own directory, nothing written
over. The shapes are set side by side afresh, the PDF examined never
changed."
  (write-copy t))
