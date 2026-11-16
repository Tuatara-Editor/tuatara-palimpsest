;;;; pieces.lisp --- Pieces of a scanned page saved as JPEGs on their own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A JPEG keeps a picture as blocks of 8 by 8 pixels. A piece laid into a scan
;;;; after it was saved, or saved apart before, carries blocks of its own or none
;;;; where the page carries its grid: such pieces are found and named.

(in-package #:palimpsest)

;;; Pieces saved as JPEGs on their own

(defparameter *block-basis*
  (let ((basis (make-array 64 :element-type 'double-float)))
    (dotimes (u 8 basis)
      (dotimes (x 8)
        (setf (aref basis (+ (* u 8) x))
              (* (if (zerop u) (sqrt 0.125d0) 0.5d0) (cos (/ (* (+ (* 2 x) 1) u pi) 16)))))))
  "The cosines a JPEG turns a row of eight pixels into its frequencies by:
the Uth frequency's weight for the Xth pixel at U * 8 + X.")

(defparameter *block-flat-within* 0.75d0
  "How near nothing, in grey levels, a frequency of a block must be to be
flat (GLYPH-BLOCK-SCORES): a JPEG sets it to nothing, and its pixels, made
whole grey levels again, move it a quarter of a level or so.")

(defun glyph-block-scores (glyph &optional (rows (make-array 0 :element-type 'double-float)))
  "For each way an 8-by-8 block can sit on the picture GLYPH is in, how flat the
highest frequencies are of the most JPEG-like block of GLYPH's cut (GLYPH-CUT)
sitting that way. Of the 28 frequencies a JPEG of quality 75 or less sets to
nothing (across and down adding up to 8 or more), it counts how many are within
*BLOCK-FLAT-WITHIN* of nothing. A block of which fewer than two of the four
highest are is counted as none, the rest not worked out. As two values: a vector
of 64, the block's left edge's column within every eighth plus eight times its
top row's, -1 for a way no block of the cut sits but those passed over; and
ROWS, room for the work, given again for the next character so that it is made
once. A block more than an eighth of whose pixels are black or white, the paper
or ink clipped, is passed over: it is flat however it was saved. Worked out
once for a character and kept with it (GLYPH-BLOCKS)."
  (when (glyph-blocks glyph)
    (return-from glyph-block-scores (values (glyph-blocks glyph) rows)))
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)) (down (floor (length pixels) across))
         (starts (- across 7)) (basis *block-basis*) (within (float *block-flat-within* 1d0))
         (scores (make-array 64 :element-type '(signed-byte 8) :initial-element -1)))
    (declare (type grey-pixels pixels) (type (integer 1 100000) across) (type (integer 0 100000) down)
             (type fixnum starts)
             (type (simple-array double-float (64)) basis)
             (type double-float within)
             (optimize speed))
    (when (and (plusp starts) (>= down 8))
      (when (< (length rows) (* down starts 8))
        (setf rows (make-array (* down starts 8 2) :element-type 'double-float)))
      (let ((rows rows) (left (mod (- (the fixnum (glyph-x0 glyph)) 2) 8))
            (top (mod (- (the fixnum (glyph-y0 glyph)) 2) 8))
            (stride (* starts 8)))
        (declare (type (simple-array double-float (*)) rows) (type (integer 0 7) left top) (type fixnum stride))
        (flet ((row (j i u)
                 ;; the Uth frequency of the eight pixels of row J from column I, kept at its place in ROWS
                 (let ((at (+ (* j across) i)) (k (* u 8)) (sum 0d0))
                   (declare (type fixnum at k) (type double-float sum))
                   (dotimes (x 8) (incf sum (* (aref basis (+ k x)) (aref pixels (+ at x)))))
                   (setf (aref rows (+ (* j stride) (* i 8) u)) sum)))
               (flat-p (first u v)
                 ;; true when the frequency U across and V down of the block whose first row's work is at FIRST is
                 ;; nothing
                 (let ((k (* v 8)) (sum 0d0))
                   (declare (type fixnum k) (type double-float sum))
                   (dotimes (y 8) (incf sum (* (aref basis (+ k y)) (aref rows (+ first (* y stride) u)))))
                   (< (abs sum) within))))
          (declare (inline row flat-p))
          ;; each row's frequencies 6 and 7, which the four highest of each block are made of
          (dotimes (j down)
            (dotimes (i starts) (row j i 6) (row j i 7)))
          (dotimes (j0 (- down 7))
            (dotimes (i0 starts)
              (let* ((first (+ (* j0 stride) (* i0 8)))
                     (flat (+ (if (flat-p first 7 7) 1 0)
                              (if (flat-p first 6 7) 1 0)
                              (if (flat-p first 7 6) 1 0)
                              (if (flat-p first 6 6) 1 0)))
                     (way (+ (mod (+ left i0) 8) (* 8 (mod (+ top j0) 8)))))
                (declare (type fixnum first flat) (type (integer 0 63) way))
                (when (<= (loop for y below 8
                                sum (let ((at (+ (* (+ j0 y) across) i0)))
                                      (declare (type fixnum at))
                                      (loop for x below 8
                                            count (let ((g (aref pixels (+ at x)))) (or (= g 0) (= g 255)))))
                                  of-type fixnum)
                          8)
                  (if (< flat 2)
                      (setf flat 0)
                      (progn
                        (dotimes (y 8) (loop for u from 1 to 5 do (row (+ j0 y) i0 u)))
                        (loop for u of-type (integer 1 8) from 1 below 8
                              do (loop for v of-type (integer 0 8) from (- 8 u) below 8
                                       unless (and (>= u 6) (>= v 6))
                                         do (when (flat-p first u v) (incf flat))))))
                  (setf (aref scores way) (max (aref scores way) flat)))))))))
    (values (setf (glyph-blocks glyph) scores) rows)))

(defparameter *block-grid-least* 20
  "How many of a block's 28 finest frequencies must typically be flat
\(GLYPH-BLOCK-SCORES), in the blocks of a scanned picture's characters that
sit one way, for the picture to carry a JPEG's grid there, as one of
quality 95 or less does.")

(defparameter *block-smooth-least* 14
  "How many of a block's 28 finest frequencies the blocks of a scanned
picture's characters may typically have flat, at most, for the picture to
be grainy enough for a piece saved as a JPEG of its own to stand out:
half.")

(defparameter *block-grid-above* 8
  "How many more of the 28 finest frequencies the characters' blocks must
typically have flat at a picture's grid than at the placements typically
do, for the picture to carry JPEG blocks on that grid, not to be smooth
throughout.")

(defparameter *block-piece-least* 20
  "How many of its block's 28 finest frequencies a character must have flat
at a placement to carry a JPEG block there.")

(defparameter *block-piece-above* 12
  "How many more of the 28 finest frequencies a character must have flat at
a placement than it typically has at the placements apart from it, for its
flatness to be a JPEG's blocks there and not a smooth stretch of the scan,
flat however a block sits on it.")

(defparameter *block-off-grid-most* 8
  "How many of the 28 finest frequencies of its blocks on the picture's JPEG
grid a character may have flat, at most, to lack the grid the rest of the
picture carries.")

(defstruct (block-piece (:copier nil) (:predicate nil))
  "A region of a scanned picture whose JPEG traces are unlike the rest of the
picture's, with PAGE and PICTURE, the page index and picture object it is on.
X0 Y0 X1 Y1 are its pixels in the picture (the last excluded), from the top
left. KIND is :OWN when it carries JPEG blocks of its own and the picture
none, and :OFF-GRID when it lacks the blocks the picture carries on its grid.
STRENGTH is :STRONG or :LOOK (JPEG-PIECES). FLAT is how many of the 28 finest
frequencies are flat, of its flattest block on its own grid (:OWN) or of its
blocks on the picture's grid (:OFF-GRID). ABOVE is, for :OWN, how many more
than at its other placements. USUAL is the same of the picture's characters
typically. WAY is where its blocks sit (:OWN) or the picture's do
\(:OFF-GRID), as (COLUMN ROW) within every eighth. CHARACTERS is how many
characters it holds. SHOWING is how many of them carry blocks there. LAID is
true when it lies at the same place on two pages or more, or is taller by
half than the taller type of its page, as a stamp, a logo or a signature laid
on the pages by software is (LAID-PIECES). BACKING is, for an :OWN region,
what else on it agrees (BACKED-PIECES). :COPIES means characters within it
are copies of others the same pixel for pixel. :WORDS means a word within it
is unlike the rest both in how it looks and where it sits. :ALONE means
nothing agrees, and it is worth a look though its blocks alone would make it
strong."
  page picture x0 y0 x1 y1 kind strength flat (above 0) usual way (characters 1) (showing 1) laid backing)

(defun counted-middle (counts)
  "The middle of the values COUNTS counts, a vector whose Kth holds how many
are K, as MEDIAN-OF finds it; NIL for none."
  (let ((n (reduce #'+ counts)))
    (and (plusp n) (counted-level counts (1+ (floor n 2))))))

(defun near-ways-p (a b)
  "True when the block placements A and B (0 to 63, GLYPH-BLOCK-SCORES) are
the same or a pixel apart, across, down or both, within every eighth."
  (flet ((apart (p q) (let ((d (abs (- p q)))) (min d (- 8 d)))))
    (and (<= (apart (mod a 8) (mod b 8)) 1) (<= (apart (floor a 8) (floor b 8)) 1))))

(defun block-support (scores way)
  "How far the character whose GLYPH-BLOCK-SCORES are SCORES stands out at
the placement WAY: how many more of the 28 finest frequencies are flat
there than typically at the placements neither WAY nor a pixel from it;
NIL when it has fewer than *BLOCK-PIECE-LEAST* flat there, or no other
placement to set it beside."
  (let ((here (aref scores way)))
    (when (>= here *block-piece-least*)
      (let ((counts (make-array 29 :initial-element 0)))
        (dotimes (w 64)
          (let ((v (aref scores w))) (when (and (>= v 0) (not (near-ways-p w way))) (incf (aref counts v)))))
        (let ((typical (counted-middle counts)))
          (and typical (- here typical)))))))

(defun supported-ways (scores)
  "The block placements at which the character whose GLYPH-BLOCK-SCORES are
SCORES carries a JPEG block: *BLOCK-PIECE-LEAST* or more of the 28 finest
frequencies flat, and *BLOCK-PIECE-ABOVE* more than at its other
placements (BLOCK-SUPPORT)."
  (loop for w below 64
        when (let ((above (block-support scores w))) (and above (>= above *block-piece-above*)))
          collect w))

(defun picture-block-grid (scores &optional way)
  "How the blocks of a scanned picture's characters sit, from SCORES, their
GLYPH-BLOCK-SCORES, as four values. The first is the placement (0 to 63)
whose blocks are typically flattest, or WAY when given (a JPEG's own, 0).
The second is how many of its 28 finest frequencies typically are. The
third is whether the picture carries a JPEG grid there (typically
*BLOCK-GRID-LEAST* flat or more, *BLOCK-GRID-ABOVE* more than the
placements typically are). The fourth is how far the characters' blocks
there typically stray from that. Only placements at which a quarter of the
characters or more have a block are set beside one another; typically -1
when there are none."
  (let ((counts (coerce (loop repeat 64 collect (make-array 29 :initial-element 0)) 'vector)))
    (dolist (s scores)
      (dotimes (w 64)
        (let ((v (aref s w))) (when (>= v 0) (incf (aref (svref counts w) v))))))
    (let* ((enough (* 1/4 (length scores)))
           ;; a placement only a few characters' cuts hold a block at is not set beside the rest
           (typicals (loop for w below 64
                           collect (and (>= (reduce #'+ (svref counts w)) enough) (counted-middle (svref counts w)))))
           (best (or way
                     (let ((top (reduce #'max (substitute -1 nil typicals))))
                       (and (>= top 0) (position top typicals)))))
           (typical (or (and best (nth best typicals)) -1))
           (middle (median-of (or (remove nil typicals) '(0))))
           (spread (if (and best (>= typical 0))
                       (let ((strays (make-array 29 :initial-element 0)))
                         (loop for v from 0 below 29
                               do (incf (aref strays (abs (- v typical))) (aref (svref counts best) v)))
                         (or (counted-middle strays) 0))
                       0)))
      (values (or best 0) typical
              (and (>= typical *block-grid-least*) (>= (- typical middle) *block-grid-above*))
              spread))))

(defun laid-pieces (pieces heights)
  "PIECES (BLOCK-PIECEs of all the scanned pictures of a file), each told
whether it is LAID: at the same place as a piece on another page (their
boxes overlapping over half of the smaller), or taller by half than the
taller type of its page, HEIGHTS giving that height of each (PICTURE .
PAGE) (UPPER-HEIGHT), the 2 pixels of paper about it left out. A stamp, a
logo or a signature kept as a picture and laid on each page by an
office's software carries the JPEG blocks of the picture it was kept as,
as a piece saved apart and laid back does: the strength stays, the
honest cause is named. PIECES."
  (flet ((same-place-p (a b)
           (let ((w (- (min (block-piece-x1 a) (block-piece-x1 b)) (max (block-piece-x0 a) (block-piece-x0 b))))
                 (h (- (min (block-piece-y1 a) (block-piece-y1 b)) (max (block-piece-y0 a) (block-piece-y0 b)))))
             (and (plusp w) (plusp h)
                  (> (* 2 w h)
                     (min (* (- (block-piece-x1 a) (block-piece-x0 a)) (- (block-piece-y1 a) (block-piece-y0 a)))
                          (* (- (block-piece-x1 b) (block-piece-x0 b))
                             (- (block-piece-y1 b) (block-piece-y0 b)))))))))
    (dolist (p pieces pieces)
      (let ((upper (gethash (cons (block-piece-picture p) (block-piece-page p)) heights)))
        (setf (block-piece-laid p)
              (or (and upper (> (- (block-piece-y1 p) (block-piece-y0 p) 4) (* 3/2 upper)))
                  (some (lambda (o) (and (not (eql (block-piece-page o) (block-piece-page p))) (same-place-p o p)))
                        pieces)))))))

(defun box-in-piece-p (piece page picture x0 y0 x1 y1)
  "True when the middle of the box X0 Y0 X1 Y1 of the picture PICTURE on PAGE
lies within PIECE (a BLOCK-PIECE)."
  (let ((x (/ (+ x0 x1) 2)) (y (/ (+ y0 y1) 2)))
    (and (eql page (block-piece-page piece)) (eql picture (block-piece-picture piece))
         (<= (block-piece-x0 piece) x) (< x (block-piece-x1 piece))
         (<= (block-piece-y0 piece) y) (< y (block-piece-y1 piece)))))

(defun glyph-in-piece-p (piece glyph)
  "True when the middle of GLYPH lies within PIECE (a BLOCK-PIECE), on its page and picture."
  (box-in-piece-p piece (glyph-page glyph) (glyph-picture glyph)
                  (glyph-x0 glyph) (glyph-y0 glyph) (glyph-x1 glyph) (glyph-y1 glyph)))

(defun copies-on-piece-grid (copies pieces)
  "COPIES (groups of COPIED-GLYPHS), each strong group made worth a look, on the
JPEG's own grid (WHY :GRID), when its characters lie each a whole number of
blocks of 8 pixels from the others across and down. They must also be all within
one region of PIECES that carries JPEG blocks of its own, or carrying a JPEG's
block at one placement (SUPPORTED-WAYS), two of them at least and every one
whose blocks could be measured. A JPEG keeps the same character drawn twice on
its grid the same pixel for pixel, as the repeated figures of a date stamp kept
as a JPEG and laid into a page kept without loss are. COPIES."
  (let ((rows (make-array 0 :element-type 'double-float)))
    (flet ((ways (glyph)
             ;; the placements it carries a JPEG's block at, and whether its blocks could be measured at all
             (if (glyph-pixels glyph)
                 (multiple-value-bind (scores room) (glyph-block-scores glyph rows)
                   (setf rows room)
                   (values (supported-ways scores) (some (lambda (v) (>= v 0)) scores)))
                 (values nil nil))))
      (dolist (c copies copies)
        (destructuring-bind (group difference block alike strength &rest more) c
          (declare (ignore difference block alike more))
          (when (and (eq strength :strong)
                     (every (lambda (g) (and (zerop (mod (- (glyph-x0 g) (glyph-x0 (first group))) 8))
                                             (zerop (mod (- (glyph-y0 g) (glyph-y0 (first group))) 8))))
                            group)
                     (or (some (lambda (p)
                                 (and (eq (block-piece-kind p) :own)
                                      (every (lambda (g) (glyph-in-piece-p p g)) group)))
                               pieces)
                         (let ((measured (loop for g in group
                                               for (ways seen) = (multiple-value-list (ways g))
                                               when seen collect ways)))
                           (and (>= (count-if #'identity measured) 2)
                                (every #'identity measured)
                                (some (lambda (w)
                                        (every (lambda (ws) (some (lambda (v) (near-ways-p v w)) ws)) measured))
                                      (find-if #'identity measured))))))
            (setf (fifth c) :look (sixth c) :grid)))))))

(defparameter *stamp-ink-apart* 60
  "How far apart, at least, the colour of a stamp's ink lies from the
typical colour of its page's characters (the root of the summed squares
of the differences of red, green and blue, each 0 to 255). It must also
lie four times as far as those characters' colours typically lie from it.
Red, blue or green ink on black type is so, or a grey far lighter than
the type's.")

(defparameter *stamp-turn-least* 19/2
  "How many degrees, at least, a stamp's characters are turned against the
lines of their page: half a degree under ten, as the line through a word's
characters reads a stamp's turn up to a third of a degree off.")

(defparameter *stamp-characters-most* 2000
  "The most stamp-like characters a picture may hold for them to be gathered
into stamps (STAMP-COPIES); a picture of more is no page with a stamp on it.")

(defun stamp-copies (copies scans)
  "COPIES (groups of COPIED-GLYPHS), each strong group whose characters are
all stamp-like and lie in one stamp that a program drew (DRAWN-STAMP-P),
made worth a look, a stamp drawn by
software (WHY :STAMP). A character is stamp-like in an ink unlike its
page's characters' (*STAMP-INK-APART*), or turned *STAMP-TURN-LEAST*
degrees or more against its page's lines (WORDS-TURNED), and read at its
page's level of ink, not in a mark too faint to be letters (those are judged
by LIGHT-COPIES); a stamp is stamp-like characters
of one picture each within three of its characters' heights of another. A
program draws a stamp's figures the same every time, as an office's
software lays a date or a received stamp. Characters typed in the page's
own ink and at its angle stay strong, as does a group any one of whose
characters lies outside the stamp: a stamp's figure the same as one in the
text. So does a group carrying the grain of its page's characters
\(DRAWN-GRAIN-P), however clean the stamp about it: characters of the page
copied into a stamp drawn by software. The page's characters it is set
beside are those read at its level of ink, not those of marks too faint to
be letters. A stamp all of whose characters are
drawn in one ink smoothed into a ground of their own (DRAWN-COLOURS-P) is
drawn by a program too, however its prints of a shape came out, a blur
lying on each as its neighbours' falls. SCANS gives the characters of each
picture, of which the typical ink is measured. COPIES, and as a second
value a table of each stamp-like character to its stamp."
  (let ((stamp-of (make-hash-table :test 'eq)) (stamps 0)
        (on-page (make-hash-table :test 'equal)) (turn-of (make-hash-table :test 'eq)))
    (dolist (scan scans)
      (dolist (g (scan-picture-glyphs scan)) (push g (gethash (picture-key g) on-page))))
    (dolist (scan scans)
      (let* ((glyphs (scan-picture-glyphs scan))
             (colours (remove nil (mapcar #'glyph-ink-colour glyphs))))
        (when (>= (length colours) 20)
          (let* ((middle (loop for k below 3 collect (median-of (mapcar (lambda (c) (nth k c)) colours))))
                 (apart (max *stamp-ink-apart*
                             (* 4 (median-of (mapcar (lambda (c) (colour-distance c middle)) colours)))))
                 (turns (words-turned glyphs))
                 (like (remove-if-not (lambda (g)
                                        (let ((turn (gethash (glyph-word g) turns)) (colour (glyph-ink-colour g)))
                                          (and (not (word-box-faint (glyph-word g)))
                                               (or (and turn (>= (abs turn) *stamp-turn-least*))
                                                   (and colour (>= (colour-distance colour middle) apart))))))
                                      glyphs)))
            (dolist (g like) (setf (gethash g turn-of) (gethash (glyph-word g) turns)))
            (when (<= (length like) *stamp-characters-most*)
              ;; stamps: the stamp-like characters each within three heights of another, joined
              (let ((group (make-hash-table :test 'eq)))
                (labels ((root (g) (let ((up (gethash g group g))) (if (eq up g) g (setf (gethash g group) (root up)))))
                         (near-p (a b)
                           (let ((reach (* 3 (max (- (glyph-y1 a) (glyph-y0 a)) (- (glyph-y1 b) (glyph-y0 b))))))
                             (and (<= (- (max (glyph-x0 a) (glyph-x0 b)) (min (glyph-x1 a) (glyph-x1 b))) reach)
                                  (<= (- (max (glyph-y0 a) (glyph-y0 b)) (min (glyph-y1 a) (glyph-y1 b))) reach)))))
                  (loop for (a . rest) on like
                        do (dolist (b rest)
                             (when (near-p a b)
                               (let ((ra (root a)) (rb (root b))) (unless (eq ra rb) (setf (gethash ra group) rb))))))
                  (let ((numbers (make-hash-table :test 'eq)))
                    (dolist (g like)
                      (setf (gethash g stamp-of)
                            (or (gethash (root g) numbers) (setf (gethash (root g) numbers) (incf stamps)))))))))))))
    (let ((members (make-hash-table)) (group-of (make-hash-table :test 'eq)) (drawn (make-hash-table)))
      (maphash (lambda (g stamp) (push g (gethash stamp members))) stamp-of)
      ;; each character's group of copies, the same pixel for pixel or nearer than prints are
      (dolist (c copies) (dolist (g (first c)) (setf (gethash g group-of) c)))
      (dolist (c copies copies)
        (let ((stamp (gethash (first (first c)) stamp-of)))
          (when (and (eq (fifth c) :strong) stamp
                     (every (lambda (g) (eql (gethash g stamp-of) stamp)) (first c))
                     (eq (multiple-value-bind (known seen) (gethash stamp drawn)
                           (if seen
                               known
                               (setf (gethash stamp drawn)
                                     (or (drawn-stamp-p (gethash stamp members) group-of)
                                         ;; drawn in one ink smoothed into a ground of its own, however its
                                         ;; prints of a shape came out
                                         (drawn-colours-p (gethash stamp members)
                                                          (remove-if (lambda (g) (gethash g stamp-of))
                                                                     (gethash (picture-key (first (first c)))
                                                                              on-page)))))))
                         t)
                     (drawn-grain-p (first c) (remove-if (lambda (g)
                                                           (or (gethash g stamp-of)
                                                               (eq (fifth (gethash g group-of)) :strong)
                                                               (word-box-faint (glyph-word g))))
                                                         (gethash (picture-key (first (first c))) on-page))
                                    (scan-picture-paper (scan-of-glyph scans (first (first c))))
                                    (some (lambda (g) (gethash g turn-of)) (first c))))
            (setf (fifth c) :look (sixth c) :stamp))))
      (light-copies copies scans stamp-of)
      (values copies stamp-of))))

(defun coarse-regreyed-distance (copy source)
  "A quick REGREYED-DISTANCE of COPY from SOURCE, in SOURCE's grey levels:
SOURCE laid under COPY middle to middle and a whole pixel either way, read
at its pixels, not between them. What sets a page's characters in order for
the full measure (LIGHT-COPIES). NIL when their boxes differ by more than
three pixels across or down, a character's pixels are no longer kept, or no
way of laying them fits."
  (let ((pc (glyph-pixels copy)) (ps (glyph-pixels source)))
    (when (and pc ps)
      (let ((w (- (glyph-x1 copy) (glyph-x0 copy))) (h (- (glyph-y1 copy) (glyph-y0 copy)))
            (sw (- (glyph-x1 source) (glyph-x0 source))) (sh (- (glyph-y1 source) (glyph-y0 source)))
            (wc (glyph-width copy)) (ws (glyph-width source)) (lo 255) (hi 0) (best nil))
        (declare (type grey-pixels pc ps) (type fixnum w h sw sh wc ws lo hi))
        (when (and (<= (abs (- w sw)) 3) (<= (abs (- h sh)) 3))
          (dotimes (j h)
            (dotimes (i w)
              (let ((g (aref pc (+ (* (+ j 2) wc) i 2)))) (setf lo (min lo g) hi (max hi g)))))
          (let ((cut (- hi (floor (- hi lo) 4))) (rows (floor (length ps) ws)))
            (declare (type fixnum cut rows))
            (loop for dy fixnum from -1 to 1
                  do (loop for dx fixnum from -1 to 1
                           do (let ((n 0) (sx 0) (sy 0) (sxx 0) (sxy 0) (syy 0) (fits t)
                                    (ox (+ dx (floor (- sw w) 2))) (oy (+ dy (floor (- sh h) 2))))
                                (declare (type fixnum n sx sy sxx sxy syy ox oy))
                                (dotimes (j h)
                                  (dotimes (i w)
                                    (let ((y (aref pc (+ (* (+ j 2) wc) i 2))))
                                      (when (< y cut)
                                        (let ((si (+ i 2 ox)) (sj (+ j 2 oy)))
                                          (if (and (< -1 si ws) (< -1 sj rows))
                                              (let ((x (aref ps (+ (* sj ws) si))))
                                                (incf n) (incf sx x) (incf sy y) (incf sxx (* x x))
                                                (incf sxy (* x y)) (incf syy (* y y)))
                                              (setf fits nil)))))))
                                (when (and fits (> n 1))
                                  (let ((var (- sxx (/ (* sx sx) n))) (spread (- syy (/ (* sy sy) n)))
                                        (cov (- sxy (/ (* sx sy) n))))
                                    (when (and (plusp var) (plusp cov))
                                      (let* ((slope (/ cov var))
                                             (d (/ (sqrt (max 0 (/ (- spread (* slope cov)) n))) slope)))
                                        (when (or (null best) (< d best)) (setf best d)))))))))
            best))))))

(defparameter *light-sources-most* 40
  "How many of a page's characters, at most, a light copy is set beside for
its source (LIGHT-COPIES): those it fits best at a glance
(COARSE-REGREYED-DISTANCE). Each is laid under the
copy 49 ways, so the most bounds the time a page of many light marks takes.")

(defun light-copies (copies scans &optional (stamp-of (make-hash-table)))
  "COPIES (groups of COPIED-GLYPHS), each strong group all of whose
characters were read in marks too faint to be letters (WORD-BOX-FAINT)
made worth a look (WHY :LIGHT), unless one of them is a character of its
page with its greys changed by one rule: its ink lies off a straight line
through the source's greys (REGREYED-DISTANCE) by less than *REGREYED-SHARE*
of how far the page's blank paper strays pixel by pixel (BLANK-PAPER-NOISE),
measured in the source's grey levels, so that a clean shape drawn light
beside a grained source is no copy of it. A character of the page copied
and made lighter is so. A page whose paper shows no grain pixel by pixel,
whitened or drawn, cannot tell a copy from a light print, and its light
copies stay worth a look. Nor is one whose grain follows a character of
its page, turned or not (GRAIN-SOURCE): a character copied, turned and
made lighter. A stamp or a form's print in a light ink is drawn
the same every time, and is read so too. The page's characters are those
read at its level of ink and in no stamp (STAMP-OF, a character's stamp,
from STAMP-COPIES): a stamp's figure read at the page's level is its own
fainter figures with their greys changed too. Of each group, a copy of
each set of pixels, four at most, is set beside the *LIGHT-SOURCES-MOST*
of them it fits best at a glance (COARSE-REGREYED-DISTANCE): copies cut
from marks with a neighbour's ink in them differ. SCANS gives the characters
of each picture. COPIES."
  (let ((on-page (make-hash-table :test 'equal)))
    (dolist (scan scans)
      (dolist (g (scan-picture-glyphs scan))
        (unless (or (word-box-faint (glyph-word g)) (gethash g stamp-of))
          (push g (gethash (picture-key g) on-page)))))
    (flet ((source-p (copy most)
             ;; one of the page's characters COPY fits best at a glance is COPY with its greys changed
             (let ((near (sort (loop for g in (gethash (picture-key copy) on-page)
                                     for apart = (coarse-regreyed-distance copy g)
                                     when apart collect (cons apart g))
                               #'< :key #'car)))
               (or (loop for (nil . g) in near
                         repeat *light-sources-most*
                         do (job-check-stop)
                         thereis (and (not (same-pixels-p copy g))
                                      (multiple-value-bind (d slope) (regreyed-distance copy g)
                                        (and d (<= (/ d slope) most)))))
                   ;; a light copy turned as well: its grain follows its source's
                   (grain-source copy (gethash (picture-key copy) on-page))))))
      (dolist (c copies copies)
        (when (and (eq (fifth c) :strong) (every (lambda (g) (word-box-faint (glyph-word g))) (first c)))
          (let* ((grain (scan-picture-paper-grain (scan-of-glyph scans (first (first c)))))
                 (most (and grain (plusp grain) (* *regreyed-share* grain))))
            ;; copies the same pixel for pixel stand for one another; a copy cut with a neighbour's ink differs
            (unless (and most
                         (let ((apart '()))
                           (dolist (g (first c))
                             (unless (some (lambda (a) (same-pixels-p a g)) apart) (push g apart)))
                           (loop for copy in (nreverse apart) repeat 4 thereis (source-p copy most))))
              (setf (fifth c) :look (sixth c) :light))))))))

(defparameter *stamp-grain-most* 0.6
  "How far, at most, the cores of a group of copies in a stamp drawn by a
program typically stray (GLYPH-CORE-SPREAD), as a share of how far the cores
of its page's characters of its size typically do (DRAWN-GRAIN-P): groups of
stamps drawn by software, their JPEG ringing the same in every print,
measured 0.36 to 0.82 of it, groups of a stamp pressed and scanned whose
every print was copied from one 0.64 to 1.10. The stricter value keeps the
pressed ones strong; drawn stamps over it stay strong too.")

(defparameter *stamp-paper-grain-least* 1
  "How far, at least, the paper within the boxes of a page's characters
typically strays (GLYPH-PAPER-GRAIN), in grey levels, for the paper within
a stamp's figures to be set beside it (DRAWN-GRAIN-P). Pages of grain 1
give about 1, of grain 7 about 4; on paper cleaner than this a copy's paper
is as clean as a drawn figure's, and the cores alone are judged.")

(defparameter *stamp-paper-grain-most* 1/5
  "How far, at most, the paper within the boxes of a group of copies in a
stamp drawn by a program typically strays (GLYPH-PAPER-GRAIN), as a share of
how far the paper within its page's characters does (DRAWN-GRAIN-P). Honest
stamps drawn by software on clean ground, in thin and bold faces, red, blue
and black, turned 0 to 15 degrees, on pages of grain 1.5 to 7, measured 0 to
0.12 of it (the most where a turn smooths the ground at a figure's edge);
characters of the page copied with their paper 0.39 to 2.8, and one thin 1
turned with its corners filled flat 0.18.")

(defparameter *regreyed-share* 1
  "How far, at most, a copy's ink lies from a straight line through its
source's greys (REGREYED-DISTANCE), as a share of how far the paper within
its page's characters strays (GLYPH-PAPER-GRAIN), for it to be the source
recoloured or made lighter or darker (DRAWN-GRAIN-P): both grow with the
scanner's grain. Characters cut out onto white and made red from one of
their colours lie 0.86 to 0.92 of it from their source at grain 6 to 9;
honest stamps drawn in the page's face and blurred as the scan is, 1.12 or
more (sharp ones 2.9 or more). Copies made red keeping the page's green and
blue lie 1.11, among the blurred stamps, and are not told so.")

(declaim (inline cut-grey-at))
(defun cut-grey-at (pixels across rows x y)
  "The grey of the cut PIXELS (ACROSS wide, ROWS tall) at X Y, read between
its pixels by a straight line each way, a pixel's middle at its index and a
half; -1 off the cut."
  (declare (type grey-pixels pixels) (type fixnum across rows) (type double-float x y))
  (let* ((x (- x 0.5d0)) (y (- y 0.5d0)) (x0 (floor x)) (y0 (floor y)) (fx (- x x0)) (fy (- y y0)))
    (declare (type fixnum x0 y0) (type double-float fx fy))
    (if (and (<= 0 x0) (< (1+ x0) across) (<= 0 y0) (< (1+ y0) rows))
        (flet ((p (i j) (float (aref pixels (+ (* j across) i)) 1d0)))
          (+ (* (- 1 fx) (- 1 fy) (p x0 y0)) (* fx (- 1 fy) (p (1+ x0) y0))
             (* (- 1 fx) fy (p x0 (1+ y0))) (* fx fy (p (1+ x0) (1+ y0)))))
        -1d0)))

(defun copy-ink (copy)
  "The ink of the character COPY as a copy is set beside its source: its
pixels darker than three quarters of the way from the darkest to the
lightest grey of its box, each (ACROSS DOWN GREY), ACROSS and DOWN taken
from the middle of the box, in a list. As second and third values, the
darkest and lightest grey of its box."
  (let ((w (- (glyph-x1 copy) (glyph-x0 copy))) (h (- (glyph-y1 copy) (glyph-y0 copy)))
        (pc (glyph-pixels copy)) (wc (glyph-width copy)) (lo 255) (hi 0) (ink '()))
    (declare (type grey-pixels pc) (type fixnum w h wc lo hi))
    (dotimes (j h)
      (dotimes (i w)
        (let ((g (aref pc (+ (* (+ j 2) wc) i 2)))) (setf lo (min lo g) hi (max hi g)))))
    (let ((cut (- hi (floor (- hi lo) 4))))
      (dotimes (j h)
        (dotimes (i w)
          (let ((g (aref pc (+ (* (+ j 2) wc) i 2))))
            (when (< g cut) (push (list (- (+ i 0.5d0) (/ w 2d0)) (- (+ j 0.5d0) (/ h 2d0)) g) ink))))))
    (values ink lo hi)))

(defun source-fit (ink source pixels radians dx dy)
  "How near INK (COPY-INK) comes to PIXELS, SOURCE's grey levels or one of
its colours, with their greys changed by one rule, SOURCE laid under it
middle to middle, DX and DY pixels across and down, and turned RADIANS: the
root of the mean squared difference, in grey levels, of INK from a straight
line through the greys of PIXELS under it, read between its pixels; as a
second value the slope of that line. NIL when the ink falls off the cut,
the line falls, or either spreads by less than 10 grey levels."
  (declare (type grey-pixels pixels) (type double-float radians dx dy))
  (let* ((across (glyph-width source)) (rows (floor (length pixels) across))
         (n 0) (sx 0d0) (sy 0d0) (sxx 0d0) (sxy 0d0) (syy 0d0)
         (c (cos radians)) (s (sin radians))
         (mx (+ 2 (/ (- (glyph-x1 source) (glyph-x0 source)) 2d0) dx))
         (my (+ 2 (/ (- (glyph-y1 source) (glyph-y0 source)) 2d0) dy)))
    (declare (type fixnum across rows n) (type double-float sx sy sxx sxy syy c s mx my))
    (loop for (u v y) in ink
          do (let ((x (cut-grey-at pixels across rows
                                   (+ mx (* c (the double-float u)) (* s (the double-float v)))
                                   (+ my (- (* c (the double-float v)) (* s (the double-float u))))))
                   (y (float (the fixnum y) 1d0)))
               (declare (type double-float x y))
               (when (< x 0d0) (return-from source-fit nil))
               (incf n) (incf sx x) (incf sy y) (incf sxx (* x x)) (incf sxy (* x y)) (incf syy (* y y))))
    (when (plusp n)
      (let ((var (- sxx (/ (* sx sx) n))) (spread (- syy (/ (* sy sy) n))) (cov (- sxy (/ (* sx sy) n))))
        (when (and (> var (* n 100)) (> spread (* n 100)) (plusp cov))
          (let ((slope (/ cov var)))
            ;; what is left of the ink's greys once the straight line through the source's is taken out
            (values (sqrt (max 0d0 (/ (- spread (* slope cov)) n))) slope)))))))

(defparameter *source-offsets* '(-1.5d0 -1d0 -0.5d0 0d0 0.5d0 1d0 1.5d0)
  "The offsets across and down, in pixels, at which a source is laid under a
copy (REGREYED-DISTANCE, SOURCE-PLACEMENT).")

(defun regreyed-distance (copy source &optional (turn 0))
  "How near the ink of the character COPY comes to SOURCE's pixels with their
greys changed by one rule for all of them, a darker grey always the darker,
as an ink recoloured is: the least root of the mean squared difference, in
grey levels, of COPY's ink from a straight line through SOURCE's greys under
it. COPY's ink is its pixels darker than three quarters of the way from the
darkest to the lightest grey of its box, so a copy cut out and laid on a
white ground is judged by its ink alone. SOURCE is laid under COPY middle to
middle, and up to a pixel and a half either way by half pixels, as laid or
turned by TURN degrees either way, read between its pixels. Where SOURCE
holds colours (GLYPH-CHANNELS) the line may run through its red, green or
blue instead, as a recolouring that takes one colour of the source keeps
that colour's grain and drops the others'. As a second value, the slope of
that line, how many of COPY's grey levels one of SOURCE's makes. NIL when the
boxes are unlike in size (within a pixel unturned, within three turned), or
the ink is under 20 pixels or spreads by less than 10 grey levels. A copy
read in a mark too faint to be letters (WORD-BOX-FAINT), cut at a level of
its own, may be as small as half its source or three pixels larger: a
character made lighter until its edge is paper loses that edge."
  (let* ((w (- (glyph-x1 copy) (glyph-x0 copy))) (h (- (glyph-y1 copy) (glyph-y0 copy)))
         (sw (- (glyph-x1 source) (glyph-x0 source))) (sh (- (glyph-y1 source) (glyph-y0 source)))
         (turned (>= (abs turn) 1/2))
         (radians (float (* (abs turn) (/ pi 180)) 1d0))
         (faint (and (glyph-word copy) (word-box-faint (glyph-word copy))))
         (slack (if turned 3 1)))
    (flet ((sized-p (own other)
             ;; a faint copy cut at its own level loses the lighter edge its source had
             (if faint (<= (/ other 2) own (+ other 3)) (<= (abs (- own other)) slack))))
      (when (and (glyph-pixels copy) (glyph-pixels source)
                 (sized-p w (if turned (+ (* sw (cos radians)) (* sh (sin radians))) sw))
                 (sized-p h (if turned (+ (* sh (cos radians)) (* sw (sin radians))) sh)))
        (let ((ink (copy-ink copy)) (best nil) (best-slope nil))
          (when (>= (length ink) 20)
            (dolist (pixels (cons (glyph-pixels source) (glyph-channels source)))
              (dolist (angle (if turned (list radians (- radians)) '(0d0)))
                (dolist (dy *source-offsets*)
                  (dolist (dx *source-offsets*)
                    (multiple-value-bind (d slope) (source-fit ink source pixels angle dx dy)
                      (when (and d (or (null best) (< d best))) (setf best d best-slope slope))))))))
          (values best best-slope))))))

(defun regreyed-copy-p (copy source most &optional (turn 0))
  "True when the ink of the character COPY is SOURCE's pixels with their
greys changed by one rule, turned TURN degrees or not: within MOST grey
levels of it (REGREYED-DISTANCE), but not the same pixels (SAME-PIXELS-P),
which are prints of one figure, found as copies of each other. The scanner's
grain in the colours a recolouring drops is what is left; another character
of the same size stands tens of grey levels off."
  (and (not (same-pixels-p copy source))
       (let ((d (regreyed-distance copy source turn)))
         (and d (<= d most)))))

(defparameter *source-turn-most* 15
  "How many degrees either way, at most, a copy may be turned against its
source for the grain they share to be sought (SOURCE-PLACEMENT): a stamp
is laid turned up to 15 degrees, and so are copies laid among its figures.")

(defun source-placement (copy source ink)
  "Where COPY's INK (COPY-INK) fits SOURCE's greys best by one rule
\(SOURCE-FIT), SOURCE turned up to *SOURCE-TURN-MOST* degrees either way:
sought by whole degrees and whole pixels, then by quarter degrees and half
pixels about the best of those. As four values, the turn in radians, the
offsets across and down, and how near the ink comes there (SOURCE-FIT); NIL
when at no turn are their boxes alike in size (within three pixels), or no
placement fits."
  (let ((w (- (glyph-x1 copy) (glyph-x0 copy))) (h (- (glyph-y1 copy) (glyph-y0 copy)))
        (sw (- (glyph-x1 source) (glyph-x0 source))) (sh (- (glyph-y1 source) (glyph-y0 source)))
        (pixels (glyph-pixels source)) (best nil))
    (flet ((try (degrees offsets)
             (let ((radians (float (* degrees (/ pi 180)) 1d0)))
               (when (and (<= (abs (- w (+ (* sw (cos radians)) (* sh (abs (sin radians)))))) 3)
                          (<= (abs (- h (+ (* sh (cos radians)) (* sw (abs (sin radians)))))) 3))
                 (dolist (dy offsets)
                   (dolist (dx offsets)
                     (let ((d (source-fit ink source pixels radians dx dy)))
                       (when (and d (or (null best) (< d (first best))))
                         (setf best (list d degrees dx dy))))))))))
      (loop for degrees from (- *source-turn-most*) to *source-turn-most* do (try degrees '(-1d0 0d0 1d0)))
      (when best
        (let ((middle (second best)))
          (loop for k from -3 to 3 do (try (+ middle (* k 1/4)) *source-offsets*)))
        (destructuring-bind (d degrees dx dy) best
          (values (float (* degrees (/ pi 180)) 1d0) dx dy d))))))

(defparameter *shared-grain-least* 3/4
  "How closely, at least, the fine grain of a copy's paper must follow its
source's (SHARED-GRAIN, a correlation, 1 for the same grain) for the copy to
be the source turned and recoloured (GRAIN-SOURCE), over many pixels; over
few the bound rises (SHARED-GRAIN-BOUND).")

(defparameter *shared-grain-pixels* 16
  "How many pixels of paper, at least, a copy must hold about its ink for
the grain it shares with a source to be judged (SHARED-GRAIN).")

(defparameter *shared-grain-margin* 3/10
  "How much more closely, at least, a copy's grain must follow its source's
than it follows the grain of the next character its ink fits as well
\(GRAIN-SOURCE): a shape alike in every print of a letter fits them all
alike, the grain of a copy only one.")

(defparameter *grain-candidates-most* 40
  "How many of the characters of its page, at most, a copy is laid on in
full for the grain it may share with them (GRAIN-SOURCE), some 620 ways
each; ten times as many are laid at a glance first, 39 ways each. So a copy
costs some 40,500 fits of its ink at most, however many characters its page
holds.")

(defparameter *grain-sources-most* 5
  "How many of the characters a copy's ink fits best are set beside it for
the grain they share (GRAIN-SOURCE): a copy's source is among the few its
ink fits best, and each more set beside it is another chance of a grain
alike by chance.")

(defun box-light (pixels across w h)
  "The grey three quarters of the way from the darkest to the lightest of
the box W by H of the cut PIXELS, ACROSS wide, the box two pixels into the
cut: lighter than it is paper."
  (declare (type grey-pixels pixels) (type fixnum across w h))
  (let ((lo 255) (hi 0))
    (declare (type fixnum lo hi))
    (dotimes (j h)
      (dotimes (i w)
        (let ((g (aref pixels (+ (* (+ j 2) across) i 2)))) (setf lo (min lo g) hi (max hi g)))))
    (- hi (floor (- hi lo) 4))))

(defun grain-pairs (copy source pixels radians dx dy)
  "The fine grain of the paper about COPY's ink beside SOURCE's, SOURCE laid
under COPY turned RADIANS, DX and DY pixels off middle to middle. For each
pixel of COPY's box two pixels or more from its ink (the 25 about it all
lighter than three quarters of the way from the darkest to the lightest grey
of the box) whose 9 fall on paper of PIXELS too (SOURCE's grey levels or one
of its colours, read between its pixels, lighter than three quarters of the
way across SOURCE's box), how far it lies from the middle grey of its 9 and
how far PIXELS lie from theirs there, as (COPY'S . SOURCE'S). Pixels of a
column or row of COPY's box holding fewer than 3 are left out, and the mean
of each column and then of each row is taken out of both, as a streak a
scanner draws down or across a page lies in every print alike. In a list,
NIL for none; as a second value, how many of them are free once the means
are taken out, their number less the columns and rows and one more."
  (let* ((w (- (glyph-x1 copy) (glyph-x0 copy))) (h (- (glyph-y1 copy) (glyph-y0 copy)))
         (pc (glyph-pixels copy)) (wc (glyph-width copy))
         (across (glyph-width source)) (rows (floor (length pixels) across))
         (c (cos radians)) (s (sin radians))
         (mx (+ 2 (/ (- (glyph-x1 source) (glyph-x0 source)) 2d0) dx))
         (my (+ 2 (/ (- (glyph-y1 source) (glyph-y0 source)) 2d0) dy))
         (light (box-light pc wc w h))
         (theirs-light (float (box-light pixels across (- (glyph-x1 source) (glyph-x0 source))
                                         (- (glyph-y1 source) (glyph-y0 source)))
                              1d0))
         (own (make-array 9 :element-type 'double-float))
         (theirs (make-array 9 :element-type 'double-float))
         (cells '()))
    (declare (type grey-pixels pc pixels) (type fixnum w h wc across rows light)
             (type double-float c s mx my theirs-light))
    (flet ((middle (nine)
             ;; the middle of the 9, sorted in place
             (declare (type (simple-array double-float (9)) nine))
             (loop for a from 1 below 9
                   do (loop for b downfrom a above 0
                            while (< (aref nine b) (aref nine (1- b)))
                            do (rotatef (aref nine b) (aref nine (1- b)))))
             (aref nine 4))
           (clear-p (i j)
             ;; the 25 about I J all paper; the cut's margin of two pixels holds them
             (loop for dj fixnum from -2 to 2
                   always (loop for di fixnum from -2 to 2
                                always (> (aref pc (+ (* (+ j 2 dj) wc) i 2 di)) light)))))
      (dotimes (j h)
        (dotimes (i w)
          (when (clear-p i j)
            (let ((k 0) (paper t))
              (declare (type fixnum k))
              (loop for dj fixnum from -1 to 1
                    do (loop for di fixnum from -1 to 1
                             do (let* ((u (- (+ i di 0.5d0) (/ w 2d0)))
                                       (z (- (+ j dj 0.5d0) (/ h 2d0)))
                                       (g (cut-grey-at pixels across rows (+ mx (* c u) (* s z))
                                                       (+ my (- (* c z) (* s u))))))
                                  (declare (type double-float u z g))
                                  (unless (> g theirs-light) (setf paper nil))
                                  (setf (aref own k) (float (aref pc (+ (* (+ j 2 dj) wc) i 2 di)) 1d0)
                                        (aref theirs k) g)
                                  (incf k))))
              (when paper
                (let ((mine (aref own 4)) (source-grey (aref theirs 4)))
                  (push (list i j (- mine (middle own)) (- source-grey (middle theirs))) cells))))))))
    ;; cells of columns or rows holding fewer than 3 left out, until none is; then the mean of each column, then
    ;; of each row, taken out of both
    (let ((counts (list (make-hash-table) (make-hash-table))))
      (loop for before = (length cells)
            do (loop for key in (list #'first #'second) for count in counts
                     do (clrhash count)
                        (dolist (cell cells) (incf (gethash (funcall key cell) count 0)))
                        (setf cells (remove-if (lambda (cell) (< (gethash (funcall key cell) count) 3)) cells)))
            until (= before (length cells)))
      (let ((columns (hash-table-count (first counts)))
            (rows (hash-table-count (second counts))))
        (dolist (key (list #'first #'second))
          (let ((sums (make-hash-table)))
            (dolist (cell cells)
              (let ((at (gethash (funcall key cell) sums (list 0 0d0 0d0))))
                (setf (gethash (funcall key cell) sums)
                      (list (1+ (first at)) (+ (second at) (third cell)) (+ (third at) (fourth cell))))))
            (dolist (cell cells)
              (destructuring-bind (n a b) (gethash (funcall key cell) sums)
                (decf (third cell) (/ a n))
                (decf (fourth cell) (/ b n))))))
        (values (mapcar (lambda (cell) (cons (third cell) (fourth cell))) cells)
                (max 0 (- (+ (length cells) 1) columns rows)))))))

(defun clipped-correlation (pairs)
  "The correlation, from -1 to 1, of the pairs of numbers PAIRS, each
\(A . B), each number farther from 0 than three times the middle of its
kind's distances from 0 counted as that far: a few far from the rest do not
carry it. NIL when half or more of either kind are 0, or either does not
vary."
  (let ((most-a (* 3 (median-of (mapcar (lambda (p) (abs (car p))) pairs))))
        (most-b (* 3 (median-of (mapcar (lambda (p) (abs (cdr p))) pairs))))
        (n (length pairs)) (sa 0d0) (sb 0d0) (saa 0d0) (sbb 0d0) (sab 0d0))
    (declare (type double-float sa sb saa sbb sab))
    (when (and (plusp most-a) (plusp most-b))
      (loop for (a . b) in pairs
            do (let ((a (max (- most-a) (min most-a a))) (b (max (- most-b) (min most-b b))))
                 (incf sa a) (incf sb b) (incf saa (* a a)) (incf sbb (* b b)) (incf sab (* a b))))
      (let ((va (- saa (/ (* sa sa) n))) (vb (- sbb (/ (* sb sb) n))))
        (when (and (plusp va) (plusp vb))
          (/ (- sab (/ (* sa sb) n)) (sqrt (* va vb))))))))

(defun shared-grain (copy source radians dx dy)
  "How closely the fine grain of the paper about the ink of the character
COPY follows SOURCE's, SOURCE laid under it turned RADIANS, DX and DY pixels
off middle to middle: the correlation (CLIPPED-CORRELATION) of how far each
pixel of paper two pixels or more from COPY's ink, on paper of SOURCE too,
lies from the middle grey of the 9 about it, in COPY and in SOURCE where it
falls (GRAIN-PAIRS). Where SOURCE holds colours (GLYPH-CHANNELS), the
closest of its grey, red, green and blue. The correlation does not change
when a copy's greys are changed by one rule, made lighter and its grain
fainter; a scanner's grain is its own in every print. As a second value, how
many of the pixels judged are free (GRAIN-PAIRS). NIL when fewer than
*SHARED-GRAIN-PIXELS* pixels are judged, or either holds no grain."
  (let ((best nil) (best-n 0))
    (dolist (pixels (cons (glyph-pixels source) (glyph-channels source)))
      (multiple-value-bind (pairs free) (grain-pairs copy source pixels radians dx dy)
        (when (>= (length pairs) *shared-grain-pixels*)
          (let ((r (clipped-correlation pairs)))
            (when (and r (or (null best) (> r best))) (setf best r best-n free))))))
    (and best (values best best-n))))

(defun shared-grain-bound (pixels)
  "How closely, at least, a copy's grain must follow a source's over PIXELS
free pixels (SHARED-GRAIN): *SHARED-GRAIN-LEAST*, or more over few pixels, where
a correlation as close comes by chance more often, as far as four times the
spread such a correlation has by chance (Fisher's)."
  (max *shared-grain-least* (tanh (/ 4 (sqrt (max 1 (- pixels 3)))))))

(defun turned-size-apart (copy source)
  "How far apart, in pixels across and down together, COPY's box and SOURCE's
turned lie in size at the whole degree up to *SOURCE-TURN-MOST* either way
that brings them nearest; NIL when at no such turn are they alike within
three pixels each way."
  (let ((w (- (glyph-x1 copy) (glyph-x0 copy))) (h (- (glyph-y1 copy) (glyph-y0 copy)))
        (sw (- (glyph-x1 source) (glyph-x0 source))) (sh (- (glyph-y1 source) (glyph-y0 source)))
        (best nil))
    (loop for degrees from 0 to *source-turn-most*
          do (let* ((r (* degrees (/ pi 180)))
                    (across (abs (- w (+ (* sw (cos r)) (* sh (sin r))))))
                    (down (abs (- h (+ (* sh (cos r)) (* sw (sin r)))))))
               (when (and (<= across 3) (<= down 3) (or (null best) (< (+ across down) best)))
                 (setf best (+ across down)))))
    best))

(defun grain-source (copy candidates)
  "The one of CANDIDATES, characters of COPY's page, whose grain COPY
carries, turned or not; NIL for none. Of the ten times
*GRAIN-CANDIDATES-MOST* nearest COPY in size, turned or not
\(TURNED-SIZE-APART, ties taken top to bottom and left to right), the
*GRAIN-CANDIDATES-MOST* its ink fits best at a glance (by whole degrees,
then a whole pixel either way at the best of them) are laid where it fits
each best (SOURCE-PLACEMENT); of the *GRAIN-SOURCES-MOST* it fits best of
those, the one whose paper's grain COPY's follows most closely
\(SHARED-GRAIN) is its source when that is SHARED-GRAIN-BOUND or more over
its free pixels and *SHARED-GRAIN-MARGIN* more than the next. Where no other
scores, the margin cannot be shown and none is taken: a letter with one
print on its page is no source this way. One laid less than 2 degrees turned on the same 8 pixel step as
COPY is passed over, as a JPEG's blocks ring alike on its grid. A print the
same pixel for pixel is no source, but another print of one figure."
  (when (glyph-pixels copy)
    (let ((ink (copy-ink copy)) (fits '()) (scores '()))
      (labels ((before-p (a b)
                 ;; by the first number, then top to bottom and left to right; each (NUMBER . GLYPH) or
                 ;; (NUMBER GLYPH ...)
                 (let ((ga (if (listp (cdr a)) (cadr a) (cdr a))) (gb (if (listp (cdr b)) (cadr b) (cdr b))))
                   (or (< (car a) (car b))
                       (and (= (car a) (car b))
                            (or (< (glyph-y0 ga) (glyph-y0 gb))
                                (and (= (glyph-y0 ga) (glyph-y0 gb)) (< (glyph-x0 ga) (glyph-x0 gb))))))))
               (fit-at (g degrees dx dy)
                 (source-fit ink g (glyph-pixels g) (float (* degrees (/ pi 180)) 1d0) dx dy))
               (glance (g)
                 ;; the best whole degree middle to middle, then a whole pixel either way at it
                 (let ((best nil) (best-degrees 0))
                   (loop for degrees from (- *source-turn-most*) to *source-turn-most*
                         for fit = (fit-at g degrees 0d0 0d0)
                         do (when (and fit (or (null best) (< fit best))) (setf best fit best-degrees degrees)))
                   (when best
                     (dolist (dy '(-1d0 0d0 1d0))
                       (dolist (dx '(-1d0 0d0 1d0))
                         (unless (and (zerop dx) (zerop dy))
                           (let ((fit (fit-at g best-degrees dx dy)))
                             (when (and fit (< fit best)) (setf best fit)))))))
                   best)))
        (when (>= (length ink) 20)
          (let* ((sized (stable-sort (loop for g in candidates
                                           for apart = (and (glyph-pixels g) (not (eq g copy))
                                                            (turned-size-apart copy g))
                                           when (and apart (not (same-pixels-p copy g))) collect (cons apart g))
                                     #'before-p))
                 (glanced (stable-sort (loop for (nil . g) in sized
                                             for k from 0 below (* 10 *grain-candidates-most*)
                                             for d = (progn (when (zerop (mod k 50)) (job-check-stop)) (glance g))
                                             when d collect (cons d g))
                                       #'before-p)))
            (loop for (nil . g) in glanced
                  repeat *grain-candidates-most*
                  do (job-check-stop)
                     (multiple-value-bind (radians dx dy d) (source-placement copy g ink)
                       (flet ((step-p (offset) (< (abs (- offset (* 8 (round offset 8)))) 1)))
                         (when (and radians
                                    (not (and (< (abs radians) (* 2 (/ pi 180)))
                                              (step-p (- (+ (glyph-x0 g) (/ (- (glyph-x1 g) (glyph-x0 g)) 2) dx)
                                                         (+ (glyph-x0 copy) (/ (- (glyph-x1 copy) (glyph-x0 copy)) 2))))
                                              (step-p (- (+ (glyph-y0 g) (/ (- (glyph-y1 g) (glyph-y0 g)) 2) dy)
                                                         (+ (glyph-y0 copy)
                                                            (/ (- (glyph-y1 copy) (glyph-y0 copy)) 2)))))))
                           (push (cons d (list g radians dx dy)) fits))))))
          (loop for (nil g radians dx dy) in (stable-sort (nreverse fits) #'before-p)
                repeat *grain-sources-most*
                do (multiple-value-bind (r n) (shared-grain copy g radians dx dy)
                     (when r (push (list r n g) scores))))
          (setf scores (stable-sort (nreverse scores) #'> :key #'first))
          (destructuring-bind (&optional best next &rest others) scores
            (declare (ignore others))
            (and best next
                 (>= (first best) (shared-grain-bound (second best)))
                 (>= (- (first best) (first next)) *shared-grain-margin*)
                 (third best))))))))

(defparameter *stamp-colour-line-most* 3/2
  "How far, at most, in levels of red, green and blue, the colours of the
ink and edge of a stamp's figure typically lie from one straight line
through them (GLYPH-COLOUR-LINE) for the figure to be drawn by a program
and smoothed (DRAWN-GRAIN-P); half of what the page's characters give at
most. Stamps drawn in the page's face, red and blue, turned 0 to 10
degrees, blurred by 0.5 to 1 pixel, lay 0.33 to 1.03 from it, sharp ones
0.18 to 0.26; the characters of made-up scans of grain 1.5 to 9 lie 1.7 to
12, copies of them that keep their own colours 4.2 to 10.7.")

(defparameter *stamp-ground-apart-least* 8
  "How far apart, at least, in levels of red, green and blue, the line
through the colours of a stamp's figure (GLYPH-COLOUR-LINE) passes from the
page's paper at the paper's grey, for the figure to have been drawn on a
ground of its own (DRAWN-GRAIN-P). Red and blue stamps smoothed into white
pass 13 to 15 from it; a character of the page recoloured with its paper,
its greys mapped by one rule, passes through the paper, 0 to 2 from it.")

(defun glyph-colour-line (glyph paper)
  "How the colours of GLYPH's ink and edge (its pixels darker than three
quarters of the way from the darkest to the lightest grey of its box) lie
about the straight line through them, as two values: the root of the mean
squared distance from it, in levels of red, green and blue, and how far
from the grey PAPER (PAPER, PAPER, PAPER) the line passes where its grey is
PAPER. A program that draws a figure in one ink and smooths it into its
ground leaves every colour between the ink and the ground on that line; a
scanner's grain moves each colour on its own. NIL for a character without
colours (GLYPH-CHANNELS) or with fewer than 20 such pixels."
  (let ((pixels (glyph-pixels glyph)) (channels (glyph-channels glyph)))
    (when (and pixels channels)
      (let* ((across (glyph-width glyph))
             (w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph)))
             (lo 255) (hi 0) (points '()))
        (dotimes (j h)
          (dotimes (i w) (let ((g (aref pixels (+ (* (+ 2 j) across) 2 i)))) (setf lo (min lo g) hi (max hi g)))))
        (let ((cut (- hi (floor (- hi lo) 4))))
          (dotimes (j h)
            (dotimes (i w)
              (let ((k (+ (* (+ 2 j) across) 2 i)))
                (when (< (aref pixels k) cut)
                  (push (map 'vector (lambda (c) (float (aref c k) 1d0)) channels) points))))))
        (when (>= (length points) 20)
          (let* ((n (length points))
                 (mean (let ((m (make-array 3 :initial-element 0d0)))
                         (dolist (p points m) (dotimes (a 3) (incf (aref m a) (/ (aref p a) n))))))
                 (spread (make-array '(3 3) :initial-element 0d0))
                 (way (vector 1d0 1d0 1d0))
                 (most 0d0))
            (dolist (p points)
              (dotimes (a 3)
                (dotimes (b 3)
                  (incf (aref spread a b) (* (- (aref p a) (aref mean a)) (- (aref p b) (aref mean b)))))))
            ;; the line's way, the colours' widest spread, found by turning a way towards it again and again
            (dotimes (k 50)
              (let ((next (vector 0d0 0d0 0d0)))
                (dotimes (a 3) (dotimes (b 3) (incf (aref next a) (* (aref spread a b) (aref way b)))))
                (setf most (sqrt (reduce #'+ (map 'list (lambda (x) (* x x)) next))))
                (when (plusp most) (setf way (map 'vector (lambda (x) (/ x most)) next)))))
            (let* ((off (sqrt (max 0d0 (/ (- (+ (aref spread 0 0) (aref spread 1 1) (aref spread 2 2)) most) n))))
                   (grey-of (lambda (c) (+ (* 0.299d0 (aref c 0)) (* 0.587d0 (aref c 1)) (* 0.114d0 (aref c 2)))))
                   (rise (funcall grey-of way)))
              (values off
                      (and (> (abs rise) 1d-6)
                           (let* ((along (/ (- paper (funcall grey-of mean)) rise))
                                  (at (map 'vector (lambda (m v) (+ m (* along v))) mean way)))
                             (sqrt (reduce #'+ (map 'list (lambda (x) (expt (- x paper) 2)) at)))))))))))))

(defun drawn-colours-p (group others)
  "True when the characters of GROUP, a group of copies in a stamp, are
drawn in one ink smoothed into a ground of their own: the colours of each
lie on one line (GLYPH-COLOUR-LINE), typically within
*STAMP-COLOUR-LINE-MOST* of it and within half of what the characters
OTHERS, those of their page, typically give, and that line passes
*STAMP-GROUND-APART-LEAST* or more from the page's paper, the middle paper
of OTHERS' words. Up to 200 of OTHERS are measured."
  (let ((paper (median-of (loop for g in others for word = (glyph-word g) when word collect (word-box-paper word)))))
    (when paper
      (let ((lines (mapcar (lambda (g) (multiple-value-list (glyph-colour-line g paper))) group))
            (page (loop for g in others repeat 200
                        for off = (glyph-colour-line g paper)
                        when off collect off)))
        (and page (every #'first lines) (every #'second lines)
             (< (median-of (mapcar #'first lines)) (min *stamp-colour-line-most* (/ (median-of page) 2)))
             (>= (median-of (mapcar #'second lines)) *stamp-ground-apart-least*))))))

(defun drawn-grain-p (group page-glyphs &optional blank turn)
  "True when the characters of GROUP, a group of copies, carry less grain
than a scan gives: the middle of how far their cores stray (GLYPH-CORE-SPREAD,
the median deviation of their pixels no lighter than halfway from the darkest
to the lightest of their boxes) is under *STAMP-GRAIN-MOST* of the middle of
how far the cores of the characters of PAGE-GLYPHS, those of their page in no
stamp, of their size stray: as wide within a pixel and as tall within two,
else as wide, else all of them, three at least. A thin stroke strays unlike a
round letter, so a copied 1 is set beside the page's 1s and ls. Nor are
they drawn when their ink is that of one of PAGE-GLYPHS with its greys
changed, as laid or turned TURN degrees, the turn measured of their words,
within *REGREYED-SHARE* of how far the paper within the page's characters
strays (REGREYED-COPY-P): a character of the page recoloured, cut out of its
paper or turned, and copied into the stamp. Nor when one of them carries
the grain of one of PAGE-GLYPHS, turned up to *SOURCE-TURN-MOST* degrees
\(GRAIN-SOURCE), as a copy turned as well lies off its source's ink by
more than the scanner's grain while its grain still follows the source's.
A stamp a program drew carries no grain of the scan; a stamp pressed
on paper and scanned carries the page's, and so do the page's characters
copied into a stamp. The cores of a thin face's strokes are mostly their
smoothed edge, which strays as a scan's grain does, so they are also drawn
when the paper within their boxes strays less than *STAMP-PAPER-GRAIN-MOST*
of what the paper within the boxes of the page's characters does
\(GLYPH-PAPER-GRAIN), on a page whose paper strays by
*STAMP-PAPER-GRAIN-LEAST* or more: a figure drawn on a clean ground keeps
it clean, a piece of the page copied carries the page's grain on its
paper. Where fewer than three of the page's characters hold paper within
their boxes (thin type cut into strokes), BLANK, how far the page's blank
paper strays (BLANK-PAPER-NOISE), stands for theirs. Last, they are
drawn when their colours lie on one line from an ink to a ground of their
own (DRAWN-COLOURS-P), as a figure drawn and smoothed by a program, however
its smoothing makes its core stray."
  (flet ((size-p (g w h)
           (and (or (null h) (<= (abs (- (- (glyph-y1 g) (glyph-y0 g)) h)) 2))
                (<= (abs (- (- (glyph-x1 g) (glyph-x0 g)) w)) 1))))
    (let* ((one (first group)) (w (- (glyph-x1 one) (glyph-x0 one))) (h (- (glyph-y1 one) (glyph-y0 one)))
           (others (remove-if (lambda (g) (or (member g group) (null (glyph-spread g)))) page-glyphs))
           (peers (find-if (lambda (some) (>= (length some) 3))
                           (list (remove-if-not (lambda (g) (size-p g w h)) others)
                                 (remove-if-not (lambda (g) (size-p g w nil)) others)
                                 others)))
           (own (remove nil (mapcar #'glyph-spread group))))
      (let ((page (let ((within (remove nil (mapcar #'glyph-paper-grain others))))
                    (if (>= (length within) 3) (median-of within) blank))))
        (and own peers
             (or (< (median-of own) (* *stamp-grain-most* (median-of (mapcar #'glyph-spread peers))))
                 (let ((paper (remove nil (mapcar #'glyph-paper-grain group))))
                   (and paper page (>= page *stamp-paper-grain-least*)
                        (< (median-of paper) (* *stamp-paper-grain-most* page))))
                 (drawn-colours-p group others))
             (notany (lambda (g)
                       (and page (not (member g group))
                            (or (regreyed-copy-p one g (* *regreyed-share* page))
                                (and turn (regreyed-copy-p one g (* *regreyed-share* page) turn)))))
                     page-glyphs)
             ;; turned as well as recoloured, its ink no longer fits its source closely, but its grain still follows
             (not (grain-source one (remove-if (lambda (g) (member g group)) page-glyphs))))))))

(defun drawn-stamp-p (characters group-of)
  "True when CHARACTERS, those of a stamp, are as a program draws them, by
their shapes; how much grain each group of copies in it carries is judged
apart (DRAWN-GRAIN-P). The stamp holds a shape repeated. And every character with
another of its shape in the stamp is in the same group of copies as all the
others of that shape (GROUP-OF, a character's group of copies, the same pixel
for pixel, or nearer one another than prints of a character are, as where the
paper shows through a stamp laid over it), or the same pixel for pixel as all
of them, as a figure too plain to be judged a copy, a 1, is drawn. A program draws every print of a
shape alike, and a JPEG it is saved as keeps them so, its losses the same on
each; no two prints of a rubber stamp pressed on paper and scanned are, the
scanner's grain apart in each, so a figure copied over another in it leaves the
others of its shape unlike it. Of the same shape are characters of one size, a
pixel either way, alike to three times their page's characters of one shape
\(twenty grey levels at least); a stamp of more than 200 characters is no stamp."
  (when (<= (length characters) 200)
    (let ((shape (make-hash-table :test 'eq)))
      (labels ((root (g) (let ((up (gethash g shape g))) (if (eq up g) g (setf (gethash g shape) (root up)))))
               (alike-p (a b)
                 (or (and (gethash a group-of) (eq (gethash a group-of) (gethash b group-of)))
                     (and (<= (abs (- (- (glyph-x1 a) (glyph-x0 a)) (- (glyph-x1 b) (glyph-x0 b)))) 1)
                          (<= (abs (- (- (glyph-y1 a) (glyph-y0 a)) (- (glyph-y1 b) (glyph-y0 b)))) 1)
                          (glyph-pixels a) (glyph-pixels b)
                          (progn
                            (unless (glyph-darker a) (glyph-measured a))
                            (unless (glyph-darker b) (glyph-measured b))
                            (glyph-distance a b (expt (max 20 (* 3 (or (glyph-alike a) 0))) 2)))))))
        (loop for (a . rest) on characters
              do (dolist (b rest)
                   (when (alike-p a b)
                     (let ((ra (root a)) (rb (root b))) (unless (eq ra rb) (setf (gethash ra shape) rb))))))
        (let ((shapes (make-hash-table :test 'eq)))
          (dolist (g characters) (push g (gethash (root g) shapes)))
          (let ((repeated (loop for of-shape being the hash-values of shapes when (rest of-shape) collect of-shape)))
            (and repeated
                 (every (lambda (of-shape)
                          (let ((group (gethash (first of-shape) group-of)) (one (first of-shape)))
                            (or (and group (every (lambda (g) (eq (gethash g group-of) group)) of-shape))
                                (and (glyph-pixels one)
                                     (every (lambda (g) (and (glyph-pixels g) (same-pixels-p one g)))
                                            (rest of-shape))))))
                        repeated))))))))

(defun words-turned (glyphs)
  "How far each word of the characters GLYPHS is turned against its page's
lines, in degrees, as a hash table by WORD-BOX. It is the line through the
middles of its characters' bottoms, for a word of four characters or more
\(fewer, a letter's tail below the line could turn it), against the page's, the
middle of its words' (none for a word of fewer characters). A stamp laid turned
turns its characters' line with it, however each is cut."
  (let ((by-word (make-hash-table :test 'eq)) (angles (make-hash-table :test 'eq)))
    (dolist (g glyphs) (when (glyph-word g) (push g (gethash (glyph-word g) by-word))))
    (maphash (lambda (word members)
               (when (>= (length members) 4)
                 (let* ((n (length members))
                        (xs (mapcar (lambda (g) (/ (+ (glyph-x0 g) (glyph-x1 g)) 2)) members))
                        (ys (mapcar #'glyph-y1 members))
                        (mx (/ (reduce #'+ xs) n)) (my (/ (reduce #'+ ys) n))
                        (across (reduce #'+ (mapcar (lambda (x) (expt (- x mx) 2)) xs))))
                   (when (plusp across)
                     (setf (gethash word angles)
                           (* (/ 180 pi)
                              (atan (/ (reduce #'+ (mapcar (lambda (x y) (* (- x mx) (- y my))) xs ys)) across))))))))
             by-word)
    (let ((page (median-of (loop for a being the hash-values of angles collect a))))
      (when page
        (maphash (lambda (word angle) (setf (gethash word angles) (- angle page))) angles)))
    angles))

(defun colour-distance (a b)
  "How far apart the colours A and B, each (RED GREEN BLUE) of 0 to 255, are:
the root of the summed squares of their differences."
  (sqrt (reduce #'+ (mapcar (lambda (x y) (expt (- x y) 2)) a b))))

(defun backed-pieces (pieces copies scans)
  "PIECES (BLOCK-PIECEs of all the scanned pictures of a file), each strong
:OWN one kept strong only when another sign agrees on its region. One sign
is a character within it one of a group of COPIES (COPIED-GLYPHS) the same
pixel for pixel, a strong sign, whose others lie outside it (copies all
within it may be a stamp's repeated figures). Else it is a word within it
unlike the rest both in how it looks and where it sits, on a picture of
SCANS whose grain was judged. One that nothing else agrees on is worth a
look, its BACKING :ALONE: a stamp, a date or a patch kept as a JPEG and
laid into a page kept without loss by honest software carries the same
blocks. PIECES."
  (dolist (p pieces pieces)
    (when (and (eq (block-piece-kind p) :own) (eq (block-piece-strength p) :strong))
      (setf (block-piece-backing p)
            (cond ((loop for (group nil nil nil strength) in copies
                         thereis (and (eq strength :strong)
                                      (some (lambda (g) (glyph-in-piece-p p g)) group)
                                      (notevery (lambda (g) (glyph-in-piece-p p g)) group)))
                   :copies)
                  ((some (lambda (scan)
                           (and (scan-picture-judged scan)
                                (some (lambda (odd)
                                        (let ((word (car odd)))
                                          (and (looks-and-sits-unlike-p (cdr odd) (scan-picture-paper scan))
                                               (box-in-piece-p p (page-index (scan-picture-page scan))
                                                               (mark-object (scan-picture-mark scan))
                                                               (word-box-x0 word) (word-box-y0 word)
                                                               (word-box-x1 word) (word-box-y1 word)))))
                                      (scan-picture-odd scan))))
                         scans)
                   :words)
                  (t :alone)))
      (when (eq (block-piece-backing p) :alone)
        (setf (block-piece-strength p) :look)))))

(defun jpeg-pieces (glyphs &key jpeg)
  "The regions of a scanned picture, from its characters GLYPHS, whose JPEG
traces are unlike the rest of the picture's (BLOCK-PIECEs); JPEG true for a
picture kept as a JPEG, its grid its first pixel's. On a picture that carries a
JPEG grid (PICTURE-BLOCK-GRID): characters whose blocks on it have
*BLOCK-OFF-GRID-MOST* or fewer of their 28 finest frequencies flat, and six
times the characters' spread fewer than they typically do, laid in after the
picture was saved as a JPEG. That is worth a look, for a region of 3 characters
or more. There is none when more than a twentieth of the characters are so. On a
picture kept without loss, its blocks typically far from flat: the characters
that carry a JPEG block at a placement (SUPPORTED-WAYS), pieces saved as JPEGs
of their own and laid back. A region is named when 3 or more of its characters,
and half of those with a block there, carry blocks at one placement (or a pixel
from it). No more than a quarter of the characters within 40 pixels about it may
do so, whose blocks there are typically *BLOCK-PIECE-ABOVE* less flat than the
region's. The region is strong, kept so only when another sign agrees on it
\(BACKED-PIECES), when fewer than one in fifty of the picture's other characters
do and half of them or more have blocks measured (a page drawn on white has its
blocks passed over). Else it is worth a look. A character or two alone are not
told, and nothing is when more than a twentieth of the picture's characters
carry such blocks: a scan smooth in places, or a JPEG made larger, smaller or
turned. A picture smooth at every placement, or a JPEG without a grid of its
blocks, tells nothing. Characters within 16 pixels of each other across (a
letter between them, as in a word cut out) and 3 down are one region. NIL for a
picture of fewer than 40 characters, too few to tell its blocks."
  (when (>= (length glyphs) 40)
    (let* ((rows (make-array 0 :element-type 'double-float))
           (scores (loop for g in glyphs for k from 0
                         do (when (zerop (mod k 500)) (job-check-stop))
                         collect (multiple-value-bind (s room) (glyph-block-scores g rows)
                                   (setf rows room)
                                   s)))
           (n (length glyphs)))
      (flet ((piece (g &rest more)
               (apply #'make-block-piece (append more (list :page (glyph-page g) :picture (glyph-picture g)
                                                            :x0 (- (glyph-x0 g) 2) :y0 (- (glyph-y0 g) 2)
                                                            :x1 (+ (glyph-x1 g) 2) :y1 (+ (glyph-y1 g) 2))))))
        (multiple-value-bind (grid typical gridded spread) (picture-block-grid scores (and jpeg 0))
          (cond
            (gridded
             (let ((found (loop for g in glyphs for s in scores
                                for v = (aref s grid)
                                when (and (<= 0 v *block-off-grid-most*) (<= v (- typical (* 6 (max 1 spread)))))
                                  collect (piece g :kind :off-grid :strength :look :flat v :usual typical
                                                   :way (list (mod grid 8) (floor grid 8))))))
               (unless (> (length found) (* 1/20 n))
                 (remove-if (lambda (r) (< (block-piece-characters r) 3)) (join-block-pieces found 16)))))
            ((or jpeg (>= typical *block-smooth-least*)) nil)
            (t
             (let* ((ways (mapcar #'supported-ways scores))
                    (usual (counted-middle (let ((c (make-array 29 :initial-element 0)))
                                             (dolist (s scores c) (incf (aref c (max 0 (reduce #'max s))))))))
                    (marked (loop for g in glyphs for w in ways when w count t))
                    ;; the characters of a word cut out lie a letter apart at most, some of them unmarked
                    (regions (if (> marked (* 1/20 n))
                                 (return-from jpeg-pieces nil)
                                 (join-block-pieces (loop for g in glyphs for w in ways
                                                          when w
                                                          collect (piece g :kind :own :strength :look :flat 0
                                                                           :usual usual))
                                                    16))))
               (dolist (r regions (remove-if-not #'block-piece-strength regions))
                 (let ((inside '()) (outside '()) (around '()) (measured 0))
                   ;; every character of the region's box, marked or not; those within 40 pixels of it; and
                   ;; the marked ones of the rest of the picture
                   (flet ((within (g margin)
                            (and (< (- (glyph-x0 g) 2 margin) (block-piece-x1 r))
                                 (> (+ (glyph-x1 g) 2 margin) (block-piece-x0 r))
                                 (< (- (glyph-y0 g) 2 margin) (block-piece-y1 r))
                                 (> (+ (glyph-y1 g) 2 margin) (block-piece-y0 r)))))
                     (loop for g in glyphs for s in scores for w in ways
                           do (cond ((within g 0) (push (list s w) inside))
                                    (t (when (within g 40) (push (list s w) around))
                                       (when (find-if-not #'minusp s) (incf measured))
                                       (when w (push w outside))))))
                   ;; the placement most of its characters carry blocks at
                   (let ((best nil) (most 0))
                     (dolist (entry inside)
                       (dolist (w (second entry))
                         (let ((some (count-if (lambda (e) (some (lambda (v) (near-ways-p v w)) (second e))) inside)))
                           (when (> some most) (setf best w most some)))))
                     (let ((showing (count-if (lambda (e)
                                                (loop for v below 64
                                                      thereis (and (near-ways-p v best) (>= (aref (first e) v) 0))))
                                              inside))
                           (elsewhere (count-if (lambda (ws) (some (lambda (v) (near-ways-p v best)) ws)) outside))
                           ;; the characters about it: how many have a block there, and how many carry one
                           (near (count-if (lambda (e) (loop for v below 64 thereis (and (near-ways-p v best)
                                                                                         (>= (aref (first e) v) 0))))
                                                                                         around))
                           (near-carrying (count-if (lambda (e) (some (lambda (v) (near-ways-p v best)) (second e)))
                                                    around))
                           ;; how flat the blocks about it typically are there
                           (near-flat (median-of (or (loop for (s) in around
                                                           for v = (loop for w below 64
                                                                         when (near-ways-p w best)
                                                                         maximize (aref s w))
                                                           when (>= v 0) collect v)
                                                     '(0)))))
                       (setf (block-piece-way r) (list (mod best 8) (floor best 8))
                             (block-piece-characters r) (length inside)
                             (block-piece-showing r) most
                             (block-piece-flat r) (loop for (s w) in inside
                                                        maximize (or (loop for v in w
                                                                           when (near-ways-p v best)
                                                                           maximize (aref s v)) 0))
                             (block-piece-above r) (loop for (s w) in inside
                                                         maximize (or (loop for v in w
                                                                            when (near-ways-p v best)
                                                                            maximize (block-support s v)) 0)))
                       (setf (block-piece-strength r)
                             (cond ((or (< most 3) (< (* 2 most) showing) (> (* 4 near-carrying) near)) nil)
                                   ((> near-flat (- (block-piece-flat r) *block-piece-above*)) nil)
                                   ;; the rest of the picture measured, not passed over as black and white
                                   ((and (< elsewhere (* 1/50 (- n (length inside))))
                                         (>= (* 2 measured) (- n (length inside)))) :strong)
                                   (t :look)))))))))))))))

(defun join-block-pieces (pieces &optional (across 3))
  "PIECES (BLOCK-PIECEs of one picture, of one kind) with those whose boxes
touch or are within ACROSS pixels of each other across and three down made
one, its box round theirs, its FLAT the most of theirs (the fewest for
:OFF-GRID ones); the rest of what it says of them JPEG-PIECES works out
for the region."
  (flet ((near-p (a b)
           (and (<= (block-piece-x0 a) (+ (block-piece-x1 b) across))
                (<= (block-piece-x0 b) (+ (block-piece-x1 a) across))
                (<= (block-piece-y0 a) (+ (block-piece-y1 b) 3)) (<= (block-piece-y0 b) (+ (block-piece-y1 a) 3))))
         (into (a b)
           (setf (block-piece-x0 a) (min (block-piece-x0 a) (block-piece-x0 b))
                 (block-piece-y0 a) (min (block-piece-y0 a) (block-piece-y0 b))
                 (block-piece-x1 a) (max (block-piece-x1 a) (block-piece-x1 b))
                 (block-piece-y1 a) (max (block-piece-y1 a) (block-piece-y1 b))
                 (block-piece-flat a) (funcall (if (eq (block-piece-kind a) :own) #'max #'min)
                                               (block-piece-flat a)
                                               (block-piece-flat b))
                 (block-piece-characters a) (+ (block-piece-characters a) (block-piece-characters b)))))
    (let ((out '()))
      (dolist (p pieces)
        ;; P joins every region it is near, and they one another through it
        (let ((near (remove-if-not (lambda (o) (near-p p o)) out)))
          (setf out (set-difference out near))
          (dolist (o near) (into p o))
          (push p out)))
      (sort out (lambda (a b) (or (< (block-piece-y0 a) (block-piece-y0 b))
                                  (and (= (block-piece-y0 a) (block-piece-y0 b))
                                       (< (block-piece-x0 a) (block-piece-x0 b)))))))))

(defun picture-page-box (mark box width height &optional tilt)
  "The box (X0 Y0 X1 Y1) on the page of BOX, pixels of the picture MARK draws,
WIDTH by HEIGHT pixels: the picture fills its box on the page, its first row at
the top. BOX is a GLYPH, a WORD-BOX, a BLOCK-PIECE or a list (X0 Y0 X1 Y1) of
pixels, the last excluded. With TILT, BOX's rows are those of the picture made
level, its lines falling TILT for each pixel across, as a word's are
\(PICTURE-WORDS)."
  (multiple-value-bind (x0 y0 x1 y1)
      (etypecase box
        (glyph (values (glyph-x0 box) (glyph-y0 box) (glyph-x1 box) (glyph-y1 box)))
        (word-box (values (word-box-x0 box) (word-box-y0 box) (word-box-x1 box) (word-box-y1 box)))
        (block-piece (values (block-piece-x0 box) (block-piece-y0 box) (block-piece-x1 box) (block-piece-y1 box)))
        (list (values-list box)))
    (let ((fall (* (or tilt 0) x0)) (across (- (mark-x1 mark) (mark-x0 mark))) (down (- (mark-y1 mark) (mark-y0 mark))))
      (list (+ (mark-x0 mark) (* (/ x0 width) across))
            (- (mark-y1 mark) (* (/ (+ y1 fall) height) down))
            (+ (mark-x0 mark) (* (/ x1 width) across))
            (- (mark-y1 mark) (* (/ (+ y0 fall) height) down))))))

(defun picture-filters (pdf revision number)
  "The filters of the picture object NUMBER of REVISION, the names its
/Filter gives, in their order."
  (let* ((object (revision-object pdf revision number))
         (filter (and object (deref pdf revision (pdf-get (pdf-indirect-get object :value) "/Filter")))))
    (if (consp filter) (pdf-array-items filter) (list filter))))

(defun picture-jpeg-p (pdf revision number)
  "True when the picture object NUMBER of REVISION is kept as a JPEG: its
filter, or one of its filters, is /DCTDecode; or, for a picture file
examined as a page, when that file is a JPEG."
  (if *picture-file*
      (eq (picture-file-kind *picture-file*) :jpeg)
      (some #'jpeg-filter-p (picture-filters pdf revision number))))

(defparameter *block-piece-words*
  '(((:own :strong) #.(concatenate 'string "carries JPEG blocks of its own, which the rest of the scan lacks, and "
                                           "another sign of editing on the same place: a piece saved apart and laid in")
     #.(concatenate 'string "carry JPEG blocks of their own, which the rest of the scan lacks, and another sign of "
                            "editing on the same places: pieces saved apart and laid in")
     #.(concatenate 'string "A JPEG keeps a picture as blocks of 8 by 8 pixels and sets most of each block's finest "
                            "detail to nothing; a scan kept without loss keeps its grain in every block. Several "
                            "characters of one region whose blocks are flat at one placement, and far less flat a "
                            "pixel or more from it, carry a JPEG's grid of their own, which no other part of the page "
                            "does: they were saved as a JPEG on their own and laid back. Here another sign agrees on "
                            "the same region: characters within it are copies of others the same pixel for pixel, or a "
                            "word within it is unlike the rest both in how it looks and where it sits. Together they "
                            "tell of a piece cut out to be changed or moved. A smooth or blurred patch of a scan is "
                            "flat however a block sits on it."))
    ((:own :alone) #.(concatenate 'string "carries JPEG blocks of its own, which the rest of the scan lacks: a piece "
                                          "saved apart and laid in, or a stamp, a date or a patch kept as a JPEG and "
                                          "laid into the page by the software that made it")
     #.(concatenate 'string "carry JPEG blocks of their own, which the rest of the scan lacks: pieces saved apart and "
                            "laid in, or stamps, dates or patches kept as JPEGs and laid into the page by the software "
                            "that made it")
     #.(concatenate 'string "A JPEG keeps a picture as blocks of 8 by 8 pixels and sets most of each block's finest "
                            "detail to nothing; a scan kept without loss keeps its grain in every block. Several "
                            "characters of one region whose blocks are flat at one placement, and far less flat a "
                            "pixel or more from it, carry a JPEG's grid of their own, which no other part of the page "
                            "does: they were saved as a JPEG on their own and laid into the page. A piece cut out to "
                            "be changed or moved is saved so; so is a stamp, a date or a patch of a scan that an "
                            "office's software keeps as a JPEG and lays into a page kept without loss, honestly, and "
                            "the pixels do not tell the two apart. Alone, it is worth a look; it is a strong sign only "
                            "when another sign agrees on the same region, characters within it copies of others the "
                            "same pixel for pixel, or a word within it unlike the rest both in how it looks and where "
                            "it sits."))
    ((:own :look) #.(concatenate 'string "has a block flatter at one placement than at the others, as a JPEG's are: a "
                                         "piece saved apart and laid in, or the scan smoother there by chance")
     #.(concatenate 'string "have blocks flatter at one placement than at the others, as a JPEG's are: pieces saved "
                            "apart and laid in, or the scan smoother there by chance")
     #.(concatenate 'string "A character whose block is flat at one placement and far less flat a pixel or more from "
                            "it may carry a JPEG's block of its own. Alone, or with few others of its region at that "
                            "placement, it is a sign worth a look: a single character saved as a JPEG is told only so "
                            "far."))
    ((:off-grid :look) #.(concatenate 'string "lacks the JPEG blocks the rest of the scan carries on its grid: a piece "
                                              "laid in after the scan was saved as a JPEG")
     "lack the JPEG blocks the rest of the scan carries on its grid: pieces laid in after the scan was saved as a JPEG"
     #.(concatenate 'string "A scan saved as a JPEG has its blocks of 8 by 8 pixels on one grid across the whole page, "
                            "and the finest detail of every block set to nothing. A region whose blocks on that grid "
                            "keep their finest detail was put there after the page was saved: a piece laid in from "
                            "elsewhere or from the page itself, or text a scanner's software stamped on afterwards.")))
  "What is said of the regions of JPEG-PIECES, by (KIND STRENGTH), or (:OWN
:ALONE) for a region with blocks of its own that nothing else agrees on
\(BACKED-PIECES): of one region, of several, and the rule.")

(defun laid-piece-words (words)
  "WORDS, of *BLOCK-PIECE-WORDS* (ONE SEVERAL RULE), for regions LAID
\(LAID-PIECES): the honest cause a stamp, a logo or a signature laid on the
pages by software named beside the others, as for characters copied so."
  (destructuring-bind (one several rule) words
    (list (concatenate 'string one ", or a stamp, a logo or a signature laid on the pages by software")
          (concatenate 'string several ", or a stamp, a logo or a signature laid on the pages by software")
          (concatenate 'string rule
                       " These stand at the same place on two pages or more, or are taller than the type of their "
                       "pages: a stamp, a logo or a signature kept as a picture and laid on each page by a program "
                       "carries the JPEG blocks of the picture it was kept as, as offices do honestly, as well as a "
                       "piece laid to deceive. Whether such a mark belongs there is for the examiner to judge."))))

(defun check-scan-pieces (pdf revision)
  "Regions of a scanned page whose JPEG traces are unlike the rest of it
\(JPEG-PIECES). One kind is pieces cut out of the scan and saved as JPEGs
of their own before they were laid back, their 8-by-8 blocks on a grid of
their own where the page has none. These are strong only when another sign
agrees on the same region (BACKED-PIECES). The other kind is pieces laid
in after the scan was saved as a JPEG, lacking the blocks the rest of the
page carries on its grid."
  (let* ((reading (scans-read pdf revision))
         (scans (scan-reading-scans reading)) (pieces (scan-reading-pieces reading)))
    (let ((out '()) (groups '()))
      (dolist (p pieces)
        (let* ((key (list (block-piece-page p)
                          (block-piece-picture p)
                          (block-piece-kind p)
                          (block-piece-strength p)
                          (block-piece-laid p)
                          (eq (block-piece-backing p) :alone)))
               (entry (assoc key groups :test #'equal)))
          (if entry (push p (cdr entry)) (push (list key p) groups))))
      (loop for ((page-index picture kind strength laid alone) . some) in (reverse groups)
            do (let* ((some (reverse some))
                      (scan (find-if (lambda (scan) (and (eql (mark-object (scan-picture-mark scan)) picture)
                                                         (eql (page-index (scan-picture-page scan)) page-index)))
                                     scans))
                      (m (scan-picture-mark scan)) (width (scan-picture-width scan)) (height (scan-picture-height scan))
                      (words (let ((words (cdr (assoc (if alone (list kind :alone) (list kind strength))
                                                      *block-piece-words*
                                                      :test #'equal))))
                               (cond ((not laid) words)
                                     ;; the honest software that lays a piece is named already: the rule says more
                                     (alone (list (first words) (second words) (third (laid-piece-words words))))
                                     (t (laid-piece-words words))))))
                 (labels ((spot (p)
                            ;; where P is on the page
                            (destructuring-bind (x0 y0 x1 y1) (picture-page-box m p width height)
                              (declare (ignore x1 y1))
                              (format nil "at x ~d, y ~d" (round x0) (round y0))))
                          (at (p)
                            ;; where P is on the page, and in its picture
                            (format nil "~a (~a)" (spot p)
                                    (pixels-words m (block-piece-x0 p) (block-piece-y0 p) (block-piece-x1 p)
                                                  (block-piece-y1 p)))))
                   (push (found strength
                                (if (rest some)
                                    (format nil
                                            "~d regions of page ~d, in ~a, ~a."
                                            (length some)
                                            (1+ page-index)
                                            (scan-named m)
                                            (second words))
                                    (let ((p (first some)))
                                      (format nil "A region of page ~d, about ~d by ~d pixels ~a, in ~a, ~a."
                                              (1+ page-index)
                                              (- (block-piece-x1 p) (block-piece-x0 p))
                                              (- (block-piece-y1 p) (block-piece-y0 p))
                                              (spot p)
                                              (scan-named m :short t)
                                              (first words))))
                                :rule (third words)
                                :revision (revision-index revision) :page (1+ page-index)
                                :objects (objects-at m (mapcar (lambda (p) (cons (block-piece-y0 p) (block-piece-y1 p)))
                                                               some))
                                :details (loop for p in some
                                               collect (if (eq kind :own)
                                                           (format nil "~a, ~d character~:p, ~d with a block at column ~
                                                                        ~d, row ~d of every eight pixels: up to ~d of ~
                                                                        its 28 finest frequencies flat, ~d more than ~
                                                                        at its other placements; the scan's ~
                                                                        characters' flattest typically ~d"
                                                                   (at p)
                                                                   (block-piece-characters p)
                                                                   (block-piece-showing p)
                                                                   (first (block-piece-way p))
                                                                   (second (block-piece-way p))
                                                                   (block-piece-flat p)
                                                                   (block-piece-above p)
                                                                   (block-piece-usual p))
                                                           (format nil "~a, ~d character~:p: its blocks on the scan's ~
                                                                        grid with ~d of their 28 finest frequencies ~
                                                                        flat, the scan's characters' typically ~d"
                                                                   (at p)
                                                                   (block-piece-characters p)
                                                                   (block-piece-flat p)
                                                                   (block-piece-usual p))))
                                :evidence (pictures-evidence pdf revision
                                                             (objects-at m (mapcar (lambda (p)
                                                                                     (cons (block-piece-y0 p)
                                                                                           (block-piece-y1 p)))
                                                                                   some)))
                                :method :piece-laid-in)
                         out))))
      (nreverse out))))
