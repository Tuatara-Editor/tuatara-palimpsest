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

(defparameter *stamp-turn-least* 10
  "How many degrees, at least, a stamp's characters are turned against the
lines of their page.")

(defparameter *stamp-characters-most* 2000
  "The most stamp-like characters a picture may hold for them to be gathered
into stamps (STAMP-COPIES); a picture of more is no page with a stamp on it.")

(defun stamp-copies (copies scans)
  "COPIES (groups of COPIED-GLYPHS), each strong group whose characters are
all stamp-like and lie in one stamp that a program drew (DRAWN-STAMP-P),
made worth a look, a stamp drawn by
software (WHY :STAMP). A character is stamp-like in an ink unlike its
page's characters' (*STAMP-INK-APART*), or turned *STAMP-TURN-LEAST*
degrees or more against its page's lines (WORDS-TURNED); a stamp is stamp-like characters
of one picture each within three of its characters' heights of another. A
program draws a stamp's figures the same every time, as an office's
software lays a date or a received stamp. Characters typed in the page's
own ink and at its angle stay strong, as does a group any one of whose
characters lies outside the stamp: a stamp's figure the same as one in the
text. SCANS gives the characters of each picture, of which the typical ink
is measured. COPIES."
  (let ((stamp-of (make-hash-table :test 'eq)) (stamps 0))
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
                                          (or (and turn (>= (abs turn) *stamp-turn-least*))
                                              (and colour (>= (colour-distance colour middle) apart)))))
                                      glyphs)))
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
                                     (drawn-stamp-p (gethash stamp members) group-of))))
                         t))
            (setf (fifth c) :look (sixth c) :stamp)))))))

(defparameter *stamp-grain-most* 0.6
  "How far, at most, the cores of a stamp drawn by a program typically stray
\(GLYPH-CORE-SPREAD), as a share of how far its page's characters' cores
typically do: drawn stamps saved as JPEGs measured 4 to 10 grey levels on
pages whose text strays by 18, stamps pressed on paper and scanned 15 to
20 on pages of 19.")

(defun drawn-stamp-p (characters group-of)
  "True when CHARACTERS, those of a stamp, are as a program draws them. The
middle of how far their cores stray (GLYPH-CORE-SPREAD, the median deviation of
their pixels no lighter than halfway from the darkest to the lightest of their
boxes) is under *STAMP-GRAIN-MOST* of how far their page's characters' cores
typically stray, as no stamp pressed on paper and scanned, carrying the page's
grain, is. The stamp holds two shapes or more repeated. And every character with
another of its shape in the stamp is in the same group of copies as all the
others of that shape (GROUP-OF, a character's group of copies, the same pixel
for pixel, or nearer one another than prints of a character are, as where the
paper shows through a stamp laid over it). A program draws every print of a
shape alike, and a JPEG it is saved as keeps them so, its losses the same on
each; no two prints of a rubber stamp pressed on paper and scanned are, the
scanner's grain apart in each, so a figure copied over another in it leaves the
others of its shape unlike it. Of the same shape are characters of one size, a
pixel either way, alike to three times their page's characters of one shape
\(twenty grey levels at least); a stamp of more than 200 characters is no stamp."
  (when (and (<= (length characters) 200)
             (let ((spreads (remove nil (mapcar #'glyph-spread characters))))
               (and spreads (< (median-of spreads) (* *stamp-grain-most* (glyph-typical (first characters)))))))
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
            (and (>= (length repeated) 2)
                 (every (lambda (of-shape)
                          (let ((group (gethash (first of-shape) group-of)))
                            (and group (every (lambda (g) (eq (gethash g group-of) group)) of-shape))))
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

(defun picture-jpeg-p (pdf revision number)
  "True when the picture object NUMBER of REVISION is kept as a JPEG: its
filter, or one of its filters, is /DCTDecode."
  (let* ((object (revision-object pdf revision number))
         (filter (and object (deref pdf revision (pdf-get (pdf-indirect-get object :value) "/Filter")))))
    (some #'jpeg-filter-p
          (if (consp filter) (pdf-array-items filter) (list filter)))))

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
                            (format nil "~a (pixels ~d,~d to ~d,~d of picture ~d)" (spot p)
                                    (block-piece-x0 p)
                                    (block-piece-y0 p)
                                    (block-piece-x1 p)
                                    (block-piece-y1 p)
                                    picture)))
                   (push (found strength
                                (if (rest some)
                                    (format nil
                                            "~d regions of page ~d, in the scanned picture ~a (object ~d), ~a."
                                            (length some)
                                            (1+ page-index)
                                            (mark-text m)
                                            picture
                                            (second words))
                                    (let ((p (first some)))
                                      (format nil "A region of page ~d, about ~d by ~d pixels ~a, in the scanned ~
                                                   picture ~a, ~a." (1+ page-index)
                                              (- (block-piece-x1 p) (block-piece-x0 p))
                                              (- (block-piece-y1 p) (block-piece-y0 p))
                                              (spot p)
                                              (mark-text m)
                                              (first words))))
                                :rule (third words)
                                :revision (revision-index revision) :page (1+ page-index) :objects (list picture)
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
                                :evidence (nth-value 0 (object-evidence pdf revision picture))
                                :method :piece-laid-in)
                         out))))
      (nreverse out))))
