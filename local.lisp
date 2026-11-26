;;;; local.lisp --- A scanned page's local traces set beside the rest of the page, region by region.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A page saved as a JPEG keeps the traces of that save all over: every
;;;; block of 8 by 8 pixels was rounded to the same steps. A region laid in
;;;; after the save, a word typed again over healed paper, a stamp or a
;;;; signature put on, a patch of cloned paper, lacks them, though it may
;;;; match the page's look, ink and grain. The page is cut into tiles of
;;;; 16 by 16 pixels; what each holds of the traces is set beside what the
;;;; whole page holds, and tiles that fall short together make a region.
;;;; The same tiles tell paper painted over: a scanner leaves grain on all
;;;; the paper it reads, a box or a brush of one colour none.

(in-package #:palimpsest)

(defstruct (trace-region (:copier nil) (:predicate nil))
  "A region of a scanned picture whose traces of a JPEG save fall short of
the rest of the page's (LOCAL-TRACES). KIND is :LATTICE, a picture kept
without loss whose blocks were once a JPEG's (LATTICE-TILES), or
:REQUANTISED, a JPEG saved twice whose blocks show the first save's steps
\(REQUANTISED-TILES), or :FLAT, paper with far less grain than the page's,
painted over (FLAT-REGIONS). X0 Y0 X1 Y1 are its pixels in the picture (the
last excluded), TILES how many tiles of 16 by 16 pixels it holds. SCORE is
how many standard deviations it stands from the page (TRACE-REGIONS), or for
:FLAT how many times less its paper strays than the page's. OWN and USUAL
are the measure in the region and over the page: the mean cosine of a whole
turn times its frequencies for :LATTICE, the share of its frequencies on the
first save's empty steps for :REQUANTISED, the mean square of how far its
paper's pixels lie from their mean in each tile for :FLAT."
  kind x0 y0 x1 y1 (tiles 0) (score 0d0) (own 0d0) (usual 0d0))

(defparameter *trace-tile* 16
  "The side, in pixels, of the tiles a scanned page is cut into for its local
traces (LOCAL-TRACES): two JPEG blocks across and down, as small as a figure
or a short word, large enough for a tile to hold a few dozen frequencies.")

(defparameter *trace-tile-least* 2d0
  "How many standard deviations, at least, a tile must fall short of its
page for it to join a region (TRACE-REGIONS). The region is then judged on
all of its tiles together.")

(defun trace-regions (counts sums deviation tiles-across x0 y0 kind
                      &key (rate (/ (reduce #'+ sums) (max 1 (reduce #'+ counts)))) (least *trace-tile-least*))
  "The regions of a page cut into tiles (*TRACE-TILE*), TILES-ACROSS to a
row, the first at pixel X0, Y0, whose measure falls short of the page's.
COUNTS and SUMS hold, for each tile, how many values were measured and
their sum; DEVIATION, called with the page's RATE, a count and a sum, says
how many standard deviations that sum stands from what the rate gives for
so many values, more being further on the side that falls short. RATE is
the sum of SUMS over the sum of COUNTS unless given. A tile of 4 values or
more standing LEAST or more (*TRACE-TILE-LEAST* unless given) joins its
neighbours that do, across and down and on the slant; each region so joined
is judged on the counts and sums of all its tiles together. As
TRACE-REGIONs of KIND, the furthest first; their USUAL is RATE."
  (declare (type (simple-array fixnum (*)) counts) (type (simple-array double-float (*)) sums)
           (type fixnum tiles-across))
  (let* ((n (length counts)) (z (make-array n :element-type 'double-float :initial-element 0d0))
         (seen (make-array n :element-type 'bit :initial-element 0)) (out '()))
    (dotimes (k n)
      (when (>= (aref counts k) 4)
        (setf (aref z k) (funcall deviation rate (aref counts k) (aref sums k)))))
    (dotimes (k n)
      (when (and (zerop (sbit seen k)) (>= (aref z k) least))
        (let ((stack (list k)) (count 0) (sum 0d0) (tiles 0)
              (left most-positive-fixnum) (top most-positive-fixnum) (right -1) (bottom -1))
          (setf (sbit seen k) 1)
          (loop while stack
                do (let* ((at (pop stack)) (tx (mod at tiles-across)) (ty (floor at tiles-across)))
                     (incf tiles) (incf count (aref counts at)) (incf sum (aref sums at))
                     (setf left (min left tx) right (max right tx) top (min top ty) bottom (max bottom ty))
                     (loop for dy from -1 to 1
                           do (loop for dx from -1 to 1
                                    for nx = (+ tx dx) for ny = (+ ty dy) for next = (+ (* ny tiles-across) nx)
                                    when (and (< -1 nx tiles-across) (< -1 next n) (zerop (sbit seen next))
                                              (>= (aref z next) least))
                                      do (setf (sbit seen next) 1) (push next stack)))))
          (push (make-trace-region :kind kind :tiles tiles
                                   :x0 (+ x0 (* left *trace-tile*)) :y0 (+ y0 (* top *trace-tile*))
                                   :x1 (+ x0 (* (1+ right) *trace-tile*)) :y1 (+ y0 (* (1+ bottom) *trace-tile*))
                                   :score (funcall deviation rate count sum) :own (/ sum (max 1 count)) :usual rate)
                out))))
    (sort out #'> :key #'trace-region-score)))

(defun lattice-deviation (rate count sum)
  "How many standard deviations SUM, the sum of the cosine of a whole turn
times each of COUNT frequencies (LATTICE-TILES), stands below what RATE,
the page's mean cosine, gives for so many. Spread evenly, as a region laid
in after the JPEG save leaves them, each cosine strays by the root of a
half."
  (/ (- (* count rate) sum) (sqrt (/ count 2d0))))

(defun count-deviation (rate count sum)
  "How far SUM, how many of COUNT values fell on an empty step
\(REQUANTISED-TILES), stands above what RATE, the page's share, gives for so
many: the signed root of twice the Poisson deviance. A few values falling
where the page holds almost none cannot carry it far, as a share's plain
deviation would."
  (let ((expected (* count rate)))
    (if (<= expected 0)
        0d0
        (let ((d (* 2 (- (if (plusp sum) (* sum (log (/ sum expected))) 0) (- sum expected)))))
          (float (* (signum (- sum expected)) (sqrt (max 0 d))) 1d0)))))

(defun tile-grid (width height x0 y0)
  "How many tiles (*TRACE-TILE*) a picture WIDTH by HEIGHT holds across and
down from pixel X0, Y0, as two values; none past its edge."
  (values (max 0 (floor (- width x0) *trace-tile*)) (max 0 (floor (- height y0) *trace-tile*))))

(defun lattice-tiles (grey width height colours x0 y0)
  "For each tile (*TRACE-TILE*) of the picture GREY (WIDTH by HEIGHT, in
COLOURS as PICTURE-GREY gives them) kept without loss but once saved as a
JPEG whose grid of blocks began at X0, Y0 (JPEG-HISTORY), how near whole
numbers the frequencies of its blocks lie, as HISTORY-LATTICE measures it:
how many frequencies were measured and the sum of the cosine of a whole
turn times each, as two vectors, a tile after another along each row. The
page's whole sums them all. A block with a pixel clipped or of one grey
level is passed over."
  (declare (type grey-pixels grey) (type fixnum width height x0 y0))
  (multiple-value-bind (across down) (tile-grid width height x0 y0)
    (let* ((counts (make-array (* across down) :element-type 'fixnum :initial-element 0))
           (sums (make-array (* across down) :element-type 'double-float :initial-element 0d0))
           (clipped (clipped-pixels grey colours))
           (rows (make-array 64 :element-type 'double-float)) (out (make-array 64 :element-type 'double-float))
           (least (float *history-least* 1d0)) (per (floor *trace-tile* 8))
           (left (let ((mask (make-array 64 :element-type 'bit :initial-element 0)))
                   (dolist (k *history-rational* mask) (setf (sbit mask k) 1)))))
      (declare (type simple-bit-vector clipped left) (type double-float least) (type fixnum per))
      (dotimes (by (* down per))
        (job-check-stop)
        (dotimes (bx (* across per))
          (let ((bx0 (+ x0 (* bx 8))) (by0 (+ y0 (* by 8))) (tile (+ (* (floor by per) across) (floor bx per))))
            (declare (type fixnum bx0 by0 tile))
            (unless (loop for j below 8
                          thereis (let ((at (+ (* (+ by0 j) width) bx0)))
                                    (loop for i below 8 thereis (= 1 (sbit clipped (+ at i))))))
              (when (block-all-frequencies grey width bx0 by0 rows out)
                (dotimes (k 64)
                  (when (zerop (sbit left k))
                    (let ((c (aref out k)))
                      (when (>= (abs c) least)
                        (incf (aref sums tile) (cos (* 2d0 pi c)))
                        (incf (aref counts tile)))))))))))
      (values counts sums across))))

(defparameter *requantised-tile-frequencies* 15
  "How many of a JPEG's lowest frequencies are read in each block for the
first save's steps (REQUANTISED-TILES).")

(defun empty-steps (histogram)
  "Which steps of HISTOGRAM, a 2-dimensional array of counts (frequency,
step plus 40 for steps from -40 to 40), a first save with coarser steps
left all but empty: two or more from nothing, holding under a fifth of the
geometric mean of the steps beside them, the fuller of which holds 50
blocks or more. As a bit array of the same shape, and how many such steps
there are."
  (let ((empty (make-array (array-dimensions histogram) :element-type 'bit :initial-element 0)) (count 0))
    (dotimes (j (array-dimension histogram 0))
      (loop for v from -39 to 39
            for before = (aref histogram j (+ v 39)) for after = (aref histogram j (+ v 41))
            when (and (>= (abs v) 2) (>= (max before after) 50)
                      (< (aref histogram j (+ v 40)) (* 0.2 (sqrt (* before after)))))
              do (setf (aref empty j (+ v 40)) 1) (incf count)))
    (values empty count)))

(defparameter *requantised-corner-off* 0.15
  "How far from its steps, at most, a JPEG's frequencies may lie on the grid
from its corner (JPEG-GRID) for the grid to be taken there without trying
the other 63 places (REQUANTISED-TILES), a quarter being none: the most
REQUANTISED-MEASURE takes for a JPEG left as it was decoded.")

(defun requantised-tiles (grey width height colours table)
  "For each tile (*TRACE-TILE*) of the JPEG whose pixels are GREY (WIDTH by
HEIGHT, in COLOURS as PICTURE-GREY gives them), saved with TABLE (its 64
steps in the order it keeps them), how many of the values of its lowest
*REQUANTISED-TILE-FREQUENCIES* frequencies (those whose steps are 1 or
more), two steps or more from nothing,
fall on a step a first save with coarser steps left all but empty over the
whole picture (EMPTY-STEPS). A JPEG saved twice so leaves those steps
empty in every block that went through both saves; a block made between
the two lands on them as often as on any. As five values: how many such
values each tile holds and how many of them lie on empty steps, as two
vectors, a tile after another along each row; the tiles across; the corner
of the picture's grid (X0 Y0, JPEG-GRID); and how many empty steps the
picture has. A picture turned a quarter as its EXIF says it is held has its
frequencies across and down swapped: TABLE is read so when its frequencies
lie nearer its steps that way, at the corner or wherever the grid is
searched for. NIL when the grid is not found, or fewer than 5 frequencies
have steps."
  (declare (type grey-pixels grey) (type fixnum width height))
  (flet ((step-of (p turned)
           ;; the step of frequency P, across plus eight times down; for a picture turned a quarter, as its EXIF
           ;; says it is held, that of the frequency down plus eight times across
           (elt table (position (if turned (+ (* 8 (mod p 8)) (floor p 8)) p) *zigzag*))))
    (let* ((positions (coerce (loop for k from 1 to *requantised-tile-frequencies*
                                    for p = (aref *zigzag* k)
                                    ;; a step of nothing rounds nothing: such a frequency is not read
                                    when (and (>= (step-of p nil) 1) (>= (step-of p t) 1)) collect p)
                              '(simple-array fixnum (*))))
           (clipped (clipped-pixels grey colours))
           (ways (loop for turned in '(nil t)
                       collect (map '(simple-array double-float (*)) (lambda (p) (float (step-of p turned) 1d0))
                                    positions)))
           (steps nil))
      (when (< (length positions) 5)
        (return-from requantised-tiles nil))
      (multiple-value-bind (x0 y0)
          ;; a JPEG decoded as it was saved has its grid at the corner, on its own steps or, turned a quarter, on
          ;; its steps turned; elsewhere both are searched for on fewer blocks; the way lying nearer its steps wins
          (let ((some (subseq positions 0 5)) (best-off nil) (best-x nil) (best-y nil))
            (flet ((try (corners most)
                     (dolist (these ways)
                       (multiple-value-bind (x y off) (jpeg-grid grey clipped width height some (subseq these 0 5)
                                                                 corners most)
                         (when (and x (or (null best-off) (< off best-off)))
                           (setf steps these best-off off best-x x best-y y))))))
              (try '(0) 20000)
              (unless (and best-off (<= best-off *requantised-corner-off*))
                (setf best-off nil)
                (try (loop for k below 64 collect k) 4000))
              (values best-x best-y)))
      (when x0
        (multiple-value-bind (across down) (tile-grid width height x0 y0)
          (let* ((per (floor *trace-tile* 8)) (blocks-across (* across per)) (blocks-down (* down per))
                 (frequencies (length positions))
                 (values (make-array (* blocks-across blocks-down frequencies) :element-type '(signed-byte 8)
                                                                               :initial-element 0))
                 (histogram (make-array (list frequencies 81) :element-type 'fixnum :initial-element 0))
                 (out (make-array frequencies :element-type 'double-float))
                 (counts (make-array (* across down) :element-type 'fixnum :initial-element 0))
                 (sums (make-array (* across down) :element-type 'double-float :initial-element 0d0)))
            ;; 0 stands for a value not kept: nothing, one step, past 40 steps, or a block clipped
            (dotimes (by blocks-down)
              (job-check-stop)
              (dotimes (bx blocks-across)
                (when (block-frequencies grey clipped width (+ x0 (* bx 8)) (+ y0 (* by 8)) positions out)
                  (dotimes (j frequencies)
                    (let ((v (round (aref out j) (aref steps j))))
                      (when (<= (abs v) 40)
                        (incf (aref histogram j (+ v 40)))
                        (when (>= (abs v) 2)
                          (setf (aref values (+ (* (+ (* by blocks-across) bx) frequencies) j)) v))))))))
            (multiple-value-bind (empty count) (empty-steps histogram)
              (dotimes (by blocks-down)
                (dotimes (bx blocks-across)
                  (let ((tile (+ (* (floor by per) across) (floor bx per))))
                    (dotimes (j frequencies)
                      (let ((v (aref values (+ (* (+ (* by blocks-across) bx) frequencies) j))))
                        (unless (zerop v)
                          (incf (aref counts tile))
                          (incf (aref sums tile) (sbit empty j (+ v 40)))))))))
              (values counts sums across (list x0 y0) count)))))))))

(defparameter *flat-ink-reach* 3
  "How near ink, in pixels, a pixel may lie and still not be measured as
blank paper for its grain (GRAIN-TILES): the blur of a scan's strokes
reaches about so far.")

(defun near-ink (grey width height level)
  "Which pixels of GREY (WIDTH by HEIGHT) lie within *FLAT-INK-REACH* pixels
across, down or both of one darker than LEVEL, that one too: a vector of
bits, 1 for near ink."
  (declare (type grey-pixels grey) (type fixnum width height level))
  (let ((across (make-array (length grey) :element-type 'bit :initial-element 0))
        (near (make-array (length grey) :element-type 'bit :initial-element 0))
        (reach *flat-ink-reach*))
    (declare (type simple-bit-vector across near) (type fixnum reach) (optimize speed))
    (dotimes (y height)
      (let ((at (* y width)) (last most-negative-fixnum))
        (declare (type fixnum at last))
        (dotimes (x width)
          (when (< (aref grey (+ at x)) level)
            (loop for k of-type fixnum from (max 0 (+ last reach 1) (- x reach)) to (min (1- width) (+ x reach))
                  do (setf (sbit across (+ at k)) 1))
            (setf last x)))))
    (dotimes (x width)
      (let ((last most-negative-fixnum))
        (declare (type fixnum last))
        (dotimes (y height)
          (when (= 1 (sbit across (+ (* y width) x)))
            (loop for k of-type fixnum from (max 0 (+ last reach 1) (- y reach)) to (min (1- height) (+ y reach))
                  do (setf (sbit near (+ (* k width) x)) 1))
            (setf last y)))))
    near))

(defun grain-tiles (grey width height level)
  "For each tile (*TRACE-TILE*) of the picture GREY (WIDTH by HEIGHT) from
its corner, how far its blank paper strays from its mean: how many of its
pixels are blank paper, with no pixel darker than LEVEL near them
\(NEAR-INK) and not on the picture's edge, and the sum of the squares of how
far each lies from their mean, as two vectors, a tile after another along
each row; the tiles across; and the middle grey of that paper. A tile
holding less than half a tile of blank paper counts none: the paper between
the strokes of a word lies too near their blur to tell. A scanner's grain
and the paper's mottle both count, so paper a scanner's software smoothed
or a line out of focus keeps some, and paint none."
  (declare (type grey-pixels grey) (type fixnum width height level))
  (multiple-value-bind (across down) (tile-grid width height 0 0)
    (let* ((counts (make-array (* across down) :element-type 'fixnum :initial-element 0))
           (sums (make-array (* across down) :element-type 'double-float :initial-element 0d0))
           (levels (make-array 256 :element-type 'fixnum :initial-element 0))
           (near (near-ink grey width height level))
           (tile *trace-tile*) (half (floor (* tile tile) 2)))
      (declare (type simple-bit-vector near) (type fixnum tile half))
      (dotimes (ty down)
        (job-check-stop)
        (dotimes (tx across)
          (let ((count 0) (sum 0) (squares 0))
            (declare (type fixnum count sum squares))
            (loop for y of-type fixnum from (max 1 (* ty tile)) below (min (1- height) (* (1+ ty) tile))
                  do (loop for x of-type fixnum from (max 1 (* tx tile)) below (min (1- width) (* (1+ tx) tile))
                           for at of-type fixnum = (+ (* y width) x)
                           when (zerop (sbit near at))
                             do (let ((g (aref grey at)))
                                  (incf count) (incf sum g) (incf squares (* g g)) (incf (aref levels g)))))
            (when (>= count half)
              (setf (aref counts (+ (* ty across) tx)) count
                    (aref sums (+ (* ty across) tx)) (max 0d0 (- squares (/ (* sum sum) (float count 1d0)))))))))
      (values counts sums across
              (let ((total (reduce #'+ levels)) (seen 0))
                (dotimes (g 256 255)
                  (when (> (* 2 (incf seen (aref levels g))) total) (return g))))))))

(defparameter *flat-share* 0.05d0
  "The largest share of the page's grain a tile's blank paper may keep and
count as painted over (FLAT-REGIONS), each measured as the mean square of
how far the paper's pixels lie from their mean (GRAIN-TILES). See
doc/measures.org.")

(defparameter *flat-grain-least* 1d0
  "How much grain, at least, the tiles of a page must keep in the middle,
measured as for *FLAT-SHARE*, for paper painted over to be told on it
\(FLAT-REGIONS). A JPEG of middling quality rounds the grain of plain paper
away, and a page whose paper was made white has none: on such a page all
the paper is as smooth as paint.")

(defparameter *flat-tiles-least* 3
  "The fewest tiles (*TRACE-TILE*) a region of paper painted over must hold
\(FLAT-REGIONS): a box over a figure or two of body type.")

(defparameter *flat-pieces-most* 0.002d0
  "The most separate pieces of smooth paper (*FLAT-SHARE*) a page may hold
for each of its tiles of blank paper for paper painted over to be told on
it (FLAT-REGIONS); a page holding more is not judged so, and the report
says so for the record. See doc/measures.org.")

(defparameter *flat-white-paper* 246
  "The grey level from which a page's paper is so near white that a scanner
may have clipped some of it to white, flat as paint: a region whose paper
is half white or more (254 or 255) is not taken as painted over on such a
page (FLAT-REGIONS).")

(defun flat-times (rate count sum)
  "How many times less the blank paper of a region strays than the page's,
measured as mean squares (GRAIN-TILES): RATE the page's, SUM the sum of the
squares of COUNT of its pixels; at most 10,000, a hundred times less as
grey levels go."
  (if (plusp count) (/ (* rate count) (max sum (/ (* rate count) 10000))) 0d0))

(defun beside-text-p (words x0 y0 x1 y1)
  "True when the region from X0 Y0 to X1 Y1 (the last excluded) lies where
text runs, among WORDS (WORD-BOXes): a word on its left and one on its
right, each in its rows or within its own height of them and no further
across than six times that height; or a word above and one below, each
within twice its height and over the region's columns. So a smooth margin
or a gutter beside text, with words on one side only, is not taken, and a
line painted over from end to end is."
  (flet ((some-word (test)
           (some (lambda (w)
                   (and (not (word-box-faint w))
                        (funcall test w (- (word-box-y1 w) (word-box-y0 w)))))
                 words)))
    (flet ((in-rows-p (w high)
             (let ((middle (floor (+ (word-box-y0 w) (word-box-y1 w)) 2)))
               (<= (- y0 high) middle (+ y1 high))))
           (over-columns-p (w)
             (and (< (word-box-x0 w) x1) (> (word-box-x1 w) x0))))
      (or (and (some-word (lambda (w high)
                            (and (in-rows-p w high) (< (word-box-x0 w) x0)
                                 (<= (- x0 (word-box-x1 w)) (* 6 high)))))
               (some-word (lambda (w high)
                            (and (in-rows-p w high) (> (word-box-x1 w) x1)
                                 (<= (- (word-box-x0 w) x1) (* 6 high))))))
          (and (some-word (lambda (w high)
                            (and (over-columns-p w) (<= (word-box-y1 w) (+ y0 high))
                                 (<= (- y0 (word-box-y1 w)) (* 2 high)))))
               (some-word (lambda (w high)
                            (and (over-columns-p w) (>= (word-box-y0 w) (- y1 high))
                                 (<= (- (word-box-y0 w) y1) (* 2 high))))))))))

(defun flat-regions (grey width height level words)
  "The regions of the scanned picture GREY (WIDTH by HEIGHT, its ink darker
than LEVEL) whose blank paper keeps *FLAT-SHARE* or less of the grain the
middle tile of the page keeps (GRAIN-TILES): painted over in one colour, as
a white box or a brush hides words. Only on a page whose middle tile keeps
*FLAT-GRAIN-LEAST* or more; only regions of *FLAT-TILES-LEAST* tiles or
more, clear of the picture's edges, which a scanner's lid or a crop leaves
flat; not a region whose paper is half white or more on paper whiter than
*FLAT-WHITE-PAPER*; and only where text runs among WORDS (BESIDE-TEXT-P).
As TRACE-REGIONs of kind :FLAT, the smoothest first; their SCORE is how many
times less their paper strays than the page's, in mean squares
\(FLAT-TIMES). :CROWDED instead when the page's smooth paper lies in more
than *FLAT-PIECES-MOST* pieces for each tile of blank paper: it is not
judged."
  (declare (type grey-pixels grey) (type fixnum width height level))
  (multiple-value-bind (counts sums across paper) (grain-tiles grey width height level)
    (let* ((down (if (plusp across) (floor (length counts) across) 0))
           (shares (loop for k below (length counts)
                         when (plusp (aref counts k)) collect (/ (aref sums k) (aref counts k))))
           (rate (and shares (median-of shares))))
      (when (and rate (>= rate *flat-grain-least*))
        (let ((pieces (trace-regions counts sums #'flat-times across 0 0 :flat :rate rate :least (/ *flat-share*))))
          (if (> (length pieces) (* *flat-pieces-most* (length shares)))
              :crowded
              (flet ((keep (r)
                       (let ((x0 (trace-region-x0 r)) (y0 (trace-region-y0 r))
                             (x1 (trace-region-x1 r)) (y1 (trace-region-y1 r)))
                         (and (>= (trace-region-tiles r) *flat-tiles-least*)
                              (plusp x0) (plusp y0) (< x1 (* across *trace-tile*)) (< y1 (* down *trace-tile*))
                              (not (and (>= paper *flat-white-paper*)
                                        (let ((white 0) (all 0))
                                          (loop for y from y0 below y1
                                                do (loop for x from x0 below x1
                                                         for g = (aref grey (+ (* y width) x))
                                                         when (>= g level)
                                                           do (incf all) (when (>= g 254) (incf white))))
                                          (>= (* 2 white) all))))
                              (beside-text-p words x0 y0 x1 y1)))))
                (remove-if-not #'keep pieces))))))))

(defparameter *lattice-region-least* 10
  "How many standard deviations, at least, a region of a picture kept
without loss but once saved as a JPEG must stand below the page in how near
whole numbers its frequencies lie (LATTICE-TILES, LATTICE-DEVIATION) for it
to be worth a look. See doc/measures.org.")

(defparameter *requantised-steps-least* 30
  "How many steps, at least, of a JPEG's lowest frequencies a first save with
coarser steps must have left all but empty over the whole picture
\(EMPTY-STEPS) for its regions to be set beside it (REQUANTISED-TILES). A
JPEG's table is read turned when its frequencies lie nearer it so. See
doc/measures.org.")

(defparameter *requantised-region-least* 9
  "How far, at least, a region of a JPEG saved twice must stand above the
page in its share of values on the first save's empty steps (COUNT-DEVIATION)
for it to be worth a look. See doc/measures.org.")

(defparameter *trace-regions-most* 10
  "The most regions of one picture kept as falling short of its page's JPEG
traces (LOCAL-TRACES), the furthest first.")

(defun picture-jpeg-table (pdf revision number)
  "The luminance quantisation table of the picture object NUMBER of
REVISION, a JPEG, its 64 steps in the order the JPEG keeps them
\(JPEG-TABLES), read from its stream with the filters before the JPEG's
undone (STREAM-DATA); for a picture file examined as a page, that file's.
NIL when it holds none."
  (if *picture-file*
      (third (picture-file-tables *picture-file*))
      (let ((object (revision-object pdf revision number)))
        (when object
          (multiple-value-bind (data why) (stream-data pdf object)
            (and data (member why '("/DCTDecode" "/DCT") :test #'equal)
                 (third (multiple-value-list (jpeg-tables data)))))))))

(defun local-traces (pdf revision numbers grey width height colours once words level)
  "The local traces of the scanned picture of REVISION kept as the objects
NUMBERS, more than one for strips joined, top first, whose pixels are GREY
\(WIDTH by HEIGHT, in COLOURS as PICTURE-GREY gives them), whose WORDS are
WORD-BOXes and whose ink is darker than LEVEL. As a list of two entries,
each (KIND . FOUND), each measured on its own so that one failing leaves
the other standing:
- :JPEG, the regions lacking the traces of a JPEG save the rest of it
  carries (JPEG-TRACES);
- :FLAT, its regions of paper painted over (FLAT-REGIONS), or :CROWDED when
  the page's smooth paper lies all over it and it was not judged.
FOUND is a list of TRACE-REGIONs, the furthest first, at most
*TRACE-REGIONS-MOST*; or, when measuring failed on what the picture holds,
the condition, so that the check is said not to have finished
\(CHECK-LOCAL-TRACES)."
  (flet ((guarded (measure)
           (handler-case (let ((found (funcall measure)))
                           (if (listp found) (subseq found 0 (min (length found) *trace-regions-most*)) found))
             ((or error storage-condition) (condition) condition))))
    (list (cons :jpeg (guarded (lambda () (jpeg-traces pdf revision numbers grey width height colours once))))
          (cons :flat (guarded (lambda () (flat-regions grey width height level words)))))))

(defun jpeg-traces (pdf revision numbers grey width height colours once)
  "The regions of the scanned picture LOCAL-TRACES is given that lack the
traces of a JPEG save the rest of it carries, as TRACE-REGIONs, the
furthest first. A picture kept without loss but once saved as a JPEG whose
grid began at ONCE, (COLUMN ROW) (JPEG-HISTORY), is measured for regions
whose frequencies lie off whole numbers (LATTICE-TILES),
*LATTICE-REGION-LEAST* deviations or more. A JPEG whose first save left
*REQUANTISED-STEPS-LEAST* steps empty or more is measured for regions whose
values fall on them (REQUANTISED-TILES), *REQUANTISED-REGION-LEAST* or more;
strips are so only when they share one table. Others have none."
  (cond ((picture-jpeg-p pdf revision (first numbers))
         (let ((table (let ((tables (mapcar (lambda (n) (picture-jpeg-table pdf revision n))
                                            (or numbers (list nil)))))
                        (and (every (lambda (table) (equalp table (first tables))) tables) (first tables)))))
           (when (and table (= (length table) 64))
             (multiple-value-bind (counts sums across corner empty)
                 (requantised-tiles grey width height colours table)
               (when (and counts (>= empty *requantised-steps-least*))
                 (remove-if (lambda (r) (< (trace-region-score r) *requantised-region-least*))
                            (trace-regions counts sums #'count-deviation across (first corner)
                                           (second corner) :requantised)))))))
        (once
         (destructuring-bind (x0 y0) once
           (multiple-value-bind (counts sums across) (lattice-tiles grey width height colours x0 y0)
             (remove-if (lambda (r) (< (trace-region-score r) *lattice-region-least*))
                        (trace-regions counts sums #'lattice-deviation across x0 y0 :lattice)))))))

(defparameter *trace-region-words*
  '((:lattice
     ("lacks the traces of the JPEG the rest of the page was once saved as"
      "lack the traces of the JPEG the rest of the page was once saved as")
     #.(concatenate 'string "A JPEG rounds the frequencies of each block of 8 by 8 pixels to whole steps; a "
                            "page decoded from one and kept without loss keeps them near whole numbers in every "
                            "block. A region whose frequencies lie off them was made after that save: a word typed "
                            "again over healed paper, a stamp, a signature or a patch of cloned paper laid in, "
                            "however well its look, ink and grain match. A part of the page a program drew after "
                            "the save, as a scanner's own note or stamp, or filtered after it, as a scanner that "
                            "removes specks along an edge does, gives the same sign honestly.")
     "its frequencies' mean cosine ~,2f, the page's ~,2f, ~,1f standard deviations from the page"
     :piece-laid-in)
    (:requantised
     ("holds values on the steps the page's first JPEG save left empty"
      "hold values on the steps the page's first JPEG save left empty")
     #.(concatenate 'string "A JPEG saved twice, with coarser steps the first time, leaves some steps of its "
                            "frequencies all but empty in every block that went through both saves. A region "
                            "whose values fall on them as often as on any went through the last save only: laid "
                            "in between the two, as a word typed again, a stamp, a signature or cloned paper is. "
                            "A part of the page a program drew or filtered between the saves, as a scanner's or a "
                            "phone app's own note or its removing specks along an edge, gives the same sign "
                            "honestly.")
     "~,1f% of its values on those steps, the page's ~,1f%, ~,1f standard deviations from the page"
     :piece-laid-in)
    (:flat
     ("is blank paper without the grain the rest of the page's paper has"
      "are blank paper without the grain the rest of the page's paper has")
     #.(concatenate 'string "A scanner leaves grain on all the paper it reads: each pixel strays a little from its "
                            "neighbours. Paper painted over in one colour, a white box or a brush laid over words "
                            "in a picture editor, has little or none. A region a program drew after the scan, as a "
                            "scanner's own note or an office's blanking out of what may not be shown, gives the "
                            "same sign honestly; so do correction fluid, a glossy label or a flash's glare, which "
                            "clip to white, and a page whose paper was made white in parts.")
     "its paper strays ~,2f grey levels from its mean, the page's ~,2f, ~,1f times less"
     :painted-over))
  "For each kind of TRACE-REGION: what it shows, in words, said of one region
and of several; the rule; the detail of its measure, given its own, the
page's and its score; and the way of editing it tells of (*METHODS*).")

(defun strip-table-findings (pdf revision)
  "For each scanned page of REVISION joined from strips kept as JPEGs
\(SCAN-MARKS), whose strips were not all saved with one luminance table
\(PICTURE-JPEG-TABLE), a finding worth a look naming the strips saved with
another table than most of them, the first strip's table taken of two as
common. Only the tables are read, so a page whose
pixels cannot be read is told too."
  (loop for (mark page) in (scan-marks pdf revision)
        for strips = (mark-strips mark)
        for tables = (and strips (picture-jpeg-p pdf revision (mark-object mark))
                          (mapcar (lambda (s) (picture-jpeg-table pdf revision (mark-object (first s)))) strips))
        for usual = (and tables (let ((counts (make-hash-table :test 'equalp)))
                                  (dolist (table tables) (incf (gethash table counts 0)))
                                  ;; of tables as common, the first strip's in the order of the picture's rows
                                  (let ((most (loop for count being the hash-values of counts maximize count)))
                                    (find-if (lambda (table) (= (gethash table counts) most)) tables))))
        for odd = (loop for s in strips for table in tables unless (equalp table usual) collect s)
        when odd
          collect (found :look (format nil "On page ~d, ~d of the ~d JPEG strips the scanned page is joined from ~
                                            ~:[were~;was~] saved with other tables than the rest: saved again, or ~
                                            laid in from another scan, or a scanner that sets its quality strip by ~
                                            strip."
                                       (1+ (page-index page)) (length odd) (length strips) (= (length odd) 1))
                         :rule (concatenate 'string "A scanner that keeps a page as strips saves every strip with the "
                                            "same tables of steps, the same quality. A strip saved with other tables "
                                            "than the rest went through a save of its own: a strip saved again after "
                                            "it was changed, or put in from another scan.")
                         :revision (revision-index revision) :page (1+ (page-index page))
                         :tally (list (length odd) "strip")
                         :objects (mapcar (lambda (s) (mark-object (first s))) odd)
                         :details (loop for (s row height) in odd
                                        collect (format nil "picture ~a (object ~d), rows ~d to ~d of the page joined ~
                                                             from strips: ~:[no table found~;its own table~]"
                                                        (mark-text s) (mark-object s) row (+ row height)
                                                        (picture-jpeg-table pdf revision (mark-object s))))
                         :evidence (pictures-evidence pdf revision (mapcar (lambda (s) (mark-object (first s))) odd))
                         :method :piece-laid-in)))

(defparameter *trace-backing-reach* 12
  "How many pixels from a region whose traces of a JPEG save fall short
another sign may lie and still stand at it (TRACE-BACKING): the regions are
told in tiles of 16 pixels, and a character at the edge of one may have
most of its ink in the next.")

(defun box-at-region-p (region x0 y0 x1 y1)
  "True when the box X0 Y0 X1 Y1 lies within *TRACE-BACKING-REACH* pixels of
REGION, a TRACE-REGION of its picture."
  (let ((reach *trace-backing-reach*))
    (and (< x0 (+ (trace-region-x1 region) reach)) (< (trace-region-x0 region) (+ x1 reach))
         (< y0 (+ (trace-region-y1 region) reach)) (< (trace-region-y0 region) (+ y1 reach)))))

(defun trace-backing (region regions scan copies)
  "What else tells that REGION was laid in, one of the REGIONS of SCAN whose
traces of a JPEG save fall short of the page's: :COPY when a character at
it is one of a group of COPIES (COPIED-GLYPHS) with another character of
SCAN's picture away from every one of REGIONS and off REGION's rows, both
with grain of their own, as a figure copied from the scanned text to
another place has its source; :CHARACTER when a word at it holds a
character off its line (SCAN-PICTURE-ODD); else NIL. A stamp, a page
number or a fax header that software drew on the page after it was saved
lacks the traces too, and repeats its figures alike: but its twins stand
in the stamp, within the regions or on their rows, and its characters
stand on one line as they were drawn. Copies laid on the pages by
software, stamps, light inks and prints a page's steps make alike are no
backing."
  (let ((page (page-index (scan-picture-page scan))) (picture (mark-object (scan-picture-mark scan))))
    (flet ((here-p (g) (and (eql (glyph-page g) page) (eql (glyph-picture g) picture) (not (glyph-bare g))))
           (at-p (g r) (box-at-region-p r (glyph-x0 g) (glyph-y0 g) (glyph-x1 g) (glyph-y1 g))))
      (cond ((loop for (group nil nil nil nil why laid) in copies
                   thereis (and (not laid)
                                (not (member why '(:stamp :light :faint :stepped :flat :uneven)))
                                (some (lambda (g) (and (here-p g) (at-p g region))) group)
                                (some (lambda (g)
                                        (and (here-p g)
                                             (notany (lambda (r) (at-p g r)) regions)
                                             (or (<= (+ (glyph-y1 g) *trace-backing-reach*) (trace-region-y0 region))
                                                 (>= (glyph-y0 g) (+ (trace-region-y1 region) *trace-backing-reach*)))))
                                      group)))
             :copy)
            ((some (lambda (odd)
                     (let ((word (car odd)))
                       (and (find :character (cdr odd) :key #'first)
                            (box-at-region-p region (word-box-x0 word) (word-box-y0 word)
                                             (word-box-x1 word) (word-box-y1 word)))))
                   (scan-picture-odd scan))
             :character)))))

(defparameter *trace-backing-words*
  '((:copy . "a character at it is a copy of one elsewhere on the page")
    (:character . "a word at it holds a character off its line"))
  "What backs a region whose traces of a JPEG save fall short (TRACE-BACKING),
in words.")

(defparameter *trace-backed-rule*
  (concatenate 'string " Here another sign stands at the same place: a character there is nearer a character "
               "elsewhere on the page than prints of one character are, its twin away from every such region and off "
               "its rows, both with the grain of the scan, as a figure copied from the scanned text has its source; or "
               "a word there holds a character off its line. A stamp, a page number or a fax header drawn by software "
               "repeats its figures within itself and sets them on one line, and drew neither on the honest pages "
               "stamped so that were measured. Two measures apart from each other agree on the place: a strong sign.")
  "What the rule of a region's finding says more when another sign backs it
\(TRACE-BACKING).")

(defun regions-objects (mark regions)
  "The object numbers of the pictures holding REGIONS, TRACE-REGIONs of the
picture MARK draws (OBJECTS-AT)."
  (objects-at mark (mapcar (lambda (r) (cons (trace-region-y0 r) (trace-region-y1 r))) regions)))

(defun check-local-traces (pdf revision)
  "Regions of the scanned pages lacking the traces of a JPEG save the rest of
their page carries, or painted over (LOCAL-TRACES), worth a look, one
finding for each page and kind, the regions named with their measures; a
strong sign, in a finding of their own, the regions lacking a save's traces
that a copied character or a character off its line backs (TRACE-BACKING);
for a page whose measuring of either failed, that it could not finish; and,
for the record, a page whose smooth paper lies all over it, not judged for
paint. Before them, the strips of a page joined from JPEG strips saved with
other tables than the rest (STRIP-TABLE-FINDINGS)."
  (let ((out (reverse (strip-table-findings pdf revision))) (reading (scans-read pdf revision)))
    (dolist (scan (remove-if #'scan-picture-twin (scan-reading-scans reading)) (nreverse out))
      (let ((page (1+ (page-index (scan-picture-page scan)))) (traces '()))
        (loop for (kind . found) in (scan-picture-traces scan)
              do (cond ((typep found 'condition)
                        (push (unfinished (format nil (if (eq kind :flat) "the paper's grain of page ~d"
                                                          "the JPEG traces of page ~d")
                                                  page)
                                          found)
                              out))
                       ((eq found :crowded)
                        (push (found :record (format nil "Page ~d was not judged for paper painted over: its smooth ~
                                                          paper lies in many pieces all over it, as a JPEG whose ~
                                                          steps rounded the paper's grain away leaves it."
                                                     page)
                                     :rule (concatenate 'string "Paper painted over in one colour keeps less of "
                                                        "the scanner's grain than the paper about it; where smooth "
                                                        "paper lies in pieces all over a page, as a JPEG's rounding "
                                                        "leaves it, no piece stands out, and the page is not judged.")
                                     :revision (revision-index revision) :page page)
                              out))
                       (t (setf traces (append traces found)))))
        (let ((m (scan-picture-mark scan)) (width (scan-picture-width scan)) (height (scan-picture-height scan)))
        (dolist (kind '(:lattice :requantised :flat))
          (let* ((regions (remove kind traces :key #'trace-region-kind :test-not #'eq))
                 (lacking (remove :flat traces :key #'trace-region-kind))
                 (backing (mapcar (lambda (r)
                                    (and (not (eq kind :flat))
                                         (trace-backing r lacking scan (scan-reading-copies reading))))
                                  regions)))
            (dolist (strength '(:strong :look))
              (let ((regions (loop for r in regions for b in backing
                                   when (eq (and b t) (eq strength :strong)) collect (cons r b))))
                (when regions
                  (destructuring-bind ((shows show) rule detail method) (rest (assoc kind *trace-region-words*))
                    (flet ((at (r)
                             (pixels-place m (trace-region-x0 r) (trace-region-y0 r) (trace-region-x1 r)
                                           (trace-region-y1 r) width height)))
                      (let ((first (car (first regions))) (only (mapcar #'car regions)))
                        (push (found strength
                                     (format nil "~a~:[~;, and another sign stands at the same place~:*~p~]."
                                             (if (rest regions)
                                                 (format nil "~d regions of page ~d, in ~a, ~a" (length regions) page
                                                         (scan-named m) show)
                                                 (format nil "A region of page ~d, about ~d by ~d pixels ~a, in ~a, ~a"
                                                         page (- (trace-region-x1 first) (trace-region-x0 first))
                                                         (- (trace-region-y1 first) (trace-region-y0 first))
                                                         (string-right-trim " " (subseq (at first) 0
                                                                                        (position #\( (at first))))
                                                         (scan-named m :short t) shows))
                                             (and (eq strength :strong) (length regions)))
                                     :rule (if (eq strength :strong)
                                               (concatenate 'string rule *trace-backed-rule*)
                                               rule)
                                     :revision (revision-index revision) :page page :objects (regions-objects m only)
                                     :tally (list (length regions) "region")
                                     :details (loop for (r . b) in regions
                                                    collect (format nil "~a: ~?, over ~d tile~:p of ~d by ~d ~
                                                                         pixels~@[; ~a~]"
                                                                    (at r) detail
                                                                    (let ((own (trace-region-own r))
                                                                          (usual (trace-region-usual r)))
                                                                      (case kind
                                                                        (:requantised (list (* 100 own) (* 100 usual)
                                                                                            (trace-region-score r)))
                                                                        (:flat (list (sqrt own) (sqrt usual)
                                                                                     (sqrt (trace-region-score r))))
                                                                        (t (list own usual (trace-region-score r)))))
                                                                    (trace-region-tiles r) *trace-tile* *trace-tile*
                                                                    (cdr (assoc b *trace-backing-words*))))
                                     :evidence (pictures-evidence pdf revision (regions-objects m only))
                                     :method method)
                              out))))))))))))))
