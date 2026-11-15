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

(in-package #:palimpsest)

(defstruct (trace-region (:copier nil) (:predicate nil))
  "A region of a scanned picture whose traces of a JPEG save fall short of
the rest of the page's (LOCAL-TRACES). KIND is :LATTICE, a picture kept
without loss whose blocks were once a JPEG's (LATTICE-TILES), or
:REQUANTISED, a JPEG saved twice whose blocks show the first save's steps
\(REQUANTISED-TILES). X0 Y0 X1 Y1 are its pixels in the picture (the last
excluded), TILES how many tiles of 16 by 16 pixels it holds. SCORE is how
many standard deviations it stands from the page (TRACE-REGIONS). OWN and
USUAL are the measure in the region and over the page: the mean cosine of
a whole turn times its frequencies for :LATTICE, the share of its
frequencies on the first save's empty steps for :REQUANTISED."
  kind x0 y0 x1 y1 (tiles 0) (score 0d0) (own 0d0) (usual 0d0))

(defparameter *trace-tile* 16
  "The side, in pixels, of the tiles a scanned page is cut into for its local
traces (LOCAL-TRACES): two JPEG blocks across and down, as small as a figure
or a short word, large enough for a tile to hold a few dozen frequencies.")

(defparameter *trace-tile-least* 2d0
  "How many standard deviations, at least, a tile must fall short of its
page for it to join a region (TRACE-REGIONS). The region is then judged on
all of its tiles together.")

(defun trace-regions (counts sums deviation tiles-across x0 y0 kind)
  "The regions of a page cut into tiles (*TRACE-TILE*), TILES-ACROSS to a
row, the first at pixel X0, Y0, whose measure falls short of the page's.
COUNTS and SUMS hold, for each tile, how many values were measured and
their sum; DEVIATION, called with the page's rate, a count and a sum, says
how many standard deviations that sum stands from what the rate gives for
so many values, more being further on the side that falls short. A tile of 4
values or more standing *TRACE-TILE-LEAST* or more joins its neighbours
that do, across and down and on the slant; each region so joined is judged
on the counts and sums of all its tiles together. As TRACE-REGIONs of KIND,
the furthest first; their USUAL is the page's rate, the sum of SUMS over
the sum of COUNTS."
  (declare (type (simple-array fixnum (*)) counts) (type (simple-array double-float (*)) sums)
           (type fixnum tiles-across))
  (let* ((n (length counts)) (z (make-array n :element-type 'double-float :initial-element 0d0))
         (seen (make-array n :element-type 'bit :initial-element 0)) (out '())
         (rate (/ (reduce #'+ sums) (max 1 (reduce #'+ counts)))))
    (dotimes (k n)
      (when (>= (aref counts k) 4)
        (setf (aref z k) (funcall deviation rate (aref counts k) (aref sums k)))))
    (dotimes (k n)
      (when (and (zerop (sbit seen k)) (>= (aref z k) *trace-tile-least*))
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
                                              (>= (aref z next) *trace-tile-least*))
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

(defparameter *lattice-region-least* 10
  "How many standard deviations, at least, a region of a picture kept
without loss but once saved as a JPEG must stand below the page in how near
whole numbers its frequencies lie (LATTICE-TILES, LATTICE-DEVIATION) for it
to be worth a look. Honest pages so kept gave at most 5.1 (54 pages: made-up
scans of grain 1 to 25 and real pages, saved as JPEGs of quality 50 to 95
and kept without loss); regions laid in after the save, saved as PNG, 18.5
to 54.6, 23.5 in the middle (doc/measures.org).")

(defparameter *requantised-steps-least* 30
  "How many steps, at least, of a JPEG's lowest frequencies a first save with
coarser steps must have left all but empty over the whole picture
\(EMPTY-STEPS) for its regions to be set beside it (REQUANTISED-TILES).
JPEGs saved once gave at most 10 (made-up scans, real scans, phone pictures,
the real-based controls), but for one real scan its device saved twice, at
66; saved twice with coarser steps first, 34 to 346. A JPEG turned a
quarter as its EXIF says, its table read as kept, gave 36 saved once: its
table is read turned when its frequencies lie nearer it so
\(REQUANTISED-TILES, doc/measures.org).")

(defparameter *requantised-region-least* 9
  "How far, at least, a region of a JPEG saved twice must stand above the
page in its share of values on the first save's empty steps (COUNT-DEVIATION)
for it to be worth a look. Honest pages saved twice, uncut or cut between
the saves, and the honest regions of forged ones gave at most 7.2; regions
laid in between the saves 9.2 to 84.6, 21 in the middle (doc/measures.org).")

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

(defun local-traces (pdf revision number grey width height colours once)
  "The regions of the scanned picture object NUMBER of REVISION, whose pixels
are GREY (WIDTH by HEIGHT, in COLOURS as PICTURE-GREY gives them), that
lack the traces of a JPEG save the rest of it carries, as TRACE-REGIONs,
the furthest first, at most *TRACE-REGIONS-MOST*. A picture kept without
loss but once saved as a JPEG whose grid began at ONCE, (COLUMN ROW)
\(JPEG-HISTORY), is measured for regions whose frequencies lie off whole
numbers (LATTICE-TILES), *LATTICE-REGION-LEAST* deviations or more. A JPEG
whose first save left *REQUANTISED-STEPS-LEAST* steps empty or more is
measured for regions whose values fall on them (REQUANTISED-TILES),
*REQUANTISED-REGION-LEAST* or more. Others have none. When measuring
fails on what the picture holds, the condition instead of a list, so that
the page's other checks go on and this one is said not to have finished.
\(CHECK-LOCAL-TRACES)."
  (handler-case (picture-traces pdf revision number grey width height colours once)
    ((or error storage-condition) (condition) condition)))

(defun picture-traces (pdf revision number grey width height colours once)
  "The regions LOCAL-TRACES gives, measured with no guard."
  (let ((found
          (cond ((picture-jpeg-p pdf revision number)
                 (let ((table (picture-jpeg-table pdf revision number)))
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
                                (trace-regions counts sums #'lattice-deviation across x0 y0 :lattice))))))))
    (subseq found 0 (min (length found) *trace-regions-most*))))

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
     "its frequencies' mean cosine ~,2f, the page's ~,2f")
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
     "~,1f% of its values on those steps, the page's ~,1f%"))
  "For each kind of TRACE-REGION: what it shows, in words, said of one region
and of several; the rule; and the detail of its measure, given its own and
the page's.")

(defun check-local-traces (pdf revision)
  "Regions of the scanned pages lacking the traces of a JPEG save the rest of
their page carries (LOCAL-TRACES), worth a look at most, one finding for
each page and kind, the regions named with their measures; and, for a page
whose measuring failed, that it could not finish."
  (let ((out '()))
    (dolist (scan (remove-if #'scan-picture-twin (scan-reading-scans (scans-read pdf revision))) (nreverse out))
      (when (typep (scan-picture-traces scan) 'condition)
        (push (unfinished (format nil "the JPEG traces of page ~d" (1+ (page-index (scan-picture-page scan))))
                          (scan-picture-traces scan))
              out))
      (let ((traces (if (typep (scan-picture-traces scan) 'condition) '() (scan-picture-traces scan)))
            (m (scan-picture-mark scan)) (width (scan-picture-width scan)) (height (scan-picture-height scan)))
        (dolist (kind '(:lattice :requantised))
          (let ((regions (remove kind traces :key #'trace-region-kind :test-not #'eq)))
            (when regions
              (destructuring-bind ((shows show) rule detail) (rest (assoc kind *trace-region-words*))
                (flet ((at (r)
                         (destructuring-bind (x0 y0 x1 y1)
                             (picture-page-box m (list (trace-region-x0 r) (trace-region-y0 r) (trace-region-x1 r)
                                                       (trace-region-y1 r))
                                               width height)
                           (declare (ignore x1 y1))
                           (format nil "at x ~d, y ~d (pixels ~d,~d to ~d,~d~@[ of picture ~d~])"
                                   (round x0) (round y0) (trace-region-x0 r) (trace-region-y0 r)
                                   (trace-region-x1 r) (trace-region-y1 r) (picture-number (mark-object m))))))
                  (let ((page (1+ (page-index (scan-picture-page scan)))) (first (first regions))
                        (percent (if (eq kind :requantised) 100 1)))
                    (push (found :look
                                 (if (rest regions)
                                     (format nil "~d regions of page ~d, in ~a, ~a." (length regions) page
                                             (scan-named m) show)
                                     (format nil "A region of page ~d, about ~d by ~d pixels ~a, in ~a, ~a."
                                             page (- (trace-region-x1 first) (trace-region-x0 first))
                                             (- (trace-region-y1 first) (trace-region-y0 first))
                                             (string-right-trim " " (subseq (at first) 0 (position #\( (at first))))
                                             (scan-named m :short t) shows))
                                 :rule rule
                                 :revision (revision-index revision) :page page :objects (list (mark-object m))
                                 :details (loop for r in regions
                                                collect (format nil "~a: ~?, ~,1f standard deviations from the ~
                                                                     page over ~d tile~:p of ~d by ~d pixels"
                                                                (at r) detail
                                                                (list (* percent (trace-region-own r))
                                                                      (* percent (trace-region-usual r)))
                                                                (trace-region-score r) (trace-region-tiles r)
                                                                *trace-tile* *trace-tile*))
                                 :evidence (pictures-evidence pdf revision (list (mark-object m)))
                                 :method :piece-laid-in)
                          out)))))))))))

