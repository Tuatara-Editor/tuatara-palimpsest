;;;; copied.lisp --- Paper of a scanned page copied from paper elsewhere on the same page.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A scanner's grain is never the same twice: no two patches of a page's
;;;; paper are alike pixel for pixel. A clone stamp laid over old characters,
;;;; or a content-aware fill, copies paper from elsewhere on the page and its
;;;; grain with it. The grained paper near the words is cut into blocks of 8
;;;; by 8 pixels at every pixel; blocks alike in a coarse key are set beside
;;;; one another pixel by pixel, and the blocks alike at one shift make a
;;;; region and its source. A JPEG save rounds flat paper to the same few
;;;; values in the blocks of its grid, which then repeat at shifts of whole
;;;; blocks: those shifts are left out, and a picture ever saved as a JPEG is
;;;; not searched.

(in-package #:palimpsest)

(defstruct (copied-region (:copier nil) (:predicate nil))
  "A region of a scanned picture's paper alike pixel for pixel with paper
elsewhere on it (COPIED-PAPER). X0 Y0 X1 Y1 are the pixels of its blocks
that lie lower on the picture (the last excluded), DX and DY how far they lie
across and down from the blocks they match, so that the other region runs
from X0-DX,Y0-DY to X1-DX,Y1-DY. Either may be the copy. BLOCKS is how many
blocks of 8 by 8 pixels matched, DISTANCE how far apart in grey levels they
lie on average, their means taken out. SHIFTS is how many shifts its blocks
match at: 1 for paper copied in one piece, more for paper pieced together
from many places (PATCHED-REGIONS), DX and DY then 0 and X0 Y0 X1 Y1 the
box of its pieces and of the paper they match."
  (dx 0) (dy 0) (blocks 0) x0 y0 x1 y1 (distance 0.0) (shifts 1))

(defparameter *copied-paper-grain-least* 1.0
  "The least grain, in grey levels, of a page's paper near its words for it to
be searched for copied paper (COPIED-PAPER): the middle spread of its blocks
of 8 by 8 pixels free of ink and of white, taken every 16 pixels. Flat paper
holds nothing a copy could be told by; of 535 honest pictures measured, 336
had less, most of them white or saved as JPEGs.")

(defparameter *copied-paper-block-grain* 1/2
  "The least spread a block of 8 by 8 pixels of paper must keep, as a share of
the page's grain (*COPIED-PAPER-GRAIN-LEAST*) and never under a grey level,
for it to be searched for a copy (COPIED-PAPER). A smoother block is alike
with too many others by chance.")

(defparameter *copied-paper-step* '(0.375 . 1.5)
  "The step, as (SHARE . LEAST), to which the coarse key of a block of paper
rounds its 16 means of 2 by 2 pixels less the block's mean (COPIED-PAPER):
SHARE of the page's grain, LEAST grey levels at least. Two blocks of one key
are set beside each other pixel by pixel; a block whose key holds fewer than
8 steps other than 0 is too plain to be told and is passed over.")

(defparameter *copied-paper-tolerance* '(0.6 . 0.8)
  "How far apart two blocks of paper may lie and still match (COPIED-PAPER),
as (SHARE . LEAST): the root of the mean square of their pixels' difference,
each block's mean taken out, SHARE of the page's grain or LEAST grey levels,
whichever is more. A copy kept without loss lies 0 apart; two blocks of
honest grain about one and a half times the grain.")

(defparameter *copied-paper-key-most* 12
  "The most blocks of one key set beside one another (COPIED-PAPER). More
alike in the key is a pattern repeating, a form's print or a screen, not a
copy.")

(defparameter *copied-paper-blocks-least* 16
  "The fewest matching blocks of 8 by 8 pixels, at one shift and joined
within 4 pixels of one another, for a region of copied paper to be named
\(COPIED-PAPER). Of 199 honest pages searched, with the shifts of the JPEG
grid left out, 10 gave regions of 8 blocks or more, 2 of 12, none of 16;
the forgeries found at their place fell from 6 to 5.")

(defparameter *copied-paper-blocks-most* 3000000
  "The most blocks of paper one picture is searched with for copied paper
\(COPIED-PAPER), as a bound on the time and memory taken: about 25 MB of
keys. A page at 300 dpi holds up to about a million near its words; a page
holding more is not searched, and the report says so.")

(defparameter *copied-paper-exact* 1/4
  "How far apart, at most, in grey levels, two blocks of paper may lie, the
root of the mean square of their pixels' difference with each block's mean
taken out, to be the same pixels copied and kept without loss since
\(PATCHED-REGIONS). Honest grain lies a grey level apart or more.")

(defparameter *patched-paper-pieces-least* 6
  "The fewest blocks of paper the same pixel for pixel as paper elsewhere on
the page (*COPIED-PAPER-EXACT*), within *PATCHED-PAPER-REACH* of one
another, for paper pieced together to be named (PATCHED-REGIONS).")

(defparameter *patched-paper-shifts-least* 4
  "The fewest shifts the pieces of paper pieced together must match at
\(PATCHED-REGIONS): a content-aware fill takes each piece from another
place, a clone stamp a few dabs from one or two.")

(defparameter *patched-paper-reach* 64
  "The reach, in pixels, by which blocks of paper the same pixel for pixel as
paper elsewhere are joined into one patch (PATCHED-REGIONS, JOINED-BLOCKS):
about a word's width on a page at 300 dots an inch.")

(defparameter *copied-regions-most* 10
  "The most regions of copied paper named for one picture, the largest first
\(COPIED-PAPER).")

(defun word-bands (words tilt width height)
  "Where the blocks of 8 by 8 pixels near WORDS lie in a picture WIDTH by
HEIGHT, the WORD-BOXes read on the picture made level, its lines falling
TILT for each pixel across: a bit for each pixel, 1 where it is the top left
corner of a block within a line's box, from its first word to its last,
widened by its height on every side, and wholly within the picture. The
paper between the words of a line is searched too, so that a word covered
over is still among them."
  (let ((bands (make-array (* width height) :element-type 'bit :initial-element 0))
        (slope (if (< (abs (or tilt 0)) 0.0003) 0 tilt))
        (lines (make-hash-table :test 'equal)))
    (dolist (w words)
      (let ((line (gethash (cons (word-box-column w) (word-box-y0 w)) lines)))
        (if line
            (setf (first line) (min (first line) (word-box-x0 w)) (second line) (max (second line) (word-box-x1 w))
                  (third line) (max (third line) (word-box-y1 w)))
            (setf (gethash (cons (word-box-column w) (word-box-y0 w)) lines)
                  (list (word-box-x0 w) (word-box-x1 w) (word-box-y1 w))))))
    (maphash (lambda (key line)
               (destructuring-bind (x0 x1 y1) line
                 (let* ((y0 (cdr key)) (high (- y1 y0))
                        (fall0 (round (* slope x0))) (fall1 (round (* slope x1)))
                        (left (max 0 (- x0 high))) (right (min (- width 8) (+ x1 high))))
                   (when (< left right)
                     (loop for y from (max 0 (+ y0 (min fall0 fall1) (- high)))
                             below (min (- height 8) (+ y1 (max fall0 fall1) high))
                           do (fill bands 1 :start (+ (* y width) left) :end (+ (* y width) right)))))))
             lines)
    bands))

(defun copied-paper (grey width height words tilt level)
  "The regions of the scanned picture GREY (WIDTH by HEIGHT, its ink darker
than LEVEL) whose paper is alike pixel for pixel with paper elsewhere on it,
as a clone stamp or a content-aware fill leaves it, as COPIED-REGIONs, the
largest first, at most *COPIED-REGIONS-MOST*. Searched are the blocks of 8 by 8
pixels at every pixel near the lines of WORDS (WORD-BANDS, TILT their lines'
fall) holding no ink, fewer than 2 pixels of white (254 or 255) and a spread of
*COPIED-PAPER-BLOCK-GRAIN* of the page's grain or more, on a page whose grain is
*COPIED-PAPER-GRAIN-LEAST* or more. Blocks of one coarse key
\(*COPIED-PAPER-STEP*), *COPIED-PAPER-KEY-MOST* or fewer, match when they lie
within *COPIED-PAPER-TOLERANCE* at a shift of 8 pixels or more across or down
that is not a whole number of blocks both ways, as a JPEG's grid repeats; the
matches of one shift are joined where they lie within 4 pixels, a region needs
*COPIED-PAPER-BLOCKS-LEAST* blocks, and the regions of one shift side by side
are named as one (MERGED-REGIONS). Paper pieced together from many places, as a
content-aware fill builds it, is named too (PATCHED-REGIONS). :TOO-MANY instead
when more than *COPIED-PAPER-BLOCKS-MOST* blocks are to be searched. As more
values, how many blocks were searched (0 for a page not searched) and the
page's grain (NIL when no paper was found to measure it on)."
  (declare (type grey-pixels grey) (type fixnum width height level))
  (when (or (< width 16) (< height 16) (>= (* width height) (expt 2 27)))
    (return-from copied-paper (values (if (< width 16) nil :too-many) 0 nil)))
  (let* ((bands (word-bands words tilt width height))
         (sums (make-array width :element-type 'fixnum :initial-element 0))
         (squares (make-array width :element-type 'fixnum :initial-element 0))
         (inks (make-array width :element-type 'fixnum :initial-element 0))
         (whites (make-array width :element-type 'fixnum :initial-element 0))
         (pairs (make-array (* 8 width) :element-type '(unsigned-byte 16) :initial-element 0))
         (last (- width 8)))
    (declare (type simple-bit-vector bands) (type (simple-array fixnum (*)) sums squares inks whites)
             (type (simple-array (unsigned-byte 16) (*)) pairs) (type fixnum last))
    (labels ((add-row (y sign)
               (declare (type fixnum y sign) (optimize speed))
               (let ((at (* y width)))
                 (declare (type fixnum at))
                 (dotimes (x width)
                   (let ((g (aref grey (+ at x))))
                     (incf (aref sums x) (* sign g))
                     (incf (aref squares x) (* sign g g))
                     (when (< g level) (incf (aref inks x) sign))
                     (when (>= g 254) (incf (aref whites x) sign))))))
             (add-pairs (y)
               ;; the sums of 2 by 2 pixels from row Y, kept in the eighth of PAIRS for Y's place among 8 rows
               (declare (type fixnum y) (optimize speed))
               (let ((at (* y width)) (to (* (logand y 7) width)))
                 (declare (type fixnum at to))
                 (dotimes (x (1- width))
                   (let ((p (+ at x)))
                     (setf (aref pairs (+ to x)) (+ (aref grey p) (aref grey (1+ p)) (aref grey (+ p width))
                                                    (aref grey (+ p width 1))))))))
             (blocks-along (y every fn)
               ;; each block of row Y wholly in a band, every EVERY pixels, given its sums to FN
               (declare (type fixnum y every) (type function fn) (optimize speed))
               (let ((sum 0) (square 0) (ink 0) (white 0) (row (* y width)))
                 (declare (type fixnum sum square ink white row))
                 (dotimes (x 8) (incf sum (aref sums x)) (incf square (aref squares x))
                   (incf ink (aref inks x)) (incf white (aref whites x)))
                 (loop for x of-type fixnum from 0 below last
                       do (when (and (zerop (mod x every)) (= 1 (sbit bands (+ row x))))
                            (funcall fn x sum square ink white))
                          (incf sum (- (aref sums (+ x 8)) (aref sums x)))
                          (incf square (- (aref squares (+ x 8)) (aref squares x)))
                          (incf ink (- (aref inks (+ x 8)) (aref inks x)))
                          (incf white (- (aref whites (+ x 8)) (aref whites x))))))
             (spread (sum square)
               (declare (type (integer 0 16320) sum) (type (integer 0 4161600) square))
               (/ (float (- (* 64 square) (* sum sum)) 1.0) 4096.0)))
      ;; the page's grain: the middle spread of blocks of plain paper, every 16 pixels
      (let ((spreads '()))
        (loop for y from 0 below (- height 8) by 16
              do (fill sums 0) (fill squares 0) (fill inks 0) (fill whites 0)
                 (loop for k from y below (+ y 8) do (add-row k 1))
                 (blocks-along y 16 (lambda (x sum square ink white)
                                      (declare (ignore x))
                                      (when (and (zerop ink) (zerop white))
                                        (push (sqrt (max 0.0 (spread sum square))) spreads)))))
        (let ((grain (median-of spreads)))
          (when (or (null grain) (< grain *copied-paper-grain-least*))
            (return-from copied-paper (values nil 0 grain)))
          (let* ((least (float (expt (max 1.0 (* *copied-paper-block-grain* grain)) 2) 1.0))
                 (steps (step-table (round (* 64 (max (cdr *copied-paper-step*)
                                                      (* (car *copied-paper-step*) grain))))))
                 (keys (make-array 4096 :element-type 'fixnum :adjustable t :fill-pointer 0)))
            (declare (type single-float least))
            ;; every block of grained paper near the words, keyed
            (fill sums 0) (fill squares 0) (fill inks 0) (fill whites 0)
            (dotimes (k 8) (add-row k 1))
            (dotimes (k 6) (add-pairs k))
            (dotimes (y (- height 8))
              (job-check-stop)
              (when (plusp y) (add-row (1- y) -1) (add-row (+ y 7) 1))
              (add-pairs (+ y 6))
              (blocks-along y 1 (lambda (x sum square ink white)
                                  (declare (type fixnum x sum square ink white))
                                  (when (and (zerop ink) (< white 2) (>= (spread sum square) least))
                                    (let ((key (block-key pairs width x y sum steps)))
                                      (when key
                                        (when (>= (fill-pointer keys) *copied-paper-blocks-most*)
                                          (return-from copied-paper (values :too-many (fill-pointer keys) grain)))
                                        (vector-push-extend (logior (ash key 27) (+ (* y width) x)) keys)))))))
            (let ((tried (fill-pointer keys)))
              (values (copied-regions (sorted-keys keys) grey width height grain) tried grain))))))))

(defun step-table (step)
  "For each sixteen times a sum of 2 by 2 pixels less the sum of the block of
8 by 8 pixels about it, -16320 to 16320, that difference rounded to STEP
\(a whole number from 1), as a vector from -16320 on (BLOCK-KEY)."
  (let ((table (make-array 32641 :element-type '(signed-byte 16))))
    (dotimes (k 32641 table)
      (setf (aref table k) (floor (+ (- k 16320) (ash step -1)) step)))))

(defun block-key (pairs width x y sum steps)
  "The coarse key of the block of 8 by 8 pixels of a picture WIDTH wide at X
Y, whose pixels add up to SUM: its 16 means of 2 by 2 pixels less the
block's mean, each rounded to steps by STEPS (STEP-TABLE), made one number
of 34 bits; NIL when fewer than 8 of them are other than 0. PAIRS holds the
sums of 2 by 2 pixels of 8 rows, row R's in its eighth R mod 8."
  (declare (type (simple-array (unsigned-byte 16) (*)) pairs) (type fixnum width x y) (type (integer 0 16320) sum)
           (type (simple-array (signed-byte 16) (*)) steps) (optimize speed))
  (let ((key 0) (other 0))
    (declare (type (unsigned-byte 34) key) (type fixnum other))
    (dotimes (j 4)
      (dotimes (i 4)
        (let* ((p (+ (* (logand (+ y (* 2 j)) 7) width) x (* 2 i)))
               (v (aref steps (+ (- (* 16 (min 1020 (aref pairs p))) sum) 16320))))
          (declare (type fixnum p v))
          (unless (zerop v) (incf other))
          (setf key (ldb (byte 34 0) (+ (* key 1000003) (logand (+ v 128) 255)))))))
    (and (>= other 8) key)))

(defun sorted-keys (keys)
  "KEYS, a vector of fixnums of 0 or more with a fill pointer, as a simple
vector of them from the least, sorted 11 bits at a time from the lowest:
several times faster than SORT on the millions of keys of a page."
  (let* ((n (fill-pointer keys))
         (a (make-array n :element-type 'fixnum)) (b (make-array n :element-type 'fixnum))
         (counts (make-array 2048 :element-type 'fixnum)))
    (declare (type (simple-array fixnum (*)) a b counts) (type fixnum n) (optimize speed))
    (replace a keys)
    (loop for shift of-type (integer 0 66) from 0 below 62 by 11
          do (fill counts 0)
             (dotimes (i n) (incf (aref counts (ldb (byte 11 shift) (aref a i)))))
             (let ((total 0))
               (declare (type fixnum total))
               (dotimes (d 2048) (let ((c (aref counts d))) (setf (aref counts d) total) (incf total c))))
             (dotimes (i n)
               (let* ((v (aref a i)) (d (ldb (byte 11 shift) v)))
                 (setf (aref b (aref counts d)) v)
                 (incf (aref counts d))))
             (rotatef a b))
    a))

(defun copied-regions (keys grey width height grain)
  "The regions of copied paper COPIED-PAPER gives, from KEYS, the sorted keys
of the blocks of GREY (WIDTH by HEIGHT) searched, each a block's key above
its index in GREY's 27 lowest bits, GRAIN the page's grain."
  (declare (type (simple-array fixnum (*)) keys) (type grey-pixels grey) (type fixnum width height))
  (let* ((tolerance (max (cdr *copied-paper-tolerance*) (* (car *copied-paper-tolerance*) grain)))
         (limit (floor (* 4096 tolerance tolerance)))
         (exact (floor (* 4096 *copied-paper-exact* *copied-paper-exact*)))
         (same '())
         (votes (make-hash-table))
         (n (length keys))
         (places (1- (ash 1 27))))
    (declare (type fixnum n limit exact places))
    (flet ((apart (a b)
             ;; 4096 times the mean square of the pixels' difference, each block's mean taken out
             (declare (type fixnum a b) (optimize speed))
             (let ((sum 0) (square 0))
               (declare (type fixnum sum square))
               (dotimes (j 8 (- (* 64 square) (* sum sum)))
                 (let ((ra (+ a (* j width))) (rb (+ b (* j width))))
                   (declare (type fixnum ra rb))
                   (dotimes (i 8)
                     (let ((d (- (aref grey (+ ra i)) (aref grey (+ rb i)))))
                       (declare (type fixnum d))
                       (incf sum d) (incf square (* d d)))))))))
      (let ((i 0) (groups 0))
        (declare (type fixnum i groups))
        (loop while (< i n)
              do (let ((j i) (key (ash (aref keys i) -27)))
                   (declare (type fixnum j))
                   (loop while (and (< j n) (= (ash (aref keys j) -27) key)) do (incf j))
                   (when (zerop (mod (incf groups) 65536)) (job-check-stop))
                   (when (<= 2 (- j i) *copied-paper-key-most*)
                     (loop for a from i below j
                           do (loop for b from (1+ a) below j
                                    do (let* ((pa (logand (aref keys a) places))
                                              (pb (logand (aref keys b) places))
                                              (dx (- (mod pb width) (mod pa width)))
                                              (dy (- (floor pb width) (floor pa width))))
                                         (declare (type fixnum pa pb dx dy))
                                         (when (or (minusp dy) (and (zerop dy) (minusp dx)))
                                           (rotatef pa pb) (setf dx (- dx) dy (- dy)))
                                         (when (and (>= (max (abs dx) (abs dy)) 8)
                                                    (not (and (zerop (mod dx 8)) (zerop (mod dy 8)))))
                                           (let ((d (apart pa pb)))
                                             (when (and (<= d exact) (plusp (mod dx 8)) (plusp (mod dy 8)))
                                               (push (list pa pb dx dy) same))
                                             (when (<= d limit)
                                               (let ((entry (or (gethash (+ (* dy 2 width) dx width) votes)
                                                                (setf (gethash (+ (* dy 2 width) dx width) votes)
                                                                      (list (cons dx dy) 0.0)))))
                                                 (incf (second entry) (/ (sqrt (max 0 d)) 64))
                                                 (push pb (cddr entry))))))))))
                   (setf i j)))))
    (let ((regions '()))
      (maphash (lambda (shift entry)
                 (declare (ignore shift))
                 (destructuring-bind ((dx . dy) total &rest matched) entry
                   (when (>= (length matched) *copied-paper-blocks-least*)
                     (dolist (members (joined-blocks matched width))
                       (when (>= (length members) *copied-paper-blocks-least*)
                         (let ((xs (mapcar (lambda (p) (mod p width)) members))
                               (ys (mapcar (lambda (p) (floor p width)) members)))
                           (push (make-copied-region :dx dx :dy dy :blocks (length members)
                                                     :x0 (reduce #'min xs) :y0 (reduce #'min ys)
                                                     :x1 (min width (+ 8 (reduce #'max xs)))
                                                     :y1 (min height (+ 8 (reduce #'max ys)))
                                                     :distance (/ total (length matched)))
                                 regions)))))))
               votes)
      (let* ((shifts (mapcar (lambda (r) (cons (copied-region-dx r) (copied-region-dy r))) regions))
             (sorted (sort (append (merged-regions regions)
                                   (patched-regions (remove-if (lambda (m) (member (cons (third m) (fourth m)) shifts
                                                                                 :test #'equal))
                                                               same)
                                                    width height))
                           #'> :key #'copied-region-blocks)))
        (subseq sorted 0 (min (length sorted) *copied-regions-most*))))))

(defun patched-regions (same width height)
  "The regions of paper pieced together from many places of a picture WIDTH
by HEIGHT, as a content-aware fill builds it over what it hides, each a
COPIED-REGION: from SAME, the blocks the same pixel for pixel as others
\(*COPIED-PAPER-EXACT*), each (A B DX DY), B lying DX across and DY down
from A, neither a whole number of blocks: paper once saved as a JPEG whose
blocks keep their grain in one direction only repeats along a row of
blocks, or down a column of them, at any shift. The blocks of both ends of
each pair are joined by their places (JOINED-BLOCKS, with a reach of
*PATCHED-PAPER-REACH*); a group is named when it holds
*PATCHED-PAPER-PIECES-LEAST* pairs or more matching at
*PATCHED-PAPER-SHIFTS-LEAST* shifts or more. Each piece of such a fill is
paper of the page laid a little way off, and its grain matches only at the
one shift it was laid at; one patch copied whole matches at one shift
\(COPIED-REGIONS)."
  (when (>= (length same) *patched-paper-pieces-least*)
    (let ((of (make-hash-table)) (out '()))
      (dolist (m same) (push m (gethash (first m) of)) (push m (gethash (second m) of)))
      (dolist (members (joined-blocks (loop for k being the hash-keys of of collect k) width *patched-paper-reach*)
                       out)
        (let* ((pairs (remove-duplicates (loop for k in members append (gethash k of))))
               (shifts (length (remove-duplicates (mapcar #'cddr pairs) :test #'equal)))
               (xs (mapcar (lambda (k) (mod k width)) members))
               (ys (mapcar (lambda (k) (floor k width)) members)))
          (when (and (>= (length pairs) *patched-paper-pieces-least*)
                     (>= shifts *patched-paper-shifts-least*))
            (push (make-copied-region :blocks (length pairs) :shifts shifts
                                      :x0 (reduce #'min xs) :y0 (reduce #'min ys)
                                      :x1 (min width (+ 8 (reduce #'max xs))) :y1 (min height (+ 8 (reduce #'max ys))))
                  out)))))))

(defun merged-regions (regions)
  "REGIONS (COPIED-REGIONs), those of one shift whose boxes overlap or lie
within 8 pixels of one another made one, its blocks theirs together and its
distance their mean: the pieces of one patch the bands about the words or
its plainer blocks cut apart, named once."
  (let ((out '()))
    (dolist (r regions out)
      (let ((near (find-if (lambda (o)
                             (and (= (copied-region-dx o) (copied-region-dx r))
                                  (= (copied-region-dy o) (copied-region-dy r))
                                  (<= (copied-region-x0 o) (+ (copied-region-x1 r) 8))
                                  (<= (copied-region-x0 r) (+ (copied-region-x1 o) 8))
                                  (<= (copied-region-y0 o) (+ (copied-region-y1 r) 8))
                                  (<= (copied-region-y0 r) (+ (copied-region-y1 o) 8))))
                           out)))
        (if near
            (let ((blocks (+ (copied-region-blocks near) (copied-region-blocks r))))
              (setf (copied-region-distance near) (/ (+ (* (copied-region-distance near) (copied-region-blocks near))
                                                        (* (copied-region-distance r) (copied-region-blocks r)))
                                                     blocks)
                    (copied-region-blocks near) blocks
                    (copied-region-x0 near) (min (copied-region-x0 near) (copied-region-x0 r))
                    (copied-region-y0 near) (min (copied-region-y0 near) (copied-region-y0 r))
                    (copied-region-x1 near) (max (copied-region-x1 near) (copied-region-x1 r))
                    (copied-region-y1 near) (max (copied-region-y1 near) (copied-region-y1 r))))
            (push r out))))))

(defun joined-blocks (places width &optional (reach 4))
  "PLACES, the indices of blocks in a picture WIDTH wide, in groups: a list
of lists of indices. The picture is cut into cells of REACH by REACH
pixels, and two blocks are joined when their cells are the same or touch,
so up to 2 × REACH - 1 pixels apart across or down; a group goes on
through every block joined to one of its own, however far that takes it."
  (let* ((v (coerce places 'simple-vector)) (m (length v))
         (parent (make-array m :element-type 'fixnum)) (cells (make-hash-table)) (groups (make-hash-table)))
    (dotimes (k m) (setf (aref parent k) k))
    (labels ((root (k) (if (= (aref parent k) k) k (setf (aref parent k) (root (aref parent k)))))
             (cell (cx cy) (+ (* cy (+ 2 (ceiling width reach))) cx)))
      (dotimes (k m)
        (let ((cx (1+ (floor (mod (svref v k) width) reach))) (cy (1+ (floor (floor (svref v k) width) reach))))
          (loop for ddy from -1 to 1
                do (loop for ddx from -1 to 1
                         do (dolist (o (gethash (cell (+ cx ddx) (+ cy ddy)) cells))
                              (let ((ra (root k)) (rb (root o)))
                                (unless (= ra rb) (setf (aref parent ra) rb))))))
          (push k (gethash (cell cx cy) cells))))
      (dotimes (k m) (push (svref v k) (gethash (root k) groups)))
      (loop for members being the hash-values of groups collect members))))

(defun scan-copied-paper (pdf revision mark grey width height words tilt level)
  "The regions of copied paper (COPIED-PAPER) of the scanned picture MARK
draws in REVISION of PDF, whose grey levels are GREY (WIDTH by HEIGHT), its
WORDS read with their lines falling TILT, its ink darker than LEVEL: a list
of COPIED-REGIONs, :TOO-MANY for a picture too large to search, or the
condition measuring it failed with. NIL, not searched, when the picture is
kept as a JPEG or a JPEG 2000 (PICTURE-LOSSY-P), whose requantised blocks
of a copy no longer match. A picture once saved as a JPEG and kept without
loss since is searched: paper copied after that save is the same pixel for
pixel, and the blocks of flat paper the save repeats lie at the whole-block
shifts COPIED-PAPER leaves out."
  (unless (picture-lossy-p pdf revision mark)
    (handler-case (values (copied-paper grey width height words tilt level))
      ((or error storage-condition) (condition) condition))))

(defun check-copied-paper (pdf revision)
  "The scanned pages of REVISION of PDF whose paper is alike pixel for pixel
with paper elsewhere on the same page (SCAN-COPIED-PAPER), one finding worth
a look for each page, naming both places of each region and the shift
between them; for a page too large to search, that it was not, for the
record; for one whose measuring failed, that it could not finish. Never
strong: a form's printed pattern repeating at one shift can match too."
  (let ((out '()))
    (dolist (scan (remove-if #'scan-picture-twin (scan-reading-scans (scans-read pdf revision))) (nreverse out))
      (let ((found (scan-picture-copied scan)) (page (1+ (page-index (scan-picture-page scan))))
            (m (scan-picture-mark scan)) (width (scan-picture-width scan)) (height (scan-picture-height scan)))
        (cond ((typep found 'condition)
               (push (unfinished (format nil "the copied paper of page ~d" page) found) out))
              ((eq found :too-many)
               (push (found :record (format nil "Page ~d was not searched for paper copied from elsewhere on it: ~
                                                 its paper near the words holds more than ~:d blocks to set ~
                                                 beside one another."
                                            page *copied-paper-blocks-most*)
                            :rule (documentation 'copied-paper 'function)
                            :revision (revision-index revision) :page page)
                     out))
              (found
               (flet ((places (r)
                        (let ((x0 (copied-region-x0 r)) (y0 (copied-region-y0 r))
                              (x1 (copied-region-x1 r)) (y1 (copied-region-y1 r))
                              (dx (copied-region-dx r)) (dy (copied-region-dy r)))
                          (list (pixels-place m (- x0 dx) (- y0 dy) (- x1 dx) (- y1 dy) width height)
                                (pixels-place m x0 y0 x1 y1 width height)))))
                 (let ((first (first found)))
                   (push (found :look
                                (cond
                                  ((rest found)
                                    (format nil "~d patches of page ~d's paper, in ~a, are alike pixel for pixel ~
                                                 with paper elsewhere on it: one copied over the other, as a ~
                                                 clone stamp or a content-aware fill copies paper over what was ~
                                                 there."
                                            (length found) page (scan-named m)))
                                  ((> (copied-region-shifts first) 1)
                                    (format nil "A patch of page ~d's paper, about ~d by ~d pixels, in ~a, is ~
                                                 pieced together from ~d blocks alike pixel for pixel with paper ~
                                                 about them, laid from ~d places: as a content-aware fill builds ~
                                                 paper over what was there."
                                            page (- (copied-region-x1 first) (copied-region-x0 first))
                                            (- (copied-region-y1 first) (copied-region-y0 first))
                                            (scan-named m :short t) (copied-region-blocks first)
                                            (copied-region-shifts first)))
                                  (t
                                    (format nil "A patch of page ~d's paper, about ~d by ~d pixels, in ~a, is ~
                                                 alike pixel for pixel with the paper ~d pixels across and ~d ~
                                                 down from it: one copied over the other, as a clone stamp or a ~
                                                 content-aware fill copies paper over what was there."
                                            page (- (copied-region-x1 first) (copied-region-x0 first))
                                            (- (copied-region-y1 first) (copied-region-y0 first))
                                            (scan-named m :short t) (copied-region-dx first)
                                            (copied-region-dy first))))
                                :rule (format nil "A scanner's grain is never the same twice: no two patches of ~
                                                   a page's paper are alike pixel for pixel. Paper copied over ~
                                                   characters with a clone stamp, or by a content-aware fill, ~
                                                   brings its grain along and is alike with the place it came ~
                                                   from. Named when ~d blocks of 8 by 8 pixels or more of grained ~
                                                   paper near the words match other paper of the page pixel by ~
                                                   pixel at one shift of 8 pixels or more; or when ~d blocks or ~
                                                   more, within ~d pixels of one another, are the same pixel for ~
                                                   pixel as paper about them at ~d shifts or more, as a ~
                                                   content-aware fill pieces paper together. Which of the two is ~
                                                   the copy is not told. Searched only on pictures kept without ~
                                                   loss and never saved as a JPEG, whose paper keeps a grey level ~
                                                   of grain or more: a JPEG rounds flat paper to the same few ~
                                                   values in its blocks, which then repeat at shifts of whole ~
                                                   blocks, so those shifts are left out everywhere. A form's ~
                                                   printed pattern repeating at one shift gives the same sign."
                                              *copied-paper-blocks-least* *patched-paper-pieces-least*
                                              *patched-paper-reach* *patched-paper-shifts-least*)
                                :revision (revision-index revision) :page page :tally (list (length found) "patch")
                                :objects (objects-at m (loop for r in found
                                                             collect (cons (copied-region-y0 r) (copied-region-y1 r))
                                                             when (= (copied-region-shifts r) 1)
                                                               collect (cons (- (copied-region-y0 r)
                                                                                (copied-region-dy r))
                                                                             (- (copied-region-y1 r)
                                                                                (copied-region-dy r)))))
                                :details (loop for r in found
                                               collect (destructuring-bind (upper lower) (places r)
                                                         (if (> (copied-region-shifts r) 1)
                                                             (format nil "~a: ~d blocks of 8 by 8 pixels the same ~
                                                                          as paper about them, at ~d shifts"
                                                                     lower (copied-region-blocks r)
                                                                     (copied-region-shifts r))
                                                             (format nil "~a and ~a, the second ~d pixels across ~
                                                                          and ~d down from the first: ~d blocks of ~
                                                                          8 by 8 pixels alike, ~,2f grey levels apart"
                                                                     upper lower (copied-region-dx r)
                                                                     (copied-region-dy r) (copied-region-blocks r)
                                                                     (copied-region-distance r)))))
                                :evidence (pictures-evidence pdf revision (ends-objects m))
                                :method :paper-copied)
                         out)))))))))
