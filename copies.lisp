;;;; copies.lisp --- Characters copied within a scan: prints of a character too alike to be prints.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; No scanner draws a character twice alike. The characters of the scanned
;;;; pages are cut apart, measured for grain and set beside the others of their
;;;; shape; those sharing pixels the same, or nearer than prints of one character
;;;; are, are named as copies.

(in-package #:palimpsest)

;;; Characters copied within a scan

(defparameter *glyph-least-shape* 1/3
  "The least shape a character must have for the likeness of its ink alone
to another's to tell a copy (SHAPELESS-GLYPH-P): how many times its rows
and columns cross its ink more than once, over its width and height
together.")

(defun glyph-crossings (glyph)
  "How much shape GLYPH has: of each row and each column of its box, how
many times more than once it crosses the character's ink (darker than
halfway from its lightest grey to its darkest, GLYPH-INK-LEVEL), over its
width and height together. A stroke alone (l, I, a dash, a bar) crosses
each once, 0; i, j, r, t, f, 1, 7, L and T a little, 0.3 at most; c, v
and the rounder letters and figures 0.4 and more, 8, g and B about 1."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph)))
         (ink (car (svref (glyph-inks glyph) 0))) (more 0))
    (declare (type grey-pixels pixels) (type fixnum across w h ink more))
    (flet ((ink-p (i j) (< (aref pixels (+ (* (+ 2 j) across) 2 i)) ink)))
      (dotimes (j h)
        (let ((runs 0) (was nil))
          (declare (type fixnum runs))
          (dotimes (i w) (let ((now (ink-p i j))) (when (and now (not was)) (incf runs)) (setf was now)))
          (incf more (max 0 (1- runs)))))
      (dotimes (i w)
        (let ((runs 0) (was nil))
          (declare (type fixnum runs))
          (dotimes (j h) (let ((now (ink-p i j))) (when (and now (not was)) (incf runs)) (setf was now)))
          (incf more (max 0 (1- runs))))))
    (/ more (max 1 (+ w h)))))

(defun shapeless-glyph-p (glyph)
  "True when GLYPH has too little shape (GLYPH-CROSSINGS under
*GLYPH-LEAST-SHAPE*) for the likeness of its ink alone to another's to tell
a copy from a print: strokes alone come alike by chance."
  (< (glyph-crossings glyph) *glyph-least-shape*))

(defun rule-piece-p (glyph tallest &optional typical)
  "True when GLYPH is no character but a piece of a rule or a frame. That
is so when it is taller by half than TALLEST, the height of the page's
taller characters (UPPER-HEIGHT). It is so when it is five times as tall
as it is wide or more. It is so when it is taller by a tenth than TALLEST
and three times as tall as it is wide or more, as no l, I or 1 is. And it
is so when it is three times as wide as it is tall or more and wider by
half than TYPICAL, how wide the page's characters typically are
\(TYPICAL-WIDTH). Such a mark is a bar standing or lying, or a rule's
remains. A hyphen, a minus sign or a dash is narrower, and a character."
  (let ((w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph))))
    (or (and tallest (> h (* 3/2 tallest)))
        (>= h (* 5 w))
        (and tallest (> h (* 11/10 tallest)) (>= h (* 3 w)))
        (and typical (>= w (* 3 h)) (> w (* 3/2 typical))))))

(defun glyph-ring-distance (a b)
  "How unlike the characters A and B (GLYPHs) are over their boxes and the
two pixels of paper round them (GLYPH-CUT), set together at their boxes'
top left corners: the root of the mean squared difference of their grey
levels, over what both cuts hold. A character copied with the paper about
it keeps that paper; two prints of a stroke have paper of their own."
  (let* ((pa (glyph-pixels a)) (pb (glyph-pixels b)) (wa (glyph-width a)) (wb (glyph-width b))
         (w (min wa wb)) (h (min (floor (length pa) wa) (floor (length pb) wb))) (sum 0))
    (declare (type grey-pixels pa pb) (type fixnum wa wb w h) (type (integer 0) sum))
    (dotimes (j h)
      (dotimes (i w)
        (let ((d (- (aref pa (+ (* j wa) i)) (aref pb (+ (* j wb) i))))) (incf sum (* d d)))))
    (sqrt (/ sum (max 1 (* w h))))))

(defun glyph-distance (a b limit)
  "How unlike the characters A and B (GLYPHs) are in their ink. It is the
mean of the squared differences of their grey levels over the pixels inked
in either (darker than halfway from A's lightest to its darkest). The
narrower set is laid along the wider, a pixel higher and lower too (their
tops alike within a pixel, as one character's are). The closest way counts,
so a character copied with a sliver of its neighbour is found in its copy.
The paper about them is left out: a copy laid with a Darken blend keeps its
ink, not its paper. So is ink the scanner clipped in both, as dark as or
darker than each one's GLYPH-CLIPPED: it is one grey in every print, and
would make any two prints alike. NIL when no way of setting them together
comes under LIMIT. As a second value, true, but NIL (with NIL for the
first) when some way of setting them together leaves fewer than 20 pixels
once the clipped ink is set aside (as few as a character's least ink,
WORD-GLYPHS). The two cannot then be told apart, and are not judged. As a
third value, how many pixels the closest way counted."
  (when (> (- (glyph-x1 a) (glyph-x0 a)) (- (glyph-x1 b) (glyph-x0 b))) (rotatef a b))
  (let* ((w (+ (- (glyph-x1 a) (glyph-x0 a)) 2))
         (h (+ (min (- (glyph-y1 a) (glyph-y0 a)) (- (glyph-y1 b) (glyph-y0 b))) 2))
         (wider (- (- (glyph-x1 b) (glyph-x0 b)) (- (glyph-x1 a) (glyph-x0 a))))
         (pa (glyph-pixels a)) (pb (glyph-pixels b)) (wa (glyph-width a)) (wb (glyph-width b))
         (ca (glyph-clipped a)) (cb (glyph-clipped b)) (both-clipped (and (>= ca 0) (>= cb 0)))
         (ink 0) (most 0) (best nil) (counted 0))
    (declare (type grey-pixels pa pb) (type fixnum w h wider wa wb ca cb ink most counted)
             (optimize speed))
    ;; within each one's cut (GLYPH-CUT) its box begins two pixels in; A's window from a pixel before it
    (unless (<= (* (+ 1 h) wa) (length pa))
      (return-from glyph-distance (values nil t)))
    (let* ((k (- h (- (glyph-y1 a) (glyph-y0 a)) 1))
           (level (if (<= 0 k 1) (svref (glyph-inks a) k) (glyph-ink-level a h))))
      (setf ink (car level) most (cdr level)))
    ;; no comparison counts more pixels than both hold ink: a bound to stop early by
    (incf most (aref (the (simple-array (unsigned-byte 32) (17)) (glyph-darker b)) (min 16 (1+ (ash ink -4)))))
    (when (plusp most)
      (let ((bound (ceiling (* limit most))))
        (declare (type fixnum bound))
        (dolist (dy '(0 -1 1))
          (declare (type fixnum dy))
          (loop for dx fixnum from 0 to (+ 2 wider)
                do (let ((bx (+ 1 (if (> dx wider) -1 dx))) (by (+ 1 dy)) (sum 0) (n 0) (out 0) (whole t))
                     (declare (type fixnum bx by sum n out))
                     (when (and (>= bx 0) (>= by 0) (<= (+ bx w) wb) (<= (* (+ by h) wb) (length pb)))
                       (block rows
                         (dotimes (j h)
                           (let ((ra (+ (* (+ 1 j) wa) 1)) (rb (+ (* (+ by j) wb) bx)))
                             (declare (type fixnum ra rb))
                             (macrolet ((each-pixel (clipped)
                                          `(dotimes (i w)
                                             (let ((ga (aref pa (+ ra i))) (gb (aref pb (+ rb i))))
                                               (when (or (< ga ink) (< gb ink))
                                                 (if (and ,clipped (<= ga ca) (<= gb cb))
                                                     (incf out)
                                                     (progn (incf n)
                                                            (let ((d (- ga gb)))
                                                              (declare (type fixnum d))
                                                              (incf sum (* d d))))))))))
                               ;; only characters that reach the clipped level have ink to set aside
                               (if both-clipped (each-pixel t) (each-pixel nil)))
                             (when (>= sum bound) (setf whole nil) (return-from rows))))
                         ;; set together this way, too few pixels left once the clipped ink is set aside:
                         ;; the two cannot be told apart, nor judged
                         (when (and whole (plusp out) (< n 20))
                           (return-from glyph-distance (values nil nil)))
                         (when (plusp n)
                           (let ((mean (/ sum n)))
                             (when (and (< mean limit) (or (null best) (< mean best)))
                               (setf best mean counted n bound (min bound (ceiling (* mean most))))))))))))))
    (values best t counted)))

(defparameter *glyph-peers-most* 300
  "The most characters one is set beside for copies (GLYPH-PEERS): those
nearest it in ink, a page holding seldom more prints of one character.")

(defstruct (peer-bucket (:constructor make-peer-bucket (glyphs inks places)) (:copier nil) (:predicate nil))
  "The characters of one width, height and page, as GLYPH-PEERS keeps them:
GLYPHS from the least ink to the most, those of the same ink in the order
they were given; INKS, their ink; PLACES, where each was in that order."
  (glyphs #() :type simple-vector)
  (inks (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (places (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*))))

(defun glyph-peers (glyphs)
  "A function of a GLYPH giving those of GLYPHS that could be the same
character. They are as tall within a pixel, as wide within three, and of as
much ink within an eighth or so (a copy keeps its ink, give or take a few
pixels). Of these, the *GLYPH-PEERS-MOST* nearest it in ink are taken, on
its page or two pages either side. A character is copied from near at hand,
and so a long scan takes time as its pages do, not as their pairs. Of those
as near in ink, those of the greater width come first, then the taller,
then the later page, and of one size and page those first in GLYPHS. They
are given in a vector with a fill pointer, the same vector each time, its
contents those of the last call: a long scan's search makes no garbage for
each character. Not for two threads at once."
  (let* ((span (+ 4
                  (reduce #'max
                          glyphs
                          :key (lambda (g) (max (- (glyph-x1 g) (glyph-x0 g)) (- (glyph-y1 g) (glyph-y0 g))))
                                       :initial-value 0)))
         (buckets (make-hash-table)) (around (make-hash-table))
         (low (make-array 105 :element-type 'fixnum)) (high (make-array 105 :element-type 'fixnum))
         (room (make-array *glyph-peers-most* :fill-pointer 0)))
    (labels ((key (w h page)
               ;; one number for each width, height and page; none for those no character of GLYPHS is near
               (and (< -1 w span) (< -1 h span) (>= page 0) (+ w (* span (+ h (* span page))))))
             (glyph-key (g) (key (- (glyph-x1 g) (glyph-x0 g)) (- (glyph-y1 g) (glyph-y0 g)) (glyph-page g)))
             (beside (g)
               ;; the buckets of the sizes and pages beside G's, the widest, tallest and latest first
               (let ((w (- (glyph-x1 g) (glyph-x0 g))) (h (- (glyph-y1 g) (glyph-y0 g))) (page (glyph-page g)))
                 (coerce (loop for dw from 3 downto -3
                               nconc (loop for dh from 1 downto -1
                                           nconc (loop for dp from 2 downto -2
                                                       for bucket = (let ((k (key (+ w dw) (+ h dh) (+ page dp))))
                                                                      (and k (gethash k buckets)))
                                                       when bucket collect bucket)))
                         'simple-vector)))
             (near-buckets (g)
               ;; those of G's size and page worked out once
               (let ((own (glyph-key g)))
                 (if own
                     (or (gethash own around) (setf (gethash own around) (beside g)))
                     (beside g)))))
      (let ((given (make-hash-table)))
        (loop for g in glyphs for place fixnum from 0
              do (push (cons place g) (gethash (glyph-key g) given)))
        (loop for k being the hash-keys of given using (hash-value members)
              do (let ((members (stable-sort (nreverse members) #'< :key (lambda (m) (glyph-ink (cdr m))))))
                   (setf (gethash k buckets)
                         (make-peer-bucket (map 'simple-vector #'cdr members)
                                           (map '(simple-array fixnum (*)) (lambda (m) (glyph-ink (cdr m))) members)
                                           (map '(simple-array fixnum (*)) #'car members))))))
      (lambda (g)
        (let* ((near (near-buckets g)) (most *glyph-peers-most*) (count 0)
               (ink (glyph-ink g)) (limit (max 3 (* 0.12 ink))))
          (declare (type simple-vector near) (type fixnum most count ink))
          (when (< (array-dimension room 0) most) (setf room (make-array most :fill-pointer 0)))
          ;; in each bucket, HIGH the first of as much ink or more, LOW the last of less
          (loop for i from 0 for bucket across near
                do (let ((inks (peer-bucket-inks bucket)) (a 0) (b 0))
                     (declare (type fixnum a b))
                     (setf b (length inks))
                     (loop while (< a b)
                           do (let ((m (ash (+ a b) -1)))
                                (if (< (aref inks m) ink) (setf a (1+ m)) (setf b m))))
                     (setf (aref high i) a (aref low i) (1- a))))
          ;; ink a distance D away, the nearest first, bucket by bucket; in a bucket, those first given first
          (block peers
            (loop
             (let ((d nil))
               (loop for i from 0 for bucket across near
                     do (let ((inks (peer-bucket-inks bucket)) (h (aref high i)) (l (aref low i)))
                          (when (< h (length inks))
                            (let ((e (- (aref inks h) ink))) (when (or (null d) (< e d)) (setf d e))))
                          (when (>= l 0) (let ((e (- ink (aref inks l)))) (when (or (null d) (< e d)) (setf d e))))))
               (when (or (null d) (> d limit)) (return))
               (loop for i from 0 for bucket across near
                     do (let* ((inks (peer-bucket-inks bucket)) (places (peer-bucket-places bucket))
                               (members (peer-bucket-glyphs bucket)) (n (length inks))
                               (h (aref high i)) (h-end h) (l-end (aref low i)) (l l-end))
                          (declare (type fixnum h h-end l l-end n))
                          (loop while (and (< h-end n) (= (aref inks h-end) (+ ink d))) do (incf h-end))
                          (loop while (and (>= l 0) (= (aref inks l) (- ink d))) do (decf l))
                          (setf (aref high i) h-end (aref low i) l)
                          ;; those of less ink are L+1 to L-END, of more H to H-END, each in the order given
                          (incf l)
                          (loop while (or (< h h-end) (<= l l-end))
                                do (let ((p (if (and (<= l l-end) (or (>= h h-end) (< (aref places l) (aref places h))))
                                                (prog1 (svref members l) (incf l))
                                                (prog1 (svref members h) (incf h)))))
                                     (unless (eq p g)
                                       (setf (aref room count) p)
                                       (incf count)
                                       (when (>= count most) (return-from peers))))))))))
          (setf (fill-pointer room) count)
          room)))))

(defun numbers-hash (octets-each &rest numbers)
  "The 64-bit FNV-1a hash (OCTETS-HASH) of the lowest OCTETS-EACH octets of
each of NUMBERS in turn, the lowest octet first."
  (declare (dynamic-extent numbers))
  (let ((octets (make-array (* octets-each (length numbers)) :element-type '(unsigned-byte 8))) (i 0))
    (declare (dynamic-extent octets) (type fixnum i))
    (dolist (n numbers)
      (dotimes (k octets-each)
        (setf (aref octets i) (ldb (byte 8 (* 8 k)) n))
        (incf i)))
    (octets-hash octets)))

(defun glyph-place-hash (glyph)
  "A number for where GLYPH is, the same however many characters are about
it: the 64-bit FNV-1a hash of its picture, page and box's top left corner,
cut to a fixnum."
  (ldb (byte 62 0)
       (numbers-hash 4 (or (glyph-picture glyph) 0) (or (glyph-page glyph) 0) (glyph-x0 glyph) (glyph-y0 glyph))))

(defun likeness-sample (glyphs most)
  "Up to MOST of GLYPHS, those of the lowest GLYPH-PLACE-HASH: a sample that
characters coming or going change only by themselves, so that how alike a
page's characters are does not move because a few more were read."
  (if (<= (length glyphs) most)
      glyphs
      (mapcar #'cdr (subseq (sort (mapcar (lambda (g) (cons (glyph-place-hash g) g)) glyphs) #'< :key #'car) 0 most))))

(defun alike-characters (glyphs peers &key (most 400) (beside (constantly t)))
  "How alike characters of the same shape typically are among GLYPHS: of the
root of the mean squared difference of each, over up to MOST of them
\(LIKENESS-SAMPLE), to the nearest of its PEERS (GLYPH-PEERS) it may be set
BESIDE (a function of the character and a peer), each counted when under 30 grey
levels, the middle. Measured on characters as they were found, no pieces of
rules (RULE-PIECE-P), which come alike as no prints do. Hyphens or dashes under
6 rows tall were left out too, which come near one another by chance. So were
marks wider than half as wide again as a typical character (TYPICAL-WIDTH).
Letters the blur joined, and pieces cut from them (SPLIT-JOINED), have no print
as alike as two prints of a letter are, for the paper and the neighbour's ink
cut with them. On a page set so tight that fewer than a quarter of its
letter-sized marks stand alone, those left alone are the letters by a space or a
stop and not the page's prints; there it is measured on every letter-sized mark,
pieces too, and the lower quarter of them is taken, below which the further
pieces seldom reach. NIL for fewer than 40 such characters, too few to tell. As
three values more: how many characters were set beside others, how many of them
had one within 30 grey levels, and how alike those were, taken as the first
value is, NIL for none."
  (flet ((width (g) (- (glyph-x1 g) (glyph-x0 g))))
    (let* ((typical (typical-width glyphs))
           (tallest (upper-height glyphs))
           (letters (and typical (remove-if (lambda (g) (or (eq (glyph-split g) :unsure) (> (width g) (* 3/2 typical))
                                                            (rule-piece-p g tallest typical)
                                                            (< (- (glyph-y1 g) (glyph-y0 g)) 6)))
                                            glyphs)))
           (whole (remove-if #'glyph-split letters))
           (tight (< (length whole) (/ (length letters) 4)))
           (measured (likeness-sample (if tight letters whole) most))
           (nearest (loop for g in measured
                          do (job-check-stop)
                          append (let ((best nil))
                                         (loop for p across (funcall peers g)
                                               when (funcall beside g p)
                                                 do (let ((d (glyph-distance g p (or best 900))))
                                                      (when (and d (or (null best) (< d best))) (setf best d))))
                                         (and best (list (sqrt best)))))))
      (let* ((near (length nearest))
             (alike (and nearest
                         (if tight
                             (nth (floor near 4) (sort nearest #'<))
                             (median-of nearest)))))
        (values (and (>= near 40) alike) (length measured) near alike)))))

(defun nearest-glyph (glyph peers limit &optional but)
  "The one of PEERS (a vector) nearest GLYPH, BUT apart, as four values: it and the
root of its GLYPH-DISTANCE, NIL for both when none is nearer than LIMIT (a
root too); true when GLYPH was judged beside one of them at least, or
there are none (GLYPH-DISTANCE); and how many pixels were counted beside
the nearest."
  (let ((best nil) (bound (expt limit 2)) (judged (zerop (length peers))) (counted 0))
    (loop for p across peers
          unless (eq p but)
            do (multiple-value-bind (d judged-here n) (glyph-distance glyph p bound)
                 (when judged-here (setf judged t))
                 (when (and d (< d bound)) (setf best p bound d counted n))))
    (values best (and best (sqrt bound)) judged counted)))

(defun glyph-box-hash (glyph)
  "A number for GLYPH's pixels, the same for characters of the same pixels:
the 64-bit FNV-1a hash of its box's width and height and every grey level
in it, row by row, cut to a fixnum. Characters of different numbers differ
in some pixel; two of the same number may still differ (SAME-PIXELS-P
tells)."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph)))
         (hash (numbers-hash 2 w h)))
    (declare (type grey-pixels pixels) (type fixnum across w h) (type (unsigned-byte 64) hash) (optimize speed))
    ;; within its cut (GLYPH-CUT) the box begins two pixels in
    (dotimes (j h)
      (let ((row (+ (* (+ 2 j) across) 2)))
        (declare (type fixnum row))
        (setf hash (octets-hash pixels :start row :end (+ row w) :hash hash))))
    (ldb (byte 62 0) hash)))

(defun same-pixels-p (a b)
  "True when the characters A and B (GLYPHs) are of the same size and every
grey level of their boxes is the same."
  (let ((w (- (glyph-x1 a) (glyph-x0 a))) (h (- (glyph-y1 a) (glyph-y0 a)))
        (pa (glyph-pixels a)) (pb (glyph-pixels b)) (wa (glyph-width a)) (wb (glyph-width b)))
    (declare (type grey-pixels pa pb) (type fixnum w h wa wb))
    (and (= w (- (glyph-x1 b) (glyph-x0 b))) (= h (- (glyph-y1 b) (glyph-y0 b)))
         (loop for j below h
               for ra = (+ (* (+ 2 j) wa) 2) and rb = (+ (* (+ 2 j) wb) 2)
               never (mismatch pa pb :start1 ra :end1 (+ ra w) :start2 rb :end2 (+ rb w))))))

(defun glyph-core-spread (glyph &optional (counts (make-array 256 :element-type 'fixnum)))
  "How far the grey levels of the core of GLYPH's ink stray, as two
values. The first is the middle of their distances from their middle (the
median absolute deviation) of the core's pixels, those no lighter than
halfway from the darkest to the lightest of its box. The second is the
darkest grey of its box. Both are NIL for a core of fewer than 8 pixels. A
scanner's grain makes the core of every print stray by some grey levels; a
program that draws text fills its strokes with one grey, and so does a
scanner that clips ink, a stamp or a coloured ink, to a grey or two.
COUNTS, 256 fixnums, a buffer to count the core's greys in, for a caller
measuring many characters."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph)))
         (lo 255) (hi 0) (n 0))
    (declare (type grey-pixels pixels) (type (simple-array fixnum (256)) counts) (type fixnum across w h lo hi n))
    (fill counts 0)
    (dotimes (j h)
      (dotimes (i w) (let ((g (aref pixels (+ (* (+ 2 j) across) 2 i)))) (setf lo (min lo g) hi (max hi g)))))
    (let ((half (+ lo (floor (- hi lo) 2))))
      (dotimes (j h)
        (dotimes (i w)
          (let ((g (aref pixels (+ (* (+ 2 j) across) 2 i))))
            (when (<= g half) (incf n) (incf (aref counts g)))))))
    (when (>= n 8)
      (flet ((middle (count-of)
               ;; the grey (or distance) at which half of the core's pixels are reached
               (let ((seen 0))
                 (dotimes (k 256 255)
                   (incf seen (funcall count-of k))
                   (when (>= (* 2 seen) n) (return k))))))
        (let ((median (middle (lambda (k) (aref counts k)))))
          (values (middle (lambda (k) (if (zerop k)
                                          (aref counts median)
                                          (+ (if (<= 0 (- median k)) (aref counts (- median k)) 0)
                                             (if (<= (+ median k) 255) (aref counts (+ median k)) 0)))))
                  lo))))))

(defun glyph-paper-grain (glyph)
  "How far the paper within GLYPH's box strays, in grey levels: the middle
of how far each pixel of it lies from the middle grey of the 9 about it,
taking the pixels lighter than three quarters of the way from the darkest
to the lightest grey of the box whose 9 are all so light, away from the
ink and its smoothed edge. NIL for fewer than 8 such pixels, or for a
character whose pixels are no longer kept. A scanner leaves its grain on
the paper as on the ink, and a copy of a piece of the page carries that
paper with it; a program that draws a figure on a clean ground leaves
none. Measured on paper, it does not take in the smoothed edge of a thin
stroke, which a character's core does (GLYPH-CORE-SPREAD)."
  (let ((pixels (glyph-pixels glyph)))
    (when pixels
      (let* ((across (glyph-width glyph))
             (w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph)))
             (lo 255) (hi 0) (n 0)
             (counts (make-array 256 :element-type 'fixnum :initial-element 0))
             (nine (make-array 9 :element-type 'fixnum)))
        (declare (type grey-pixels pixels) (type fixnum across w h lo hi n))
        (flet ((at (i j) (aref pixels (+ (* (+ 2 j) across) 2 i))))
          (dotimes (j h) (dotimes (i w) (let ((g (at i j))) (setf lo (min lo g) hi (max hi g)))))
          (let ((light (- hi (floor (- hi lo) 4))))
            ;; the box begins two pixels into the cut, so every pixel of it has its 9
            (dotimes (j h)
              (dotimes (i w)
                (let ((k 0) (clear t))
                  (loop for dj from -1 to 1
                        do (loop for di from -1 to 1
                                 do (let ((v (at (+ i di) (+ j dj))))
                                      (setf (aref nine k) v) (incf k)
                                      (when (<= v light) (setf clear nil)))))
                  (when clear
                    (incf n)
                    (incf (aref counts (abs (- (at i j) (aref (sort nine #'<) 4)))))))))))
        (when (>= n 8)
          ;; the middle, told within a grey level by where it falls among the pixels of its level
          (let ((seen 0))
            (dotimes (k 256)
              (let ((c (aref counts k)))
                (when (and (plusp c) (>= (* 2 (+ seen c)) n))
                  (return (float (max 0 (+ k -1/2 (/ (- (/ n 2) seen) c))))))
                (incf seen c)))))))))

(defun grain-levels (glyphs)
  "GLYPHS, each with its GLYPH-BARE, GLYPH-CLIPPED and GLYPH-TYPICAL set from
its page's characters. TYPICAL is the middle of how far their cores stray.
Without grain of its own (BARE) is a character whose core strays
(GLYPH-CORE-SPREAD) by a grey level or less, or by a fifth of what its page's
characters typically stray (the middle of them) or less. CLIPPED is the grey at
or below which its ink is set aside when it is set beside another
(GLYPH-DISTANCE): for one without grain, its own darkest grey and two above it
(a scanner's clipping jitters by a grey or two); for one that reaches the
darkest grey its page's characters reach, that grey; else -1, none."
  (let ((pages (make-hash-table :test 'equal)) (spread (make-hash-table :test 'eq))
        (darkest (make-hash-table :test 'eq))
        (counts (make-array 256 :element-type 'fixnum)))
    (dolist (g glyphs)
      (multiple-value-bind (s lo) (glyph-core-spread g counts)
        (setf (gethash g spread) s (gethash g darkest) (or lo 255)))
      (push g (gethash (cons (glyph-picture g) (glyph-page g)) pages)))
    (loop for on-page being the hash-values of pages
          do (let* ((spreads (remove nil (mapcar (lambda (g) (gethash g spread)) on-page)))
                    (typical (if spreads (median-of spreads) 0))
                    (floor (reduce #'min on-page :key (lambda (g) (gethash g darkest)))))
               (dolist (g on-page)
                 (let* ((s (gethash g spread)) (bare (and s (<= s (max 1 (/ typical 5))))))
                   (setf (glyph-bare g) bare (glyph-typical g) typical (glyph-spread g) s
                         (glyph-clipped g) (max (if (<= (gethash g darkest) floor) floor -1)
                                                (if bare (+ (gethash g darkest) 2) -1)))))))
    glyphs))

(defun bare-glyphs (on-page)
  "The characters of ON-PAGE, those of one scanned page, without grain of
their own (GRAIN-LEVELS), in a table; NIL when the page cannot tell: when
it has fewer than a hundred characters with a core to measure, or more than
one in twenty of them without grain."
  (let ((measured 0) (out (make-hash-table :test 'eq)) (counts (make-array 256 :element-type 'fixnum)))
    (dolist (g on-page)
      (when (glyph-core-spread g counts)
        (incf measured)
        (when (glyph-bare g) (setf (gethash g out) t))))
    (and (>= measured 100) (<= (hash-table-count out) (* 1/20 measured)) out)))

(defun strip-width (glyphs)
  "How many columns wide a strip is that SAME-STRIPS sets characters of
GLYPHS beside each other by: two thirds of their typical width
\(TYPICAL-WIDTH), 4 at least (two thirds of a letter, or a narrow letter
and the paper about it)."
  (max 4 (ceiling (* 2 (or (typical-width glyphs) 6)) 3)))

(defun strip-measures (glyphs)
  "What SAME-STRIPS sets the characters GLYPHS beside each other by, as two
values: how many columns wide a strip is (STRIP-WIDTH), and the grey below
which a pixel is ink for a strip's rows, the middle of the characters' own
halfway greys (GLYPH-INKS), the same for every strip wherever it is found."
  (values (strip-width glyphs)
          (or (median-of (mapcar (lambda (g) (car (svref (glyph-inks g) 0))) glyphs)) 128)))

(defun glyph-strip (glyph at k level)
  "The strip of GLYPH's cut (GLYPH-CUT) from its column AT, K columns
wide, as three values. They are its first row and the row after its last,
the rows of those columns that hold a pixel darker than LEVEL, and how
many of its pixels are so dark but not as dark as GLYPH-CLIPPED (ink the
scanner clipped, the same grey in every print). It is NIL when those
columns hold no such pixel, or ink that goes on past the cut's first or
last row (another line's, which the cut does not hold whole)."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)) (down (floor (length pixels) across))
         (clipped (glyph-clipped glyph)) (top nil) (bottom 0) (n 0))
    (declare (type grey-pixels pixels) (type fixnum across down clipped bottom n at k level) (optimize speed))
    (dotimes (j down)
      (let ((row (+ (* j across) at)) (inked nil))
        (declare (type fixnum row))
        (dotimes (i k)
          (let ((p (aref pixels (+ row i))))
            (when (< p level)
              (setf inked t)
              (when (> p clipped) (incf n)))))
        (when inked (unless top (setf top j)) (setf bottom (1+ j)))))
    (and top (> top 0) (< bottom down) (values top bottom n))))

(defun strip-hash (glyph at k top bottom)
  "A number for the pixels of GLYPH's cut from column AT, K columns wide,
in rows TOP to BOTTOM (the last excluded), the same for strips of the same
pixels: the 64-bit FNV-1a hash of its size and every grey level in it,
row by row, cut to 38 bits."
  (let ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)) (hash (numbers-hash 2 k (- bottom top))))
    (declare (type grey-pixels pixels) (type fixnum across at k top bottom) (type (unsigned-byte 64) hash)
             (optimize speed))
    (loop for j from top below bottom
          do (let ((row (+ (* j across) at)))
               (declare (type fixnum row))
               (setf hash (octets-hash pixels :start row :end (+ row k) :hash hash))))
    (ldb (byte 38 0) hash)))

(defun same-strip-p (a at b bt k top bottom b-top)
  "True when the strip of A's cut from column AT and row TOP to BOTTOM, K
columns wide, is the same grey level for grey level as B's from column BT
and row B-TOP."
  (let ((pa (glyph-pixels a)) (pb (glyph-pixels b)) (wa (glyph-width a)) (wb (glyph-width b)))
    (declare (type grey-pixels pa pb) (type fixnum wa wb))
    (loop for j from 0 below (- bottom top)
          for ra = (+ (* (+ top j) wa) at) and rb = (+ (* (+ b-top j) wb) bt)
          never (mismatch pa pb :start1 ra :end1 (+ ra k) :start2 rb :end2 (+ rb k)))))

(defun strip-core-spread (glyph at k top bottom)
  "How far the core of the ink of GLYPH's strip from column AT, K columns
wide, in rows TOP to BOTTOM, strays, as GLYPH-CORE-SPREAD measures a
character's: the middle of the distances from their middle grey of the
strip's pixels no lighter than halfway from its darkest to its lightest;
NIL for a core of fewer than 8 pixels."
  (let ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)) (lo 255) (hi 0) (core '()))
    (declare (type grey-pixels pixels) (type fixnum across lo hi))
    (loop for j from top below bottom
          do (dotimes (i k) (let ((g (aref pixels (+ (* j across) at i)))) (setf lo (min lo g) hi (max hi g)))))
    (let ((half (+ lo (floor (- hi lo) 2))))
      (loop for j from top below bottom
            do (dotimes (i k) (let ((g (aref pixels (+ (* j across) at i)))) (when (<= g half) (push g core))))))
    (when (>= (length core) 8)
      (let ((middle (median-of core)))
        (median-of (mapcar (lambda (g) (abs (- g middle))) core))))))

(defun same-strips (glyphs &key dct)
  "The pairs of the characters of GLYPHS with grain of their own that share
a strip of pixels the same grey level for grey level, as two values. The
first is the pairs, each (A B BLOCK GRID). BLOCK is the widest block of them
found the same, (WIDTH HEIGHT INK), INK its pixels of ink the scanner did
not clip. GRID is true when the two lie on the same grid of 8 by 8 pixels of
pictures kept as JPEGs (DCT, a list of picture objects) and the block is
less than two characters wide. The second is how many strips were not set
beside others, past the most a file's characters may give (a strip's place
packed in a number) or of a character over 1,000 pixels across. A strip is a
run of columns of a character's cut, STRIP-WIDTH wide, from any column of
it, and the rows of those columns that hold ink (GLYPH-STRIP). Take the ink
of a letter laid over another. Every strip within what was laid is the same
as its source's. That holds wherever the letters about it begin and end, and
whether or not the blur joined it to a neighbour, at its source or where it
was laid. Strips holding fewer than 20 pixels of ink once ink the scanner
clipped is set aside are not set beside others (as GLYPH-DISTANCE does not
judge characters with fewer). Nor is a strip whose own core strays no more
than a character without grain of its own does, a grey level or a fifth of
what its page's characters typically stray (STRIP-CORE-SPREAD). Such a strip
is alike in every print where the grain is faint. Nor is a strip set beside
one of the same pixels of the picture (the cuts of characters side by side
take in the same paper), nor beside one of the same character. A JPEG keeps
each block of 8 by 8 pixels alike when its pixels are alike, and so keeps
two clean prints the same where they fall alike on its grid: such a pair is
marked GRID. Ink is what is darker than the grey STRIP-MEASURES gives, so
that a strip's rows are the same wherever it is found; only strips of the
same STRIP-HASH are set beside each other, and of more than 64 of one hash
each is set beside the first alone. That is a limit on work, not on what is
found. A character laid 70 times gives one number 70 times, and
every copy is still found, each beside the first."
  (multiple-value-bind (k level) (strip-measures (remove-if #'glyph-bare glyphs))
    (let* ((glyphs (coerce (remove-if #'glyph-bare glyphs) 'simple-vector))
           (typical (or (typical-width (coerce glyphs 'list)) 6))
           (most (min (ash 1 22) (loop for g across glyphs sum (max 0 (1+ (- (glyph-width g) k))))))
           ;; each strip's hash and its number, packed; and its character, column, first row and the row after its last
           (keys (make-array most :element-type 'fixnum))
           (places (make-array most :element-type 'fixnum))
           (count 0) (left 0)
           (blocks (make-hash-table :test 'equal)))
      (declare (type fixnum most count left) (type (simple-array fixnum (*)) keys places))
      (loop for g across glyphs for gi fixnum from 0
            do (when (zerop (mod gi 1000)) (job-check-stop))
               (loop for at from 0 to (- (glyph-width g) k)
                     do (multiple-value-bind (top bottom n) (glyph-strip g at k level)
                          (when (and top (>= n 20))
                            (if (and (< count most) (< (max at bottom) 1024) (< gi (ash 1 20)))
                                (progn (setf (aref keys count) (logior (ash (strip-hash g at k top bottom) 24) count)
                                             (aref places count) (logior (ash gi 30) (ash at 20) (ash top 10) bottom))
                                       (incf count))
                                (incf left))))))
      (let ((sorted (sort (subseq keys 0 count) #'<)) (grained (make-hash-table :test 'equal)))
        (declare (type (simple-array fixnum (*)) sorted))
        (labels ((place (key) (aref places (ldb (byte 24 0) key)))
                 (grained-p (g at top bottom)
                   ;; the strip's core strays as a print's does
                   (let ((key (list g at top)))
                     (multiple-value-bind (known there) (gethash key grained)
                       (if there known
                           (setf (gethash key grained)
                                 (let ((spread (strip-core-spread g at k top bottom)))
                                   (and spread (> spread (max 1 (/ (glyph-typical g) 5))))))))))
                 (try (u v)
                   (let* ((pu (place u)) (pv (place v))
                          (ga (ash pu -30)) (gb (ash pv -30))
                          (a (svref glyphs ga)) (b (svref glyphs gb))
                          (at (ldb (byte 10 20) pu)) (bt (ldb (byte 10 20) pv))
                          (top (ldb (byte 10 10) pu)) (bottom (ldb (byte 10 0) pu))
                          (b-top (ldb (byte 10 10) pv)))
                     (when (and (/= ga gb)
                                (= (- bottom top) (- (ldb (byte 10 0) pv) b-top))
                                ;; the same pixels of the picture, in the cuts of two characters side by side
                                (not (and (eql (glyph-picture a) (glyph-picture b)) (eql (glyph-page a) (glyph-page b))
                                          (= (+ (glyph-x0 a) at) (+ (glyph-x0 b) bt))
                                          (= (+ (glyph-y0 a) top) (+ (glyph-y0 b) b-top))))
                                (same-strip-p a at b bt k top bottom b-top)
                                (grained-p a at top bottom))
                       ;; the block the same: the columns of every strip found so, the tallest rows
                       (let* ((key (list ga gb (- bt at) (- b-top top)))
                              (entry (or (gethash key blocks) (setf (gethash key blocks) (list nil top bottom)))))
                         (loop for c from at below (+ at k) do (pushnew c (first entry)))
                         (setf (second entry) (min (second entry) top) (third entry) (max (third entry) bottom)))))))
          (loop with start fixnum = 0
                while (< start count)
                do (let ((end (1+ start)))
                     (declare (type fixnum end))
                     (loop while (and (< end count) (= (ash (aref sorted end) -24) (ash (aref sorted start) -24)))
                           do (incf end))
                     (cond ((< (- end start) 2))
                           ((<= (- end start) 64)
                            (loop for u from start below end
                                  do (loop for v from (1+ u) below end do (try (aref sorted u) (aref sorted v)))))
                           (t (loop for v from (1+ start) below end do (try (aref sorted start) (aref sorted v)))))
                     (setf start end))))
        (let ((pairs (make-hash-table :test 'equal)))
          (loop for (ga gb dx dy) being the hash-keys of blocks using (hash-value (columns top bottom))
                do (let* ((a (svref glyphs ga)) (b (svref glyphs gb))
                          (pixels (glyph-pixels a)) (across (glyph-width a)) (clipped (glyph-clipped a))
                          (ink (loop for j from top below bottom
                                     sum (loop for c in columns
                                               count (let ((g (aref pixels (+ (* j across) c))))
                                                       (and (< g level) (> g clipped))))))
                          (grid (and (member (glyph-picture a) dct) (member (glyph-picture b) dct)
                                     (zerop (mod (- (+ (glyph-x0 b) dx) (glyph-x0 a)) 8))
                                     (zerop (mod (- (+ (glyph-y0 b) dy) (glyph-y0 a)) 8))
                                     (< (length columns) (* 2 typical))))
                          (block (list (length columns) (- bottom top) ink))
                          (was (gethash (list ga gb) pairs)))
                     ;; of the ways a pair is found the same, the widest; a way off the grid before one on it
                     (when (or (null was)
                               (and (third was) (not grid))
                               (and (eq (not (third was)) (not grid)) (> (third block) (third (second was)))))
                       (setf (gethash (list ga gb) pairs) (list ga block grid)))))
          (values (loop for (ga gb) being the hash-keys of pairs using (hash-value (nil block grid))
                        collect (list (svref glyphs ga) (svref glyphs gb) block grid))
                  left))))))

(defun exact-copies (glyphs &key (grained t) dct)
  "The characters of GLYPHS that are copies the same pixel for pixel, in
groups of two or more. Of those with grain of their own (GLYPH-BARE not),
it takes those sharing a strip of the same pixels (SAME-STRIPS, given
DCT), with as second and third values its pairs and how many strips it
left out. With GRAINED NIL, of those without, it takes those whose whole
boxes are the same, grey level for grey level (SAME-PIXELS-P). Only those
of the same GLYPH-BOX-HASH are set beside each other."
  (if grained
      (multiple-value-bind (pairs left) (same-strips glyphs :dct dct)
        (let ((index (make-hash-table :test 'eq)) (parent '()) (n 0))
          (flet ((id (g) (or (gethash g index) (progn (push n parent) (setf (gethash g index) n) (prog1 n (incf n))))))
            (dolist (pair pairs) (id (first pair)) (id (second pair)))
            (let ((parent (coerce (nreverse parent) 'simple-vector)) (groups (make-hash-table)))
              (labels ((root (i) (if (= (svref parent i) i) i (setf (svref parent i) (root (svref parent i))))))
                (dolist (pair pairs)
                  (let ((ra (root (gethash (first pair) index))) (rb (root (gethash (second pair) index))))
                    (unless (= ra rb) (setf (svref parent rb) ra))))
                (loop for g being the hash-keys of index using (hash-value i) do (push g (gethash (root i) groups)))
                (values (loop for members being the hash-values of groups collect members) pairs left))))))
      (let ((by-hash (make-hash-table)) (out '()))
        (dolist (g (remove-if-not #'glyph-bare glyphs))
          (let* ((key (glyph-box-hash g))
                 (group (find-if (lambda (group) (same-pixels-p (first group) g)) (gethash key by-hash))))
            (if group
                (push g (cdr group))
                (push (list g) (gethash key by-hash)))))
        (loop for groups being the hash-values of by-hash
              do (dolist (group groups) (when (cdr group) (push group out))))
        out)))

(defun picture-likeness (glyphs peers)
  "How alike characters of the same shape typically are on each scanned picture
of GLYPHS (ALIKE-CHARACTERS), as a list of ((PICTURE . PAGE) ALIKE CHARACTERS
SAMPLED NEAR MIDDLE): SAMPLED how many of its own characters were set beside its
own, NEAR how many of them had one within 30 grey levels and MIDDLE how alike
those were (ALIKE-CHARACTERS); and each character's GLYPH-ALIKE set to its
picture's, or NIL where that is under 4 grey levels or cannot be told. Each
picture is first measured by its own prints alone, set beside its own characters
only: a page whose grain is too faint to tell prints apart is not judged,
whatever the other pages of its file are. A picture judged is then measured
again with the prints of the pictures judged near it too, as a character is set
beside them; a page of more grain is never measured against a cleaner one left
unjudged. Both times up to 400 of its characters are measured, the same ones
however many others are read beside them (LIKENESS-SAMPLE)."
  (let ((pictures (make-hash-table :test 'equal)) (judged (make-hash-table :test 'equal))
        (own (make-hash-table :test 'equal))
        (counts (make-hash-table :test 'equal)))
    (flet ((key (g) (cons (glyph-picture g) (glyph-page g))))
      (dolist (g glyphs) (push g (gethash (key g) pictures)))
      (loop for key being the hash-keys of pictures using (hash-value members)
            do (multiple-value-bind (alone sampled near middle) (alike-characters members (glyph-peers members))
                 (setf (gethash key own) alone (gethash key counts) (list sampled near middle))
                 (when (and alone (>= alone 4)) (setf (gethash key judged) t))))
      (loop for key being the hash-keys of pictures using (hash-value members)
            collect (let ((alike (if (gethash key judged)
                                     (alike-characters members
                                                       peers
                                                       :beside (lambda (g p)
                                                                 (declare (ignore g))
                                                                 (gethash (key p) judged)))
                                     (gethash key own))))
                      (dolist (g members) (setf (glyph-alike g) (and (gethash key judged) alike (>= alike 4) alike)))
                      (list* key alike (length members) (gethash key counts)))))))

(defparameter *exact-grain-least* 1
  "How alike, at least, characters of the same shape must typically be
on a scanned picture, in grey levels, for its characters to be sought
for copies the same pixel for pixel when they are too alike to be judged
by their likeness (under 4, COPIED-GLYPHS). Prints that differ by this
much carry grain of their own, which no two prints share pixel for
pixel.")

(defun stepped-picture-p (glyphs &optional (grid t))
  "True when the characters GLYPHS of one scanned picture show its grey
levels stepped alike for every print, so that two honest prints of a
character can come out the same pixel for pixel, grain and all. One case
is a picture of few grey levels: fewer than half of the grey levels its
characters' pixels use, between their darkest and lightest fiftieth, next
to another used. Posterising or a picture of 16 or 32 levels leaves this,
where a scan of two inks with nothing between them fills each with its
neighbours. The other case is a picture that carries a JPEG's grid of
blocks (PICTURE-BLOCK-GRID, over up to 400 of its characters), as a page
saved as a JPEG and kept without loss since does. That is asked only with
GRID. A picture kept as a JPEG carries its grid as it is, and its pairs
on the grid are told so by SAME-STRIPS."
  (let ((counts (make-array 256 :element-type 'fixnum :initial-element 0)) (n 0))
    (declare (type fixnum n))
    (dolist (g glyphs)
      (grey-histogram (glyph-pixels g) :into counts)
      (incf n (length (glyph-pixels g))))
    (or (and (plusp n)
             (let* ((dark (counted-level counts (max 1 (floor n 50))))
                    (light (counted-level counts (max 1 (- n (floor n 50)))))
                    (used (loop for v from dark to light count (plusp (aref counts v))))
                    (beside (loop for v from dark to light
                                  count (and (plusp (aref counts v))
                                             (or (and (> v 0) (plusp (aref counts (1- v))))
                                                 (and (< v 255) (plusp (aref counts (1+ v)))))))))
               (< (* 2 beside) used)))
        (and grid (>= (length glyphs) 40)
             (let ((rows (make-array 0 :element-type 'double-float)))
               (nth-value 2 (picture-block-grid
                             (loop for g in (likeness-sample glyphs 400)
                                   collect (multiple-value-bind (scores room) (glyph-block-scores g rows)
                                             (setf rows room)
                                             scores)))))))))

;;; Characters copied among the scanned pictures
;;;
;;; How alike characters of the same shape typically are is measured on each
;;; picture apart, and only pictures where they differ by 4 grey levels or more
;;; are judged: on a picture drawn rather than scanned, or smoothed, every e is
;;; the same. A picture whose blank paper strays by less than half a grey level
;;; and whose characters' cores by a grey level or less was drawn by a program,
;;; and its characters have no grain of their own; paper alone tells nothing, as
;;; a scanner set to make its paper white leaves the grain of the ink untouched.
;;; The characters of a picture whose characters differ by less than 4 grey
;;; levels, but by *EXACT-GRAIN-LEAST* or more, are sought for copies the same
;;; pixel for pixel alone, worth a look at most: the steps of a JPEG saved and
;;; kept without loss, or of a picture of few grey levels, can make two honest
;;; prints the same, grain and all.
;;;
;;; Two characters are set beside each other by the less of their pictures'
;;; likeness, ALIKE. They are copies when they share a block of pixels the same,
;;; grey level for grey level, with grain of their own (EXACT-COPIES): a strong
;;; sign, but not on a JPEG's own grid. They are also copies when they are alike
;;; to half of ALIKE in their ink, worth a look; a strong sign only when their
;;; ink is the same in every pixel, and then not for characters without grain of
;;; their own, of pictures kept as JPEGs, of fewer than 20 pixels of ink, or
;;; whose cores stray by less than half what their page's typically do (smoothed
;;; by the scanner's software). Or they are copies when each is the other's
;;; nearest, nearer than three quarters of ALIKE, and at least twice as near each
;;; other as either is to the next of its shape: two prints of a character are as
;;; far from each other as from the rest, but a copy saved again as a JPEG is
;;; near its source alone. Copies told by strong signs are gathered into groups,
;;; and copies worth a look into groups of their own, so that a character comes
;;; into a strong group only by a strong sign. Marks twice as tall as the taller
;;; characters, pieces of a table's border, are no characters; a group taller by
;;; half than the type of its pages, or at the same place on most pages, is laid
;;; on by software (a stamp, a logo, a signature), which only says how it is
;;; named.

(defun picture-key (glyph)
  "The picture GLYPH was cut from and its page, (PICTURE . PAGE), a key of the
tables by picture."
  (cons (glyph-picture glyph) (glyph-page glyph)))

(defun faint-pictures (pictures)
  "A table of the pictures of PICTURES (PICTURE-LIKENESS) whose prints differ,
though too little to be judged by their likeness: those sought for copies
the same pixel for pixel alone, each its likeness."
  (let ((kept (make-hash-table :test 'equal)))
    (loop for (key alike) in pictures
          when (and alike (>= alike *exact-grain-least*) (< alike 4)) do (setf (gethash key kept) alike))
    kept))

(defun stepped-pictures (glyphs dct)
  "A table of the pictures of GLYPHS whose grey levels are stepped alike for
every print (STEPPED-PICTURE-P), DCT listing the pictures kept as JPEGs."
  (let ((by (make-hash-table :test 'equal)) (out (make-hash-table :test 'equal)))
    (dolist (g glyphs) (push g (gethash (picture-key g) by)))
    (loop for key being the hash-keys of by using (hash-value members)
          when (stepped-picture-p members (not (member (car key) dct))) do (setf (gethash key out) t))
    out))

(defstruct (copy-joins (:constructor %make-copy-joins) (:copier nil) (:predicate nil))
  "The groups of copies being gathered among GLYPHS: INDEX, each glyph's place
in them; STRONG and LOOK, the parents of the groups of each strength (each
place its own root at first); CLOSEST, the least difference of each group by
\(STRENGTH . ROOT); BLOCKS, the widest block found the same of each glyph by
\(STRENGTH . GLYPH); WHY, why a glyph's copy is worth a look at most."
  glyphs (index (make-hash-table :test 'eq)) strong look
  (closest (make-hash-table :test 'equal)) (blocks (make-hash-table :test 'equal)) (why (make-hash-table :test 'eq)))

(defun make-copy-joins (glyphs)
  "New COPY-JOINS of GLYPHS, each a group of its own."
  (let ((joins (%make-copy-joins :glyphs glyphs :strong (make-array (length glyphs))
                                 :look (make-array (length glyphs)))))
    (loop for g in glyphs for k from 0
          do (setf (gethash g (copy-joins-index joins)) k
                   (aref (copy-joins-strong joins) k) k
                   (aref (copy-joins-look joins) k) k))
    joins))

(defun copy-root (parent k)
  "The root of the group the place K is in, by PARENT, the path to it made
short on the way."
  (if (= (aref parent k) k) k (setf (aref parent k) (copy-root parent (aref parent k)))))

(defun join-copies (joins a b d &key block reason (strength :strong))
  "Join in JOINS the glyphs A and B as copies of STRENGTH, D apart, BLOCK the
widest block they share the same, REASON why they are worth a look at most."
  (let* ((parent (if (eq strength :strong) (copy-joins-strong joins) (copy-joins-look joins)))
         (index (copy-joins-index joins)) (closest (copy-joins-closest joins)) (blocks (copy-joins-blocks joins))
         (ra (copy-root parent (gethash a index))) (rb (copy-root parent (gethash b index))))
    (unless (= ra rb) (setf (aref parent rb) ra))
    (setf (gethash (cons strength (copy-root parent ra)) closest)
          (min d (or (gethash (cons strength ra) closest) d) (or (gethash (cons strength rb) closest) d)))
    (dolist (g (list a b))
      (when block
        (let ((was (gethash (cons strength g) blocks)))
          (when (or (null was) (> (third block) (third was)))
            (setf (gethash (cons strength g) blocks) block))))
      (when reason (setf (gethash g (copy-joins-why joins)) reason)))))

(defun join-exact-copies (joins dct stepped)
  "Join in JOINS the characters of the same pixels (EXACT-COPIES), however
far apart: worth a look on a JPEG's grid, on a page STEPPED, or on one whose
prints are too alike to judge. Two values: how many strips were left out,
and a table of those within two pages of their copy, set beside no other."
  (let ((settled (make-hash-table :test 'eq)))
    (multiple-value-bind (groups pairs left) (exact-copies (copy-joins-glyphs joins) :dct dct)
      (declare (ignore groups))
      (loop for (a b block grid) in pairs
            do (cond (grid (join-copies joins a b 0 :block block :reason :grid :strength :look))
                     (t (cond ((or (gethash (picture-key a) stepped) (gethash (picture-key b) stepped))
                               (join-copies joins a b 0 :block block :reason :stepped :strength :look))
                              ((and (glyph-alike a) (glyph-alike b))
                               (join-copies joins a b 0 :block block))
                              ;; the same pixels may be the same steps of a JPEG's or a posterising's levels
                              (t (join-copies joins a b 0 :block block :reason :faint :strength :look)))
                        (when (<= (abs (- (glyph-page a) (glyph-page b))) 2)
                          (setf (gethash a settled) t (gethash b settled) t)))))
      (values left settled))))

(defun nearest-copies (judged peers settled)
  "Two values: a table of each character of JUDGED not SETTLED to its nearest
among its PEERS (GLYPH-PEERS)
\(P D NEXT COUNTED) when nearer than three quarters of its picture's
likeness, NEXT the distance to the next within twice it; and the characters
not judged, too few pixels left beside any other."
  (let ((nearest (make-hash-table :test 'eq)) (not-judged '()))
    (loop for g in judged for k from 0
          do (when (zerop (mod k 50)) (job-check-stop))
             (unless (gethash g settled)
               (let ((near (funcall peers g)) (alike (glyph-alike g)))
                 (multiple-value-bind (p d judged counted) (nearest-glyph g near (* 3/4 alike))
                   (unless judged (push g not-judged))
                   (when p
                     (setf (gethash g nearest)
                           (list p d (nth-value 1 (nearest-glyph g near (* 2 alike) p)) counted)))))))
    (values nearest (nreverse not-judged))))

(defun nearest-copy-p (g p d next theirs type-height type-width)
  "True when the characters G and P, D apart, are copies by likeness: alike
to half of their likeness, or each the other's nearest (THEIRS, P's nearest
entry) twice as near as the NEXT of either; no marks without grain, pieces
of rules (TYPE-HEIGHT and TYPE-WIDTH being the type's), or strokes alone but
by the paper about them too."
  (let ((both (min (glyph-alike g) (glyph-alike p))))
    (and (or (<= d (/ both 2))
             (and theirs (eq (first theirs) g) next (third theirs) (< d (* 3/4 both))
                  (<= (* 2 d) next) (<= (* 2 d) (third theirs))))
         ;; the same pixel for pixel without grain of their own: marks, not copies
         (not (and (glyph-bare g) (same-pixels-p g p)))
         ;; pieces of rules are no characters; strokes alone come alike by chance, and tell a copy only by the paper
         ;; about them too, each the other's nearest
         (not (and (plusp d) (or (rule-piece-p g type-height type-width) (rule-piece-p p type-height type-width))))
         (not (and (plusp d) (or (shapeless-glyph-p g) (shapeless-glyph-p p))
                   (not (and theirs (eq (first theirs) g) (<= (glyph-ring-distance g p) (/ both 2)))))))))

(defun join-nearest-copies (joins nearest type-height type-width dct stepped)
  "Join in JOINS the characters of NEAREST (NEAREST-COPIES) that are copies by
likeness (NEAREST-COPY-P): worth a look at most, but for ink the same in
every pixel with the page's grain, on a picture not kept as a JPEG (DCT) nor
STEPPED, over 20 pixels of ink or more."
  (loop for g being the hash-keys of nearest using (hash-value (p d next counted))
        when (nearest-copy-p g p d next (gethash p nearest) type-height type-width)
          do (cond ((or (plusp d) (glyph-bare g) (glyph-bare p))
                    (join-copies joins g p d :strength :look))
                   ;; a JPEG keeps clean prints alike: their likeness proves nothing there
                   ((or (member (glyph-picture g) dct) (member (glyph-picture p) dct))
                    (join-copies joins g p d :strength :look :reason :jpeg))
                   ((or (gethash (picture-key g) stepped) (gethash (picture-key p) stepped))
                    (join-copies joins g p d :strength :look :reason :stepped))
                   ((and (zerop d) (< counted 20))
                    (join-copies joins g p d :strength :look))
                   ;; a print smoothed by the scanner's software keeps little of its grain, and comes near another so
                   ;; smoothed: likeness is proof only of prints with their page's grain
                   ((notevery (lambda (c) (and (glyph-spread c) (>= (* 2 (glyph-spread c)) (glyph-typical c))))
                              (list g p))
                    (join-copies joins g p d :strength :look :reason :smooth))
                   (t (join-copies joins g p d)))))

(defun bare-marks (judged bare)
  "The groups of the characters JUDGED the same pixel for pixel without grain
of their own, each within two pages of another, those of the words and
lines BARE (BARE-WORDS) left out."
  (let ((in-bare (make-hash-table :test 'eq)))
    (loop for (nil nil members) in bare do (dolist (g members) (setf (gethash g in-bare) t)))
    (loop for group in (exact-copies judged :grained nil)
          for near = (remove-if-not (lambda (g)
                                      (and (not (gethash g in-bare))
                                           (find-if (lambda (o)
                                                      (and (not (eq o g))
                                                           (<= (abs (- (glyph-page o) (glyph-page g))) 2)))
                                                    group)))
                                    group)
          when (cdr near) collect near)))

(defun laid-group-p (group middle pictures)
  "True when GROUP, characters copied, was laid on by software: each taller by
half than the type of its page (MIDDLE, a table by picture of that
height), or one of them at the same place on two pages of PICTURES and half
of them or more."
  (or (every (lambda (g) (> (- (glyph-y1 g) (glyph-y0 g)) (* 3/2 (gethash (picture-key g) middle)))) group)
      (some (lambda (g)
              (let ((pages (remove-duplicates
                            (mapcar #'glyph-page
                                    (remove-if-not (lambda (o) (and (<= (abs (- (glyph-x0 o) (glyph-x0 g))) 3)
                                                                    (<= (abs (- (glyph-y0 o) (glyph-y0 g))) 3)))
                                                   group)))))
                (and (>= (length pages) 2) (>= (* 2 (length pages)) (length pictures)))))
            group)))

(defun type-heights (glyphs)
  "A table of the taller type of each picture of GLYPHS (UPPER-HEIGHT), by
\(PICTURE . PAGE)."
  (let ((by (make-hash-table :test 'equal)) (heights (make-hash-table :test 'equal)))
    (dolist (g glyphs) (push g (gethash (picture-key g) by)))
    (loop for key being the hash-keys of by using (hash-value gs) do (setf (gethash key heights) (upper-height gs)))
    heights))

(defun copy-groups (joins strength faint middle pictures)
  "The groups of copies of STRENGTH gathered in JOINS, each ((GLYPH...)
DIFFERENCE BLOCK ALIKE STRENGTH WHY LAID) as COPIED-GLYPHS gives them; FAINT
and PICTURES as COPIED-GLYPHS reads them, MIDDLE the type's height on each
picture (TYPE-HEIGHTS)."
  (let* ((glyphs (copy-joins-glyphs joins))
         (parent (if (eq strength :strong) (copy-joins-strong joins) (copy-joins-look joins)))
         (closest (copy-joins-closest joins)) (blocks (copy-joins-blocks joins))
         (members (make-hash-table)))
    (loop for g in glyphs for k from 0
          do (unless (= (copy-root parent k) k) (push g (gethash (copy-root parent k) members)))
             (when (and (= (copy-root parent k) k) (gethash (cons strength k) closest))
               (push g (gethash k members))))
    (loop for r being the hash-keys of members using (hash-value group)
          when (cdr group)
            collect (let ((d (gethash (cons strength r) closest)) (group (nreverse group)))
                      (list group d
                            (and (zerop d)
                                 (reduce (lambda (a b) (if (or (null a) (and b (> (third b) (third a)))) b a))
                                         group :key (lambda (g) (gethash (cons strength g) blocks)) :initial-value nil))
                            (reduce #'min group :key (lambda (g) (or (glyph-alike g) (gethash (picture-key g) faint))))
                            strength
                            (and (eq strength :look) (some (lambda (g) (gethash g (copy-joins-why joins))) group))
                            (laid-group-p group middle pictures))))))

(defun read-pictures (glyphs drawn)
  "How alike the characters of GLYPHS are on each picture (PICTURE-LIKENESS),
their grain measured first (GRAIN-LEVELS) and those of the pictures DRAWN by
a program marked as without grain of their own."
  (grain-levels glyphs)
  (dolist (g glyphs)
    (when (and (member (glyph-picture g) drawn) (<= (glyph-typical g) 1))
      (setf (glyph-bare g) t)))
  (picture-likeness glyphs (glyph-peers glyphs)))

(defstruct (scan-reading (:copier nil) (:predicate nil))
  "What reading the scanned pictures of a revision found (SCANS-READ). SCANS
is the SCAN-PICTUREs, COPIES the groups of characters copied among them
\(COPIED-GLYPHS), ALIKE how alike characters typically are, PIECES the
BLOCK-PIECEs (JPEG-PIECES). MARKS is the groups of marks without grain the
same pixel for pixel, BARE the words and lines without grain, NOT-JUDGED the
characters not judged, PICTURES how alike characters are on each picture,
LEFT how many strips were left out (COPIED-GLYPHS), and NOT-READ the scanned
pictures that could not be read, each (PAGE MARK WHY) as SCAN-PICTURES gives
them."
  scans copies alike pieces marks bare not-judged pictures (left 0) not-read)

(defun copied-glyphs (glyphs &key dct drawn)
  "The characters of GLYPHS, those of all the scanned pictures of a file, that
are copies of one another, as a SCAN-READING whose SCANS, PIECES and
NOT-READ are left for SCANS-READ to fill. Its COPIES are the groups of
copies, each ((GLYPH...) DIFFERENCE BLOCK ALIKE STRENGTH WHY LAID). ALIKE is
how alike characters of the same shape typically are, the least of it over
the pictures judged (or over all of them, none judged). MARKS is the groups
of marks the same pixel for pixel without grain of their own (GRAIN-LEVELS),
each (GLYPH...), which are no copies, those of the words and lines of BARE
left out. BARE is the words and lines without grain of their own on pages
with it (BARE-WORDS). NOT-JUDGED is the characters too few of whose pixels
are left beside any other once the ink the scanner clipped is set aside.
PICTURES is the pictures (PICTURE-LIKENESS), each ((PICTURE . PAGE) ALIKE
CHARACTERS SAMPLED NEAR MIDDLE), and LEFT how many strips EXACT-COPIES left
out. DCT lists the pictures
kept as JPEGs, DRAWN those whose blank paper strays by less than half a
grey level. STRENGTH is :STRONG or :LOOK; DIFFERENCE the root of the mean
squared difference of the closest pair, 0 for an exact copy; BLOCK the
widest block found the same, (WIDTH HEIGHT INK), else NIL; WHY the reason a
group is worth a look at most (:GRID :STAMP :JPEG :SMOOTH :FAINT :STEPPED);
LAID true when the group was laid on the pages by software."
  (let* ((type-height (upper-height glyphs))
         (type-width (typical-width glyphs))
         (tall (* 2 (or type-height 0)))
         ;; a piece of a table's border, too short to be taken out as a rule, is no character
         (glyphs (remove-if (lambda (g) (> (- (glyph-y1 g) (glyph-y0 g)) tall)) glyphs))
         (pictures (read-pictures glyphs drawn))
         (judged-alike (remove nil (mapcar (lambda (p) (and (second p) (>= (second p) 4) (second p))) pictures)))
         (alike (if judged-alike
                    (reduce #'min judged-alike)
                    (let ((all (remove nil (mapcar #'second pictures)))) (and all (reduce #'min all)))))
         (faint (faint-pictures pictures))
         (stepped (stepped-pictures glyphs dct))
         (judged (remove-if-not #'glyph-alike glyphs))
         (glyphs (append judged (remove-if-not (lambda (g) (gethash (picture-key g) faint)) glyphs))))
    (if (null glyphs)
        (make-scan-reading :alike alike :pictures pictures)
        (let ((peers (glyph-peers judged)) (joins (make-copy-joins glyphs)))
          (multiple-value-bind (left settled) (join-exact-copies joins dct stepped)
            (multiple-value-bind (nearest not-judged) (nearest-copies judged peers settled)
              (join-nearest-copies joins nearest type-height type-width dct stepped)
              (let* ((bare (bare-words judged))
                     (marks (bare-marks judged bare))
                     (told (make-hash-table :test 'eq))
                     (middle (type-heights glyphs)))
                (dolist (group marks) (dolist (g group) (setf (gethash g told) t)))
                (make-scan-reading :copies (append (copy-groups joins :strong faint middle pictures)
                                                   (copy-groups joins :look faint middle pictures))
                                   :alike alike :marks marks :bare bare
                                   :not-judged (remove-if (lambda (g) (gethash g told)) not-judged)
                                   :pictures pictures :left left))))))))

(defun bare-words (glyphs)
  "The words and lines of GLYPHS' scanned pages without grain of their own
\(BARE-GLYPHS): each (KIND WORDS CHARACTERS), KIND :WORD for a word of 3
characters or more, 2 of them measured and every one measured without
grain; :LINE for a line of 2 words or more, every character of it measured
without grain, 3 of them at least. Marks standing alone (WORD-BOX-LONE) are
no words, and are not counted here."
  (let ((pages (make-hash-table :test 'equal)) (out '()))
    (dolist (g glyphs)
      (unless (word-box-lone (glyph-word g)) (push g (gethash (list (glyph-picture g) (glyph-page g)) pages))))
    (loop for on-page being the hash-values of pages
          do (let ((bare (bare-glyphs on-page)))
               (when (and bare (plusp (hash-table-count bare)))
                 (let ((words (make-hash-table :test 'eq)) (lines (make-hash-table))
                       (counts (make-array 256 :element-type 'fixnum)))
                   (dolist (g on-page) (push g (gethash (glyph-word g) words)))
                   (flet ((without (members)
                            ;; every character measured is without grain; how many are
                            (let ((measured (remove-if-not (lambda (g) (glyph-core-spread g counts)) members)))
                              (and (every (lambda (g) (gethash g bare)) measured) (length measured)))))
                     (loop for word being the hash-keys of words using (hash-value members)
                           do (push (list word (without members) (length members)) (gethash (word-box-y0 word) lines)))
                     (loop for line being the hash-values of lines
                           do (let* ((all (loop for (nil n) in line always n))
                                     (measured (loop for (nil n) in line sum (or n 0)))
                                     (taken (if (and all (>= (length line) 2) (>= measured 3))
                                                (list (list :line (sort (mapcar #'first line) #'< :key #'word-box-x0)))
                                                (loop for (word n count) in line
                                                      when (and n (>= n 2) (>= count 3))
                                                      collect (list :word (list word))))))
                                (loop for (kind ws) in taken
                                      do (push (list kind ws (loop for w in ws append (gethash w words))) out)))))))))
    out))

(defun scan-of-glyph (scans glyph)
  "The SCAN-PICTURE of SCANS GLYPH is in."
  (find-if (lambda (scan) (and (eql (mark-object (scan-picture-mark scan)) (glyph-picture glyph))
                               (eql (page-index (scan-picture-page scan)) (glyph-page glyph))))
           scans))

(defun glyph-words (scans glyph)
  "Where GLYPH is, in words: on the page and in its picture, and whether it
is a piece cut from a wider mark (SPLIT-JOINED)."
  (let* ((scan (scan-of-glyph scans glyph)))
    (destructuring-bind (x0 y0 x1 y1)
        (picture-page-box (scan-picture-mark scan) glyph (scan-picture-width scan) (scan-picture-height scan))
      (declare (ignore x1 y1))
      (format nil "page ~d at x ~d, y ~d (pixels ~d,~d to ~d,~d~@[ of picture ~d~]~@[, ~a~])"
              (1+ (glyph-page glyph)) (round x0) (round y0)
              (glyph-x0 glyph) (glyph-y0 glyph) (glyph-x1 glyph) (glyph-y1 glyph) (picture-number (glyph-picture glyph))
              (case (glyph-split glyph)
                (:sure "cut at a neck from a mark wider than a letter")
                (:unsure "a mark wider than a letter, no neck found to cut it at"))))))

(defun not-judged-words (not-judged)
  "How many characters of NOT-JUDGED were not set beside others, and why, in words."
  (format nil
          "~:d character~:p not judged: too few pixels left once the ink the scanner clipped is set aside"
          (length not-judged)))

(defun scans-read (pdf revision)
  "The scanned pictures of REVISION's pages read (SCAN-PICTURES), the
characters copied among them (COPIED-GLYPHS) and the regions of each whose JPEG
traces are unlike the rest of it (JPEG-PIECES, of the words' characters, not
the marks standing alone), as a SCAN-READING. Each SCAN-PICTURE is told whether it was
JUDGED. That means the likeness of its characters is 4 grey levels or more, and
it was not drawn by a program (its blank paper straying by less than half a
grey level and its characters' cores by a grey level or less, DRAWN). The words
of one not judged are worth a look at most, as its copies are not told. Its
JPEG pieces are worth a look at most only when it was drawn by a program, as a
screenshot or a chart is. A scan whose grain is too faint to tell prints apart
still keeps a JPEG's blocks where a piece was saved apart, which no grain of
the prints is needed to tell. A piece of blocks of its own stays strong only
when another sign agrees on it (BACKED-PIECES). Read once and kept with PDF,
its characters' pixels let go but for those named, for the drawing
\(OVERLAY-OPERATORS) to find them again."
  (kept-for-pdf (pdf :scans revision)
    (multiple-value-bind (scans not-read) (scan-pictures pdf revision)
      (let* ((reading (copied-glyphs (loop for scan in scans append (scan-picture-glyphs scan))
                                     :drawn (loop for scan in scans
                                                  when (and (scan-picture-paper scan) (< (scan-picture-paper scan) 0.5))
                                                    collect (mark-object (scan-picture-mark scan)))
                                     :dct (loop for scan in scans
                                                for object = (mark-object (scan-picture-mark scan))
                                                when (picture-jpeg-p pdf revision object) collect object)))
             (copies (scan-reading-copies reading)))
        ;; judged: its characters differ as prints do (the gate of PICTURE-LIKENESS), and not drawn
        (dolist (scan scans)
          (let* ((glyphs (scan-picture-glyphs scan))
                 (entry (assoc (cons (mark-object (scan-picture-mark scan))
                                     (page-index (scan-picture-page scan)))
                               (scan-reading-pictures reading)
                               :test #'equal))
                 (drawn (and (scan-picture-paper scan) (< (scan-picture-paper scan) 0.5) glyphs
                             (<= (reduce #'max glyphs :key #'glyph-typical) 1))))
            (setf (scan-picture-drawn scan) (and drawn t)
                  (scan-picture-judged scan) (and entry (second entry) (>= (second entry) 4) (not drawn))
                  (scan-picture-rendered scan) (rendered-page-p scan))))
        (dolist (scan scans)
          (let ((twin (scan-picture-twin scan)))
            (when twin (setf (scan-picture-judged scan) (scan-picture-judged twin)
                             (scan-picture-drawn scan) (scan-picture-drawn twin)
                             (scan-picture-rendered scan) (scan-picture-rendered twin)))))
        (let ((pieces (loop for scan in scans
                            for words = (remove-if (lambda (g) (word-box-lone (glyph-word g)))
                                                   (scan-picture-glyphs scan))
                            for jpeg = (picture-jpeg-p pdf revision (mark-object (scan-picture-mark scan)))
                            append (let ((pieces (jpeg-pieces words :jpeg jpeg)))
                                     ;; a picture drawn by a program gives pieces worth a look at most
                                     (when (scan-picture-drawn scan)
                                       (dolist (p pieces) (setf (block-piece-strength p) :look)))
                                     pieces)))
              (heights (make-hash-table :test 'equal)))
          (dolist (scan scans)
            (let ((upper (upper-height (scan-picture-glyphs scan))))
              (when upper
                (setf (gethash (cons (mark-object (scan-picture-mark scan))
                                     (page-index (scan-picture-page scan))) heights) upper))))
          (laid-pieces pieces heights)
          (copies-on-piece-grid copies pieces)
          (stamp-copies copies scans)
          (backed-pieces pieces copies scans)
          (dolist (scan scans) (setf (scan-picture-glyphs scan) nil))
          (setf (scan-reading-scans reading) scans
                (scan-reading-pieces reading) pieces
                (scan-reading-not-read reading) not-read)
          reading)))))

(defparameter *rendered-paper-most* 0.05
  "How far, in grey levels, the blank paper of a page that is one picture may
stray for the page to have been drawn by a program (RENDERED-PAGE-P): all
of it one grey. A born-digital page drawn as a picture gave 0.0; a scanner's
or camera's paper strays by a grey level or more (an honest letter 1.42), and
the same letter with its contrast raised until paper and ink clipped, 0.085.")

(defparameter *rendered-half-tones-least* 1/20
  "The share of the pixels round the characters of a page that is one picture
that must be greys between black and white for it to have been drawn by a
program from text (RENDERED-PAGE-P): a program smooths the edges of the text
it draws (a born-digital page drawn at 150 dots an inch gave 0.22), while a
scanner set to black and white, or a fax, leaves none.")

(defun rendered-page-p (scan)
  "True when SCAN, a SCAN-PICTURE read with its characters, covers its whole
page (85 % of it or more) and was drawn by a program from a page whose text
it would have kept as text, as when a PDF is opened in a picture editor and
saved again. Its blank paper strays by less than *RENDERED-PAPER-MOST*, and
of the pixels round its characters *RENDERED-HALF-TONES-LEAST* or more are
half-tones, as a program smooths text's edges. A scanner's or camera's
grain, and a scanner set to black and white, which leaves no half-tones,
are told apart so; a scan whose paper was whitened and clipped all over is
not, which the finding says."
  (let* ((page (scan-picture-page scan)) (m (scan-picture-mark scan)) (glyphs (scan-picture-glyphs scan))
         (media (page-media page))
         (page-area (* (- (third media) (first media)) (- (fourth media) (second media)))))
    (when (and glyphs (scan-picture-paper scan) (< (scan-picture-paper scan) *rendered-paper-most*)
               (plusp page-area) (>= (area m) (* 0.85 page-area)))
      (let ((half-tones 0) (pixels 0))
        (dolist (g glyphs)
          (loop for p across (glyph-pixels g)
                do (incf pixels) (when (< 15 p 240) (incf half-tones))))
        (and (plusp pixels) (>= (/ half-tones pixels) *rendered-half-tones-least*))))))

(defun rendered-page-findings (pdf revision scans)
  "The findings, worth a look, of SCANS that are a whole page drawn by a
program from text (RENDERED-PAGE-P), one for all such pages."
  (let ((rendered (remove-if-not #'scan-picture-rendered scans)))
    (when rendered
      (let ((pages (sort (remove-duplicates (mapcar (lambda (s) (1+ (page-index (scan-picture-page s)))) rendered))
                         #'<)))
        (list (found :look (format nil "~:[~@(~a~) ~:[is one picture~;are one picture each~]~;The picture is a ~
                                        page~*~*~], drawn by a program, not scanned: the whole page is one ~
                                        picture; text a program would have kept as text. A page opened in a ~
                                        picture editor and saved again is so, and so is one an app or printer ~
                                        driver printed as a picture, a screenshot, or a scan whose paper was ~
                                        whitened until all of it is one grey."
                                     *picture-file* (pages-words pages) (rest pages))
                     :rule (format nil "A page a program makes keeps its text as text; a scanned page is one picture ~
                                        with a scanner's grain in its paper. One picture with none, its blank paper ~
                                        straying by less than ~a of a grey level, the edges of its text smoothed into ~
                                        half-tones (~d % or more of the pixels round them, which a scanner set to ~
                                        black and white leaves none of), was drawn by a program from a page that held ~
                                        its text as text, or its paper whitened all over."
                                   *rendered-paper-most* (round (* 100 *rendered-half-tones-least*)))
                     :revision (revision-index revision) :page (first pages)
                     :objects (remove-duplicates (mapcar (lambda (s) (mark-object (scan-picture-mark s))) rendered))
                     :details (loop for s in rendered
                                    collect (format nil "page ~d: ~a, ~:d by ~:d pixels, blank paper straying ~,2f"
                                                    (1+ (page-index (scan-picture-page s)))
                                                    (scan-named (scan-picture-mark s))
                                                    (scan-picture-width s) (scan-picture-height s)
                                                    (scan-picture-paper s)))
                     :evidence (pictures-evidence pdf revision
                                                  (loop for s in rendered repeat 10
                                                        collect (mark-object (scan-picture-mark s))))
                     :method :page-made-picture))))))

(defun scan-areas (odd bands)
  "The blocks of lines of a scanned picture set apart together and its bands
of paper set apart, from ODD, its words unlike the rest (UNLIKE-WORDS), and
BANDS (PAPER-BANDS), those over the same rows and columns made one, as two
values. The first is the areas, each (X0 Y0 X1 Y1 BANDS MEMBERS). It holds
its box in the picture made level, its bands, and its words each with one
way it is unlike the rest, (WORD . WAY), from the top of the page down; and
the words of ODD with their ways not of a block, each (WORD WAY...), told on
their own wherever they stand, so that a word pasted into a band printed
again keeps its place in the report."
  (let ((areas '()) (blocks '()))
    (loop for (word . ways) in odd
          do (dolist (way ways)
               (when (consp (fifth way))
                 (let ((entry (or (assoc (fifth way) blocks) (car (push (list (fifth way)) blocks)))))
                   (push (cons word way) (cdr entry))))))
    (loop for (nil . members) in (nreverse blocks)
          for words = (mapcar #'car members)
          do (push (list (reduce #'min words :key #'word-box-x0) (reduce #'min words :key #'word-box-y0)
                         (reduce #'max words :key #'word-box-x1) (reduce #'max words :key #'word-box-y1)
                         '() (reverse members))
                   areas))
    (dolist (band bands)
      (push (list (paper-band-x0 band) (paper-band-top band) (paper-band-x1 band) (paper-band-bottom band)
                  (list band) '())
            areas))
    ;; those over the same rows and columns made one, until no two are
    (setf areas (nreverse areas))
    (loop for merged = nil
          do (loop for (a . rest) on areas until merged
                   do (dolist (b rest)
                        (destructuring-bind (ax0 ay0 ax1 ay1 abands amembers) a
                          (destructuring-bind (bx0 by0 bx1 by1 bbands bmembers) b
                            (when (and (< ay0 by1) (< by0 ay1) (< ax0 bx1) (< bx0 ax1))
                              (setf areas (cons (list (min ax0 bx0) (min ay0 by0) (max ax1 bx1) (max ay1 by1)
                                                      (append abands bbands) (append amembers bmembers))
                                                (remove b (remove a areas)))
                                    merged t)
                              (return))))))
          while merged)
    ;; the words' other ways, each word of its own
    (values (sort areas #'< :key #'second)
            (loop for (word . ways) in odd
                  for others = (remove-if (lambda (way) (consp (fifth way))) ways)
                  when others collect (cons word others)))))

(defun pages-words (pages)
  "PAGES, numbers counting from 1, in words: page 3, pages 3 and 5, pages 3,
4 and 5."
  (format nil "page~p ~{~d~#[~; and ~:;, ~]~}" (length pages) pages))

(defun pictures-evidence (pdf revision objects)
  "The evidence of each picture of OBJECTS, object numbers in REVISION of PDF
\(OBJECT-EVIDENCE), one after another; none while a picture file is
examined (*PICTURE-FILE*), whose picture is the whole file."
  (unless *picture-file*
    (loop for object in objects append (nth-value 0 (object-evidence pdf revision object)))))

(defun odd-way-words (key own usual word)
  "One way WORD, a word of a scanned picture, is unlike the words beside it,
in words: KEY of *SCAN-MEASURES* with its OWN value and the USUAL one."
  (case key
    (:ink (format nil "ink grey ~d, the words beside it ~d" (round own) (round usual)))
    (:edges (format nil "half-tones ~,2f of its ink, the words beside it ~,2f" own usual))
    (:noise (format nil "paper straying ~,1f, the words beside it ~,1f" own usual))
    (:offset (let ((off (word-box-offset word)))
               (format nil "its baseline ~,1f pixels ~:[above~;below~] its line's" (abs off) (plusp off))))
    (:turn (let ((turn (word-box-turn word)))
             (format nil "turned ~,1f° ~:[anticlockwise~;clockwise~] against its line" (abs turn) (plusp turn))))))

(defun scan-row (scan x y)
  "The row of SCAN's picture as it was, before it was made level, of the
pixel at X in its row Y made level."
  (round (+ y (* (or (scan-picture-tilt scan) 0) x))))

(defun odd-word-details (scan odd)
  "For each word of ODD, (WORD WAY...) of SCAN, where it is on the page and
in the picture and each way it is unlike the words beside it, in words."
  (let ((m (scan-picture-mark scan)))
    (loop for (word . ways) in odd
          collect (destructuring-bind (x0 y0 x1 y1)
                      (picture-page-box m word (scan-picture-width scan) (scan-picture-height scan)
                                        (scan-picture-tilt scan))
                    (declare (ignore x1 y1))
                    (format nil "at x ~d, y ~d (pixels ~d,~d to ~d,~d~@[ of picture ~d~]): ~{~a~^; ~}"
                            (round x0) (round y0) (word-box-x0 word)
                            (scan-row scan (word-box-x0 word) (word-box-y0 word))
                            (word-box-x1 word) (scan-row scan (word-box-x0 word) (word-box-y1 word))
                            (picture-number (mark-object m))
                            (append (loop for (key own usual) in ways collect (odd-way-words key own usual word))
                                    (when (some (lambda (way) (eq (fifth way) :pair)) ways)
                                      (list (format nil "set beside the words it stands with, which stand as far apart ~
                                                         the other way: one of them was moved or put there")))))))))

(defparameter *odd-words-rules*
  '("A page scanned in one go has one ink, one blur and one paper throughout, and its words stand on their lines; a ~
     word whose edges are sharper than its line's, or whose paper strays half as far as the page's blank paper or ~
     less, and that also sits off its line or turned was put there another way: two measures apart from each other ~
     agree. Honest scans have such words too (a line printed in another pass, a heading, a word the light or the ~
     paper's curl set apart), so it is worth a look, not proof: a strong sign comes only from pixels the same where ~
     no scanner makes them so, or from the file's own structure. Another ink and another place alone come together ~
     from one cause, as a stamp, a form filled in or a line printed in another pass have them; and a word of narrow ~
     strokes reads as softer at its edges and cleaner within its box."
    "A page scanned or photographed in one go has one ink, one blur and one paper, changing little from one word to ~
     the next however the light and focus change across it, and its words stand on their lines, however the paper ~
     curls; words whose ink is darker or lighter, whose edges are sharper or softer, or whose paper is cleaner or ~
     noisier than the words on both sides of them, or that step off their line from both sides, were put there ~
     another way: pasted or printed over the scan, or a stamp, a form filled in or a line printed in another pass.")
  "The rules of the findings of words unlike the rest of a scan: of those
unlike it both in how they look and where they sit, and of the others;
format controls of nothing.")

(defun odd-words-finding (pdf revision scan odd both)
  "The finding of the words ODD, (WORD WAY...), of SCAN unlike the rest of it,
worth a look: with BOTH, those unlike it both in how they look and where
they sit. NIL when ODD is empty."
  (when odd
    (let ((page (scan-picture-page scan)) (m (scan-picture-mark scan)))
      (found :look (format nil "On page ~d, ~a has ~:d word~:p unlike the rest of ~
                                it~a, ~{~a~^, ~}: pasted into the scan, or printed over it~:[, or a stamp, a form ~
                                filled in, or printed in another pass~;~]~:[; its grain too faint to tell prints ~
                                apart, or drawn by a program, as a screenshot or a chart is: worth a look at most~;~]."
                           (1+ (page-index page)) (scan-named m) (length odd)
                           (cond ((not both) "")
                                 ((= (length odd) 1) " both in how it looks and where it sits")
                                 (t " both in how they look and where they sit"))
                           (remove-duplicates (loop for (nil . ways) in odd append (unlike-words-description ways))
                                              :test #'string=)
                           both (or (not both) (scan-picture-judged scan)))
             :rule (format nil (if both (first *odd-words-rules*) (second *odd-words-rules*)))
             :revision (revision-index revision)
             :page (1+ (page-index page))
             :objects (list (mark-object m))
             :details (odd-word-details scan odd)
             :evidence (pictures-evidence pdf revision (list (mark-object m)))
             ;; a word unlike its line in how it looks was put there; one only off its line was moved
             :method (if (some (lambda (o) (some (lambda (w) (member (first w) *scan-looks*)) (cdr o))) odd)
                         :pasted-on-scan
                         :moved-in-scan)))))

(defun area-way-words (key members)
  "How the words of a block of lines, MEMBERS ((WORD . WAY)...), are unlike
the lines about it in the way KEY, in words: the middle of their own values
and of the usual ones."
  (let* ((ways (loop for (nil . way) in members when (eq (first way) key) collect way))
         (own (median-of (mapcar #'second ways)))
         (usual (median-of (mapcar #'third ways))))
    (case key
      (:ink (format nil "ink grey ~d, the lines about it ~d" (round own) (round usual)))
      (:edges (format nil "half-tones ~,2f of its ink, the lines about it ~,2f" own usual))
      (:noise (format nil "paper straying ~,1f, the lines about it ~,1f" own usual))
      (:offset (format nil "baselines ~,1f pixels off their lines" (abs own)))
      (t (format nil "turned ~,1f° against their lines" (abs own))))))

(defun area-finding (pdf revision scan area)
  "The finding of AREA, (X0 Y0 X1 Y1 BANDS MEMBERS) as SCAN-AREAS gives it, a
block of lines of SCAN set apart together from the lines above and below,
worth a look."
  (destructuring-bind (x0 y0 x1 y1 bands members) area
    (let* ((page (scan-picture-page scan)) (m (scan-picture-mark scan))
           (words (remove-duplicates (mapcar #'car members)))
           (tall (and words (median-of (mapcar (lambda (w) (- (word-box-y1 w) (word-box-y0 w))) words))))
           (tops (sort (mapcar #'word-box-y0 words) #'<))
           (lines (if words (1+ (loop for (a b) on tops while b count (> (- b a) (/ tall 2)))) 0))
           (keys (remove-duplicates (mapcar (lambda (pair) (first (cdr pair))) members)))
           (band (first bands))
           (paper (and band (if (< (paper-band-paper band) (paper-band-above band)) "darker paper" "lighter paper")))
           (box (picture-page-box m (list x0 y0 x1 y1) (scan-picture-width scan) (scan-picture-height scan)
                                  (scan-picture-tilt scan)))
           (count (reduce #'max bands :key #'paper-band-words :initial-value (length words)))
           (spans (reduce #'max bands :key #'paper-band-lines :initial-value lines)))
      (found :look (format nil "On page ~d, ~a has a block ~:[of ~:d word~:p over ~:d ~
                                line~:p~;over ~*~:d line~:p, its words not read,~] unlike the rest of it, set apart ~
                                together from the lines above and below, ~{~a~^, ~}: printed again or retouched ~
                                there, or a band printed in another pass."
                           (1+ (page-index page)) (scan-named m) (zerop count) count spans
                           (append (and paper (list paper))
                                   (remove-duplicates (unlike-words-description (mapcar #'cdr members))
                                                      :test #'string=)))
             :rule (format nil "A page scanned in one go has one ink, one blur and one paper throughout, however ~
                                evenly or unevenly the light falls across it; a block of lines whose paper stands ~
                                apart from the paper above and below it with edges of its own, or whose ink, edges or ~
                                paper stand apart together from the lines above and below it, by less than a word ~
                                alone must and more than half that, was printed again, retouched or pasted there, or ~
                                printed in another pass. The light's smooth fall across the page is taken out first, ~
                                and a shadow or a curl of the paper fades where a band printed again has edges; a ~
                                block set apart by its ink alone is named only where its paper, its edges or the ~
                                grain of its paper stand apart too, as a band printed again or a patch brings them: a ~
                                line printed darker in a second pass of the press, or set in bold, keeps the page's ~
                                paper, and so cannot be told from one printed so honestly. Honest pages have such ~
                                blocks too, a shaded row of a form among them, so it is worth a look, not proof.")
             :revision (revision-index revision) :page (1+ (page-index page))
             :objects (list (mark-object m))
             :details (list (format nil "from x ~d, y ~d to x ~d, y ~d (pixels ~d,~d to ~d,~d~@[ of picture ~d~]): ~
                                         ~{~a~^; ~}"
                                    (round (first box)) (round (second box)) (round (third box)) (round (fourth box))
                                    x0 (scan-row scan x0 y0) x1 (scan-row scan x0 y1) (picture-number (mark-object m))
                                    (append (and band (list (format nil "paper grey ~d, the paper above it ~d and ~
                                                                         below it ~d"
                                                                    (round (paper-band-paper band))
                                                                    (round (paper-band-above band))
                                                                    (round (paper-band-below band)))))
                                            (loop for key in keys collect (area-way-words key members)))))
             :evidence (pictures-evidence pdf revision (list (mark-object m)))
             :method :pasted-on-scan))))

(defun scan-word-findings (pdf revision scan)
  "The findings of SCAN's words unlike the rest of it: its blocks of lines
and bands of paper set apart together, each one finding, then its other
words unlike the rest, those unlike it both in how they look and where they
sit apart from the others. All are worth a look at most."
  (multiple-value-bind (areas odd) (scan-areas (scan-picture-odd scan) (scan-picture-bands scan))
    (flet ((both-p (o) (looks-and-sits-unlike-p (cdr o) (scan-picture-paper scan))))
      (remove nil (append (loop for area in areas collect (area-finding pdf revision scan area))
                          (list (odd-words-finding pdf revision scan (remove-if-not #'both-p odd) t)
                                (odd-words-finding pdf revision scan (remove-if #'both-p odd) nil)))))))

(defparameter *copy-why-words*
  '((:grid . "on the JPEG's own grid of 8 by 8 pixels")
    (:stamp . "in an ink or at an angle unlike the page's text")
    (:jpeg . "on a page kept as a JPEG, which keeps clean prints alike")
    (:smooth . "their cores straying less than their page's, as smoothed prints do")
    (:faint . "their page's prints too alike to tell grain from compression or level steps")
    (:stepped . "their page's grey levels stepped alike for every print, as a JPEG kept without loss since or a ~
                 picture of few grey levels steps them"))
  "Why a group of copies is named as it is (the sixth of a group COPIED-GLYPHS
gives), in words.")

(defparameter *copy-reports*
  (list
   (list :strong
         (lambda (c) (not (or (eq (fifth c) :look) (seventh c))))
         "~:d characters of the scanned page~p are copies of one another, the grain of their ink too: copied within ~
          the scan, or typed onto it."
         "No scanner draws a character twice alike: each print of it differs in the grain of its ink and in where the ~
          blur falls on the pixels. Two characters that share a block of pixels the same, grey level for grey level, ~
          the ink of the block straying as a print's does, or whose ink is the same in every pixel on a page kept ~
          without loss, are one character copied, or both drawn by a program, as typed text is. Characters only alike, ~
          however near, are not named so: clean prints can come near each other. Letters the blur joined to a ~
          neighbour are cut apart at the neck between them and set beside the rest as pieces. How alike characters ~
          typically are is measured on each scanned page apart, and two characters are set beside each other by the ~
          less of their pages'.")
   (list :strong
         (lambda (c) (and (eq (fifth c) :strong) (seventh c)))
         "~:d characters of the scanned page~p are copies of one another, the grain of their ink too: copied within ~
          the scan, or a stamp, a logo or a signature laid on the pages by software."
         "No scanner draws a character twice alike: two characters that share a block of pixels the same, grey level ~
          for grey level, the ink of the block straying as a print's does, are one copied. These are taller than the ~
          type of their pages, or stand at the same place on most pages: a stamp, a logo or a signature kept as a ~
          picture and laid on each page by a program does so, as offices do honestly, as well as a mark copied to ~
          deceive. Whether such a mark belongs there is for the examiner to judge.")
   (list :look
         (lambda (c) (eq (sixth c) :stamp))
         "~:d characters of the scanned page~p are the same pixel for pixel, in an ink or at an angle unlike the ~
          page's text: a stamp drawn by software, or characters copied there."
         "No scanner draws a character twice alike, but a program draws its characters the same every time: a stamp an ~
          office's software lays on a scan, a date or a received mark, repeats its figures pixel for pixel. These ~
          characters are in an ink unlike the page's text (red, blue or green on black type, or a grey far from the ~
          type's), or turned 10 degrees or more against the page's lines, as a stamp is, and none of them is the same ~
          as a character outside them; and the stamp is as a program draws it: the grey levels of its characters' ~
          cores (their pixels no lighter than halfway from the darkest to the lightest of each box) typically stray ~
          from their middle by less than 0.6 of what the page's characters' cores do, or the paper within their boxes ~
          by less than a fifth of what the paper within the page's characters' boxes does, as a stamp drawn and laid ~
          on a scan does and a stamp pressed on paper and scanned with the page, carrying its grain, does not; it ~
          repeats two shapes or more; and every character of a shape repeated in it is the same as the others of that shape, ~
          or nearer to them than prints are. A figure copied over another in a pressed stamp, or over every print of ~
          its shape, stays a strong sign. Characters typed onto a scan in the page's own ink and at its angle are a ~
          strong sign; whether such a stamp belongs there is for the examiner to judge.")
   (list :look
         (lambda (c) (and (eq (fifth c) :look) (member (sixth c) '(:faint :stepped))))
         "~:d characters of the scanned page~p are the same pixel for pixel on a page whose prints a JPEG kept without ~
          loss, few grey levels or faint grain can make the same: copied within the scan, or prints made the same so."
         "No scanner draws a character twice alike, but the steps a JPEG saved and kept without loss since, or a ~
          picture of few grey levels, lays on a page's grey levels are the same for every print of a character, and ~
          where its prints also differ by less than 4 grey levels they lay them alike: two honest prints can come out ~
          the same pixel for pixel, the grain and all. A copy laid pixel for pixel on such a page is named here, with ~
          which of these holds, worth a look, never missed; a strong sign it is not.")
   (list :look
         (lambda (c) (not (or (eq (fifth c) :strong) (member (sixth c) '(:stamp :faint :stepped)))))
         "~:d characters of the scanned page~p are nearer one another than prints of one character are: copied within ~
          the scan and saved again, or alike by chance, as clean prints on pages kept as JPEG or JPEG 2000, or ~
          smoothed, can be."
         "A copy saved again as a JPEG or a JPEG 2000 with loss is no longer alike pixel for pixel, but stays nearer ~
          its source than prints of a character are to one another: two characters whose ink is alike to half of how ~
          alike the scan's characters of the same shape typically are, or each the other's nearest and twice as near ~
          as to the next of its shape. Two prints of a character now and then come as near, on a clean page nearer ~
          still, so likeness alone is worth a look, never a strong sign; pieces cut from letters the blur joined, set ~
          beside others by their likeness, are worth a look at most, as two prints of the same letters cut at the same ~
          neck come nearer each other than prints of a letter do; and so are characters the same pixel for pixel over ~
          less than two characters' width where they fall alike on a JPEG's grid of 8 by 8 pixels, which keeps clean ~
          prints alike block for block. On a page kept as a JPEG, or smoothed by the scanner's software, clean prints ~
          of one character come alike, and two characters alike there by their likeness alone are worth a look, never ~
          a strong sign. A character of too little shape, a stroke alone (l, I, a plain 1 or 7, a dash), comes near ~
          another stroke by chance, and is named by its likeness only when the two pixels of paper about it are as ~
          alike as its ink, the two each the other's nearest, as a stroke copied with the paper round it stays; a mark ~
          taller by half than the type, five times as tall as it is wide, taller by a tenth than the type and three ~
          times as tall as it is wide, or three times as wide as it is tall and wider by half than a character, is a ~
          piece of a rule and no character; a hyphen, a minus sign or a dash is a character, a stroke alone."))
  "The findings of characters copied, in order: each (STRENGTH TEST WORDS
RULE). TEST, a function of a group COPIED-GLYPHS gives, picks the groups
of the finding; WORDS, a format control of the count of characters and of
pages; RULE, a format control of nothing.")

(defun copy-group-words (scans group difference block alike why)
  "A group of copies, GROUP of SCANS, in words: where each is, how alike they
are (DIFFERENCE grey levels, or the same pixel for pixel over BLOCK, (WIDTH
HEIGHT INK)), WHY they are named as they are, and how alike the scan's
characters of the same shape are (ALIKE)."
  (format nil "~{~a~^, ~}: ~:[~:[alike to ~,1f grey levels~;the same grey in every pixel of their ink~*, their ~
               paper apart~]~*~;~2*the same pixel for pixel over ~{~d by ~d pixels, ~:d of them ink~}~]~@[, ~a~], the ~
               scan's characters of the same shape alike to ~,1f"
          (mapcar (lambda (g) (glyph-words scans g)) group)
          block (zerop difference) difference block
          (let ((words (cdr (assoc why *copy-why-words*)))) (and words (format nil words)))
          alike))

(defun copy-findings (pdf revision scans copies not-judged)
  "The findings of the groups of characters COPIES of SCANS copied among
them (COPIED-GLYPHS), one for each report of *COPY-REPORTS* that has any,
each saying how many characters NOT-JUDGED were not judged."
  (loop for (strength test words rule) in *copy-reports*
        for these = (remove-if-not test copies)
        when these
          collect (let ((pages (sort (remove-duplicates (loop for (group) in these append (mapcar #'glyph-page group)))
                                     #'<))
                        (pictures (remove-duplicates (loop for (group) in these
                                                           append (mapcar #'glyph-picture group)))))
                    (found strength
                           (format nil words (loop for (group) in these sum (length group)) (length pages))
                           :rule (format nil rule)
                           :revision (revision-index revision) :page (1+ (first pages)) :objects pictures
                           :details (append (loop for (group difference block alike nil why) in these
                                                  collect (copy-group-words scans group difference block alike why))
                                            (and not-judged (list (not-judged-words not-judged))))
                           :evidence (pictures-evidence pdf revision pictures)
                           :method (if (eq strength :strong) :characters-composed :characters-composed-resaved)))))

(defun unjudged-reasons (scan alike sampled near measured)
  "Every reason a scanned picture, SCAN (or NIL), was not judged: :unread
alone when no words were read from it, else those of :drawn, :faint (its
characters ALIKE to less than 4 grey levels), :uneven (of SAMPLED characters
set beside others only NEAR had one of their shape within 30 grey levels),
:ruled, :joined (its lines run together, MEASURED characters read), or :few
when none holds and too few were sampled."
  (if (and scan (scan-picture-unread scan))
      (list :unread)
      (let ((reasons (remove nil (list (and scan (scan-picture-drawn scan) :drawn)
                                       (and alike (< alike 4) :faint)
                                       (and sampled (>= sampled 40) (< near 40) :uneven)
                                       (and scan (plusp (scan-picture-rules scan))
                                            (>= (* 10 (scan-picture-rules scan)) (scan-picture-ink scan)) :ruled)
                                       ;; lines run together only where they leave enough unread to tell
                                       (and scan (plusp (scan-picture-tall-bands scan))
                                            (or (null measured) (null (scan-picture-characters scan))
                                                (< measured (* *scan-read-least* (scan-picture-characters scan))))
                                            :joined)))))
        (if (or reasons (and sampled (>= sampled 40))) reasons (list :few)))))

(defun unjudged-pages (scans pictures)
  "The scanned pages of SCANS not judged, each (PAGE REASONS ALIKE SAMPLED
NEAR MIDDLE SCAN), PAGE counting from 1: those PICTURES (how alike
characters are on each) measured and found too faint or drawn, and those of
no character to measure."
  (flet ((scan-of (page) (find page scans :key (lambda (s) (page-index (scan-picture-page s))))))
    (append (loop for ((nil . page) alike measured sampled near middle) in pictures
                  for scan = (scan-of page)
                  unless (and alike (>= alike 4) (not (and scan (scan-picture-drawn scan))))
                    collect (list (1+ page) (unjudged-reasons scan alike sampled near measured)
                                  alike sampled near middle scan))
            (loop for scan in scans
                  for key = (cons (mark-object (scan-picture-mark scan)) (page-index (scan-picture-page scan)))
                  ;; a picture kept twice is checked where it was read first
                  unless (or (find key pictures :key #'first :test #'equal)
                             (and (scan-picture-twin scan) (not (scan-picture-unread scan))))
                    collect (list (1+ (page-index (scan-picture-page scan))) (unjudged-reasons scan nil nil nil nil)
                                  nil nil nil nil scan)))))

(defun unjudged-reason-words (reason plural)
  "Why copies and grain were not checked on a page, or on more than one
when PLURAL, for REASON (UNJUDGED-REASONS), in words."
  (ecase reason
    (:unread (if plural "no words could be read from their pictures" "no words could be read from its picture"))
    (:joined (if plural
                 "their lines run together, too close to be told apart"
                 "its lines run together, too close to be told apart"))
    (:few "too few characters to tell how alike prints of one are")
    (:ruled "characters could not be found among the rules")
    (:uneven "prints of one character are too unlike each other to measure, as where light, focus or sharpening ~
              change across a photograph")
    (:faint "the scan's grain is too faint to tell prints apart")
    (:drawn (if plural
                "their paper and ink as flat as a program draws them, or as an app's clipping leaves them"
                "its paper and ink as flat as a program draws them, or as an app's clipping leaves them"))))

(defun unjudged-detail (reason page alike sampled near middle scan)
  "What was measured on PAGE, not judged for REASON, in words: ALIKE, how
alike its characters are, SAMPLED and NEAR as UNJUDGED-REASONS takes them,
MIDDLE, how alike those near are; NIL when nothing measured is said for
REASON."
  (case reason
    (:faint (format nil "page ~d: characters of the same shape alike to ~,1f grey levels; 4 are needed~:[~;; ~
                         characters the same pixel for pixel, grain and all, sought all the same~]"
                    page alike (>= alike *exact-grain-least*)))
    (:uneven (format nil "page ~d: of ~:d characters set beside others, ~:d had one of their shape within 30 grey ~
                          levels~@[, alike to ~,1f~]; 40 are needed"
                     page sampled near middle))
    (:drawn (format nil "page ~d: its blank paper strays by ~,1f grey levels and its characters' cores by a grey level ~
                         or less~@[; characters of the same shape alike to ~,1f~]"
                    page (scan-picture-paper scan) alike))
    ((:few :ruled :joined)
     (and sampled (< sampled 40)
          (format nil "page ~d: ~:d character~:p set beside others; 40 are needed" page sampled)))))

(defun unjudged-findings (revision scans pictures)
  "The findings, for the record, of the scanned pages of SCANS not judged
\(UNJUDGED-PAGES), one for each reason with the pages it holds for, each
detail said once, under the first reason it belongs to."
  (let ((unjudged (unjudged-pages scans pictures)) (told '()))
    (loop for reason in '(:unread :joined :few :ruled :uneven :faint :drawn)
          for these = (remove-duplicates (sort (remove-if-not (lambda (u) (member reason (second u))) unjudged)
                                               #'< :key #'first)
                                         :key #'first)
          when these
            collect (found :record (format nil "Copies and grain not checked on ~a: ~a."
                                           (pages-words (mapcar #'first these))
                                           (format nil (unjudged-reason-words reason (cdr these))))
                           :rule (format nil "Copies are told by the grain of the scan, which no two prints of a ~
                                              character share. Where characters of the same shape differ by less ~
                                              than 4 grey levels on a scanned page (a picture drawn rather than ~
                                              scanned, made black and white, or smoothed) every print looks like a ~
                                              copy, and none is told on it; nor where there are too few characters ~
                                              to tell how alike prints are, as on a page whose characters touch the ~
                                              rules of a table so that they cannot be cut apart from them, or whose ~
                                              lines are set so close that they run together; nor where fewer than ~
                                              40 characters have a print of their shape within 30 grey levels, as ~
                                              on a photograph whose light, focus or sharpening change across the ~
                                              page; nor on a picture whose blank paper strays by less than half a ~
                                              grey level and whose characters' cores by a grey level or less, as a ~
                                              program draws a page or an app's clipping leaves one. Every reason ~
                                              that holds for a page is given, but for a page of which no words could ~
                                              be read, which is said alone, and what was measured is said once, ~
                                              under the first reason it belongs to; lines run together are named ~
                                              only where fewer than half the characters the page's ink holds were ~
                                              read. Each page is measured apart, so that a page of a file put ~
                                              together from two sources is not judged by the other's grain. Nothing ~
                                              is judged on such a page: neither copies nor characters without grain ~
                                              of their own; but where its characters of the same shape differ by a ~
                                              grey level or more, characters the same pixel for pixel, the grain of ~
                                              their ink too, are still sought and named, as no two prints share ~
                                              their grain.")
                           :revision (revision-index revision) :page (first (first these))
                           :objects (remove-duplicates (mapcar (lambda (s) (mark-object (scan-picture-mark s))) scans))
                           :details (loop for (page nil alike sampled near middle scan) in these
                                          for said = (unjudged-detail reason page alike sampled near middle scan)
                                          when (and said
                                                    (not (member said details :test #'string=))
                                                    (not (member said told :test #'string=)))
                                            collect said into details
                                          finally (setf told (append details told)) (return details))))))

(defun partly-read-findings (revision scans pictures)
  "The findings, for the record, of the judged scanned pages of SCANS on
which only some of the characters were read: a band of lines run together
left unread, or fewer than *SCAN-READ-LEAST* of those its ink holds."
  (loop for ((nil . page) alike measured) in pictures
        for scan = (find page scans :key (lambda (s) (page-index (scan-picture-page s))))
        for about = (and scan (scan-picture-characters scan))
        for bands = (if scan (scan-picture-tall-bands scan) 0)
        when (and alike (>= alike 4) about (or (plusp bands) (< measured (* *scan-read-least* about))))
          collect (found :record (if (plusp bands)
                                     (format nil "Copies and grain checked on ~:d of about ~:d characters on page ~d: ~
                                                  ~:d band~:p of lines run together ~:[was~;were~] left unread, the ~
                                                  rest of the page read."
                                             measured about (1+ page) bands (> bands 1))
                                     (format nil "Copies and grain checked on ~:d of about ~:d characters on page ~d: ~
                                                  the rest could not be cut from the rules, from one another or from ~
                                                  lines run together."
                                             measured about (1+ page)))
                         :rule (format nil "Characters are cut apart where a column holds no ink, and rules are taken ~
                                            out first. Characters set against the rules of a table on both sides, or ~
                                            run together, cannot always be cut apart; nor can lines set so close that ~
                                            their small letters run into one another, or crossed by a mark such as a ~
                                            signature, which are left unread where they meet, the rest of the page ~
                                            read. Such characters are not set beside others, and a copy among them ~
                                            goes unseen. How many characters a page holds is reckoned from its ink ~
                                            once the rules are taken out, over its characters' middle ink; the ~
                                            reckoning strays, so a page is named when fewer than half are read, and ~
                                            whenever lines run together were left unread.")
                         :revision (revision-index revision) :page (1+ page)
                         :objects (list (mark-object (scan-picture-mark scan))))))

(defun left-out-finding (revision scans left)
  "The finding, for the record, of the LEFT strips of SCANS' characters not
set beside others; NIL when there are none."
  (when (plusp left)
    (found :record (format nil "~:d strip~:p of the scanned pages' characters not set beside others: exact copies ~
                                among them may be missed." left)
           :rule (format nil "Exact copies are told by strips of characters the same pixel for pixel. A file of more ~
                              characters than can be counted so, or a character more than 1,000 pixels across, has ~
                              strips that are not set beside others; copies among them can still be told by their ~
                              likeness.")
           :revision (revision-index revision) :page (1+ (page-index (scan-picture-page (first scans))))
           :objects (remove-duplicates (mapcar (lambda (s) (mark-object (scan-picture-mark s))) scans)))))

(defun not-judged-finding (revision scans not-judged)
  "The finding, for the record, of the characters NOT-JUDGED of SCANS, too
few pixels left once clipped ink is set aside; NIL when there are none."
  (when not-judged
    (let ((pages (sort (remove-duplicates (mapcar #'glyph-page not-judged)) #'<)))
      (found :record (format nil "~a, on ~a." (not-judged-words not-judged) (pages-words (mapcar #'1+ pages)))
             :rule (format nil "Ink a scanner clipped to one grey is the same in every print, and would make any two ~
                                prints alike: it is set aside when characters are set beside each other, and a ~
                                character with too few pixels left beside every other is not judged, neither a copy ~
                                nor a print.")
             :revision (revision-index revision) :page (1+ (first pages))
             :objects (remove-duplicates (mapcar #'glyph-picture not-judged))
             :details (mapcar (lambda (g) (glyph-words scans g)) (subseq not-judged 0 (min 50 (length not-judged))))))))

(defun not-read-findings (pdf revision not-read)
  "The findings, for the record, of the scanned pictures NOT-READ, each
\(PAGE MARK WHY) as SCAN-PICTURES gives them: one for each reason."
  (loop for why in (remove-duplicates (mapcar #'third not-read) :test #'equal)
        collect (let* ((these (remove why not-read :key #'third :test-not #'equal))
                       (pages (sort (remove-duplicates (mapcar (lambda (u) (1+ (page-index (first u)))) these)) #'<)))
                  (found :record (format nil "The scanned picture~:[~;s~] on ~a ~:[was~;were~] not read (~:[~;of ~
                                              each, ~]~a): no copies, grain, words unlike the rest or JPEG blocks ~
                                              were looked for there."
                                         (rest these) (pages-words pages) (rest these) (rest these) why)
                         :rule (format nil "The pixels of a scanned page are read to look for characters copied ~
                                            within it, words unlike the rest and pieces saved apart. A picture kept ~
                                            in a form not read here (JBIG2, fax), larger than ~
                                            *palimpsest-scan-pixels* allows, or that the sandbox could not turn into ~
                                            pixels, is named rather than passed over in silence: nothing is claimed ~
                                            of it either way.")
                         :revision (revision-index revision) :page (first pages)
                         :objects (remove-duplicates (mapcar (lambda (u) (mark-object (second u))) these))
                         :details (loop for (page mark) in these
                                        collect (format nil "page ~d: the picture~:[ ~a (object ~d)~;~]"
                                                        (1+ (page-index page)) *picture-file* (mark-text mark)
                                                        (mark-object mark)))
                         :evidence (pictures-evidence pdf revision (loop for (nil mark) in these
                                                                         repeat 10
                                                                         collect (mark-object mark)))))))

(defun bare-mark-finding (pdf revision scans marks)
  "The finding, worth a look, of the groups MARKS of SCANS' marks without
grain of their own the same pixel for pixel; NIL when there are none."
  (when marks
    (let ((pages (sort (remove-duplicates (loop for group in marks append (mapcar #'glyph-page group))) #'<))
          (pictures (remove-duplicates (loop for group in marks append (mapcar #'glyph-picture group)))))
      (found :look (format nil "~:d marks on ~a are the same pixel for pixel, with no grain of their own: drawn by a ~
                                program, or prints the scanner clipped."
                           (loop for group in marks sum (length group)) (pages-words (mapcar #'1+ pages)))
             :rule (format nil "A scanner sets each pixel of a print in part by chance, by the grain of the paper and ~
                                where the blur falls, and so prints no character twice alike. A mark whose ink strays ~
                                by a grey level or less (or a fifth of what the page's characters typically stray) ~
                                has no grain of its own: two of them the same pixel for pixel were drawn by a ~
                                program, or are prints that the scanner clipped to one grey alike.")
             :revision (revision-index revision) :page (1+ (first pages)) :objects pictures
             :details (loop for group in marks
                            collect (format nil "~{~a~^, ~}: the same pixel for pixel, with no grain of their own, ~
                                                 the scan's characters of the same shape alike to ~,1f"
                                            (mapcar (lambda (g) (glyph-words scans g)) group)
                                            (reduce #'min group :key #'glyph-alike)))
             :evidence (pictures-evidence pdf revision pictures)
             :method :typed-into-scan))))

(defun bare-entry-words (scans kind words members)
  "A word or line without grain of its own, in words: KIND :word or :line,
its WORDS, and MEMBERS, its characters, found among SCANS."
  (let* ((scan (scan-of-glyph scans (first members))) (m (scan-picture-mark scan))
         (first-word (first words)) (last-word (car (last words))))
    (destructuring-bind (x0 y0 x1 y1)
        (picture-page-box m first-word (scan-picture-width scan) (scan-picture-height scan) (scan-picture-tilt scan))
      (declare (ignore x1 y1))
      (format nil "~:[a word~;a line of ~:*~d words~] at x ~d, y ~d (pixels ~d,~d to ~d,~d~@[ of picture ~d~]): ~:d ~
                   characters, none with grain of its own"
              (and (eq kind :line) (length words)) (round x0) (round y0)
              (word-box-x0 first-word) (scan-row scan (word-box-x0 first-word) (word-box-y0 first-word))
              (word-box-x1 last-word) (scan-row scan (word-box-x1 last-word) (word-box-y1 last-word))
              (picture-number (mark-object m)) (length members)))))

(defun bare-word-findings (pdf revision scans bare)
  "The findings, worth a look, of the words and lines BARE of SCANS without
grain of their own, each (KIND WORDS MEMBERS): one for each page, from the
first."
  (let ((by-page (make-hash-table)))
    (dolist (entry bare) (push entry (gethash (glyph-page (first (third entry))) by-page)))
    (loop for page in (sort (loop for p being the hash-keys of by-page collect p) #'<)
          collect (let* ((entries (sort (gethash page by-page) #'< :key (lambda (e) (word-box-y0 (first (second e))))))
                         (lines (count :line entries :key #'first)) (words (count :word entries :key #'first))
                         (pictures (remove-duplicates (loop for e in entries
                                                            append (mapcar #'glyph-picture (third e))))))
                    (found :look (format nil "On page ~d, ~@[~a~]~:[~; and ~]~@[~a~] of the scanned page ~
                                              ~:[has~;have~] no grain of ~:[its~;their~] own: drawn into the scan by a ~
                                              program, or in bold or heavier type, or ink, a stamp or coloured ink the ~
                                              scanner clipped."
                                         (1+ page)
                                         (and (plusp lines) (if (= lines 1) "a line" (format nil "~d lines" lines)))
                                         (and (plusp lines) (plusp words))
                                         (and (plusp words) (if (= words 1) "a word" (format nil "~d words" words)))
                                         (> (+ lines words) 1) (> (+ lines words) 1))
                           :rule (format nil "A scanner sets each pixel of a print in part by chance, by the grain of ~
                                              the paper and where the blur falls, so that the core of every print ~
                                              (its pixels no lighter than halfway from its darkest to its lightest ~
                                              grey) strays by some grey levels, however wide its strokes. A word or a ~
                                              line whose characters' cores stray by a grey level or less, or by a ~
                                              fifth of what the page's characters typically stray, was drawn into the ~
                                              picture by a program, as a picture editor's text tool draws text; or ~
                                              its ink is one grey for another reason: a scanner that clips dark ink, ~
                                              a stamp or a coloured ink to one level, or a type heavier than the page ~
                                              measures it against.")
                           :revision (revision-index revision) :page (1+ page) :objects pictures
                           :details (loop for (kind ws members) in entries
                                          collect (bare-entry-words scans kind ws members))
                           :evidence (pictures-evidence pdf revision pictures)
                           :method :typed-into-scan)))))

(defun check-scans (pdf revision)
  "Pictures of a page whose words are unlike each other: words whose
ink, edges or paper stand apart from the words beside them, or that sit
off their line's baseline or are turned against it (UNLIKE-WORDS). They
are worth a look at most. Those unlike the rest both in how they look
and in where they sit are named apart from those unlike it in one of
them, as real scans have both. And characters that are copies of one
another, on one scanned page or across them (COPIED-GLYPHS); and, worth
a look, marks without grain of their own the same pixel for pixel, and
words and lines without grain of their own (BARE-WORDS). A scanned
picture whose pixels could not be read is named for the record, with
why, so that no page goes unexamined unsaid; and, for the record, the
resolution each was scanned at, where it can be told (RESOLUTION-FINDINGS)."
  (let* ((reading (scans-read pdf revision))
         (scans (scan-reading-scans reading))
         (not-judged (scan-reading-not-judged reading))
         (pictures (scan-reading-pictures reading)))
    (remove nil (append (rendered-page-findings pdf revision scans)
                        (loop for scan in scans append (scan-word-findings pdf revision scan))
                        (copy-findings pdf revision scans (scan-reading-copies reading) not-judged)
                        (unjudged-findings revision scans pictures)
                        (partly-read-findings revision scans pictures)
                        (list (left-out-finding revision scans (scan-reading-left reading))
                              (not-judged-finding revision scans not-judged))
                        (not-read-findings pdf revision (scan-reading-not-read reading))
                        (list (bare-mark-finding pdf revision scans (scan-reading-marks reading)))
                        (bare-word-findings pdf revision scans (scan-reading-bare reading))
                        (resolution-findings pdf revision scans)))))
