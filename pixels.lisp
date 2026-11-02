;;;; pixels.lisp --- The pixels of a scanned page: words whose ink, edges or paper are unlike the rest.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A page scanned in one go has one ink, one blur and one paper throughout.
;;;; Its picture is read as grey levels, made black and white at the level
;;;; that best parts ink from paper, cut into lines and the lines into words;
;;;; each word's ink, edges and paper are measured, and the words unlike all
;;;; the rest named: pasted into the scan, or printed over it.

(in-package #:palimpsest)

(define-package-setting *palimpsest-scan-pixels* (* 60 1000 1000)
  "The most pixels a picture may have for its words to be measured: a page
scanned at 600 dots an inch is some 35 million.")

(deftype grey-pixels () '(simple-array (unsigned-byte 8) (*)))

;;; Grey levels

(defun colour-space-of (pdf revision value)
  "The colour space VALUE names, as two values: :grey, :rgb, :cmyk or
:indexed, and for :indexed (BASE HIGHEST TABLE), BASE the kind of its colours
and TABLE their octets; NIL for one not read here."
  (let ((space (deref pdf revision value)))
    (cond ((null space) nil)
          ((stringp space)
           (cond ((member space '("/DeviceGray" "/G" "/CalGray") :test #'string=) :grey)
                 ((member space '("/DeviceRGB" "/RGB" "/CalRGB") :test #'string=) :rgb)
                 ((member space '("/DeviceCMYK" "/CMYK") :test #'string=) :cmyk)))
          ((consp space)
           (let ((items (pdf-array-items space)))
             (cond ((name-p (first items) "/ICCBased")
                    (let* ((object (and (pdf-ref-p (second items)) (revision-object pdf revision (second (second items)))))
                           (n (and object (pdf-get (pdf-indirect-value object) "/N"))))
                      (case n (1 :grey) (3 :rgb) (4 :cmyk))))
                   ((member (first items) '("/CalGray") :test #'equal) :grey)
                   ((member (first items) '("/CalRGB" "/Lab") :test #'equal) (and (name-p (first items) "/CalRGB") :rgb))
                   ((member (first items) '("/Indexed" "/I") :test #'equal)
                    (let* ((base (colour-space-of pdf revision (second items)))
                           (highest (deref pdf revision (third items)))
                           (lookup (deref pdf revision (fourth items)))
                           (table (cond ((stringp lookup) (map '(simple-array (unsigned-byte 8) (*)) #'char-code lookup))
                                        ((and (pdf-ref-p (fourth items)) (revision-object pdf revision (second (fourth items))))
                                         (multiple-value-bind (data why) (stream-data pdf (revision-object pdf revision (second (fourth items))))
                                           (and (null why) data))))))
                      (when (and (member base '(:grey :rgb :cmyk)) (integerp highest) table)
                        (values :indexed (list base highest table)))))))))))

(defun grey-of (kind a b c d)
  "The grey level, 0 black to 255 white, of a colour of KIND (:grey, :rgb or
:cmyk) whose parts, each 0 to 255, are A to D."
  (declare (type (integer 0 255) a b c d))
  (ecase kind
    (:grey a)
    (:rgb (round (+ (* 299 a) (* 587 b) (* 114 c)) 1000))
    (:cmyk (let ((k (- 255 d)))
             (round (* (+ (* 299 (- 255 a)) (* 587 (- 255 b)) (* 114 (- 255 c))) k) (* 1000 255))))))

(defun rgb-of (kind a b c d)
  "The red, green and blue, each 0 to 255, of a colour of KIND (:grey, :rgb
or :cmyk) whose parts, each 0 to 255, are A to D, as three values."
  (declare (type (integer 0 255) a b c d))
  (ecase kind
    (:grey (values a a a))
    (:rgb (values a b c))
    (:cmyk (let ((k (- 255 d)))
             (values (round (* (- 255 a) k) 255) (round (* (- 255 b) k) 255) (round (* (- 255 c) k) 255))))))

(defun raw-grey (data width height bits kind table invert &optional colours)
  "The grey levels of a picture's samples DATA, WIDTH by HEIGHT, BITS to a
sample, its colours of KIND (:grey, :rgb, :cmyk, or :indexed with TABLE as
COLOUR-SPACE-OF gives it), each row starting on an octet; INVERT for a
Decode array reversing them. NIL when DATA is too short. COLOURS, when
given, an array of three octets a pixel, is filled with each pixel's red,
green and blue."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum width height bits))
  (let* ((parts (ecase kind (:grey 1) (:rgb 3) (:cmyk 4) (:indexed 1)))
         (row (ceiling (* width parts bits) 8))
         (grey (make-array (* width height) :element-type '(unsigned-byte 8)))
         (most (1- (ash 1 bits))))
    (declare (type fixnum parts row most))
    (when (< (length data) (* row height))
      (return-from raw-grey nil))
    (flet ((sample (y index)
             (declare (type fixnum y index))
             ;; the INDEX-th sample of row Y, as a level 0 to 255 (an index as it is)
             (let ((value (if (= bits 8)
                              (aref data (+ (* y row) index))
                              (let* ((bit (* index bits)) (octet (aref data (+ (* y row) (ash bit -3)))))
                                (if (= bits 16)
                                    (aref data (+ (* y row) (* index 2)))
                                    (ldb (byte bits (- 8 bits (mod bit 8))) octet))))))
               (declare (type fixnum value))
               (cond ((eq kind :indexed) value)
                     ((= bits 16) (if invert (- 255 value) value))
                     (t (let ((level (if (= bits 8) value (round (* value 255) most))))
                          (if invert (- 255 level) level)))))))
      (flet ((put (at kind a b c d)
               (setf (aref grey at) (grey-of kind a b c d))
               (when colours
                 (multiple-value-bind (r g bl) (rgb-of kind a b c d)
                   (setf (aref colours (* 3 at)) r (aref colours (+ (* 3 at) 1)) g (aref colours (+ (* 3 at) 2)) bl)))))
        (dotimes (y height grey)
          (dotimes (x width)
            (let ((at (+ (* y width) x)))
              (if (eq kind :indexed)
                  (destructuring-bind (base highest lookup) table
                    (let* ((n (ecase base (:grey 1) (:rgb 3) (:cmyk 4)))
                           (from (* n (min (sample y x) highest))))
                      (if (<= (+ from n) (length lookup))
                          (put at base (aref lookup from) (if (> n 1) (aref lookup (+ from 1)) 0)
                               (if (> n 2) (aref lookup (+ from 2)) 0) (if (> n 3) (aref lookup (+ from 3)) 0))
                          (put at :grey 255 0 0 0))))
                  (let ((k (* x parts)))
                    (put at kind (sample y k) (if (> parts 1) (sample y (+ k 1)) 0)
                         (if (> parts 2) (sample y (+ k 2)) 0) (if (> parts 3) (sample y (+ k 3)) 0)))))))))))

(defun image-grey (image)
  "The grey levels of IMAGE, a picture Tuatara decoded (DECODE-PICTURE)."
  (let* ((width (image-width image)) (height (image-height image)) (channels (image-channels image))
         (pixels (image-pixels image))
         (grey (make-array (* width height) :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) pixels grey) (type fixnum width height channels))
    (dotimes (i (* width height) grey)
      (let ((at (* i channels)))
        (setf (aref grey i) (grey-of :rgb (aref pixels at) (aref pixels (+ at 1)) (aref pixels (+ at 2)) 0))))))

(defparameter *picture-filter-names*
  '(("/JBIG2Decode" . "JBIG2") ("/CCITTFaxDecode" . "fax (CCITT)") ("/CCF" . "fax (CCITT)"))
  "The kinds of picture data whose pixels are not read here, by the name of
their filter, each with what it is called.")

(defun picture-grey (pdf revision number)
  "The grey levels of the picture object NUMBER of REVISION, as four values:
the levels, a row after another from the top, its width and its height,
and for a picture in colours its colours as (OCTETS . CHANNELS), each
pixel's red, green and blue first of its CHANNELS octets (NIL for one in
greys); or NIL and why not, in words. Raw samples are read here; a JPEG or
a JPEG 2000 is turned into pixels by Tuatara, in the sandbox
\(DECODE-PICTURE)."
  (let* ((object (revision-object pdf revision number))
         (dict (and object (pdf-indirect-value object)))
         (width (and dict (pdf-get dict "/Width"))) (height (and dict (pdf-get dict "/Height"))))
    (unless (and (integerp width) (integerp height) (plusp width) (plusp height))
      (return-from picture-grey (values nil "it gives no size")))
    (when (> (* width height) *palimpsest-scan-pixels*)
      (return-from picture-grey (values nil (format nil "it has ~:d pixels, more than *palimpsest-scan-pixels* allows" (* width height)))))
    ;; no more octets than the picture's samples take, four colours of 16 bits at most: a stream that
    ;; inflates further is not read on
    (multiple-value-bind (data why) (stream-data pdf object :limit (+ 1024 (* height (ceiling (* width 4 16) 8))))
      (cond ((null data) (values nil (or why "its data cannot be read")))
            ((member why '("/DCTDecode" "/DCT" "/JPXDecode") :test #'equal)
             ;; a JPEG 2000 kept in a PDF is a bare codestream or a JP2 file: either is read as :j2k
             (multiple-value-bind (image notes) (decode-picture data (if (equal why "/JPXDecode") :j2k :jpeg) width height)
               (if image
                   (values (image-grey image) (image-width image) (image-height image)
                           (and (>= (image-channels image) 3)
                                (not (eq (colour-space-of pdf revision (pdf-get dict "/ColorSpace")) :grey))
                                (cons (image-pixels image) (image-channels image))))
                   (values nil (string-trim ", " (or (car (first notes)) (format nil "the ~:[JPEG~;JPEG 2000~] could not be decoded" (equal why "/JPXDecode"))))))))
            (why (values nil (format nil "its pixels are kept as ~@[~a, ~]~a"
                                     (cdr (assoc why *picture-filter-names* :test #'equal)) why)))
            (t
             (let ((mask (eq (pdf-get dict "/ImageMask") t))
                   (bits (let ((b (pdf-get dict "/BitsPerComponent"))) (if (integerp b) b 1)))
                   (decode (deref pdf revision (pdf-get dict "/Decode"))))
               (multiple-value-bind (kind table) (if mask :grey (colour-space-of pdf revision (pdf-get dict "/ColorSpace")))
                 (if (and kind (member (if mask 1 bits) '(1 2 4 8 16)))
                     (let* ((colours (and (not mask) (not (eq kind :grey)) (not (and (eq kind :indexed) (eq (first table) :grey)))
                                          (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
                            (grey (raw-grey data width height (if mask 1 bits) kind table
                                            ;; a stencil paints where its samples are 0, unless its Decode says 1
                                            (let ((first-decode (and (consp decode) (first (pdf-array-items decode)))))
                                              (if mask (not (eql first-decode 1)) (eql first-decode 1)))
                                            colours)))
                       (if grey (values grey width height (and colours (cons colours 3))) (values nil "its data is shorter than its size")))
                     (values nil "its colours are none of grey, RGB, CMYK or a palette of them")))))))))

;;; Black and white, lines and words

(defun otsu-level (grey &optional width skipped)
  "The grey level that best parts GREY's pixels into dark and light: the one
that makes the two groups most unlike (Otsu's way); with SKIPPED, a bit
for each row of GREY, WIDTH wide, the pixels of the rows whose bit is 1
left out."
  (declare (type grey-pixels grey))
  (let ((counts (make-array 256 :element-type 'fixnum :initial-element 0)) (total 0))
    (declare (type fixnum total))
    (if skipped
        (dotimes (y (length skipped))
          (when (zerop (sbit skipped y))
            (loop for i from (* y width) below (* (1+ y) width) do (incf (aref counts (aref grey i))))
            (incf total width)))
        (progn (loop for g across grey do (incf (aref counts g)))
               (setf total (length grey))))
    (let ((sum (loop for i below 256 sum (* i (aref counts i)))) (below 0) (below-sum 0) (best 0) (level 128))
      (dotimes (i 256 level)
        (incf below (aref counts i))
        (incf below-sum (* i (aref counts i)))
        (let ((above (- total below)))
          (when (and (plusp below) (plusp above))
            (let* ((dark (/ below-sum below)) (light (/ (- sum below-sum) above))
                   (between (* below above (expt (- dark light) 2))))
              (when (> between best) (setf best between level i)))))))))

(defparameter *band-paper-share* 19/20
  "Where in a row's grey levels, from the darkest, its paper is read
\(PAPER-BANDS): the level this share of the row's pixels across the text
reach or stay under.")

(declaim (inline counted-level))
(defun counted-level (counts wanted)
  "The darkest grey level at or below which WANTED of the pixels COUNTS
counts lie, a vector of 256 whose Kth holds how many are of grey K; 255
when fewer are counted."
  (declare (type (simple-array fixnum (256)) counts) (type fixnum wanted))
  (let ((seen 0))
    (declare (type fixnum seen))
    (dotimes (g 256 255)
      (incf seen (aref counts g))
      (when (>= seen wanted) (return g)))))

(defun page-level (grey width height)
  "The grey level that parts the ink of the scanned picture GREY (WIDTH by
HEIGHT) from its paper: Otsu's (OTSU-LEVEL), taken again without the rows
whose paper (read as PAPER-BANDS reads it, *BAND-PAPER-SHARE*) falls below
it yet nearer to it than to the darkest fiftieth of the picture, where
there are any: rows of paper darker than the rest, as a band printed again
or a shaded row brings, which would otherwise draw the level up to their
paper and leave the rest of the page read as it would not be without
them; not those that reach the picture's top or foot, what lies about the
page. A row of text, however blurred, has paper between its letters, and
a row of a rule or a bar is ink. As two values: the level, and those rows,
a bit for each row of GREY (NIL for none). On the honest sets (699
pictures) such rows were found on 113, nearly all photographed with a
phone; the level moved on 47 of them, by 1 to 3 grey levels but on 7, by
up to 24; the words read on the 113, their paper and ink read without
those rows too (PICTURE-WORDS), 3,845 before and 3,820 after, 1,022 and
1,025 on those scanned."
  (declare (type grey-pixels grey) (type fixnum width height))
  (let* ((level (otsu-level grey))
         (counts (make-array 256 :element-type 'fixnum :initial-element 0))
         (core (progn (loop for g across grey do (incf (aref counts g)))
                      (counted-level counts (max 1 (floor (length grey) 50)))))
         (skipped (make-array height :element-type 'bit :initial-element 0)))
    (dotimes (y height)
      (fill counts 0)
      (loop for i from (* y width) below (* (1+ y) width) do (incf (aref counts (aref grey i))))
      (when (< (/ (+ core level) 2) (counted-level counts (ceiling (* *band-paper-share* width))) level)
        (setf (sbit skipped y) 1)))
    ;; rows of darker paper at the picture's top or foot are what lies about the page, a table or a scanner's
    ;; lid, and are read as they are
    (loop for y from 0 below height while (= 1 (sbit skipped y)) do (setf (sbit skipped y) 0))
    (loop for y from (1- height) downto 0 while (= 1 (sbit skipped y)) do (setf (sbit skipped y) 0))
    (if (and (find 1 skipped) (find 0 skipped))
        (values (otsu-level grey width skipped) skipped)
        (values level nil))))

(defstruct (word-box (:copier nil))
  "A word found in a scanned picture: X0 Y0 X1 Y1 its pixels (the last
excluded), from the top left; INK, the grey level of the core of its
strokes (MEASURE-WORD); EDGES,
how much of it is half-tone, between ink and paper, for each pixel of ink
(a scan's blur, which a crisp print lacks); PAPER and NOISE, the mean grey of
the paper inside its box and how far the paper strays from that;
BASELINE, the row its letters stand on (WORD-BASELINE); SLOPE, how far that
row falls across its width, for each pixel across (NIL for a word too short
to tell); OFFSET, how far, in pixels, it stands below (above, when less
than 0) its line, for a word UNLIKE-WORDS sets apart by where it sits;
TURN, how far, in degrees, it is turned clockwise against its line, for
one it sets apart so; both 0 for any other, and both what its report
says; CUT,
true when its line was cut from lines set so close they touched
\(SPLIT-LINES), its baseline and turn then no measure of where it sits;
LONE, true for a mark narrower than its line is tall, standing apart from
the words about it (a figure alone in a table's cell, an initial, a list's
number, a bullet), whose characters are set beside others for copies but
which is no word set beside its line (PICTURE-WORDS)."
  x0 y0 x1 y1 (ink 0) (edges 0) (paper 0) (noise 0) (baseline 0) slope (offset 0) (turn 0) cut lone)

(defun ink-runs (counts least gap)
  "The runs of COUNTS (a vector) holding at least LEAST, joined across GAP or
fewer below it, each (START . END)."
  (let ((runs '()) (start nil) (last nil))
    (dotimes (i (length counts))
      (when (>= (aref counts i) least)
        (cond ((and start (<= (- i last) (1+ gap))) (setf last i))
              (t (when start (push (cons start (1+ last)) runs))
                 (setf start i last i)))))
    (when start (push (cons start (1+ last)) runs))
    (nreverse runs)))

(defstruct (glyph (:copier nil))
  "A character of a scanned picture, a piece of letters joined by the
scanner's blur (SPLIT-JOINED), or such letters left whole where no cut is
told: X0 Y0 X1 Y1 its ink's tight box (the last excluded) in the picture's grey
levels as read (not made level, so that a character copied within the
picture is found where it was put); PIXELS, those grey levels from two
pixels above and left of the box to two below and right, WIDTH wide, a cut
of its own so that the picture need not be kept; INK, its pixels of ink;
WORD, the WORD-BOX it is in; PICTURE and PAGE, the picture object and the
page index it is on (SCAN-PICTURES); INKS and DARKER, what GLYPH-DISTANCE
sets it beside others by, worked out once (GLYPH-MEASURED); CLIPPED, the
grey level at or below which its page's scanner clipped its ink, the
darkest any character of its page reaches, or for one without grain of its
own two greys above its own darkest (-1 for none); BARE, true when it has
no grain of its own (GRAIN-LEVELS sets both); SPLIT, NIL for a mark as it
was found, :SURE for a piece cut from letters joined by the blur that is
letter-sized, :UNSURE for a piece or a mark still wider than letters are
\(SPLIT-JOINED); TYPICAL, how far the cores of its page's characters
typically stray (GRAIN-LEVELS); SPREAD, how far its own core strays
\(GLYPH-CORE-SPREAD, NIL for too small a core); ALIKE, how alike its picture's characters
of the same shape typically are, NIL when its picture's grain is too faint
for them to be set beside others (COPIED-GLYPHS); COLOUR, the colour of its
ink, (RED GREEN BLUE) each 0 to 255, on a picture in colours, else NIL
\(GLYPH-INK-COLOUR)."
  pixels width x0 y0 x1 y1 (ink 0) word picture page inks darker (clipped -1) bare split (typical 0) spread alike colour)

(defun glyph-ink-level (glyph h)
  "The grey level halfway from the lightest to the darkest of GLYPH's pixels
over its box and a pixel round it, H rows of it from the pixel above, and how
many of those pixels are darker, as a cons: what GLYPH-DISTANCE sets GLYPH
beside another by when it is the narrower."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)) (w (+ (- (glyph-x1 glyph) (glyph-x0 glyph)) 2))
         (lo 255) (hi 0) (darker 0))
    (declare (type grey-pixels pixels) (type fixnum across w lo hi darker))
    (dotimes (j h)
      (dotimes (i w)
        (let ((g (aref pixels (+ (* (+ 1 j) across) 1 i)))) (setf lo (min lo g) hi (max hi g)))))
    (let ((ink (+ lo (floor (- hi lo) 2))))
      (dotimes (j h) (dotimes (i w) (when (< (aref pixels (+ (* (+ 1 j) across) 1 i)) ink) (incf darker))))
      (cons ink darker))))

(defun glyph-measured (glyph)
  "GLYPH, with what GLYPH-DISTANCE sets it beside others by worked out once:
its GLYPH-INK-LEVEL over its box and a pixel round it, with and without the
last row (a character of its shape a pixel shorter or taller); and how many
of its pixels are darker than each sixteenth of the grey levels, so that
those darker than any level are told at once, a few too many at most."
  (let ((h (- (glyph-y1 glyph) (glyph-y0 glyph)))
        (darker (make-array 17 :element-type '(unsigned-byte 32) :initial-element 0)))
    (setf (glyph-inks glyph) (vector (glyph-ink-level glyph (+ h 1)) (glyph-ink-level glyph (+ h 2))))
    (loop for g across (the grey-pixels (glyph-pixels glyph)) do (incf (aref darker (1+ (ash g -4)))))
    (loop for i from 1 to 16 do (incf (aref darker i) (aref darker (1- i))))
    (setf (glyph-darker glyph) darker)
    glyph))

(defun glyph-cut (grey width x0 y0 x1 y1)
  "The grey levels of GREY, WIDTH wide, from two pixels above and left of
X0 Y0 to two below and right of X1 Y1 (the last excluded), as two values:
the cut, and its width."
  (declare (type grey-pixels grey) (type fixnum width x0 y0 x1 y1))
  (let* ((left (- x0 2)) (top (- y0 2)) (across (+ (- x1 x0) 4)) (down (+ (- y1 y0) 4))
         (cut (make-array (* across down) :element-type '(unsigned-byte 8))))
    (dotimes (j down (values cut across))
      (replace cut grey :start1 (* j across) :start2 (+ (* (+ top j) width) left) :end2 (+ (* (+ top j) width) left across)))))

(defun word-glyphs (original unruled level slope straight width height x0 top x1 bottom)
  "The characters (GLYPHs) of the word from X0 TOP below X1 BOTTOM of the
picture made level, STRAIGHT (WIDTH by HEIGHT), its lines having fallen
SLOPE for each pixel across in ORIGINAL: its columns of ink cut apart where
a column holds none, each one's rows found in UNRULED, ORIGINAL with its
rules taken out (WITHOUT-RULES), ink darker than LEVEL, so that a frame
round a stamp or a line under a word is no part of a character; its pixels
cut from ORIGINAL. Those too small to compare (a dot, a comma: under 6
rows tall, unless twice as wide as tall and 6 columns wide or more, as a
hyphen, a minus sign or a dash is) or too near the picture's edge for
paper round them are left out."
  (declare (type grey-pixels original unruled straight) (type fixnum level width height x0 top x1 bottom))
  (let ((columns (make-array (- x1 x0) :element-type 'fixnum :initial-element 0)) (out '()))
    (loop for y from top below bottom
          do (loop for x from x0 below x1
                   when (< (aref straight (+ (* y width) x)) level) do (incf (aref columns (- x x0)))))
    (dolist (run (ink-runs columns 1 0) out)
      (let* ((gx0 (+ x0 (car run))) (gx1 (+ x0 (cdr run)))
             (fall (list (* slope gx0) (* slope gx1)))
             (from (max 0 (- (floor (+ top (reduce #'min fall))) 2)))
             (to (min height (+ (ceiling (+ bottom (reduce #'max fall))) 2)))
             (y0 nil) (y1 nil) (ink 0))
        (declare (type fixnum ink))
        (loop for y from from below to
              do (loop for x from gx0 below gx1
                       when (< (aref unruled (+ (* y width) x)) level)
                         do (incf ink) (unless y0 (setf y0 y)) (setf y1 (1+ y))))
        (when (and y0 (>= (- gx1 gx0) 3) (>= ink 20)
                   (or (>= (- y1 y0) 6) (and (>= (- y1 y0) 2) (>= (- gx1 gx0) (max 6 (* 2 (- y1 y0))))))
                   (>= gx0 2) (<= (+ gx1 2) width) (>= y0 2) (<= (+ y1 2) height))
          (multiple-value-bind (cut across) (glyph-cut original width gx0 y0 gx1 y1)
            (push (glyph-measured (make-glyph :pixels cut :width across :x0 gx0 :y0 y0 :x1 gx1 :y1 y1 :ink ink)) out)))))))

(defun glyph-piece (glyph from to level split)
  "The piece of GLYPH between its box's columns FROM and TO (from its
box's left, TO excluded), as a GLYPH of its own marked SPLIT: its rows those
of GLYPH's box holding ink darker than LEVEL in those columns, its pixels
cut from GLYPH's own with two pixels round it; NIL when it is too small to
compare, as WORD-GLYPHS leaves such marks out."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (h (- (glyph-y1 glyph) (glyph-y0 glyph))) (top nil) (bottom nil) (ink 0))
    (declare (type grey-pixels pixels) (type fixnum across h ink))
    (dotimes (j h)
      (loop for i from from below to
            when (< (aref pixels (+ (* (+ 2 j) across) 2 i)) level)
              do (incf ink) (unless top (setf top j)) (setf bottom (1+ j))))
    (when (and top (>= (- to from) 3) (>= (- bottom top) 6) (>= ink 20))
      (multiple-value-bind (cut width) (glyph-cut pixels across (+ 2 from) (+ 2 top) (+ 2 to) (+ 2 bottom))
        (glyph-measured (make-glyph :pixels cut :width width :ink ink :split split :word (glyph-word glyph)
                                    :x0 (+ (glyph-x0 glyph) from) :y0 (+ (glyph-y0 glyph) top)
                                    :x1 (+ (glyph-x0 glyph) to) :y1 (+ (glyph-y0 glyph) bottom)))))))

(defun upper-height (glyphs)
  "How tall the taller characters of GLYPHS are, capitals, figures and the
letters that rise above the others (b, d, l): the height a quarter of them
reach or pass, however many commas, dots and bits of rules the page also
holds; or, when more, how far the lines of their words reach above their
baselines, the middle over the words, for a page whose letters are mostly
small ones, where fewer than a quarter rise, as text set in a book face
often is: nearly every line has a capital, a figure or a tall letter in
it, and the middle is told by many lines, not by the few a rule or a frame
makes taller. That reach is taken as far as eight fifths of the quarter's
height, no more: in the faces measured the letters that rise stand 1.3 to
1.5 times as tall as the small ones, and a page whose lines run together
with marks above them would reach further. NIL for no GLYPHS."
  (let ((heights (sort (mapcar (lambda (g) (- (glyph-y1 g) (glyph-y0 g))) glyphs) #'>))
        (rising (let ((seen (make-hash-table :test 'eq)))
                  (loop for g in glyphs
                        for word = (glyph-word g)
                        when (and word (not (word-box-cut word)) (not (gethash word seen)))
                          collect (progn (setf (gethash word seen) t)
                                         (- (word-box-baseline word) (word-box-y0 word)))))))
    (and heights
         (let ((quarter (nth (floor (length heights) 4) heights)))
           (max quarter (min (if rising (floor (median-of rising)) 0) (floor (* 8 quarter) 5)))))))

(defun typical-width (glyphs)
  "How wide a character of GLYPHS typically is: the middle width of those no
wider than they are tall, as nearly every letter is and letters joined by
the blur seldom are, so that a page set so tight that most of its letters
touch still gives the width of one; of them all when none is so; NIL for
no GLYPHS."
  (flet ((width (g) (- (glyph-x1 g) (glyph-x0 g))))
    (median-of (mapcar #'width (or (remove-if (lambda (g) (> (width g) (- (glyph-y1 g) (glyph-y0 g)))) glyphs) glyphs)))))

(defun split-joined (glyphs level deep)
  "GLYPHS, the characters of a scanned page, with marks of letters joined
by the scanner's blur cut into letter-sized pieces (GLYPH-SPLIT), each in
the place of the mark it was cut from. A scanner's blur fills the gap
between two letters set close with half-tones, so that their ink runs
together and no column of paper parts them; but where one letter ends and
the next begins, a column holds none of a stroke's core, only the
half-tones the blur spread there. A mark wider than half as wide again as
the page's typical character (TYPICAL-WIDTH) is cut at such a
neck: of its columns holding no pixel as dark as DEEP (halfway between the
darkest of the page's ink and LEVEL, the grey that parts ink from paper)
and ink in no more than a third of the mark's rows (letters touch over a
few rows, where their serifs or curves come near; the half-toned edge of a
stroke runs its whole height), the one holding the least ink (summed below
LEVEL), with half a typical
character's width (3 pixels at least) left on either side; each side is
cut again while it is still that wide. A letter's strokes are as dark as
DEEP where they are a pixel wide or more; at a small size the hairline of a
wide letter (the arch of an m) may not be, and the letter is cut there too,
into pieces each compared as any character is. A piece as wide as a letter
is :SURE; one still half as wide again as a typical character, where no
neck parts letters that touch with their strokes' cores (a kerned pair, a
ligature), is :UNSURE, and so is a mark that wide where no neck is found
at all. Half as wide again parts no letter from a pair: in the faces tried
\(DejaVu Serif, Sans, Condensed and Mono, Nimbus Sans Narrow, Liberation
Serif, bold too) a W, an M, an m, an @ or a % is 1.5 to 2.2 times the
typical width, and is cut only where it has a neck; two narrow letters run
together (il, ii, ij) are 0.8 to 1.4 times it, and are left whole and
compared as one mark, in which honest prints still differ. A piece told a
copy by its likeness alone
\(COPIED-GLYPHS) is worth a look at most, the same letters cut at the
same neck being nearer each other than prints of a letter are; pieces the
same pixel for pixel are told as any characters are (SAME-STRIPS),
wherever the cut falls. The cut column itself is in no piece's box, but in the paper about
each."
  (let ((typical (typical-width glyphs)))
    (if (null typical)
        glyphs
        (loop for g in glyphs
              for w = (- (glyph-x1 g) (glyph-x0 g))
              if (> w (* 3/2 typical))
                append (let* ((pixels (glyph-pixels g)) (across (glyph-width g))
                              (h (- (glyph-y1 g) (glyph-y0 g)))
                              (mass (make-array w :element-type 'fixnum :initial-element 0))
                              (inked (make-array w :element-type 'fixnum :initial-element 0))
                              (cored (make-array w :element-type 'bit :initial-element 0))
                              (side (max 3 (ceiling typical 2))) (pieces '()))
                         (declare (type grey-pixels pixels) (type fixnum across h))
                         (dotimes (j h)
                           (dotimes (i w)
                             (let ((p (aref pixels (+ (* (+ 2 j) across) 2 i))))
                               (when (< p level) (incf (aref mass i) (- level p)) (incf (aref inked i)))
                               (when (<= p deep) (setf (aref cored i) 1)))))
                         (labels ((cut (from to)
                                    ;; the neck of least ink, or the piece as it is
                                    (let ((best nil))
                                      (when (> (- to from) (* 3/2 typical))
                                        (loop for c from (+ from side) below (- to side)
                                              when (and (zerop (aref cored c)) (<= (* 3 (aref inked c)) h)
                                                        (or (null best) (< (aref mass c) (aref mass best))))
                                                do (setf best c)))
                                      (if best
                                          (progn (cut from best) (cut (1+ best) to))
                                          (push (cons from to) pieces)))))
                           (cut 0 w))
                         (if (cdr pieces)
                             (loop for (from . to) in (nreverse pieces)
                                   for piece = (glyph-piece g from to level (if (> (- to from) (* 3/2 typical)) :unsure :sure))
                                   when piece collect piece)
                             (progn (setf (glyph-split g) :unsure) (list g))))
              else collect g))))

(defun page-tilt (grey width height level)
  "How far the lines of the scanned picture GREY (WIDTH by HEIGHT, ink darker
than LEVEL) fall for each pixel across: the slope at which its ink, summed
along rows of that slope, is heaped most sharply into lines, tried from -3 to
3 degrees by quarters and then by fiftieths of a degree about the best; of
slopes alike the levellest, for rows heaped alike at slopes too slight to
move a line a whole row across the picture."
  (declare (type grey-pixels grey) (type fixnum width height level))
  (let* ((inked (loop for g across grey count (< g level)))
         (step (max 1 (floor inked 200000)))
         (n (floor inked step))
         ;; every STEP-th pixel of ink, its column and its row
         (xs (make-array n :element-type 'fixnum)) (ys (make-array n :element-type 'fixnum))
         (bins (make-array (+ height (* 2 width)) :element-type 'fixnum)))
    (declare (type fixnum inked step n))
    (let ((k 0) (i 0))
      (declare (type fixnum k i))
      (dotimes (y height)
        (dotimes (x width)
          (when (and (< (aref grey (+ (* y width) x)) level) (zerop (mod (incf k) step)) (< i n))
            (setf (aref xs i) x (aref ys i) y)
            (incf i)))))
    (flet ((sharpness (degrees)
             (let ((slope (tan (* (float degrees 1d0) (/ pi 180)))) (sum 0))
               (declare (type double-float slope) (type fixnum sum) (optimize speed))
               (fill bins 0)
               (dotimes (i n)
                 (let ((k (+ width (the fixnum (round (- (float (aref ys i) 1d0) (* slope (float (aref xs i) 1d0))))))))
                   (declare (type fixnum k))
                   (when (< -1 k (length bins)) (incf (aref bins k)))))
               (loop for c of-type fixnum across bins do (incf sum (* c c)))
               sum)))
      (let* ((coarse (loop with best = 0 and most = -1
                           for d from -3 to 3 by 1/4
                           do (let ((v (sharpness d))) (when (or (> v most) (and (= v most) (< (abs d) (abs best)))) (setf most v best d)))
                           finally (return best)))
             (fine (loop with best = coarse and most = -1
                         for d from (- coarse 1/4) to (+ coarse 1/4) by 1/50
                         do (let ((v (sharpness d))) (when (or (> v most) (and (= v most) (< (abs d) (abs best)))) (setf most v best d)))
                         finally (return best))))
        (tan (* fine (/ pi 180)))))))

(defun straightened (grey width height slope paper)
  "GREY (WIDTH by HEIGHT) with its lines falling SLOPE pixels for each pixel
across made level: each column moved up by its share of the fall, a fraction
of a row blended from the two rows about it, so that no step is made where
none was; where nothing was, PAPER."
  (declare (type grey-pixels grey) (type fixnum width height paper))
  (let ((out (make-array (* width height) :element-type '(unsigned-byte 8)))
        (slope (float slope 1d0)))
    (declare (type double-float slope))
    (dotimes (x width out)
      (let* ((shift (* slope x)) (whole (floor shift)) (part (- shift whole)) (rest (- 1d0 part)))
        (declare (type double-float shift part rest) (type fixnum whole))
        (flet ((at (row) (if (< -1 row height) (aref grey (+ (* row width) x)) paper)))
          (declare (inline at))
          (dotimes (y height)
            (let ((from (+ y whole)))
              (declare (type fixnum from))
              (setf (aref out (+ (* y width) x))
                    (round (+ (* rest (at from)) (* part (at (1+ from)))))))))))))

(defun page-columns (grey width height level)
  "The columns of text of the picture GREY (WIDTH by HEIGHT, its rules taken
out, ink darker than LEVEL), as runs (LEFT . RIGHT) of its columns of
pixels: the page parted at each gutter of paper running down it, a run of
columns holding ink in no more than a hundredth of the rows, a sixtieth of
the page wide and 12 pixels at least, between a sixth and five sixths of
the way across, with ink on either side; the whole page when there is
none."
  (let* ((counts (make-array width :element-type 'fixnum :initial-element 0))
         (least (max 12 (floor width 60))) (most (floor height 100)) (cuts '()))
    (dotimes (y height)
      (let ((at (* y width)))
        (dotimes (x width) (when (< (aref grey (+ at x)) level) (incf (aref counts x))))))
    (let ((total (reduce #'+ counts)) (start nil))
      (loop for x from 0 to width
            do (if (and (< x width) (<= (aref counts x) most))
                   (unless start (setf start x))
                   (when start
                     (when (and (>= (- x start) least) (> start (floor width 6)) (< x (floor (* 5 width) 6))
                                (> (* 5 (loop for k below start sum (aref counts k))) total)
                                (> (* 5 (loop for k from x below width sum (aref counts k))) total))
                       (push (floor (+ start x) 2) cuts))
                     (setf start nil)))))
    (loop for (from to) on (cons 0 (append (nreverse cuts) (list width))) while to
          collect (cons from to))))

(defun line-cores (counts top bottom)
  "The cores of small letters in the band of rows TOP to BOTTOM of COUNTS
\(each row's count of ink), as runs (START . END) of the band's own rows:
runs of rows holding a third of the ink of the band's inkier rows (a
quarter of its rows that hold ink having that much or more), each at least
two fifths as tall as the band's tall runs (a quarter of its runs being as
tall or taller), so that a mark running across several lines, such as a
signature, does not hide the cores of the others; and that third, the
least ink a core's rows hold."
  (let* ((inked (sort (loop for y from top below bottom when (plusp (aref counts y)) collect (aref counts y)) #'>))
         (core (max 1 (if inked (floor (nth (floor (length inked) 4) inked) 3) 1)))
         (zones (ink-runs (subseq counts top bottom) core 1))
         (heights (sort (mapcar (lambda (z) (- (cdr z) (car z))) zones) #'>))
         (tall (if heights (nth (floor (length heights) 4) heights) 0)))
    (values (remove-if (lambda (z) (< (* 5 (- (cdr z) (car z))) (* 2 tall))) zones) core)))

(defun split-lines (rows bands)
  "The lines of a picture, as BANDS (each (TOP . BOTTOM), the runs of ROWS,
each row's count of ink, that hold ink) cut where lines set close touch:
as three values, the lines, those of them made by cutting, and how many
bands were left unread. In every line
most ink lies in the band of its small letters, between the baseline and
the top of an x; above and below it only ascenders and descenders reach,
and where lines are set so close that those of one touch those of the
next, two lines run into one band. A band is cut only when it is clearly
taller than one line, more than half again as tall as the page's bands of
one line (those holding one core, LINE-CORES; or, where no band holds one,
the middle distance between cores), so that a row of figures, whose top
and bottom strokes are inkier than their middle, is never cut through.
Such a band is cut between two cores at the row of least ink, measured
against the band's own ink, where that row holds no more than half of the
least a core's rows hold: there only the ascenders and descenders of two
lines meet. Where the strokes of two lines overlap more, or a mark such as
a signature runs across them, the two are left together, and a band still
holding two cores and too tall to be a line is left unread on its own, the
rest of the page read."
  (let* ((cored (mapcar (lambda (band) (multiple-value-list (line-cores rows (car band) (cdr band)))) bands))
         (single (loop for band in bands for (zones) in cored
                       when (null (cdr zones)) collect (- (cdr band) (car band))))
         (typical (if (>= (length single) 3)
                      (median-of single)
                      (median-of (loop for band in bands for (zones) in cored
                                       append (loop for (a b) on zones while b collect (- (car b) (car a)))))))
         (out '()) (made '()) (left 0))
    (loop for band in bands for (zones core) in cored
          do (destructuring-bind (top . bottom) band
               (if (and typical (cdr zones) (> (* 2 (- bottom top)) (* 3 typical)))
                   (let ((from top) (cores 1) (pieces '()))
                     (flet ((piece (to)
                              (if (and (> cores 1) (> (* 2 (- to from)) (* 3 typical)))
                                  (incf left)
                                  (push (cons from to) pieces))))
                       (loop for (a b) on zones while b
                             do (let ((cut (loop with best = (cdr a)
                                                 for y from (cdr a) below (car b)
                                                 when (< (aref rows (+ top y)) (aref rows (+ top best))) do (setf best y)
                                                 finally (return best))))
                                  (if (<= (* 2 (aref rows (+ top cut))) core)
                                      (progn (piece (+ top cut)) (setf from (+ top cut 1) cores 1))
                                      (incf cores))))
                       (piece bottom))
                     (dolist (piece (nreverse pieces))
                       (push piece out)
                       (unless (equal piece band) (push piece made))))
                   (push band out))))
    (values (nreverse out) made left)))

(defun without-rules (grey width height level paper &key (slanted t))
  "GREY (WIDTH by HEIGHT) with its rules taken out, PAPER where they were, as
two values: the copy, and how many pixels of ink of rules it took out. A rule is a
run of ink (darker than LEVEL) straight across or down longer than a
letter can be, a twentieth of the picture's height and 80 pixels at least
\(a table's borders, a line to sign on, an underline), so that words are
not run together with them: one unbroken, or one a pixel wide whose grey
the scanner left lighter here and there, broken by gaps of 6 pixels or
less, ink along four fifths of it, that is thin: paper 3 pixels to both sides of it along half its
length, and to one side along nine tenths (figures set against a rule
touch it on one side; a row of letters is crossed by their strokes, and so
is never so); or one inked along nine tenths of it, as no row through
letters is, though figures set close in a cell touch it on both sides. A
rule down the page need not be thin, figures in narrow cells touching it
on both sides, but it must be inked along nine tenths of its length:
letters standing one over another in lines set close leave more paper
between them than that.
With SLANTED, the default, rules traced at a slant are taken out too
(WITHOUT-SLANTED-RULES). GREY is not changed."
  (declare (type grey-pixels grey) (type fixnum width height level))
  (let ((out (copy-seq grey)) (longest (max 80 (floor height 20))) (taken 0))
    (declare (type grey-pixels out) (type fixnum longest taken))
    (labels ((at (along across horizontal)
               (if horizontal (+ (* across width) along) (+ (* along width) across)))
             (ink-p (along across horizontal)
               (< (aref grey (at along across horizontal)) level))
             (thin-p (from to across horizontal span)
               ;; paper 3 pixels to both sides along half the run's ink, and to one side along nine tenths
               (let ((inked 0) (both 0) (either 0))
                 (declare (type fixnum inked both either))
                 (loop for k from from below to
                       when (ink-p k across horizontal)
                         do (incf inked)
                            (when (and (>= (- across 3) 0) (< (+ across 3) span))
                              (let ((before (not (ink-p k (- across 3) horizontal))) (after (not (ink-p k (+ across 3) horizontal))))
                                (when (and before after) (incf both))
                                (when (or before after) (incf either)))))
                 (and (>= (* 5 inked) (* 4 (- to from)))
                      ;; down the page, nine tenths: strokes of letters one over another in lines set close are not
                      (or horizontal (>= (* 10 inked) (* 9 (- to from))))
                      ;; down a page, a run that long, so little broken, is a rule even where figures touch it on both
                      ;; sides: the lines of text are parted by more than such gaps
                      (or (not horizontal) (>= (* 10 inked) (* 9 (- to from)))
                          (and (>= (* 2 both) inked) (>= (* 10 either) (* 9 inked)))))))
             (take (from to across horizontal)
               (loop for k from from below to
                     when (and (ink-p k across horizontal) (/= (aref out (at k across horizontal)) paper))
                       do (incf taken) (setf (aref out (at k across horizontal)) paper)))
             (sweep (length span horizontal)
               (dotimes (across span)
                 (let ((start nil) (bridged nil) (last nil))
                   (loop for k from 0 to length
                         do (if (and (< k length) (ink-p k across horizontal))
                                (progn (unless start (setf start k))
                                       (unless bridged (setf bridged k))
                                       (setf last k))
                                (progn
                                  (when start
                                    (when (> (- k start) longest) (take start k across horizontal))
                                    (setf start nil))
                                  ;; a run broken by gaps of 6 pixels or less goes on
                                  (when (and bridged (or (>= k length) (> (- k last) 6)))
                                    (when (and (> (- (1+ last) bridged) longest)
                                               (thin-p bridged (1+ last) across horizontal span))
                                      (take bridged (1+ last) across horizontal))
                                    (setf bridged nil)))))))))
      (sweep width height t)
      (sweep height width nil)
      (when slanted (incf taken (without-slanted-rules grey out width height level paper longest)))
      (values out taken))))

(defparameter *rule-breadth* 4
  "How many pixels across a rule may be where it is taken out at a slant
\(WITHOUT-SLANTED-RULES): ink broader than that there is a stroke crossing
or touching the rule, and is left whole.")

(defvar *scratch* nil
  "Where the pictures of a file being read keep the scratch arrays they
reuse from page to page (SCRATCH-PIXELS): a table bound while a file's
scanned pictures are read (SCAN-PICTURES), else NIL, each array made
afresh.")

(defun scratch-pixels (name size type)
  "An array of SIZE elements of TYPE or more for the use NAME, reused from
the last picture of the file being read when it is as large, else made
\(*SCRATCH*); its contents are what the last use left."
  (let ((kept (and *scratch* (gethash name *scratch*))))
    (if (and kept (>= (length kept) size))
        kept
        (let ((made (make-array size :element-type type)))
          (when *scratch* (setf (gethash name *scratch*) made))
          made))))

(defun without-slanted-rules (grey out width height level paper longest &optional changed)
  "OUT, GREY (WIDTH by HEIGHT) as WITHOUT-RULES has left it, with the rules
that slant taken out too, PAPER where they were; how many pixels of ink it
took out. A rule photographed at a slant or bowed runs along a row for a
while and then steps to the next: a path of ink (darker than LEVEL) across
the page, or down it, that goes on from each pixel to the next column (or
row) in the same row or one beside it, LONGEST pixels or more, every pixel
of it with paper 3 pixels to one side of it at least, and half of them
with paper 3 pixels to both sides, is a rule. A line of letters is crossed
by their strokes and parted by the paper between them, and is never so.
The rule is taken out across its breadth, but where a stroke crosses or
touches it (the ink straight across it there broader than *RULE-BREADTH*)
it is left, so that letters set on a rule stay whole."
  (declare (type grey-pixels grey out) (type fixnum width height level paper longest) (optimize speed))
  (let ((from (scratch-pixels :trace (* width height) '(unsigned-byte 2))) (taken 0))
    (declare (type (simple-array (unsigned-byte 2) (*)) from) (type fixnum taken))
    (flet ((trace-rules (length span along-step across-step)
             ;; ALONG the way a rule runs, ACROSS the way it steps; a pixel's place is ALONG times
             ;; ALONG-STEP and ACROSS times ACROSS-STEP
             (declare (type fixnum length span along-step across-step))
             (let ((before-length (make-array span :element-type 'fixnum :initial-element 0))
                   (before-thin (make-array span :element-type 'fixnum :initial-element 0))
                   (now-length (make-array span :element-type 'fixnum :initial-element 0))
                   (now-thin (make-array span :element-type 'fixnum :initial-element 0))
                   (reach (* 3 across-step)))
               (declare (type (simple-array fixnum (*)) before-length before-thin now-length now-thin) (type fixnum reach))
               (fill from 0 :end (* width height))
               (dotimes (along length)
                 (let ((base (* along along-step)))
                   (declare (type fixnum base))
                   (dotimes (across span)
                     (let ((i (+ base (* across across-step))))
                       (declare (type fixnum i))
                       (if (>= (aref grey i) level)
                           (setf (aref now-length across) 0 (aref now-thin across) 0)
                           (let ((above (or (< across 3) (>= (aref grey (- i reach)) level)))
                                 (below (or (>= (+ across 3) span) (>= (aref grey (+ i reach)) level))))
                             (if (not (or above below))
                                 (setf (aref now-length across) 0 (aref now-thin across) 0)
                                 ;; the longest path reaching it from the line before, the same row first
                                 (let ((best 0) (thin 0) (way 0))
                                   (declare (type fixnum best thin) (type (integer 0 3) way))
                                   (when (plusp along)
                                     (when (> (aref before-length across) best)
                                       (setf best (aref before-length across) thin (aref before-thin across) way 2))
                                     (when (and (plusp across) (> (aref before-length (1- across)) best))
                                       (setf best (aref before-length (1- across)) thin (aref before-thin (1- across)) way 1))
                                     (when (and (< (1+ across) span) (> (aref before-length (1+ across)) best))
                                       (setf best (aref before-length (1+ across)) thin (aref before-thin (1+ across)) way 3)))
                                   (let ((now (1+ best)) (now-thin-here (+ thin (if (and above below) 1 0))))
                                     (declare (type fixnum now now-thin-here))
                                     (setf (aref now-length across) now
                                           (aref now-thin across) now-thin-here
                                           (aref from i) way)
                                     ;; long enough and thin enough: the path back to its start is a rule
                                     (when (and (>= now longest) (>= (* 2 now-thin-here) now))
                                       (let ((a along) (c across))
                                         (declare (type fixnum a c))
                                         (loop
                                           (let ((k (+ (* a along-step) (* c across-step))))
                                             (declare (type fixnum k))
                                             ;; the rule's breadth here, the ink straight across it; where that is
                                             ;; more than a rule's, a stroke crosses or touches it, and is left whole
                                             (let ((first c) (last c))
                                               (declare (type fixnum first last))
                                               (loop while (and (plusp first) (< (- c first) *rule-breadth*)
                                                                (< (aref grey (- k (* (- c (1- first)) across-step))) level))
                                                     do (decf first))
                                               (loop while (and (< (1+ last) span) (< (- last c) *rule-breadth*)
                                                                (< (aref grey (+ k (* (- (1+ last) c) across-step))) level))
                                                     do (incf last))
                                               (when (< (- last first) *rule-breadth*)
                                                 (loop for e fixnum from first to last
                                                       for j fixnum = (+ k (* (- e c) across-step))
                                                       do (when (/= (aref out j) paper)
                                                            (when (< (aref grey j) level) (incf taken))
                                                            (when changed (vector-push-extend (+ (* j 256) (aref out j)) changed))
                                                            (setf (aref out j) paper)))))
                                             (let ((w (aref from k)))
                                               (when (zerop w) (return))
                                               (setf (aref from k) 0)
                                               (decf a) (incf c (- w 2)))))))))))))))
                 (rotatef before-length now-length)
                 (rotatef before-thin now-thin)))))
      (trace-rules width height 1 width)
      (trace-rules height width width 1))
    taken))

(defparameter *scan-words-most* 20000
  "The most words a scanned picture may hold to be read as a page of text,
marks standing alone counted with them: a page holds a few hundred, and a picture of more is no page of text but
something made to keep the examination busy.")

(defparameter *scan-characters-most* 40000
  "The most characters of a scanned picture set beside others for copies
\(COPIED-GLYPHS); a picture of more has none set beside them. A bound of
the work, not of judgement: a page of small print holds some 5,000
characters.")

(defparameter *scan-read-least* 1/2
  "The least part of the characters a scanned page is reckoned to hold
\(its ink over its characters' middle ink) that may be judged before the
report says how few were: the reckoning strays, and honest pages of small
print scanned at 200 dots an inch are read at three fifths of it, pages of
ordinary print at four fifths to all of it. A page on which a band of lines
run together was left unread is named whatever part was read.")

(defparameter *scan-characters-in-all* 300000
  "The most characters of all the scanned pictures of a file set beside
others for copies; the pictures past them have none set beside them. A
bound of the work, not of judgement: some sixty pages of small print.")

(defun picture-words (grey width height &optional level skipped)
  "The words of the scanned picture GREY, WIDTH by HEIGHT, measured (WORD-BOX),
as seven values: the words, the slope the page's lines fall by for each
pixel across (PAGE-TILT), the words' characters (WORD-GLYPHS) and those of
the marks standing alone, those joined by the blur cut apart
\(SPLIT-JOINED), how far its blank paper strays (BLANK-PAPER-NOISE), how many pixels of rules were taken out of it
\(WITHOUT-RULES), how many pixels of ink are left once they are, and how
many bands of rows were too tall to be lines and left unread. Lines are
found across the page and cut where they touch (SPLIT-LINES); where that
leaves bands unread, they are found in each column of the page apart
\(PAGE-COLUMNS) instead when that leaves fewer unread, as two columns of
text set at different leading do. The words are found and measured on the
picture made level (STRAIGHTENED), their rows those of the level picture: a
word's row in GREY is its row plus the tilt times its column. A mark
narrower than its line is tall, standing apart from the words about it (a
figure alone in a table's cell, an initial, a list's number), is LONE: its
characters are among the characters, set beside others for copies as any
are, but it is no word, neither among the words nor set beside its line.
Ink is darker than LEVEL, and the page's paper and darkest ink are read
from its rows but those SKIPPED (a bit for each row), as PAGE-LEVEL gives
both unless they are given, so that a band printed darker does not make
the rest of the page's words read as marks too faint to be letters. NIL
when it holds too little or too much ink to be a page of text, or more
than *SCAN-WORDS-MOST* words and marks standing alone."
  (declare (type grey-pixels grey) (type fixnum width height))
  (unless level
    (multiple-value-setq (level skipped) (page-level grey width height)))
  (let* ((level level) (ink (loop for g across grey count (< g level))))
    (declare (type fixnum level))
    (when (or (< ink (* 0.002 (length grey))) (> ink (* 0.4 (length grey))))
      (return-from picture-words nil))
    (let* ((counts (let ((h (make-array 256 :element-type 'fixnum :initial-element 0)))
                     (dotimes (y height h)
                       (unless (and skipped (= 1 (sbit skipped y)))
                         (loop for i from (* y width) below (* (1+ y) width) do (incf (aref h (aref grey i))))))))
           ;; the paper the middle of what is lighter than LEVEL, the ink the darkest fiftieth
           (paper (let ((half (floor (loop for g from level below 256 sum (aref counts g)) 2)) (seen 0))
                    (loop for g from level below 256 do (incf seen (aref counts g)) when (>= seen half) return g finally (return 255))))
           (core (let ((wanted (max 1 (floor (loop for g below level sum (aref counts g)) 50))) (seen 0))
                   (loop for g below 256 do (incf seen (aref counts g)) when (>= seen wanted) return g finally (return 0))))
           (tilt (page-tilt grey width height level))
           (original grey)
           ;; a character is found with only the straight rules about it taken out (a stamp's frame, an
           ;; underline): a rule traced at a slant runs along the letters it touches, and would reshape them
           (unruled (without-rules grey width height level paper :slanted nil))
           (slope (if (< (abs tilt) 0.0003) 0 tilt))
           (level-grey (if (zerop slope) grey (straightened grey width height tilt paper)))
           (rules 0)
           (grey (multiple-value-bind (cleaned taken) (without-rules level-grey width height level paper :slanted nil)
                   (setf rules taken) cleaned))
           ;; words are measured with the rules at a slant taken out as well, lines, words and characters
           ;; found as they are
           ;; the rules at a slant taken out of the same picture, each pixel's place and grey kept to put back
           (changed (make-array 256 :element-type 'fixnum :adjustable t :fill-pointer 0))
           (measured (progn (incf rules (without-slanted-rules level-grey grey width height level paper (max 80 (floor height 20)) changed))
                            grey))
           ;; the rules at a slant put back into the rows FROM to TO, or taken out again, where characters
           ;; are cut or the page's columns and blank paper read with the straight rules alone taken out
           (slanted (lambda (from to back)
                      (let ((low (* from width)) (high (* to width)))
                        (loop for entry across changed
                              for at = (floor entry 256)
                              when (and (<= low at) (< at high))
                                do (setf (aref grey at) (if back (mod entry 256) paper))))))
           (rows (make-array height :element-type 'fixnum :initial-element 0)) (words '()) (glyphs '()) (skipped 0) (cut-lines '()))
      (declare (type grey-pixels grey))
      (dotimes (y height)
        (job-check-stop)
        (let ((at (* y width)) (n 0))
          (declare (type fixnum at n))
          (dotimes (x width) (when (< (aref measured (+ at x)) level) (incf n)))
          (setf (aref rows y) n)))
      (multiple-value-bind (placed unread made)
          (flet ((lines-of (columns)
                   ;; each line as (LEFT RIGHT TOP . BOTTOM), how many bands were left unread, and the lines made by cutting
                   (let ((placed '()) (unread 0) (made '()))
                     (loop for (left . right) in columns
                           for counts = (if (and (zerop left) (= right width))
                                            rows
                                            (let ((c (make-array height :element-type 'fixnum :initial-element 0)))
                                              (dotimes (y height c)
                                                (loop for x from left below right
                                                      when (< (aref measured (+ (* y width) x)) level) do (incf (aref c y))))))
                           do (multiple-value-bind (lines cut left-unread)
                                  (split-lines counts (ink-runs counts (max 2 (floor (- right left) 500)) 1))
                                (setf made (append cut made))
                                (incf unread left-unread)
                                (dolist (line lines)
                                  (when (> (- (cdr line) (car line)) (floor height 8)) (incf unread))
                                  (push (list* left right line) placed))))
                     (values (nreverse placed) unread made))))
            (multiple-value-bind (placed unread made) (lines-of (list (cons 0 width)))
              (let ((columns (and (plusp unread)
                                  (progn (funcall slanted 0 height t)
                                         (prog1 (page-columns grey width height level) (funcall slanted 0 height nil))))))
                (if (cdr columns)
                    (multiple-value-bind (by-column unread-by-column made-by-column) (lines-of columns)
                      (if (< unread-by-column unread)
                          (values by-column unread-by-column made-by-column)
                          (values placed unread made)))
                    (values placed unread made)))))
        (setf skipped unread cut-lines made)
      (loop for (left right . line) in placed do
        (destructuring-bind (top . bottom) line
          (let ((tall (- bottom top)))
            (when (<= 6 tall (floor height 8))
              (flet ((runs-of (picture)
                       ;; the runs of ink of the band in PICTURE, letters, not a dash or an underline: ink in two
                       ;; fifths of the line's rows at least
                       (let ((columns (make-array width :element-type 'fixnum :initial-element 0)))
                         (loop for y from top below bottom
                               do (let ((at (* y width)))
                                    (loop for x from left below right
                                          when (< (aref picture (+ at x)) level) do (incf (aref columns x)))))
                         (remove-if-not (lambda (run)
                                          (>= (loop for y from top below bottom
                                                    count (loop for x from (car run) below (cdr run)
                                                                thereis (< (aref picture (+ (* y width) x)) level)))
                                              (* 0.4 tall)))
                                        (ink-runs columns 1 (max 2 (floor tall 3)))))))
                ;; the words, found and measured with the rules at a slant taken out
                (let ((here '()))
                  (dolist (run (runs-of measured))
                    (let ((word (measure-word measured width level (car run) top (cdr run) bottom paper core)))
                      (when (member line cut-lines :test #'eq) (setf (word-box-cut word) t))
                      (when (< (- (cdr run) (car run)) tall) (setf (word-box-lone word) t))
                      (push word words)
                      (push word here)))
                  ;; the characters, cut with only the straight rules taken out, each in the word it falls in most
                  (when here
                    (funcall slanted top bottom t)
                    (dolist (run (runs-of grey))
                      (let ((word (let ((best nil) (most -1))
                                    (dolist (w here best)
                                      (let ((over (- (min (cdr run) (word-box-x1 w)) (max (car run) (word-box-x0 w)))))
                                        (when (> over most) (setf best w most over)))))))
                        (dolist (glyph (word-glyphs original unruled level slope grey width height (car run) top (cdr run) bottom))
                          (setf (glyph-word glyph) word)
                          (push glyph glyphs))))
                    (funcall slanted top bottom nil)))))))))
      (funcall slanted 0 height t)
      ;; marks too faint to be letters (what is left at the edges of a rule taken out) are no words
      (let* ((marks (remove-if (lambda (w) (> (word-box-ink w) (/ (+ core level) 2))) (nreverse words)))
             (words (remove-if #'word-box-lone marks)))
        (when (> (length marks) *scan-words-most*)
          (return-from picture-words nil))
        (setf glyphs (split-joined (nreverse glyphs) level (floor (+ core level) 2)))
        (when (> (length glyphs) *scan-characters-most*)
          (setf glyphs '()))
        (values words tilt
                (let ((kept (make-hash-table :test 'eq)))
                  (dolist (word marks) (setf (gethash word kept) t))
                  (remove-if-not (lambda (g) (gethash (glyph-word g) kept)) glyphs))
                (blank-paper-noise grey width height rows level)
                rules
                (reduce #'+ rows)
                skipped)))))

(defun blank-paper-noise (grey width height rows level)
  "How far the blank paper of GREY (WIDTH by HEIGHT) strays from its mean,
the root of its mean squared difference from it: the paper of the rows
holding no ink, nor any row within 3 of them (ROWS counts each row's ink,
darker than LEVEL), between the lines and in the margins above and below
them, where no ink's blur reaches; NIL when no such row is left."
  (declare (type grey-pixels grey) (type fixnum width height level) (type (simple-array fixnum (*)) rows))
  (let ((n 0) (sum 0) (squares 0))
    (declare (type fixnum n sum) (type integer squares))
    (loop for y from 3 below (- height 3)
          do (when (loop for k from (- y 3) to (+ y 3) always (zerop (aref rows k)))
               (let ((at (* y width)))
                 (dotimes (x width)
                   (let ((g (aref grey (+ at x))))
                     (when (>= g level) (incf n) (incf sum g) (incf squares (* g g))))))))
    (and (plusp n) (float (sqrt (max 0 (- (/ squares n) (expt (/ sum n) 2))))))))

(defun word-baseline (grey width x0 y0 x1 y1 paper ink)
  "Where the letters in GREY (WIDTH wide) from X0 Y0 below X1 Y1 stand, in
rows from the top, to a fraction of a row: each row's ink counted as how far
each pixel is from the picture's PAPER grey to its INK's, a half-tone at the
edge of a stroke in part; the baseline where that falls below two fifths of
the fullest row's, between the last row above it and the first below, so
that the tails of g, p and y below do not count. NIL for no ink."
  (declare (type grey-pixels grey) (type fixnum width x0 y0 x1 y1 paper ink))
  (let ((rows (make-array (- y1 y0) :element-type 'double-float :initial-element 0d0))
        (span (float (max 1 (- paper ink)) 0d0))
        (paper (float paper 0d0)))
    (loop for y from y0 below y1
          do (let ((sum 0d0))
               (declare (type double-float sum))
               (loop for x from x0 below x1
                     do (incf sum (min 1d0 (max 0d0 (/ (- paper (aref grey (+ (* y width) x))) span)))))
               (setf (aref rows (- y y0)) sum)))
    (let* ((fullest (reduce #'max rows)) (least (* 0.4d0 fullest)))
      (when (plusp fullest)
        (loop for k from (1- (length rows)) downto 0
              when (>= (aref rows k) least)
                return (let* ((here (aref rows k))
                              (below (if (< (1+ k) (length rows)) (aref rows (1+ k)) 0d0))
                              ;; between this row's middle and the next's, where the ink falls past LEAST
                              (part (if (> here below) (/ (- here least) (- here below)) 0.5d0)))
                         (+ y0 k 0.5d0 (min 1d0 (max 0d0 part)))))))))

(defun word-slope (grey width level x0 y0 x1 y1 baseline)
  "How far the letters in GREY (WIDTH wide) from X0 Y0 below X1 Y1, standing
on BASELINE, fall for each pixel across: the line best through the foot of
each column of ink that stands on the baseline (within 3 pixels of it, so
that a word turned a little keeps its ends, and the tails of g, p and y
below do not count).
NIL for a word less than five times as wide as tall, or with too few such
columns to tell, across too little of it."
  (declare (type grey-pixels grey) (type fixnum width level x0 y0 x1 y1))
  (when (and baseline (>= (- x1 x0) (* 5 (- y1 y0))))
    (let ((points '()) (last-row (min (1- y1) (+ (floor baseline) 3))))
      (loop for x from x0 below x1
            do (loop for y from last-row downto y0
                     when (< (aref grey (+ (* y width) x)) level)
                       do (when (<= (abs (- (+ y 1) baseline)) 3) (push (cons x (+ y 1)) points))
                          (return)))
      (when (and (>= (length points) (* 0.3 (- x1 x0)))
                 (>= (- (reduce #'max points :key #'car) (reduce #'min points :key #'car)) (* 0.6 (- x1 x0))))
        (let* ((n (length points))
               (mx (/ (reduce #'+ points :key #'car) n)) (my (/ (reduce #'+ points :key #'cdr) n))
               (sxx (reduce #'+ points :key (lambda (p) (expt (- (car p) mx) 2))))
               (sxy (reduce #'+ points :key (lambda (p) (* (- (car p) mx) (- (cdr p) my))))))
          (and (plusp sxx) (float (/ sxy sxx))))))))

(defun wide-enough-p (word)
  "True when WORD is three times as wide as its line is tall, so that its
baseline is told well enough to set beside its line's: a word of a letter
or two (a slash, a dot) is not."
  (>= (- (word-box-x1 word) (word-box-x0 word)) (* 3 (- (word-box-y1 word) (word-box-y0 word)))))

(defun measure-word (grey width level x0 y0 x1 y1 paper-grey ink-grey)
  "The WORD-BOX of the pixels of GREY (WIDTH wide) from X0 Y0 below X1 Y1,
ink being darker than LEVEL, its baseline found between the picture's
PAPER-GREY and INK-GREY (WORD-BASELINE): its ink the mean of its darkest quarter (the
core of its strokes, alike in thin and bold type), its paper only what is
clearly paper (lighter than three quarters of the way from ink to paper)."
  (declare (type grey-pixels grey) (type fixnum width level x0 y0 x1 y1))
  (let ((counts (make-array 256 :element-type 'fixnum :initial-element 0))
        (ink 0) (light 0) (light-sum 0))
    (declare (type fixnum ink light light-sum))
    (loop for y from y0 below y1
          do (loop for x from x0 below x1
                   for g = (aref grey (+ (* y width) x))
                   do (incf (aref counts g))
                      (if (< g level)
                          (incf ink)
                          (progn (incf light) (incf light-sum g)))))
    (let* ((core (let ((wanted (max 1 (ceiling ink 4))) (taken 0) (sum 0))
                   (dotimes (g level (if (plusp taken) (/ sum taken) level))
                     (let ((n (min (aref counts g) (- wanted taken))))
                       (incf taken n) (incf sum (* n g))
                       (when (>= taken wanted) (return (/ sum taken)))))))
           (rough-paper (if (plusp light) (/ light-sum light) 255))
           (low (+ level (* 0.25 (- rough-paper level))))
           (high (- rough-paper (* 0.25 (- rough-paper level))))
           (half 0) (paper 0) (paper-sum 0) (paper-squares 0))
      (declare (type fixnum half paper paper-sum) (type integer paper-squares))
      (dotimes (g 256)
        (let ((n (aref counts g)))
          (cond ((< low g high) (incf half n))
                ((>= g high) (incf paper n) (incf paper-sum (* n g)) (incf paper-squares (* n g g))))))
      (let ((baseline (word-baseline grey width x0 y0 x1 y1 paper-grey ink-grey)))
       (make-word-box :x0 x0 :y0 y0 :x1 x1 :y1 y1
                     :baseline (or baseline y1)
                     :slope (word-slope grey width level x0 y0 x1 y1 baseline)
                     :ink (float core) :edges (if (plusp ink) (float (/ half ink)) 0.0)
                     :paper (if (plusp paper) (float (/ paper-sum paper)) 255.0)
                     :noise (if (plusp paper) (float (sqrt (max 0 (- (/ paper-squares paper) (expt (/ paper-sum paper) 2))))) 0.0))))))

;;; Words unlike the rest

(defun middle-in-place (vector count)
  "The middle of the first COUNT numbers of VECTOR, a simple vector: of an
even count, the upper of the two middle ones. They are set in order in
place only as far as the middle needs, and left in another order."
  (declare (type simple-vector vector) (type fixnum count))
  (let ((k (floor count 2)) (low 0) (high (1- count)))
    (declare (type fixnum k low high))
    (loop
      (when (>= low high) (return (svref vector k)))
      (let ((pivot (svref vector (ash (+ low high) -1))) (i low) (j high))
        (declare (type fixnum i j))
        (loop while (<= i j)
              do (loop while (< (svref vector i) pivot) do (incf i))
                 (loop while (> (svref vector j) pivot) do (decf j))
                 (when (<= i j)
                   (rotatef (svref vector i) (svref vector j))
                   (incf i) (decf j)))
        (cond ((<= k j) (setf high j))
              ((>= k i) (setf low i))
              (t (return (svref vector k))))))))

(defun median-of (values)
  "The middle of VALUES, a list of numbers: of an even count, the upper of
the two middle ones."
  (let ((count (length values)))
    (declare (type fixnum count))
    (cond ((zerop count) nil)
          ((<= count 4096)
           (let ((vector (make-array count)))
             (declare (dynamic-extent vector))
             (middle-in-place (replace vector values) count)))
          (t (middle-in-place (replace (make-array count) values) count)))))

(defparameter *scan-measures*
  '((:offset word-box-baseline 1.5 5 "a ~a baseline" "higher" "lower")
    (:turn word-box-slope 1.5 4 "turned ~a" "anticlockwise" "clockwise")
    (:ink word-box-ink 30 6 "~a ink" "darker" "lighter")
    (:edges word-box-edges 0.25 6 "~a edges" "sharper" "softer")
    (:noise word-box-noise 4 6 "~a paper" "cleaner" "noisier"))
  "What is measured of each word of a scan, and how it is judged
\(UNLIKE-WORDS): its key; what is measured (its baseline, set beside
its neighbours' to tell how far it sits off its line; its slope, set beside
its line's to tell how far it is turned; its ink, edges and paper); the
least difference from what its neighbours say that counts, in grey levels,
pixels or degrees; how many times as far from it as the page's words
typically are from theirs a word must be; and its words: how a word unlike
the rest is said to be, ~a standing for what a word with less, and with
more, of it is. :OFFSET and :TURN tell where a word sits, the others how it
looks (*SCAN-LOOKS*).

Measured on the honest sets (649 pictures: 231 of real documents scanned,
381 of real documents photographed with a phone, 37 made up), words named
at these values, and with the least or the multiple three quarters of it:
where it sits 356, 409, 426; turned 0, 0, 0 (7 with the least halved);
ink 95, 135, 97; edges 332, 448, 498; paper 220, 306, 277. Honest pages
have such words (a heading, a line printed in another pass, a form filled
in), which is why each is worth a look at most; the values sit where a
quarter less names up to half as many honest words again. Made up and
changed: a word moved 6 pixels on a bowed page and a column 3 pixels lower
are named still with the least or the multiple half again as large;
words pasted into the made-up scans are named so by their ink, half of
them by where they sit, none by their edges or paper.")

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
the page down, each a vector of its words from left to right."
  (let ((lines (make-hash-table)))
    (dolist (word words) (push word (gethash (word-box-y0 word) lines)))
    (sort (coerce (loop for line being the hash-values of lines
                        collect (sort (coerce line 'simple-vector) #'< :key #'word-centre))
                  'simple-vector)
          #'< :key (lambda (line) (word-box-y0 (svref line 0))))))

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
its line on one side of it, say it should be: for each mate and each of
the lines ABOUT it (LINE-POINTS each) that reaches both columns, the mate's
value with the difference that line makes from the mate's column to
WORD's, so that a line bowed, turned or seen at a slant as the lines about
it are sets no word apart; where none reaches both, the mate's value with
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

(defun nth-least (values count k)
  "The Kth least (from 0) of the first COUNT of VALUES, a vector of
double-floats, found by setting them in order in place as far as it needs:
VALUES's first COUNT are left in another order."
  (declare (type (simple-array double-float (*)) values) (type fixnum count k))
  (let ((low 0) (high (1- count)))
    (declare (type fixnum low high))
    (loop
      (when (>= low high) (return (aref values k)))
      (let ((pivot (aref values (ash (+ low high) -1))) (i low) (j high))
        (loop while (<= i j)
              do (loop while (< (aref values i) pivot) do (incf i))
                 (loop while (> (aref values j) pivot) do (decf j))
                 (when (<= i j)
                   (rotatef (aref values i) (aref values j))
                   (incf i) (decf j)))
        (cond ((<= k j) (setf high j))
              ((>= k i) (setf low i))
              (t (return (aref values k))))))))

(defparameter *scan-column-rows* 5
  "The fewest words one under another that make a column set apart
together (COLUMNS-UNLIKE).")

(defun middles-leaving-out (values)
  "For each of VALUES, a vector of numbers, the middle of the others as
MIDDLE-OF finds it (halfway between the two middle ones of an even count),
as a vector: the values sorted once, each one's left out by its place."
  (let* ((n (length values))
         (order (sort (let ((v (make-array n))) (dotimes (i n v) (setf (svref v i) i))) #'< :key (lambda (i) (elt values i))))
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

(defun middle-of (values)
  "The middle of VALUES, a list of numbers: of an even count, halfway
between the two middle ones."
  (let* ((sorted (sort (copy-list values) #'<)) (n (length sorted)))
    (if (oddp n)
        (nth (floor n 2) sorted)
        (/ (+ (nth (1- (floor n 2)) sorted) (nth (floor n 2) sorted)) 2))))

(defun page-trend (lines value usable)
  "How far, by VALUE (a function of a word), the lines of LINES (from
SCAN-LINES) rise or fall together across the page, as a function of a
column: a curve of the second degree fitted to the words of lines of five
USABLE words or more (lines of text), each line's own height left out, the
words further from the middle of their line than three times as far as
words typically are (and than half a pixel) left out but where nine lines
in ten or more hold such words, by least squares,
then fitted again without the words more than three times
as far from it as the words typically are (and more than half a pixel),
as words moved or pasted are; 0 everywhere where there are no such
lines. A page lying bowed in a scanner, or seen at a slant, bends all its
lines alike, and the lines of text show it."
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
                                                 append (let ((m (median-of (coerce ys 'list)))) (map 'list (lambda (y) (abs (- y m))) ys)))))
                       (bar (if typical (max 0.5d0 (* 3 typical)) 0d0))
                       ;; words far off their lines in nearly every line are the page's own shape, and kept
                       (bar (if (>= (count-if (lambda (text) (let ((m (median-of (coerce (cdr text) 'list))))
                                                                   (some (lambda (y) (> (abs (- y m)) bar)) (cdr text))))
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
  "The words of LINES (from SCAN-LINES) in columns set off their lines
together, each (WORD . HOW-FAR). Words one under another, *SCAN-COLUMN-ROWS*
or more, in lines of two USABLE words or more, make a column; the columns
and lines of such words are a table, of each word's VALUE (a function of a
word: where it sits, the page's tilt allowed for), the page's TREND at
its place (PAGE-TREND, found when not given) taken out. Each line's middle and each column's
middle are taken out of it in turn, twice, so that every column's own
offset is told apart from every line's, however many columns are
moved. Words one under another in a column, *SCAN-COLUMN-ROWS* or
more, set off the same way by more than LEAST and more than TIMES as far
as words of lines of text typically stand from the middle of theirs, alike within a
third of their middle, are set off together, columns next to each other
moved together each so; runs set off opposite ways in the same lines, as
moving some columns of a table draws the others the other way, are both
given, EITHER true, as one of them moved. Not when every column of the
table is so: the page itself is turned or curled. Those words are given,
each (WORD HOW-FAR EITHER)."
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
                                                       (find-if (lambda (o) (and (funcall usable o) (not (gethash o taken)) (over-p word o)))
                                                                (svref lines i)))
                                      when below do (push (cons i below) column))
                                (dolist (c column) (setf (gethash (cdr c) taken) t))
                                (when (>= (length column) *scan-column-rows*) (push (nreverse column) columns))))))
      (when (cdr columns)
        (let* ((columns (coerce (sort columns #'< :key (lambda (c) (word-centre (cdr (first c))))) 'simple-vector))
               (n (length columns))
               ;; the table: each column's words by line, their values, less the line's and the column's middles
               (cells (make-hash-table :test 'eq)) (row-of (make-hash-table)) (column-offset (make-array n :initial-element 0)))
          (loop for c across columns
                do (loop for (at . w) in c do (setf (gethash w cells) (funcall level w)) (push w (gethash at row-of))))
          (dotimes (pass 2)
            (loop for words being the hash-values of row-of
                  do (when (cdr words)
                       (let ((m (middle-of (mapcar (lambda (w) (gethash w cells)) words))))
                         (dolist (w words) (decf (gethash w cells) m)))))
            (loop for c across columns for k from 0
                  do (let ((m (middle-of (mapcar (lambda (cell) (gethash (cdr cell) cells)) c))))
                       (incf (aref column-offset k) m)
                       (dolist (cell c) (decf (gethash (cdr cell) cells) m)))))
          (let* ((spread (if texts (middle-of texts) 0))
                 (runs (make-array n :initial-element nil)))
            ;; in each column, the runs of words one under another set off the same way, by more than the least
            ;; and the multiple of the lines of text, and set off alike, within a third of their middle
            (dotimes (k n)
              (let* ((c (svref columns k))
                     (offsets (mapcar (lambda (cell) (+ (aref column-offset k) (gethash (cdr cell) cells))) c))
                     (run '()))
                (flet ((close-run ()
                         (when (>= (length run) *scan-column-rows*)
                           (let* ((ds (mapcar #'cdr run)) (middle (middle-of ds)))
                             (when (<= (middle-of (mapcar (lambda (d) (abs (- d middle))) ds)) (/ (abs middle) 3))
                               (push run (aref runs k)))))
                         (setf run '())))
                  (loop for cell in c for d in offsets
                        do (if (and (> (abs d) least) (> (abs d) (* times spread))
                                    (or (null run) (plusp (* d (cdr (first run))))))
                               (push (cons (cdr cell) d) run)
                               (progn (close-run)
                                      (when (and (> (abs d) least) (> (abs d) (* times spread))) (push (cons (cdr cell) d) run)))))
                  (close-run))))
            ;; every column of the table set off the same way: the page, not a column
            (unless (let ((signs (loop for k below n collect (and (aref runs k) (signum (cdr (first (first (aref runs k)))))))))
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
                                                                   (intersection rows (mapcar (lambda (p) (gethash (car p) line-of)) other))))
                                                            (aref runs j))))))
                      (loop for (word . d) in run do (push (list word d either) out))))))))))
      out)))

(defparameter *scan-region-lines* 6
  "The most lines a region of words set apart together may span
\(REGIONS-UNLIKE).")

(defun regions-unlike (lines measure bar &optional (aside (constantly nil)))
  "The blocks of words of LINES (from SCAN-LINES) set apart together by
MEASURE (a function of a word, how it looks), each a list of (WORD .
HOW-FAR), a word in one block at most: a
block of lines next to each other, *SCAN-REGION-LINES* at most, over the
same columns, whose words all stand apart the same way, by more than BAR,
both from the words over the block (in the three lines above it) and from
those under it (in the three lines below), each of those set beside the
words over or under it, the block itself left out; at the foot of the
page, or over or under a blank gap three lines tall, from the side that
has lines alone. A block at the top of the page, where headings and forms
stand apart honestly, is not judged so. Four words at least; a block of one line
three quarters of it at least, or the rest of the line no further from
the lines about it than half of BAR. Words ASIDE says (a function of a
word) are no words over or under a block for others, as the words of a band
of paper set apart are not (PAPER-BANDS). A page darkening
or lightening down its length stands apart from the lines on one side one
way and from those on the other the other way, and is not so."
  (let* ((out '()) (count (length lines)) (taken (make-hash-table :test 'eq)) (values (make-hash-table :test 'eq))
         (tops (map 'vector (lambda (line) (word-box-y0 (svref line 0))) lines))
         (pitch (if (> count 1) (median-of (loop for i from 1 below count collect (- (aref tops i) (aref tops (1- i))))) 0)))
    (labels ((value (word)
               ;; MEASURE of WORD, taken once
               (or (gethash word values) (setf (gethash word values) (funcall measure word))))
             (over-p (a b) (and (< (word-box-x0 b) (word-box-x1 a)) (> (word-box-x1 b) (word-box-x0 a))))
             (open-p (from to)
               ;; no lines of the page from FROM to TO: the top or the foot of the page, or a blank gap
               (or (< to 0) (>= from count)
                   (> (abs (- (aref tops (max 0 (min (1- count) to))) (aref tops (max 0 (min (1- count) from))))) (* 3 pitch))))
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
                     (middle-in-place found n)))))
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
                                                                     (and d (> (abs d) bar) (or (null sign) (= (signum d) sign))))))
                                                    (if (and at (good at))
                                                        (progn
                                                          (setf sign (signum (svref ways at)))
                                                          (let ((from at) (to at))
                                                            (loop while (and (plusp from) (good (1- from))) do (decf from))
                                                            (loop while (and (< (1+ to) (length line)) (good (1+ to))) do (incf to))
                                                            (loop for k from from to to do (push (cons (svref line k) (svref ways k)) block))))
                                                        (setf whole nil)))))
                                       (when (and whole (>= (length block) 4)
                                                  ;; a block of one line is the whole line, near enough, or the rest
                                                  ;; of the line is like the lines about it
                                                  (or (< top bottom) (>= (* 4 (length block)) (* 3 (length (svref lines top))))
                                                      (loop for o across (svref lines top)
                                                            always (or (assoc o block)
                                                                       (plain-p o top bottom (/ bar 2)))))
                                                  ;; a side to judge it by
                                                  (or (beside word (- top 3) (1- top)) (beside word (1+ bottom) (+ bottom 3))))
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
  "True when the lines of LINES (from SCAN-LINES) keep straight as a flat
page's do: for each line of five words or more, a line of text rather
than a table's row, a column of which may be moved, how far its words'
baselines stand from the middle of the others, the page's TILT and its
BEND allowed for (a function of a column, what all its lines rise or fall
by there, PAGE-TREND), the lower middle of them; the middle of those over
the page a third of a pixel at most; where FEWEST lines at least are
asked for, as to tell a bend all the lines share, over that many, and
three quarters of them half a pixel at most. Where FEWEST is one and
words typically stand a third of a pixel or less from the middle of their
lines, as on a scanner's glass, a word standing further from the middle
of its line than three times that (and than half a pixel), as a column
moved or pasted in a table's row does, is left out of its line, so that
what is changed does not hide itself, but where nine lines in ten or
more hold such words; a bend, a page photographed, or one whose every line
droops at its end, is told from all the words. A page lying in a scanner bowed bends all its lines
alike, and the bend taken out leaves them straight; a page photographed
curls and slants each of its lines its own way."
  (let* ((rows (loop for line across lines
                     for others = (remove-if #'word-box-cut line)
                     when (>= (length others) 5)
                       collect (map 'vector (lambda (o) (- (word-box-baseline o) (* tilt (word-centre o)) (funcall bend (word-centre o))))
                                    others)))
         ;; how far words typically stand from the middle of their lines; a word further than three times
         ;; that, as a column moved or pasted in a table's row is, is left out of its line
         (typical (median-of (loop for at in rows
                                   append (let ((m (median-of (coerce at 'list)))) (map 'list (lambda (v) (abs (- v m))) at)))))
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
                           collect (let ((ds (sort (map 'list (lambda (v m) (abs (- v m))) kept (middles-leaving-out kept)) #'<)))
                                     (nth (floor (1- (length ds)) 2) ds)))))
    (and straight (>= (length straight) fewest)
         (let ((sorted (sort straight #'<)))
           (and (<= (nth (floor (length sorted) 2) sorted) 1/3)
                (or (= fewest 1) (<= (nth (floor (* 3 (length sorted)) 4) sorted) 1/2)))))))

(defun light-trend (words)
  "How the light falls across the page of WORDS (WORD-BOXes), as a function
of a column and a row: the grey of their paper there, a smooth surface of
the second degree fitted to each word's paper at its middle by least
squares, three times, each time without the words more than three times
as far from the surface so far as the words typically are (and more than
a grey level and a half), the first time from the middle paper: words
pasted on paper of their own, or a band printed again, are left out. A lamp or a window lights a page
unevenly but smoothly; a scanner nearly evenly."
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
               (+ (aref c 0) (* (aref c 1) x) (* (aref c 2) y) (* (aref c 3) x x) (* (aref c 4) x y) (* (aref c 5) y y)))
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
            (let ((bar (max 1.5d0 (* 3 (nth-least (replace (make-array n :element-type 'double-float) offs) n (floor n 2)))))
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
                                                   (funcall light (word-centre w) (/ (+ (word-box-y0 w) (word-box-y1 w)) 2)))))
                              words))
           2)))

;;; Bands of paper set apart

(defstruct (paper-band (:copier nil) (:predicate nil))
  "A band of a scanned picture's rows whose paper stands apart from the
paper above and below it (PAPER-BANDS): TOP and BOTTOM its rows in the
picture made level (the last excluded), X0 and X1 the columns of the text
it crosses; PAPER the grey of its blank paper, ABOVE that of the blank
paper within a line above it and BELOW within a line below it; WORDS how
many of the page's words read it holds, LINES how many lines of text its
rows span."
  top bottom x0 x1 paper above below (words 0) (lines 0))

(defparameter *band-least* 6
  "The fewest grey levels the paper of a band of rows must stand apart from
the paper above and below it to be named (PAPER-BANDS). Measured on the
honest sets (699 pictures: 231 of real documents scanned, 381 photographed
with a phone, the rest made up at grain 1 to 3, bold, clipped, blurred,
kept as JPEG, lit unevenly): no band on a real document; on three made-up
forms, their shaded header row, about 20 levels darker than the paper
above and below it, as honest pages have, which is why a band is worth a
look at most. Measured on bands of lines printed again over 13 kinds of
made-up page (serif, sans, bold and DejaVu type, one to four lines, 14 to
28 lines a page, light falling across, JPEG of quality 75 and 90, 200 dots
an inch, kept without loss or as JPEG): every band from half its grey to
0.96 of it named once, a word pasted into it named on its own besides. The
floor depends on how much of the page the band is, as the rows about it
are its measure: on pages of 21 lines, bands of two lines named to 0.97
\(seeds 41 to 46); on pages of 12 lines, bands of two or three lines to
0.96, of four lines to 0.92 and missed from 0.94 (three seeds); at 0.97 a
band is missed on some pages at 200 dots an inch kept as JPEG of quality
90.")

(defun row-papers (grey width height slope x0 x1 top rows)
  "Three vectors of ROWS fixnums, for each row of the picture GREY (WIDTH by
HEIGHT) from TOP, followed along SLOPE (how far the lines fall for each
pixel across) over the columns X0 below X1: its paper, the middle of the
grey levels *BAND-PAPER-SHARE* of the pixels of each fifth of it reach or
stay under, so that a patch across less than half the row moves it not;
its ink, the darkest fiftieth of it; and its lightest hundredth."
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
          (setf (aref paper r) (median-of (coerce fifths 'list)) (aref dark r) (at 1/50) (aref light r) (at 99/100)))))))

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
  "The bands of rows of the scanned picture GREY (WIDTH by HEIGHT, its lines
falling TILT for each pixel across) whose paper stands apart from the
paper above and below them, as PAPER-BANDs, from WORDS, its words
\(WORD-BOXes), which tell where its text is: each row's paper read across
the columns the text spans, followed along the page's tilt (ROW-PAPERS),
and set beside the middle of the rows of its kind about it, as far as
seven lines either way (ROWS-AWAY); a run of rows standing apart the same
way, from one line of text tall to *SCAN-REGION-LINES* lines, more than
four times as far as the page's rows typically stray and by *BAND-LEAST*
grey levels at least, its own paper read in its rows of blank paper,
between its lines or about them, which a hatching or a bar across a line
has none of. The blank paper within a line above it, and that within a
line below it, each stand the other way from it by *BAND-LEAST* at least:
a step or a slope of the light, or a tinted panel closed by a rule, is on
one side only. Its edges sharp, a quarter of a line's height at most of
the blank rows within a line of each edge standing between a quarter and
three quarters of the way towards it, as no shadow, curl of the paper or
fall of the light leaves them; ink in it, as dark beside its paper as half
the page's ink is beside the page's, so that a box of one grey is not one
and thin light type scanned at 200 dots an inch is; and, on darker paper,
no marks lighter than halfway back to the paper about it, as type set in
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
                                                                    (<= (+ top r) (* 1/2 (+ (word-box-y0 w) (word-box-y1 w)))
                                                                        (+ top end)))
                                                                  text)
                                                          :lines (max 1 (round (- end r) pitch)))
                                         out))))
                             (setf r end)))))))
        (nreverse out)))))

(defun words-unlike (lines tilt kept &optional flat light (bend (constantly 0)) (aside (constantly nil)))
  "One reading of the words of LINES (from SCAN-LINES), the page's lines
falling TILT for each pixel across, by each measure of *SCAN-MEASURES*, as
UNLIKE-WORDS gives them, the words KEPT says (a function of a word) alone
set beside others; on a FLAT page the line's shape is not taken from the
lines about it, but from the page's TILT and its BEND (a function of a
column, what all its lines rise or fall by there, PAGE-TREND); blocks of
lines set apart together (REGIONS-UNLIKE) only where the paper's light is
even, LIGHT then how it falls across the page (LIGHT-TREND), else NIL:
their ink set beside the paper's light there, a block set apart by its
ink alone named only where its paper (PAPER-APART-P), edges or grain
stand apart too; words ASIDE says (a function of a word) set beside no
block of others (REGIONS-UNLIKE)."
  (let* ((out '()) (inked '())
         ;; a value of a word as it would be under the page's middle light
         (middle (and light (median-of (loop for line across lines append (map 'list #'word-box-paper line)))))
         (factors (make-hash-table :test 'eq))
         (lit (lambda (w v)
                (* v (or (gethash w factors)
                         (setf (gethash w factors)
                               (let ((here (funcall light (word-centre w) (/ (+ (word-box-y0 w) (word-box-y1 w)) 2))))
                                 (if (plusp here) (/ middle here) 1d0)))))))
         (widths (loop for line across lines append (loop for w across line collect (- (word-box-x1 w) (word-box-x0 w)))))
         ;; scratch vectors for a line's words and values, as long as the longest line
         (longest (reduce #'max lines :key #'length :initial-value 0))
         (others (make-array longest)) (order (make-array longest :element-type 'fixnum))
         (places (make-array longest :element-type 'fixnum))
         (values (make-array longest :element-type 'double-float)) (sorted (make-array longest :element-type 'double-float))
         (middles (make-array longest :element-type 'double-float)) (scratch (make-array longest :element-type 'double-float))
         ;; how far each word stands from its neighbours on each side, and from the rest of its line
         (words-count (loop for line across lines sum (length line)))
         (sides (make-array (* 2 words-count) :element-type 'double-float))
         (fars (make-array words-count :element-type 'double-float))
        ;; the tables each measure keeps its words' distances in, made once
        (near-table (make-hash-table :test 'eq)) (two-sided-table (make-hash-table :test 'eq)) (far-table (make-hash-table :test 'eq))
        (line-near-table (make-hash-table :test 'eq)) (line-far-table (make-hash-table :test 'eq)))
    (let ((widest (* *scan-widest-word* (median-of widths))))
      (loop for (key measure least times nil fewer more) in *scan-measures*
            do (job-check-stop)
               (let* ((sits (member key '(:offset :turn)))
                      (value (if sits (lambda (w) (- (word-box-baseline w) (funcall bend (word-centre w)))) measure))
                      (usable (lambda (w) (and (funcall kept w) (<= (- (word-box-x1 w) (word-box-x0 w)) widest)
                                               (not (and sits (word-box-cut w))))))
                      (judged (case key
                                (:offset (lambda (w) (and (<= (- (word-box-x1 w) (word-box-x0 w)) widest) (not (word-box-cut w)) (wide-enough-p w))))
                                (:turn (lambda (w) (and (<= (- (word-box-x1 w) (word-box-x0 w)) widest) (not (word-box-cut w)) (word-box-slope w))))
                                (t (lambda (w) (<= (- (word-box-x1 w) (word-box-x0 w)) widest)))))
                      (fallback (if sits tilt 0))
                      ;; where a word sits is set beside its line's shape; how it looks, beside its neighbours alone
                      ;; on a page lying flat, where a word sits is set beside a line of the page's tilt
                      (points (if (and sits (not flat))
                                  (map 'simple-vector (lambda (line) (line-points line value usable)) lines)
                                  (make-array (length lines) :initial-element #())))
                      (near (clrhash near-table)) (two-sided (clrhash two-sided-table))
                      (far (clrhash far-table)) (line-near (clrhash line-near-table))
                      (line-far (clrhash line-far-table)) (straightness (make-array (length lines) :initial-element nil))
                      (n-sides 0) (n-fars 0) (between '()))
                 (flet ((straight (w)
                          ;; what places it on a line of the page's tilt, or how it looks
                          (if sits (- (funcall value w) (* tilt (word-centre w))) (funcall measure w))))
                   ;; each word's distance from its neighbours, and from the rest of its line, once
                   (loop for line across lines for at from 0
                         for about = (lines-about points at)
                         for counted = (count-if usable line)
                         ;; the line's words set beside others, their values, sorted once, and where each
                         ;; word's stands among them, in the scratch vectors
                         for n = (let ((n 0))
                                   (declare (type fixnum n))
                                   (loop for w across line
                                         when (funcall usable w)
                                           do (setf (svref others n) w (aref values n) (float (straight w) 1d0))
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
                                   n)
                         do (let ((nears '()))
                              (loop for word across line
                                    when (and (funcall judged word)
                                              ;; the left one of a line of two is what the right one is set beside
                                              (not (and (= counted 2) (not (eq key :turn)) (eq word (find-if usable line)))))
                                      do (multiple-value-bind (left right)
                                             (if (eq key :turn)
                                                 (let ((turn (turn-residual word about tilt))) (values turn turn))
                                                 (line-residuals word line value usable about fallback))
                                           (let ((both (both-sides left right)))
                                             (when both
                                               (setf (gethash word near) both
                                                     (gethash word two-sided) (and left right t))
                                               (when left (setf (aref sides n-sides) (abs (float left 1d0))) (incf n-sides))
                                               (when (and right (not (eq key :turn))) (setf (aref sides n-sides) (abs (float right 1d0))) (incf n-sides))
                                               (when (and left right) (push (abs both) nears) (push (funcall measure word) between)))))
                                         ;; beside the rest of its line, two words of it at least besides it (three
                                         ;; for where it sits), and how far the rest stand from one another, it left out
                                         (when (and (>= n 2) (not (eq key :turn)) (>= (- n (if (find word others :end n) 1 0)) 2))
                                           (let ((at (position word others :end n)))
                                             (setf (gethash word far)
                                                   (- (straight word) (if at (aref middles at) (middle-skipping sorted nil nil n)))
                                                   ;; how far each other word stands from the rest, both left out, the
                                                   ;; lower middle of them
                                                   (gethash word line-far)
                                                   (let ((m 0) (skip (and at (aref places at))))
                                                     (declare (type fixnum m))
                                                     (dotimes (j n)
                                                       (unless (eql j at)
                                                         (setf (aref scratch m)
                                                               (abs (- (aref values j) (middle-skipping sorted skip (aref places j) n))))
                                                         (incf m)))
                                                     (and (> m 1) (nth-least scratch m (floor (1- m) 2))))))))
                              ;; how far the words of the page stand from the rest of their lines, and how
                              ;; straight and even each line keeps, the lower middle of its words' distances
                              (when (and (not (eq key :turn)) (>= n 3))
                                (dotimes (i n)
                                  (let ((d (abs (- (aref values i) (aref middles i)))))
                                    (setf (aref fars n-fars) d) (incf n-fars)
                                    (setf (aref scratch i) d)))
                                (setf (svref straightness at) (nth-least scratch n (floor (1- n) 2))))
                              (let ((jitter (if (>= (length nears) 3) (median-of nears) 0)))
                                (loop for word across line do (setf (gethash word line-near) jitter)))))
                   (let ((spread (max (if (plusp n-sides) (nth-least sides n-sides (floor n-sides 2)) 0) (/ least 6)))
                         (far-spread (max (if (plusp n-fars) (nth-least fars n-fars (floor n-fars 2)) 0) (/ least 6)))
                         (usual (and between (median-of between))))
                    (flet ((record (word off &optional block)
                             (when (eq key :offset) (setf (word-box-offset word) off))
                             (when (eq key :turn) (setf (word-box-turn word) off))
                             (let ((entry (or (assoc word out) (car (push (list word) out)))))
                               (unless (assoc key (cdr entry))
                                 (push (if sits
                                           (list key off 0 (if (minusp off) fewer more) block)
                                           (let ((v (funcall measure word))) (list key v (- v off) (if (minusp off) fewer more) block)))
                                       (cdr entry))))))
                     (loop for line across lines for at from 0
                           do (loop for word across line
                                    for own = (gethash word near)
                                    for off = nil
                                    do (when own
                                         (let ((bar (gethash word line-near)))
                                           (setf off own)
                                           ;; at the end of its line: beside the words over and under it; the right
                                           ;; one of a line of two on a flat page, beside the left one alone
                                           (unless (or (gethash word two-sided)
                                                       (and flat (= (count-if (lambda (w) (not (and sits (word-box-cut w)))) line) 2)))
                                             (let ((column (column-mates word lines at near)))
                                               (if (>= (length column) 2)
                                                   (let ((middle (median-of (mapcar (lambda (o) (gethash o near)) column))))
                                                     (setf off (- own middle))
                                                     (when (>= (length column) 3)
                                                       (setf bar (max bar (median-of (mapcar (lambda (o) (abs (- (gethash o near) middle))) column)))))
                                                     ;; how it looks, unlike the words over and under it, or the page's
                                                     ;; words between neighbours, the same way
                                                     (unless sits
                                                       (let ((v (funcall measure word)))
                                                         (unless (some (lambda (apart) (and apart (plusp (* apart off)) (> (abs apart) (* times (max spread bar)))))
                                                                       (list (- v (median-of (mapcar measure column))) (and usual (- v usual))))
                                                           (setf off nil)))))
                                                   (setf off nil))))
                                           (unless (and off (> (abs off) (* times (max spread bar))) (> (abs off) least))
                                             (setf off nil))))
                                       ;; beside the rest of its line, as far as it and the lines about it keep
                                       ;; straight and even, two of those at least
                                       (unless off
                                         (let* ((d (gethash word far)) (line (gethash word line-far))
                                                (kept (loop for i from (max 0 (- at *scan-lines-about*))
                                                              to (min (1- (length lines)) (+ at *scan-lines-about*))
                                                            unless (= i at) when (svref straightness i) collect it))
                                                (bar (and d line (cdr kept) (* times (max far-spread line (median-of kept))))))
                                           (when (and bar (> (abs d) bar) (> (abs d) least)) (setf off d))))
                                       ;; the two words of a line of two on a flat page, set beside each other: one
                                       ;; of them moved, which cannot be told
                                       (when off
                                         (let ((partner (and flat (not (eq key :turn)) own (eql off own) (not (gethash word two-sided))
                                                             (let ((pair (remove-if (lambda (w) (and sits (word-box-cut w))) line)))
                                                               (and (= (length pair) 2) (find word pair) (find-if-not (lambda (w) (eq w word)) pair))))))
                                           (if partner
                                               (progn (record word off :pair) (record partner (- off) :pair))
                                               (record word off))))))
                     ;; how it looks: blocks of lines set apart together, their ink set beside the paper's light;
                     ;; a block of ink alone is kept until the others are known
                     (when (and light (not sits))
                       (let ((blocks (if (eq key :ink)
                                         (regions-unlike lines (lambda (w) (funcall lit w (word-box-ink w)))
                                                         (max (/ least 2) (* times 1/2 far-spread)) aside)
                                         (regions-unlike lines measure (max (/ least 2) (* times far-spread)) aside))))
                         (dolist (block blocks)
                           (if (eq key :ink)
                               (push block inked)
                               (loop for (word . d) in block unless (assoc word out) do (record word d block)))))))))))
      ;; blocks set apart by their ink, named only where their paper, edges or grain stand apart too
      (destructuring-bind (key measure least times words fewer more) (assoc :ink *scan-measures*)
        (declare (ignore least times words))
        (loop for block in (nreverse inked)
              when (or (some (lambda (pair)
                               (some (lambda (way) (and (member (first way) '(:edges :noise)) (consp (fifth way))))
                                     (cdr (assoc (car pair) out))))
                             block)
                       (paper-apart-p block lines (lambda (w) (funcall lit w (word-box-paper w)))))
                do (loop for (word . d) in block
                         unless (assoc word out)
                           do (let ((v (funcall measure word)))
                                (push (list word (list key v (- v d) (if (minusp d) fewer more) block)) out))))))
    (dolist (entry out) (setf (cdr entry) (nreverse (cdr entry))))
    (nreverse out)))

(defparameter *scan-region-paper* 2
  "How many grey levels the paper of a block of lines set apart by its ink
must stand apart from the paper of the lines about it, the same way, for
the block to be named (PAPER-APART-P). On the honest sets (649 pictures)
blocks of ink named at 1 grey level: 2 more than at 2; at 3 or 4: as many
as at 2, and every band of the made-up series named still.")

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
                                     when (and (< (word-box-x0 o) x1) (> (word-box-x1 o) x0)) collect (funcall paper o))))
         (way (median-of (mapcar #'cdr block))))
    (and about
         (let ((d (- (median-of (mapcar (lambda (pair) (funcall paper (car pair))) block)) (median-of about))))
           (and (>= (abs d) *scan-region-paper*) (plusp (* d way)))))))

(defun unlike-words (words &optional bands)
  "Those of WORDS (WORD-BOXes) that stand apart from the words beside them,
each (WORD (KEY OWN USUAL MORE-OR-LESS-WORDS)...), by each measure of
*SCAN-MEASURES*, read twice: the second time the words the first found are
no one's neighbours, so that two or three words pasted side by side or in
a column do not hide each other. Then, on a page that lies flat
\(FLAT-PAGE-P), columns set off their lines together are added
\(WITH-COLUMNS).

A word is set beside its neighbours along its line, *SCAN-NEIGHBOURS* on
each side: where it sits, with the shape of its line between them taken
from the lines about it (SIDE-RESIDUAL), so that lines bowed, turned or
seen at a slant set no word apart, but on a page that lies flat
\(FLAT-PAGE-P) along the page's tilt and the bow all its lines share
\(PAGE-TREND), so that words moved together in the lines about it do not
hide it; how it looks, beside its neighbours'
alone, so that light and focus, which change little from word to word, set
none apart either. A word between neighbours on both sides stands apart
only by a step away from both the same way (BOTH-SIDES). A word at the end
of its line, the right one of a line of two, is set beside its neighbour
as the words at the end of the lines above and below are beside theirs,
two of them at least (COLUMN-MATES): it stands apart by how far that
differs from theirs, and by how it looks only when it is also unlike those
words themselves, or the page's words between neighbours, the same way;
with none, it is not judged so. On a page that lies flat (FLAT-PAGE-P),
the right word of a line of two, a label and its value, is set beside the
left one alone, along the page's tilt. It must stand further off than the least
the measure counts, and further than its multiple of how far the page's
words typically stand from what one side of their neighbours says, of how
far the words of its line between neighbours stand from theirs (three at
least), and of how far the words over and under it differ from each other
\(three at least), whichever is most.

A word is also set beside the rest of its line as a whole, two other words
of it at least: where it sits, beside the line of the page's tilt through
them; how it looks, beside the middle of them. It stands apart so by its
multiple of the most of: how far the page's words typically stand from
the rest of their lines; how far the other words of its line stand from
one another, the lower middle of them; and how straight and even the lines
about it keep, *SCAN-LINES-ABOUT* either side, the middle of them, two of
those measured at least, else it is not set beside its line so. A line
that bows or darkens along itself, or among lines that do, keeps its words
from being set apart so.

Words that look unlike the words over and under them (COLUMN-MATES, four
at least) by half the least a word alone needs and by the multiple of how
far the page's words stand from the rest of their lines, the same way,
four of them at least in lines next to each other and over or under one
another, with words not so over and under them, are a region set apart
together, as a picture editor's smoothing or a patch leaves one, and named,
where the paper's light is even once its smooth fall across the page is
taken out (LIGHT-TREND, EVEN-LIGHT-P), as a scanner's is (REGIONS-UNLIKE);
the words of BANDS, the page's bands of paper set apart (PAPER-BANDS), are
over or under no block of others, so that the lines next to a band are not
set apart the other way by it.
Their ink is set beside the light where they stand; a block set apart by
its ink alone is named only where its paper, edges or grain stand apart
too (PAPER-APART-P), and each block is one finding.

Its baseline is judged only for a word three times as wide as its line is
tall (WIDE-ENOUGH-P), its turn for one whose slope is told; neither for a
word of a line cut from lines that touch, nor is that word a neighbour for
them, its baseline being where the cut fell. A word more than
*SCAN-WIDEST-WORD* times as wide as the page's words typically are is not
judged, nor set beside others. Words of a line less than half as tall as
the page's lines typically are (dots, a rule's remains, the paper's edge)
are not judged, nor set beside others. OWN is how far the word's baseline
is from its line's, in pixels, or how far it is turned, in degrees, its
USUAL 0; for the others, its own measure, and USUAL what it is set beside.
NIL for a page of fewer than 12 words."
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
                                                              (notany (lambda (way) (eq (fifth way) :pair)) (cdr entry))))
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
            (setf (cdr entry) (append (cdr entry) (list (list key d 0 (if (minusp d) fewer more) (and either :pair)))))))))))

(defun picture-page-box (mark x0 y0 x1 y1 width height)
  "The box (X0 Y0 X1 Y1) on the page of the pixels from X0 Y0 below X1 Y1 of
the picture MARK draws, WIDTH by HEIGHT pixels: the picture fills its box on
the page, its first row at the top."
  (let ((across (- (mark-x1 mark) (mark-x0 mark))) (down (- (mark-y1 mark) (mark-y0 mark))))
    (list (+ (mark-x0 mark) (* (/ x0 width) across))
          (- (mark-y1 mark) (* (/ y1 height) down))
          (+ (mark-x0 mark) (* (/ x1 width) across))
          (- (mark-y1 mark) (* (/ y0 height) down)))))

(defun word-page-box (mark word width height tilt)
  "The box (X0 Y0 X1 Y1) on the page of WORD, found in the picture MARK draws
\(WIDTH by HEIGHT pixels, its lines falling TILT for each pixel across, the
word's rows those of the picture made level, PICTURE-PAGE-BOX)."
  (let ((fall (* (or tilt 0) (word-box-x0 word))))
    (picture-page-box mark (word-box-x0 word) (+ (word-box-y0 word) fall) (word-box-x1 word) (+ (word-box-y1 word) fall)
                      width height)))

(defstruct (scan-picture (:copier nil) (:predicate nil))
  "A scanned picture of a page, read: PAGE the page and MARK the picture as
it draws it; WIDTH and HEIGHT its pixels; TILT how far its lines fall for
each pixel across; ODD its words unlike the rest (UNLIKE-WORDS), each (WORD
\(KEY OWN USUAL WORDS)...); GLYPHS its characters (WORD-GLYPHS); PAPER how
far its blank paper strays (BLANK-PAPER-NOISE); RULES how many pixels of
rules were taken out of it (WITHOUT-RULES) and INK how many of ink were
left; JOINED how many bands of rows too tall to be lines were left unread
\(PICTURE-WORDS); CHARACTERS about how many
characters its ink holds, its ink once the rules are taken out over its
characters' middle ink (NIL when none is found); UNREAD true when no
words could be read from it at all; JUDGED true when its grain was judged
\(SCANS-READ): its characters differ as prints do, and it was not drawn by
a program; TWIN, the SCAN-PICTURE read before it whose picture is kept in
the file the same octet for octet and drawn alike (SCAN-PICTURES), its
characters left to that one's; DRAWN true when it was drawn by a program,
not scanned (SCANS-READ); BANDS its bands of rows whose paper stands apart
from the paper above and below them (PAPER-BANDS)."
  page mark width height tilt odd glyphs paper (rules 0) (ink 0) (joined 0) characters unread judged twin drawn bands)

(defun bands-lifted (grey width height tilt bands)
  "A copy of the scanned picture GREY (WIDTH by HEIGHT, its lines falling
TILT for each pixel across) with the rows of each of BANDS (PAPER-BANDS)
whose paper is darker than the paper about it lifted to it, each grey
times the paper about it over its own, so that a band printed far darker
than the page, whose paper lies under the level its ink is told by, is
read at the page's level as the rest is. NIL when none is darker."
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
  "The scanned picture MARK draws on PAGE of REVISION, read (PICTURE-WORDS)
as a SCAN-PICTURE, its characters each told its picture and page, its
words read again on the picture with its bands of darker paper lifted to
the page's paper (BANDS-LIFTED), so that their words, read near or under
the page's level of ink as they are, are read as the rest and set beside
one another and the rest; UNREAD,
with no characters, when no words could be read from it; NIL when it
cannot be read, and why not, in words."
  (multiple-value-bind (grey width height colours) (picture-grey pdf revision (mark-object mark))
    (if (null grey)
        ;; the second value says why it was not read
        (values nil width)
        (multiple-value-bind (level skipped) (page-level grey width height)
          (multiple-value-bind (words tilt glyphs paper rules ink joined)
              (picture-words grey width height level skipped)
            (if words
                (let* ((middle (median-of (mapcar #'glyph-ink glyphs)))
                       (bands (paper-bands grey width height tilt words))
                       ;; the words of a band of darker paper, read near or under the page's level of ink as
                       ;; they are, read with its paper lifted to the page's, and the page's words with them
                       (words (or (let ((lifted (bands-lifted grey width height tilt bands)))
                                    (and lifted
                                         (multiple-value-bind (level skipped) (page-level lifted width height)
                                           (picture-words lifted width height level skipped))))
                                  words)))
                  (dolist (band bands)
                    (setf (paper-band-words band)
                          (count-if (lambda (w) (<= (paper-band-top band) (* 1/2 (+ (word-box-y0 w) (word-box-y1 w)))
                                                    (paper-band-bottom band)))
                                    (remove-if #'word-box-lone words))))
                  (dolist (g glyphs)
                    (setf (glyph-picture g) (mark-object mark) (glyph-page g) (page-index page))
                    (when colours
                      (setf (glyph-colour g) (ink-colour grey width level colours g))))
                  (make-scan-picture :page page :mark mark :width width :height height :tilt tilt
                                     :odd (unlike-words words bands) :glyphs glyphs :paper paper :rules rules
                                     :bands bands
                                     :ink ink :joined joined
                                     :characters (and middle (plusp middle) (round ink middle))))
                ;; a picture of a page from which no words could be read: said so, not passed over
                (make-scan-picture :page page :mark mark :width width :height height :unread t
                                   :rules (or rules 0) :ink (or ink 0) :joined (or joined 0))))))))

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

(defun resolved-value (pdf revision value &optional (depth 6))
  "VALUE with every reference within it followed, DEPTH deep at most: an
object with a stream given as (:STREAM NUMBER-FOR-ITS-OCTETS DICTIONARY),
as PICTURE-OCTETS numbers them, so that a palette or a mask kept as a
stream is told by its octets."
  (cond ((minusp depth) :deep)
        ((pdf-ref-p value)
         (let ((object (revision-object pdf revision (second value))))
           (cond ((null object) nil)
                 ((pdf-indirect-stream-start object)
                  (list :stream (nth-value 1 (picture-octets pdf revision (second value) nil))
                        (resolved-value pdf revision (pdf-indirect-value object) (1- depth))))
                 (t (resolved-value pdf revision (pdf-indirect-value object) (1- depth))))))
        ((consp value)
         (cons (resolved-value pdf revision (car value) depth) (resolved-value pdf revision (cdr value) depth)))
        (t value)))

(defun picture-octets (pdf revision number &optional (drawn t))
  "Where the stream of the picture object NUMBER of REVISION lies in the
file, undecoded, as (START . END), and a key for what it draws, the same
for pictures that draw the same: the 64-bit FNV-1a hash and the count of
its octets, and with DRAWN the entries of its dictionary that say how they
are drawn (*PICTURE-DRAWN-BY*), every reference followed (RESOLVED-VALUE),
so that the same octets with another palette, mask or decoding are told
apart; NIL for none."
  (let ((object (revision-object pdf revision number)))
    (when (and object (pdf-indirect-stream-start object))
      (let ((octets (pdf-octets pdf)) (start (pdf-indirect-stream-start object)) (end (pdf-indirect-stream-end object))
            (hash #xcbf29ce484222325))
        (declare (type (unsigned-byte 64) hash) (type fixnum start end))
        (loop for i from start below end
              do (setf hash (ldb (byte 64 0) (* (logxor hash (aref octets i)) #x100000001b3))))
        (values (cons start end)
                (list* hash (- end start)
                       (and drawn
                            (let ((dict (pdf-indirect-value object)))
                              (loop for key in *picture-drawn-by*
                                    collect (cons key (resolved-value pdf revision (pdf-get dict key))))))))))))

(defun scan-pictures (pdf revision)
  "The scanned pictures of the pages of REVISION (SCAN-PICTURE-P), each read
once however many pages draw it, as SCAN-PICTUREs (SCAN-PICTURE-READ); a
picture kept twice in the file, its stream the same octet for octet as one
read already and drawn alike (its size, colours, palette, decoding and
masks the same, PICTURE-OCTETS), is read with its characters left out, its TWIN that one: a
document showing one picture twice would make every character of it a
copy; one that cannot be read left out, and given as a second value, each
\(PAGE MARK WHY), WHY in words; the characters of those past
*SCAN-CHARACTERS-IN-ALL* not kept."
  (let ((out '()) (not-read '()) (characters 0) (kept (make-hash-table :test 'equal)) (octets (pdf-octets pdf))
        (*scratch* (make-hash-table)))
    (loop for (m page) in (pictures-of pdf revision :keep #'scan-picture-p)
          for (place key) = (multiple-value-list (picture-octets pdf revision (mark-object m)))
          do (job-check-stop)
             (let ((twin (and place (cdr (find-if (lambda (seen) (not (mismatch octets octets :start1 (car (car seen)) :end1 (cdr (car seen))
                                                                                       :start2 (car place) :end2 (cdr place))))
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
           (* 0.3 (- (third (page-media page)) (first (page-media page))) (- (fourth (page-media page)) (second (page-media page)))))))

(defun unlike-words-words (ways)
  "How a word unlike the rest is, from its WAYS ((KEY OWN USUAL WORDS)...), in
words: darker ink, a lower baseline."
  (remove-duplicates (mapcar (lambda (w) (format nil (fifth (assoc (first w) *scan-measures*)) (fourth w))) ways)
                     :test #'string=))

(defparameter *scan-looks* '(:ink :edges :noise)
  "The measures of *SCAN-MEASURES* that tell how a word looks; the others
tell where it sits.")

(defun looks-and-sits-unlike-p (ways &optional paper)
  "True when the WAYS ((KEY OWN USUAL WORDS)...) a word is unlike the rest
hold both a way it looks, by edges sharper than its line's or by paper
straying half as far as the page's blank PAPER or less (BLANK-PAPER-NOISE),
and a way it sits: two measures apart from each other agree, and the
finding says so, still worth a look at most. Its ink alone
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
  "True when GLYPH is no character but a piece of a rule or a frame: taller
by half than TALLEST, the height of the page's taller characters
\(UPPER-HEIGHT); five times as tall as it is wide or more; taller by a
tenth than TALLEST and three times as tall as it is wide or more, as no l,
I or 1 is; or three times as
wide as it is tall or more and wider by half than TYPICAL, how wide the
page's characters typically are (TYPICAL-WIDTH): a bar standing or lying,
or a rule's remains. A hyphen, a minus sign or a dash is narrower, and a
character.

Measured on the faces of the made-up scans (Liberation Serif and Sans,
regular and bold, DejaVu Sans): the tallest characters, j, brackets and
the bar |, reach 1.35 to 1.4 times the height of the capitals, under half
again; the narrowest stand 2.5 to 4.6 times as tall as wide before the
scanner's blur widens them, l at 2.7 to 3.3, the bar alone past 4. An l
stands no taller than the letters that rise above the others, which
UPPER-HEIGHT measures however few they are. So the bar, and j and
brackets in a narrow sans face (3.3 to 3.7 times as tall as wide), taller
by a tenth and narrow, are taken as pieces of rules too, and are not set
beside others for copies: a limit. A piece of a rule 4.2
times as tall as wide and 1.42 times the type, as a photographed form
gave, is told; one cut to the height of the type, or 2.65 times as tall
as wide, is not, and is set beside the characters as one."
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
  "How unlike the characters A and B (GLYPHs) are in their ink: the mean of
the squared differences of their grey levels over the pixels inked in
either (darker than halfway from A's lightest to its darkest), the
narrower set along the wider, a pixel higher and lower too (their tops
alike within a pixel, as one character's are), for the closest (a character
copied with a sliver of its neighbour is found in its copy). The paper
about them is left out: a copy laid with a Darken blend keeps its ink, not
its paper. So is ink the scanner clipped in both, as dark as or darker
than each one's GLYPH-CLIPPED: it is one grey in every print, and would
make any two prints alike. NIL when no way of setting them together comes
under LIMIT; and as a second value true, but NIL (with NIL for the
first) when some way of setting them together leaves fewer than 20
pixels once the clipped ink is set aside (as few as a character's least
ink, WORD-GLYPHS): the two cannot be told apart, and are not judged; and
as a third value how many pixels the closest way counted."
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
                                                            (let ((d (- ga gb))) (declare (type fixnum d)) (incf sum (* d d))))))))))
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
character: as tall within a pixel, as wide within three, and of as much ink
within an eighth or so (a copy keeps its ink, give or take a few pixels),
the *GLYPH-PEERS-MOST* nearest it in ink, on its page or two pages either
side (a character is copied from near at hand; and so a long scan takes
time as its pages do, not as their pairs). Of those as near in ink, those
of the greater width come first, then the taller, then the later page, and
of one size and page those first in GLYPHS. They are given in a vector with
a fill pointer, the same vector each time, its contents those of the last
call: a long scan's search makes no garbage for each character. Not for two
threads at once."
  (let* ((span (+ 4 (reduce #'max glyphs :key (lambda (g) (max (- (glyph-x1 g) (glyph-x0 g)) (- (glyph-y1 g) (glyph-y0 g))))
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
                          (when (< h (length inks)) (let ((e (- (aref inks h) ink))) (when (or (null d) (< e d)) (setf d e))))
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

(defun glyph-place-hash (glyph)
  "A number for where GLYPH is, the same however many characters are about
it: the 64-bit FNV-1a hash of its picture, page and box's top left corner,
cut to a fixnum."
  (let ((hash #xcbf29ce484222325))
    (declare (type (unsigned-byte 64) hash))
    (dolist (n (list (or (glyph-picture glyph) 0) (or (glyph-page glyph) 0) (glyph-x0 glyph) (glyph-y0 glyph)))
      (dotimes (k 4)
        (setf hash (ldb (byte 64 0) (* (logxor hash (ldb (byte 8 (* 8 k)) n)) #x100000001b3)))))
    (ldb (byte 62 0) hash)))

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
\(LIKENESS-SAMPLE), to the nearest of its PEERS (GLYPH-PEERS) it may be set BESIDE (a
function of the character and a peer), each counted when under 30 grey
levels, the middle. Measured on characters as they were found, no
pieces of rules (RULE-PIECE-P), which come alike as no prints do, no
hyphens or dashes under 6 rows tall, which come near one another by
chance, and no wider than half as wide again as a typical character
\(TYPICAL-WIDTH):
letters the blur joined, and pieces cut from them (SPLIT-JOINED), have no
print as alike as two prints of a letter are, for the paper and the
neighbour's ink cut with them. On a page set so tight that fewer than a
quarter of its letter-sized marks stand alone, those left alone are the
letters by a space or a stop and not the page's prints; there it is
measured on every letter-sized mark, pieces too, and the lower quarter of
them is taken, below which the further pieces seldom reach. NIL for fewer
than 40 such characters, too few to tell. As three values more: how many
characters were set beside others, how many of them had one within 30 grey
levels, and how alike those were, taken as the first value is, NIL for
none."
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
         (hash #xcbf29ce484222325))
    (declare (type grey-pixels pixels) (type fixnum across w h) (type (unsigned-byte 64) hash) (optimize speed))
    (flet ((mix (octet)
             (declare (type (unsigned-byte 8) octet))
             (setf hash (ldb (byte 64 0) (* (logxor hash octet) #x100000001b3)))))
      (declare (inline mix))
      (mix (ldb (byte 8 0) w)) (mix (ldb (byte 8 8) w)) (mix (ldb (byte 8 0) h)) (mix (ldb (byte 8 8) h))
      ;; within its cut (GLYPH-CUT) the box begins two pixels in
      (dotimes (j h)
        (let ((row (+ (* (+ 2 j) across) 2)))
          (declare (type fixnum row))
          (dotimes (i w) (mix (aref pixels (+ row i)))))))
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
  "How far the grey levels of the core of GLYPH's ink stray, as two values:
the middle of their distances from their middle (the median absolute
deviation) of the core's pixels, those no lighter than halfway from the
darkest to the lightest of its box, and the darkest grey of its box; NIL
for a core of fewer than 8 pixels. A scanner's grain makes the core of
every print stray by some grey levels; a program that draws text fills its
strokes with one grey, and so does a scanner that clips ink, a stamp or a
coloured ink, to a grey or two. COUNTS, 256 fixnums, a buffer to count
the core's greys in, for a caller measuring many characters."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph)))
         (lo 255) (hi 0) (n 0))
    (declare (type grey-pixels pixels) (type (simple-array fixnum (256)) counts) (type fixnum across w h lo hi n))
    (fill counts 0)
    (dotimes (j h) (dotimes (i w) (let ((g (aref pixels (+ (* (+ 2 j) across) 2 i)))) (setf lo (min lo g) hi (max hi g)))))
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

(defun grain-levels (glyphs)
  "GLYPHS, each with its GLYPH-BARE, GLYPH-CLIPPED and GLYPH-TYPICAL set from
its page's characters. TYPICAL is the middle of how far their cores stray. Without grain of its own (BARE) is a character whose core
strays (GLYPH-CORE-SPREAD) by a grey level or less, or by a fifth of what
its page's characters typically stray (the middle of them) or less: on the
practice scans honest characters stray by 5 or more where the typical is
17 to 19. CLIPPED is the grey at or below which its ink is set aside when
it is set beside another (GLYPH-DISTANCE): for one without grain, its own
darkest grey and two above it (a scanner's clipping jitters by a grey or
two); for one that reaches the darkest grey its page's characters reach,
that grey; else -1, none."
  (let ((pages (make-hash-table :test 'equal)) (spread (make-hash-table :test 'eq)) (darkest (make-hash-table :test 'eq))
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
it has fewer than a hundred characters with a core to measure, or more
than one in twenty of them without grain. Of 720 honest pages of made-up
scans none had fewer than 216 characters measured; 676 had 2 % or fewer of
them without grain, 28 from 2 to 5 %, and 16 more than a twentieth (small
type against a table's rules, bold type the scanner clipped, prints with no
blur), on which words without grain cannot be told from the page's own.
Pages with words typed onto them hold 0.5 to 1.5 % without grain."
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
  "The strip of GLYPH's cut (GLYPH-CUT) from its column AT, K columns wide,
as three values: its first row and the row after its last, the rows of
those columns that hold a pixel darker than LEVEL, and how many of its
pixels are so dark but not as dark as GLYPH-CLIPPED (ink the scanner
clipped, the same grey in every print); NIL when those columns hold no
such pixel, or ink that goes on past the cut's first or last row (another
line's, which the cut does not hold whole)."
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
  (let ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)) (hash #xcbf29ce484222325))
    (declare (type grey-pixels pixels) (type fixnum across at k top bottom) (type (unsigned-byte 64) hash) (optimize speed))
    (flet ((mix (octet)
             (declare (type (unsigned-byte 8) octet))
             (setf hash (ldb (byte 64 0) (* (logxor hash octet) #x100000001b3)))))
      (declare (inline mix))
      (mix (ldb (byte 8 0) k)) (mix (ldb (byte 8 8) k)) (mix (ldb (byte 8 0) (- bottom top))) (mix (ldb (byte 8 8) (- bottom top)))
      (loop for j from top below bottom
            do (let ((row (+ (* j across) at)))
                 (declare (type fixnum row))
                 (dotimes (i k) (mix (aref pixels (+ row i)))))))
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
a strip of pixels the same grey level for grey level, as two values: the
pairs, each (A B BLOCK GRID), BLOCK the widest block of them found the
same, (WIDTH HEIGHT INK), INK its pixels of ink the scanner did not clip,
and GRID true when the two lie on the same grid of 8 by 8 pixels of
pictures kept as JPEGs (DCT, a list of picture objects) and the block is
less than two characters wide; and how many strips were not set beside
others, past the most a file's characters may give (a strip's place packed
in a number) or of a character over 1,000 pixels across. A strip is a run
of columns of a character's cut, STRIP-WIDTH wide, from any column of it,
and the rows of those columns that hold ink (GLYPH-STRIP): of the ink of a
letter laid over another, every strip within what was laid is the same as
its source's, wherever the letters about it begin and end, and whether or
not the blur joined it to a neighbour, at its source or where it was laid.
Strips holding fewer than 20 pixels of ink once ink the scanner clipped is
set aside are not set beside others (as GLYPH-DISTANCE does not judge
characters with fewer); nor is a strip whose own core strays no more than a
character without grain of its own does, a grey level or a fifth of what
its page's characters typically stray (STRIP-CORE-SPREAD): such a strip is
alike in every print where the grain is faint. Nor is a strip set beside
one of the same pixels of the picture (the cuts of characters side by side
take in the same paper), nor beside one of the same character. A JPEG keeps
each block of 8 by 8 pixels alike when its pixels are alike, and so keeps
two clean prints the same where they fall alike on its grid: such a pair is
marked GRID. Ink is what is darker than the grey STRIP-MEASURES gives, so
that a strip's rows are the same wherever it is found; only strips of the
same STRIP-HASH are set beside each other, and of more than 64 of one hash
each is set beside the first alone. That is a limit on work, not on what
is found: of 720 honest pages the most strips of one number on a page with
grain was 11, and 291 to 333 on pages drawn by a program set in a file
beside a scan; a character laid 70 times gives one number 70 times, and
every copy is still found, each beside the first."
  (multiple-value-bind (k level) (strip-measures (remove-if #'glyph-bare glyphs))
    (let* ((glyphs (coerce (remove-if #'glyph-bare glyphs) 'simple-vector))
           (typical (or (typical-width (coerce glyphs 'list)) 6))
           (most (min (ash 1 24) (loop for g across glyphs sum (max 0 (1+ (- (glyph-width g) k))))))
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
                     (loop while (and (< end count) (= (ash (aref sorted end) -24) (ash (aref sorted start) -24))) do (incf end))
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
                                               count (let ((g (aref pixels (+ (* j across) c)))) (and (< g level) (> g clipped))))))
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
groups of two or more: of those with grain of their own (GLYPH-BARE not),
those sharing a strip of the same pixels (SAME-STRIPS, given DCT), with as
second and third values its pairs and how many strips it left out; with
GRAINED NIL, of those without, those whose whole boxes are the same, grey
level for grey level (SAME-PIXELS-P), only those of the same
GLYPH-BOX-HASH set beside each other."
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
  "How alike characters of the same shape typically are on each scanned
picture of GLYPHS (ALIKE-CHARACTERS), as a list of ((PICTURE . PAGE) ALIKE
CHARACTERS SAMPLED NEAR MIDDLE): SAMPLED how many of its own characters
were set beside its own, NEAR how many of them had one within 30 grey
levels and MIDDLE how alike those were (ALIKE-CHARACTERS); and each
character's GLYPH-ALIKE set to its picture's, or NIL
where that is under 4 grey levels or cannot be told. Each picture is first
measured by its own prints alone, set beside its own characters only: a page whose grain is too faint to tell
prints apart is not judged, whatever the other pages of its file are. A
picture judged is then measured again with the prints of the pictures
judged near it too, as a character is set beside them; a page of more
grain is never measured against a cleaner one left unjudged. Both times up
to 400 of its characters are measured, the same ones however many others
are read beside them (LIKENESS-SAMPLE): on 939 pages of made-up scans,
reading the marks standing alone moved a page's likeness by 6 % at most,
by under 1 % on 19 pages of 20, where 80 characters taken at even steps
moved it by up to a quarter; measuring 400 rather than 80 takes a tenth
longer on a scan of 60 pages."
  (let ((pictures (make-hash-table :test 'equal)) (judged (make-hash-table :test 'equal)) (own (make-hash-table :test 'equal))
        (counts (make-hash-table :test 'equal)))
    (flet ((key (g) (cons (glyph-picture g) (glyph-page g))))
      (dolist (g glyphs) (push g (gethash (key g) pictures)))
      (loop for key being the hash-keys of pictures using (hash-value members)
            do (multiple-value-bind (alone sampled near middle) (alike-characters members (glyph-peers members))
                 (setf (gethash key own) alone (gethash key counts) (list sampled near middle))
                 (when (and alone (>= alone 4)) (setf (gethash key judged) t))))
      (loop for key being the hash-keys of pictures using (hash-value members)
            collect (let ((alike (if (gethash key judged)
                                     (alike-characters members peers :beside (lambda (g p) (declare (ignore g)) (gethash (key p) judged)))
                                     (gethash key own))))
                      (dolist (g members) (setf (glyph-alike g) (and (gethash key judged) alike (>= alike 4) alike)))
                      (list* key alike (length members) (gethash key counts)))))))

(defun copied-glyphs (glyphs &key dct drawn)
  "The characters of GLYPHS, those of all the scanned pictures of a file,
that are copies of one another, as seven values: the groups of copies, each
\((GLYPH...) DIFFERENCE BLOCK ALIKE STRENGTH WHY LAID); how alike characters of the same
shape typically are, the least of it over the pictures judged (or over all
of them, none judged); the groups of marks the same pixel for pixel without
grain of their own (GRAIN-LEVELS), each (GLYPH...), which are no copies, as
two honest prints of a mark the scanner clipped can be the same, those of
the words and lines of the fourth value left out; the words and lines
without grain of their own on pages with it (BARE-WORDS); the characters
not judged, too few of their pixels left beside any other once the ink the
scanner clipped is set aside (GLYPH-DISTANCE, GRAIN-LEVELS); the pictures
\(PICTURE-LIKENESS), each ((PICTURE . PAGE) ALIKE CHARACTERS SAMPLED NEAR
MIDDLE); and how many strips EXACT-COPIES left out. DCT lists the pictures
kept as JPEGs; DRAWN those whose blank paper strays by less than half a
grey level: where their characters' cores typically stray by a grey level
or less too, the page was drawn by a program rather than scanned, and its
characters have no grain of their own. Paper alone tells nothing: a scanner set to make its paper
white leaves it clean, the grain of the ink untouched; on the made-up
scans so set the paper strays by 0 to 0.4 and the cores by 13 to 30, while
the pages whose paper strays by less than half a grey level and whose cores
stray by a grey level or less were drawn by a program or printed with no
blur, and scanned paper not made white strays by nearly 1 or more (0.97 on
a denoised page).
How alike characters typically are is measured on each picture apart, and
only pictures where they differ by 4 grey levels or more are judged: on a
picture drawn rather than scanned, or smoothed, every e is the same. Two
characters are set beside each other by the less of their pictures'
likeness, ALIKE below.
Two characters are copies when they share a block of pixels the same, grey
level for grey level, with grain of their own (EXACT-COPIES), on any of the
pages, a difference of 0, a strong sign but on a JPEG's own grid (WHY
:GRID); when they are alike to half of ALIKE in their ink, its grain and
all, worth a look, and a strong sign only when their ink is the same in
every pixel (a difference of 0), but for characters without grain of their
own, characters of pictures kept as JPEGs (WHY :JPEG; a JPEG keeps clean
prints alike, and their likeness proves nothing there), characters the
same in every pixel of fewer than 20 pixels of ink, and characters whose
cores stray by less than half what their page's typically do (WHY :SMOOTH;
a region the scanner's software smoothed keeps prints alike); likeness
alone, however near, is never a strong sign, as prints of a clean page can
come near; or when each is the
other's nearest, nearer than three quarters of ALIKE, and at least twice
as near each other as either is to the next of its shape (there being one
within twice ALIKE): two prints of a character are as far from each other
as from the rest, but a copy saved again as a JPEG is near its source
alone. Copies told by strong signs are gathered into groups, and copies
worth a look into groups of their own, so that a character comes into a
strong group only by a strong sign: STRENGTH :STRONG or :LOOK; DIFFERENCE
the root of the mean squared difference of the closest pair, 0 for an
exact copy, whose BLOCK is the widest block found the same, (WIDTH HEIGHT
INK), else NIL; LAID true when its characters are taller by half than the
taller type of their pages (UPPER-HEIGHT), or stand at the same place on
two pages and half of them or more: a stamp, a logo or a signature laid on
the pages by software, which an office may do honestly. The tallest
characters of type (a bar, brackets, a j) are 1.32 to 1.36 times the
taller type in the faces tried (DejaVu, Nimbus Sans Narrow, Liberation
Serif); the stamps tried stand at 1.97, signatures and bar codes at 2.0 to
2.1. LAID only says how a strong group is named, never whether it is
strong. Marks twice as tall
as the taller characters (UPPER-HEIGHT), pieces of a table's border, are no
characters."
  (let* ((type-height (upper-height glyphs))
         (type-width (typical-width glyphs))
         (tall (* 2 (or type-height 0)))
         ;; a piece of a table's border, too short to be taken out as a rule, is no character
         (glyphs (remove-if (lambda (g) (> (- (glyph-y1 g) (glyph-y0 g)) tall)) glyphs))
         (pictures (progn (grain-levels glyphs)
                          (dolist (g glyphs) (when (and (member (glyph-picture g) drawn) (<= (glyph-typical g) 1)) (setf (glyph-bare g) t)))
                          (picture-likeness glyphs (glyph-peers glyphs))))
         (judged-alike (remove nil (mapcar (lambda (p) (and (second p) (>= (second p) 4) (second p))) pictures)))
         (alike (if judged-alike (reduce #'min judged-alike) (let ((all (remove nil (mapcar #'second pictures)))) (and all (reduce #'min all)))))
         (glyphs (remove-if-not #'glyph-alike glyphs))
         (not-judged '()))
    (if (null glyphs)
        (values nil alike nil nil nil pictures 0)
        (let* ((peers (glyph-peers glyphs)) (index (make-hash-table :test 'eq))
               (strong (make-array (length glyphs))) (look (make-array (length glyphs)))
               (closest (make-hash-table :test 'equal)) (blocks (make-hash-table :test 'equal)) (why (make-hash-table :test 'eq))
               (nearest (make-hash-table :test 'eq)) (settled (make-hash-table :test 'eq)) (left 0))
          (loop for g in glyphs for k from 0 do (setf (gethash g index) k (aref strong k) k (aref look k) k))
          (labels ((root (parent k) (if (= (aref parent k) k) k (setf (aref parent k) (root parent (aref parent k)))))
                   (pair-alike (a b) (min (glyph-alike a) (glyph-alike b)))
                   (join (a b d &key block reason (strength :strong))
                     (let* ((parent (if (eq strength :strong) strong look))
                            (ra (root parent (gethash a index))) (rb (root parent (gethash b index))))
                       (unless (= ra rb) (setf (aref parent rb) ra))
                       (let ((r (root parent ra)))
                         (setf (gethash (cons strength r) closest)
                               (min d (or (gethash (cons strength ra) closest) d) (or (gethash (cons strength rb) closest) d))))
                       (dolist (g (list a b))
                         (when block
                           (let ((was (gethash (cons strength g) blocks)))
                             (when (or (null was) (> (third block) (third was))) (setf (gethash (cons strength g) blocks) block))))
                         (when reason (setf (gethash g why) reason))))))
            ;; characters of the same pixels are copies at once, however far apart; one with such a copy
            ;; among those it is set beside (GLYPH-PEERS) has that copy for its nearest, and is set beside
            ;; no other
            (multiple-value-bind (groups pairs strips-left) (exact-copies glyphs :dct dct)
              (declare (ignore groups))
              (setf left strips-left)
              (loop for (a b block grid) in pairs
                    do (if grid
                           (join a b 0 :block block :reason :grid :strength :look)
                           (progn (join a b 0 :block block)
                                  (when (<= (abs (- (glyph-page a) (glyph-page b))) 2)
                                    (setf (gethash a settled) t (gethash b settled) t))))))
            ;; each one's nearest, when nearer than three quarters of its picture's likeness; the next, for those
            (loop for g in glyphs for k from 0
                  do (when (zerop (mod k 50)) (job-check-stop))
                     (unless (gethash g settled)
                       (let ((near (funcall peers g)) (alike (glyph-alike g)))
                         (multiple-value-bind (p d judged counted) (nearest-glyph g near (* 3/4 alike))
                           (unless judged (push g not-judged))
                           (when p
                             (setf (gethash g nearest)
                                   (list p d (nth-value 1 (nearest-glyph g near (* 2 alike) p)) counted)))))))
            (loop for g being the hash-keys of nearest using (hash-value (p d next counted))
                  do (let ((theirs (gethash p nearest)) (both (pair-alike g p)))
                       (when (and (or (<= d (/ both 2))
                                      (and theirs (eq (first theirs) g) next (third theirs) (< d (* 3/4 both))
                                           (<= (* 2 d) next) (<= (* 2 d) (third theirs))))
                                  ;; the same pixel for pixel without grain of their own: marks, not copies
                                  (not (and (glyph-bare g) (same-pixels-p g p)))
                                  ;; pieces of rules are no characters; strokes alone come alike by chance, and
                                  ;; tell a copy only by the paper about them too, each the other's nearest
                                  (not (and (plusp d) (or (rule-piece-p g type-height type-width) (rule-piece-p p type-height type-width))))
                                  (not (and (plusp d) (or (shapeless-glyph-p g) (shapeless-glyph-p p))
                                            (not (and theirs (eq (first theirs) g) (<= (glyph-ring-distance g p) (/ both 2)))))))
                         ;; likeness alone is worth a look at most: only ink the same in every pixel is a strong sign
                         (cond ((or (plusp d) (glyph-bare g) (glyph-bare p))
                                (join g p d :strength :look))
                               ;; a JPEG keeps clean prints alike: their likeness proves nothing there
                               ((or (member (glyph-picture g) dct) (member (glyph-picture p) dct))
                                (join g p d :strength :look :reason :jpeg))
                               ((and (zerop d) (< counted 20))
                                (join g p d :strength :look))
                               ;; a print smoothed by the scanner's software keeps little of its grain, and comes near
                               ;; another so smoothed: likeness is proof only of prints with their page's grain
                               ((notevery (lambda (c) (and (glyph-spread c) (>= (* 2 (glyph-spread c)) (glyph-typical c)))) (list g p))
                                (join g p d :strength :look :reason :smooth))
                               (t (join g p d))))))
            (let* ((bare (bare-words glyphs)) (in-bare (make-hash-table :test 'eq))
                   ;; marks the same pixel for pixel, within two pages of another, but for the words without grain
                   (marks (progn (loop for (nil nil members) in bare do (dolist (g members) (setf (gethash g in-bare) t)))
                                 (loop for group in (exact-copies glyphs :grained nil)
                                       for near = (remove-if-not (lambda (g) (and (not (gethash g in-bare))
                                                                                  (find-if (lambda (o) (and (not (eq o g)) (<= (abs (- (glyph-page o) (glyph-page g))) 2)))
                                                                                           group)))
                                                                 group)
                                       when (cdr near) collect near)))
                   (told (make-hash-table :test 'eq)))
              (dolist (group marks) (dolist (g group) (setf (gethash g told) t)))
              (let ((heights (make-hash-table :test 'equal)) (middle (make-hash-table :test 'equal)))
                (dolist (g glyphs) (push g (gethash (cons (glyph-picture g) (glyph-page g)) heights)))
                (loop for key being the hash-keys of heights using (hash-value gs) do (setf (gethash key middle) (upper-height gs)))
              (labels ((laid-p (group)
                       ;; a mark taller than the page's type, or one at the same place on most pages
                       (or (every (lambda (g) (> (- (glyph-y1 g) (glyph-y0 g))
                                                 (* 3/2 (gethash (cons (glyph-picture g) (glyph-page g)) middle))))
                                  group)
                           (some (lambda (g)
                                   (let ((pages (remove-duplicates
                                                 (mapcar #'glyph-page
                                                         (remove-if-not (lambda (o) (and (<= (abs (- (glyph-x0 o) (glyph-x0 g))) 3)
                                                                                         (<= (abs (- (glyph-y0 o) (glyph-y0 g))) 3)))
                                                                        group)))))
                                     (and (>= (length pages) 2) (>= (* 2 (length pages)) (length pictures)))))
                                 group)))
                     (groups (strength parent)
                       (let ((members (make-hash-table)))
                         (loop for g in glyphs for k from 0
                               do (unless (= (root parent k) k) (push g (gethash (root parent k) members)))
                                  (when (and (= (root parent k) k) (gethash (cons strength k) closest)) (push g (gethash k members))))
                         (loop for r being the hash-keys of members using (hash-value group)
                               when (cdr group)
                                 collect (let ((d (gethash (cons strength r) closest)) (group (nreverse group)))
                                           (list group d
                                                 (and (zerop d)
                                                      (reduce (lambda (a b) (if (or (null a) (and b (> (third b) (third a)))) b a))
                                                              group :key (lambda (g) (gethash (cons strength g) blocks)) :initial-value nil))
                                                 (reduce #'min group :key #'glyph-alike)
                                                 strength
                                                 (and (eq strength :look) (some (lambda (g) (gethash g why)) group))
                                                 (laid-p group)))))))
                (values (append (groups :strong strong) (groups :look look))
                        alike marks bare
                        (remove-if (lambda (g) (gethash g told)) (nreverse not-judged))
                        pictures left)))))))))

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
                 (let ((words (make-hash-table :test 'eq)) (lines (make-hash-table)) (counts (make-array 256 :element-type 'fixnum)))
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
                                                      when (and n (>= n 2) (>= count 3)) collect (list :word (list word))))))
                                (loop for (kind ws) in taken
                                      do (push (list kind ws (loop for w in ws append (gethash w words))) out)))))))))
    out))

(defun glyph-page-box (mark glyph width height)
  "The box (X0 Y0 X1 Y1) on the page of GLYPH, found in the picture MARK
draws, WIDTH by HEIGHT pixels (PICTURE-PAGE-BOX)."
  (picture-page-box mark (glyph-x0 glyph) (glyph-y0 glyph) (glyph-x1 glyph) (glyph-y1 glyph) width height))

(defun scan-of-glyph (scans glyph)
  "The SCAN-PICTURE of SCANS GLYPH is in."
  (find-if (lambda (scan) (and (eql (mark-object (scan-picture-mark scan)) (glyph-picture glyph))
                               (eql (page-index (scan-picture-page scan)) (glyph-page glyph))))
           scans))

(defun glyph-words (scans glyph)
  "Where GLYPH is, in words: on the page and in its picture, and whether it
is a piece cut from a wider mark (SPLIT-JOINED)."
  (let* ((scan (scan-of-glyph scans glyph)))
    (destructuring-bind (x0 y0 x1 y1) (glyph-page-box (scan-picture-mark scan) glyph (scan-picture-width scan) (scan-picture-height scan))
      (declare (ignore x1 y1))
      (format nil "page ~d at x ~d, y ~d (pixels ~d,~d to ~d,~d of picture ~d~@[, ~a~])"
              (1+ (glyph-page glyph)) (round x0) (round y0)
              (glyph-x0 glyph) (glyph-y0 glyph) (glyph-x1 glyph) (glyph-y1 glyph) (glyph-picture glyph)
              (case (glyph-split glyph)
                (:sure "cut at a neck from a mark wider than a letter")
                (:unsure "a mark wider than a letter, no neck found to cut it at"))))))

(defun not-judged-words (not-judged)
  "How many characters of NOT-JUDGED were not set beside others, and why, in words."
  (format nil "~:d character~:p not judged: too few pixels left once the ink the scanner clipped is set aside" (length not-judged)))

(defun scans-read (pdf revision)
  "The scanned pictures of REVISION's pages read (SCAN-PICTURES), the
characters copied among them (COPIED-GLYPHS) and the regions of each whose
JPEG traces are unlike the rest of it (JPEG-PIECES, of the words'
characters, not the marks standing alone), as ten values: the
SCAN-PICTUREs, the groups of copies, how alike characters typically are,
the BLOCK-PIECEs, the groups of marks without grain the same pixel for
pixel, the words and lines without grain, the characters not judged, how
alike characters are on each picture, how many strips were left out
\(COPIED-GLYPHS), and the scanned pictures that could not be read, each
\(PAGE MARK WHY) as SCAN-PICTURES gives them. Each SCAN-PICTURE is told whether it was JUDGED: the
likeness of its characters 4 grey levels or more, and not drawn by a
program (its blank paper straying by less than half a grey level and its
characters' cores by a grey level or less, DRAWN); the words of one not
judged are worth a look at most, as its copies are not told. Its JPEG
pieces are worth a look at most only when it was drawn by a program, as a
screenshot or a chart is: a scan whose grain is too faint to tell prints
apart still keeps a JPEG's blocks where a piece was saved apart, which no
grain of the prints is needed to tell; and a piece of blocks of its own
stays strong only when another sign agrees on it (BACKED-PIECES).
Read once and kept with PDF, its characters' pixels let go but for those
named, for the drawing (OVERLAY-OPERATORS) to find them again."
  (let ((kept (or (getf (pdf-cache pdf) :scans) (setf (getf (pdf-cache pdf) :scans) (make-hash-table :test 'eq)))))
    (values-list
     (or (gethash revision kept)
         (setf (gethash revision kept)
               (multiple-value-bind (scans not-read) (scan-pictures pdf revision)
                 (multiple-value-bind (copies alike marks bare not-judged pictures left)
                     (copied-glyphs (loop for scan in scans append (scan-picture-glyphs scan))
                                    :drawn (loop for scan in scans
                                                 when (and (scan-picture-paper scan) (< (scan-picture-paper scan) 0.5))
                                                   collect (mark-object (scan-picture-mark scan)))
                                    :dct (loop for scan in scans
                                               for object = (mark-object (scan-picture-mark scan))
                                               when (picture-jpeg-p pdf revision object) collect object))
                   ;; judged: its characters differ as prints do (the gate of PICTURE-LIKENESS), and not drawn
                   (dolist (scan scans)
                     (let* ((glyphs (scan-picture-glyphs scan))
                            (entry (assoc (cons (mark-object (scan-picture-mark scan)) (page-index (scan-picture-page scan))) pictures :test #'equal))
                            (drawn (and (scan-picture-paper scan) (< (scan-picture-paper scan) 0.5) glyphs
                                        (<= (reduce #'max glyphs :key #'glyph-typical) 1))))
                       (setf (scan-picture-drawn scan) (and drawn t)
                             (scan-picture-judged scan) (and entry (second entry) (>= (second entry) 4) (not drawn)))))
                   (dolist (scan scans)
                     (let ((twin (scan-picture-twin scan)))
                       (when twin (setf (scan-picture-judged scan) (scan-picture-judged twin)
                                        (scan-picture-drawn scan) (scan-picture-drawn twin)))))
                   (let ((pieces (loop for scan in scans
                                       append (let ((pieces (jpeg-pieces (remove-if (lambda (g) (word-box-lone (glyph-word g)))
                                                                                    (scan-picture-glyphs scan))
                                                                         :jpeg (picture-jpeg-p pdf revision (mark-object (scan-picture-mark scan))))))
                                                ;; a picture drawn by a program gives pieces worth a look at most
                                                (when (scan-picture-drawn scan)
                                                  (dolist (p pieces) (setf (block-piece-strength p) :look)))
                                                pieces)))
                         (heights (make-hash-table :test 'equal)))
                     (dolist (scan scans)
                       (let ((upper (upper-height (scan-picture-glyphs scan))))
                         (when upper (setf (gethash (cons (mark-object (scan-picture-mark scan)) (page-index (scan-picture-page scan))) heights) upper))))
                     (laid-pieces pieces heights)
                     (copies-on-piece-grid copies pieces)
                     (stamp-copies copies scans)
                     (backed-pieces pieces copies scans)
                     (dolist (scan scans) (setf (scan-picture-glyphs scan) nil))
                     (list scans copies alike pieces marks bare not-judged pictures left not-read)))))))))

(defun scan-areas (odd bands)
  "The blocks of lines of a scanned picture set apart together and its bands
of paper set apart, from ODD, its words unlike the rest (UNLIKE-WORDS), and
BANDS (PAPER-BANDS), those over the same rows and columns made one, as two
values: the areas, each (X0 Y0 X1 Y1 BANDS MEMBERS), its box in the picture
made level, its bands and its words each with one way it is unlike the rest,
\(WORD . WAY), from the top of the page down; and the words of ODD with
their ways not of a block, each (WORD WAY...), told on their own wherever
they stand, so that a word pasted into a band printed again keeps its
place in the report."
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

(defun check-scans (pdf revision)
  "Pictures of a page whose words are unlike each other: words whose ink,
edges or paper stand apart from the words beside them, or that sit off
their line's baseline or are turned against it (UNLIKE-WORDS): worth a
look at most, those unlike the rest both in how they look and in where
they sit named apart from those unlike it in one of them, as real scans
have both. And
characters that are copies of one another, on one scanned page or across
them (COPIED-GLYPHS); and, worth a look, marks without grain of their
own the same pixel for pixel, and words and lines without grain of their
own (BARE-WORDS). A scanned picture whose pixels could not be read is
named for the record, with why, so that no page goes unexamined unsaid."
  (multiple-value-bind (scans copies alike pieces marks bare not-judged pictures left not-read) (scans-read pdf revision)
    (declare (ignore pieces alike))
    (let ((out '()))
      (dolist (scan scans)
        (let ((page (scan-picture-page scan)) (m (scan-picture-mark scan)) (odd (scan-picture-odd scan))
              (width (scan-picture-width scan)) (height (scan-picture-height scan)) (tilt (scan-picture-tilt scan)))
          (labels ((row (word y) (round (+ y (* (or tilt 0) (word-box-x0 word)))))
                   (report (strength odd both)
                     (when odd
                       (push (found strength (format nil "On page ~d, the scanned picture ~a (object ~d) has ~:d word~:p unlike the rest of it~a, ~{~a~^, ~}: pasted into the scan, or printed over it~:[, or a stamp, a form filled in, or printed in another pass~;~]~:[; its grain too faint to tell prints apart, or drawn by a program, as a screenshot or a chart is: worth a look at most~;~]."
                                                     (1+ (page-index page)) (mark-text m) (mark-object m) (length odd)
                                                     (cond ((not both) "")
                                                           ((= (length odd) 1) " both in how it looks and where it sits")
                                                           (t " both in how they look and where they sit"))
                                                     (remove-duplicates (loop for (nil . ways) in odd append (unlike-words-words ways)) :test #'string=)
                                                     both (or (not both) (scan-picture-judged scan)))
                                    :rule (if both
                                              "A page scanned in one go has one ink, one blur and one paper throughout, and its words stand on their lines; a word whose edges are sharper than its line's, or whose paper strays half as far as the page's blank paper or less, and that also sits off its line or turned was put there another way: two measures apart from each other agree. Honest scans have such words too (a line printed in another pass, a heading, a word the light or the paper's curl set apart), so it is worth a look, not proof: a strong sign comes only from pixels the same where no scanner makes them so, or from the file's own structure. Another ink and another place alone come together from one cause, as a stamp, a form filled in or a line printed in another pass have them; and a word of narrow strokes reads as softer at its edges and cleaner within its box."
                                              "A page scanned or photographed in one go has one ink, one blur and one paper, changing little from one word to the next however the light and focus change across it, and its words stand on their lines, however the paper curls; words whose ink is darker or lighter, whose edges are sharper or softer, or whose paper is cleaner or noisier than the words on both sides of them, or that step off their line from both sides, were put there another way: pasted or printed over the scan, or a stamp, a form filled in or a line printed in another pass.")
                                    :revision (revision-index revision) :page (1+ (page-index page)) :objects (list (mark-object m))
                                    :details (loop for (word . ways) in odd
                                                   collect (destructuring-bind (x0 y0 x1 y1) (word-page-box m word width height tilt)
                                                             (declare (ignore x1 y1))
                                                             (format nil "at x ~d, y ~d (pixels ~d,~d to ~d,~d of picture ~d): ~{~a~^; ~}"
                                                                     (round x0) (round y0)
                                                                     (word-box-x0 word) (row word (word-box-y0 word)) (word-box-x1 word) (row word (word-box-y1 word))
                                                                     (mark-object m)
                                                                     (append
                                                                      (loop for (key own usual nil) in ways
                                                                            collect (case key
                                                                                      (:ink (format nil "ink grey ~d, the words beside it ~d" (round own) (round usual)))
                                                                                      (:edges (format nil "half-tones ~,2f of its ink, the words beside it ~,2f" own usual))
                                                                                      (:noise (format nil "paper straying ~,1f, the words beside it ~,1f" own usual))
                                                                                      (:offset (let ((off (word-box-offset word))) (format nil "its baseline ~,1f pixels ~:[above~;below~] its line's" (abs off) (plusp off))))
                                                                                      (:turn (let ((turn (word-box-turn word))) (format nil "turned ~,1f° ~:[anticlockwise~;clockwise~] against its line" (abs turn) (plusp turn))))))
                                                                      (when (some (lambda (way) (eq (fifth way) :pair)) ways)
                                                                        (list "set beside the words it stands with, which stand as far apart the other way: one of them was moved or put there"))))))
                                    :evidence (nth-value 0 (object-evidence pdf revision (mark-object m)))
                                    ;; a word unlike its line in how it looks was put there; one only off its line was moved
                                    :method (if (some (lambda (o) (some (lambda (w) (member (first w) *scan-looks*)) (cdr o))) odd)
                                                :pasted-on-scan :moved-in-scan))
                             out))))
            ;; blocks of lines set apart together and bands of paper set apart, each one finding
            (multiple-value-bind (areas left) (scan-areas odd (scan-picture-bands scan))
              (setf odd left)
              (loop for (x0 y0 x1 y1 bands members) in areas
                    do (let* ((words (remove-duplicates (mapcar #'car members)))
                              (tall (and words
                                         (median-of (mapcar (lambda (w) (- (word-box-y1 w) (word-box-y0 w))) words))))
                              (tops (sort (mapcar #'word-box-y0 words) #'<))
                              (lines (if words (1+ (loop for (a b) on tops while b count (> (- b a) (/ tall 2)))) 0))
                              (keys (remove-duplicates (mapcar (lambda (pair) (first (cdr pair))) members)))
                              (band (first bands))
                              (paper (and band (if (< (paper-band-paper band) (paper-band-above band))
                                                   "darker paper"
                                                   "lighter paper")))
                              (box (word-page-box m (make-word-box :x0 x0 :y0 y0 :x1 x1 :y1 y1) width height tilt))
                              (count (reduce #'max bands :key #'paper-band-words :initial-value (length words)))
                              (spans (reduce #'max bands :key #'paper-band-lines :initial-value lines))
                              (corner (make-word-box :x0 x0)))
                         (push (found :look (format nil "On page ~d, the scanned picture ~a (object ~d) has a block ~:[of ~:d word~:p over ~:d line~:p~;over ~*~:d line~:p, its words not read,~] unlike the rest of it, set apart together from the lines above and below, ~{~a~^, ~}: printed again or retouched there, or a band printed in another pass."
                                                    (1+ (page-index page)) (mark-text m) (mark-object m)
                                                    (zerop count) count spans
                                                    (append (and paper (list paper))
                                                            (remove-duplicates (unlike-words-words (mapcar #'cdr members))
                                                                               :test #'string=)))
                                      :rule "A page scanned in one go has one ink, one blur and one paper throughout, however evenly or unevenly the light falls across it; a block of lines whose paper stands apart from the paper above and below it with edges of its own, or whose ink, edges or paper stand apart together from the lines above and below it, by less than a word alone must and more than half that, was printed again, retouched or pasted there, or printed in another pass. The light's smooth fall across the page is taken out first, and a shadow or a curl of the paper fades where a band printed again has edges; a block set apart by its ink alone is named only where its paper, its edges or the grain of its paper stand apart too, as a band printed again or a patch brings them: a line printed darker in a second pass of the press, or set in bold, keeps the page's paper, and so cannot be told from one printed so honestly. Honest pages have such blocks too, a shaded row of a form among them, so it is worth a look, not proof."
                                      :revision (revision-index revision) :page (1+ (page-index page))
                                      :objects (list (mark-object m))
                                      :details
                                      (list (format nil "from x ~d, y ~d to x ~d, y ~d (pixels ~d,~d to ~d,~d of picture ~d): ~{~a~^; ~}"
                                                    (round (first box)) (round (second box))
                                                    (round (third box)) (round (fourth box))
                                                    x0 (row corner y0) x1 (row corner y1) (mark-object m)
                                                    (append
                                                     (and band (list (format nil "paper grey ~d, the paper above it ~d and below it ~d"
                                                                             (round (paper-band-paper band))
                                                                             (round (paper-band-above band))
                                                                             (round (paper-band-below band)))))
                                                     (loop for key in keys
                                                           for ways = (loop for (nil . way) in members
                                                                            when (eq (first way) key) collect way)
                                                           for own = (median-of (mapcar #'second ways))
                                                           for usual = (median-of (mapcar #'third ways))
                                                           collect (case key
                                                                     (:ink (format nil "ink grey ~d, the lines about it ~d"
                                                                                   (round own) (round usual)))
                                                                     (:edges (format nil "half-tones ~,2f of its ink, the lines about it ~,2f"
                                                                                     own usual))
                                                                     (:noise (format nil "paper straying ~,1f, the lines about it ~,1f"
                                                                                     own usual))
                                                                     (:offset (format nil "baselines ~,1f pixels off their lines"
                                                                                      (abs own)))
                                                                     (t (format nil "turned ~,1f° against their lines" (abs own))))))))
                                      :evidence (nth-value 0 (object-evidence pdf revision (mark-object m)))
                                      :method :pasted-on-scan)
                               out))))
            ;; words unlike their line are worth a look at most, however many ways they are so
            (report :look
                    (remove-if-not (lambda (o) (looks-and-sits-unlike-p (cdr o) (scan-picture-paper scan))) odd) t)
            (report :look (remove-if (lambda (o) (looks-and-sits-unlike-p (cdr o) (scan-picture-paper scan))) odd) nil))))
      (flet ((report (strength copies words rule)
               (when copies
                 (let ((pages (sort (remove-duplicates (loop for (group) in copies append (mapcar #'glyph-page group))) #'<))
                       (pictures (remove-duplicates (loop for (group) in copies append (mapcar #'glyph-picture group)))))
                   (push (found strength (format nil words (loop for (group) in copies sum (length group)) (length pages))
                                :rule rule
                                :revision (revision-index revision) :page (1+ (first pages)) :objects pictures
                                :details (append (loop for (group difference block alike nil why) in copies
                                                       collect (format nil "~{~a~^, ~}: ~:[~:[alike to ~,1f grey levels~;the same grey in every pixel of their ink~*, their paper apart~]~*~;~2*the same pixel for pixel over ~{~d by ~d pixels, ~:d of them ink~}~]~@[, ~a~], the scan's characters of the same shape alike to ~,1f"
                                                                       (mapcar (lambda (g) (glyph-words scans g)) group)
                                                                       block (zerop difference) difference block
                                                                       (case why
                                                                         (:grid "on the JPEG's own grid of 8 by 8 pixels")
                                                                         (:stamp "in an ink or at an angle unlike the page's text")
                                                                         (:jpeg "on a page kept as a JPEG, which keeps clean prints alike")
                                                                         (:smooth "their cores straying less than their page's, as smoothed prints do"))
                                                                       alike))
                                                 (and not-judged (list (not-judged-words not-judged))))
                                :evidence (loop for p in pictures append (nth-value 0 (object-evidence pdf revision p)))
                                :method (if (eq strength :strong) :characters-composed :characters-composed-resaved))
                         out)))))
        (report :strong (remove-if (lambda (c) (or (eq (fifth c) :look) (seventh c))) copies)
                "~:d characters of the scanned page~p are copies of one another, the grain of their ink too: copied within the scan, or typed onto it."
                "No scanner draws a character twice alike: each print of it differs in the grain of its ink and in where the blur falls on the pixels. Two characters that share a block of pixels the same, grey level for grey level, the ink of the block straying as a print's does, or whose ink is the same in every pixel on a page kept without loss, are one character copied, or both drawn by a program, as typed text is. Characters only alike, however near, are not named so: clean prints can come near each other. Letters the blur joined to a neighbour are cut apart at the neck between them and set beside the rest as pieces. How alike characters typically are is measured on each scanned page apart, and two characters are set beside each other by the less of their pages'.")
        (report :strong (remove-if-not (lambda (c) (and (eq (fifth c) :strong) (seventh c))) copies)
                "~:d characters of the scanned page~p are copies of one another, the grain of their ink too: copied within the scan, or a stamp, a logo or a signature laid on the pages by software."
                "No scanner draws a character twice alike: two characters that share a block of pixels the same, grey level for grey level, the ink of the block straying as a print's does, are one copied. These are taller than the type of their pages, or stand at the same place on most pages: a stamp, a logo or a signature kept as a picture and laid on each page by a program does so, as offices do honestly, as well as a mark copied to deceive. Whether such a mark belongs there is for the examiner to judge.")
        (report :look (remove-if-not (lambda (c) (eq (sixth c) :stamp)) copies)
                "~:d characters of the scanned page~p are the same pixel for pixel, in an ink or at an angle unlike the page's text: a stamp drawn by software, or characters copied there."
                "No scanner draws a character twice alike, but a program draws its characters the same every time: a stamp an office's software lays on a scan, a date or a received mark, repeats its figures pixel for pixel. These characters are in an ink unlike the page's text (red, blue or green on black type, or a grey far from the type's), or turned 10 degrees or more against the page's lines, as a stamp is, and none of them is the same as a character outside them; and the stamp is as a program draws it: the grey levels of its characters' cores (their pixels no lighter than halfway from the darkest to the lightest of each box) typically stray from their middle by less than 0.6 of what the page's characters' cores do, as a stamp drawn and laid on a scan does and a stamp pressed on paper and scanned with the page, carrying its grain, does not; it repeats two shapes or more; and every character of a shape repeated in it is the same as the others of that shape, or nearer to them than prints are. A figure copied over another in a pressed stamp, or over every print of its shape, stays a strong sign. Characters typed onto a scan in the page's own ink and at its angle are a strong sign; whether such a stamp belongs there is for the examiner to judge.")
        (report :look (remove-if (lambda (c) (or (eq (fifth c) :strong) (eq (sixth c) :stamp))) copies)
                "~:d characters of the scanned page~p are nearer one another than prints of one character are: copied within the scan and saved again, or alike by chance, as clean prints on pages kept as JPEG or JPEG 2000, or smoothed, can be."
                "A copy saved again as a JPEG or a JPEG 2000 with loss is no longer alike pixel for pixel, but stays nearer its source than prints of a character are to one another: two characters whose ink is alike to half of how alike the scan's characters of the same shape typically are, or each the other's nearest and twice as near as to the next of its shape. Two prints of a character now and then come as near, on a clean page nearer still, so likeness alone is worth a look, never a strong sign; pieces cut from letters the blur joined, set beside others by their likeness, are worth a look at most, as two prints of the same letters cut at the same neck come nearer each other than prints of a letter do; and so are characters the same pixel for pixel over less than two characters' width where they fall alike on a JPEG's grid of 8 by 8 pixels, which keeps clean prints alike block for block. On a page kept as a JPEG, or smoothed by the scanner's software, clean prints of one character come alike, and two characters alike there by their likeness alone are worth a look, never a strong sign. A character of too little shape, a stroke alone (l, I, a plain 1 or 7, a dash), comes near another stroke by chance, and is named by its likeness only when the two pixels of paper about it are as alike as its ink, the two each the other's nearest, as a stroke copied with the paper round it stays; a mark taller by half than the type, five times as tall as it is wide, taller by a tenth than the type and three times as tall as it is wide, or three times as wide as it is tall and wider by half than a character, is a piece of a rule and no character; a hyphen, a minus sign or a dash is a character, a stroke alone."))
      ;; pictures not judged: the analyst told every reason that holds, with what was measured
      (flet ((why (scan alike sampled near measured)
               (if (and scan (scan-picture-unread scan))
                   ;; no words read: that alone
                   (list :unread)
                   (let ((reasons (remove nil (list (and scan (scan-picture-drawn scan) :drawn)
                                                    (and alike (< alike 4) :faint)
                                                    (and sampled (>= sampled 40) (< near 40) :uneven)
                                                    (and scan (plusp (scan-picture-rules scan))
                                                         (>= (* 10 (scan-picture-rules scan)) (scan-picture-ink scan)) :ruled)
                                                    ;; lines run together only where they leave enough unread to tell
                                                    (and scan (plusp (scan-picture-joined scan))
                                                         (or (null measured) (null (scan-picture-characters scan))
                                                             (< measured (* *scan-read-least* (scan-picture-characters scan))))
                                                         :joined)))))
                     (if (or reasons (and sampled (>= sampled 40))) reasons (list :few)))))
             (scan-of (page) (find page scans :key (lambda (s) (page-index (scan-picture-page s))))))
        (let ((unjudged (append (loop for ((nil . page) alike measured sampled near middle) in pictures
                                      for scan = (scan-of page)
                                      unless (and alike (>= alike 4) (not (and scan (scan-picture-drawn scan))))
                                        collect (list (1+ page) (why scan alike sampled near measured) alike sampled near middle scan))
                                ;; pages of no character to measure
                                (loop for scan in scans
                                      for key = (cons (mark-object (scan-picture-mark scan)) (page-index (scan-picture-page scan)))
                                      ;; a picture kept twice is checked where it was read first
                                      unless (or (find key pictures :key #'first :test #'equal)
                                                 (and (scan-picture-twin scan) (not (scan-picture-unread scan))))
                                        collect (list (1+ (page-index (scan-picture-page scan))) (why scan nil nil nil nil) nil nil nil nil scan)))))
          ;; each detail said once, under the first record it belongs to
          (let ((told '()))
            (dolist (reason '(:unread :joined :few :ruled :uneven :faint :drawn))
              (let ((these (remove-duplicates (sort (remove-if-not (lambda (u) (member reason (second u))) unjudged) #'< :key #'first)
                                              :key #'first)))
                (when these
                  (push (found :record (format nil "Copies and grain not checked on page~p ~{~d~#[~; and ~:;, ~]~}: ~a."
                                               (length these) (mapcar #'first these)
                                               (ecase reason
                                                 (:unread (if (cdr these) "no words could be read from their pictures" "no words could be read from its picture"))
                                                 (:joined (if (cdr these) "their lines run together, too close to be told apart" "its lines run together, too close to be told apart"))
                                                 (:few "too few characters to tell how alike prints of one are")
                                                 (:ruled "characters could not be found among the rules")
                                                 (:uneven "prints of one character are too unlike each other to measure, as where light, focus or sharpening change across a photograph")
                                                 (:faint "the scan's grain is too faint to tell prints apart")
                                                 (:drawn (if (cdr these)
                                                            "their paper and ink as flat as a program draws them, or as an app's clipping leaves them"
                                                            "its paper and ink as flat as a program draws them, or as an app's clipping leaves them"))))
                               :rule "Copies are told by the grain of the scan, which no two prints of a character share. Where characters of the same shape differ by less than 4 grey levels on a scanned page (a picture drawn rather than scanned, made black and white, or smoothed) every print looks like a copy, and none is told on it; nor where there are too few characters to tell how alike prints are, as on a page whose characters touch the rules of a table so that they cannot be cut apart from them, or whose lines are set so close that they run together; nor where fewer than 40 characters have a print of their shape within 30 grey levels, as on a photograph whose light, focus or sharpening change across the page; nor on a picture whose blank paper strays by less than half a grey level and whose characters' cores by a grey level or less, as a program draws a page or an app's clipping leaves one. Every reason that holds for a page is given, but for a page of which no words could be read, which is said alone, and what was measured is said once, under the first reason it belongs to; lines run together are named only where fewer than half the characters the page's ink holds were read. Each page is measured apart, so that a page of a file put together from two sources is not judged by the other's grain. Nothing is judged on such a page: neither copies nor characters without grain of their own."
                               :revision (revision-index revision) :page (first (first these))
                               :objects (remove-duplicates (mapcar (lambda (s) (mark-object (scan-picture-mark s))) scans))
                               :details (loop for (page nil alike sampled near middle scan) in these
                                              for said = (case reason
                                                           (:faint (format nil "page ~d: characters of the same shape alike to ~,1f grey levels; 4 are needed" page alike))
                                                           (:uneven (format nil "page ~d: of ~:d characters set beside others, ~:d had one of their shape within 30 grey levels~@[, alike to ~,1f~]; 40 are needed"
                                                                            page sampled near middle))
                                                           (:drawn (format nil "page ~d: its blank paper strays by ~,1f grey levels and its characters' cores by a grey level or less~@[; characters of the same shape alike to ~,1f~]"
                                                                           page (scan-picture-paper scan) alike))
                                                           ((:few :ruled :joined)
                                                            (and sampled (< sampled 40) (format nil "page ~d: ~:d character~:p set beside others; 40 are needed" page sampled))))
                                              when (and said (not (member said details :test #'string=)) (not (member said told :test #'string=)))
                                                collect said into details
                                              finally (setf told (append details told)) (return details)))
                        out)))))))
      ;; pages of which only some of the characters were found
      (loop for ((nil . page) alike measured) in pictures
            for scan = (find page scans :key (lambda (s) (page-index (scan-picture-page s))))
            for about = (and scan (scan-picture-characters scan))
            for bands = (if scan (scan-picture-joined scan) 0)
            when (and alike (>= alike 4) about (or (plusp bands) (< measured (* *scan-read-least* about))))
              do (push (found :record (if (plusp bands)
                                          (format nil "Copies and grain checked on ~:d of about ~:d characters on page ~d: ~:d band~:p of lines run together ~:[was~;were~] left unread, the rest of the page read."
                                                  measured about (1+ page) bands (> bands 1))
                                          (format nil "Copies and grain checked on ~:d of about ~:d characters on page ~d: the rest could not be cut from the rules, from one another or from lines run together."
                                                  measured about (1+ page)))
                              :rule "Characters are cut apart where a column holds no ink, and rules are taken out first. Characters set against the rules of a table on both sides, or run together, cannot always be cut apart; nor can lines set so close that their small letters run into one another, or crossed by a mark such as a signature, which are left unread where they meet, the rest of the page read. Such characters are not set beside others, and a copy among them goes unseen. How many characters a page holds is reckoned from its ink once the rules are taken out, over its characters' middle ink; the reckoning strays, honest pages of small print being read at three fifths of it, so a page is named when fewer than half are read, and whenever lines run together were left unread."
                              :revision (revision-index revision) :page (1+ page)
                              :objects (list (mark-object (scan-picture-mark scan))))
                       out))
      (when (plusp left)
        (push (found :record (format nil "~:d strip~:p of the scanned pages' characters not set beside others: exact copies among them may be missed." left)
                     :rule "Exact copies are told by strips of characters the same pixel for pixel. A file of more characters than can be counted so, or a character more than 1,000 pixels across, has strips that are not set beside others; copies among them can still be told by their likeness."
                     :revision (revision-index revision) :page (1+ (page-index (scan-picture-page (first scans))))
                     :objects (remove-duplicates (mapcar (lambda (s) (mark-object (scan-picture-mark s))) scans)))
              out))
      (when not-judged
        (let ((pages (sort (remove-duplicates (mapcar #'glyph-page not-judged)) #'<)))
          (push (found :record (format nil "~a, on page~p ~{~d~#[~; and ~:;, ~]~}." (not-judged-words not-judged) (length pages) (mapcar #'1+ pages))
                       :rule "Ink a scanner clipped to one grey is the same in every print, and would make any two prints alike: it is set aside when characters are set beside each other, and a character with too few pixels left beside every other is not judged, neither a copy nor a print."
                       :revision (revision-index revision) :page (1+ (first pages))
                       :objects (remove-duplicates (mapcar #'glyph-picture not-judged))
                       :details (mapcar (lambda (g) (glyph-words scans g)) (subseq not-judged 0 (min 50 (length not-judged)))))
                out)))
      ;; scanned pictures whose pixels were not read, by why
      (dolist (why (remove-duplicates (mapcar #'third not-read) :test #'equal))
        (let* ((these (remove why not-read :key #'third :test-not #'equal))
               (pages (sort (remove-duplicates (mapcar (lambda (u) (1+ (page-index (first u)))) these)) #'<)))
          (push (found :record (format nil "The scanned picture~:[~;s~] on page~p ~{~d~#[~; and ~:;, ~]~} ~:[was~;were~] not read (~:[~;of each, ~]~a): no copies, grain, words unlike the rest or JPEG blocks were looked for there."
                                     (rest these) (length pages) pages (rest these) (rest these) why)
                       :rule "The pixels of a scanned page are read to look for characters copied within it, words unlike the rest and pieces saved apart. A picture kept in a form not read here (JBIG2, fax), larger than *palimpsest-scan-pixels* allows, or that the sandbox could not turn into pixels, is named rather than passed over in silence: nothing is claimed of it either way."
                       :revision (revision-index revision) :page (first pages)
                       :objects (remove-duplicates (mapcar (lambda (u) (mark-object (second u))) these))
                       :details (loop for (page mark) in these
                                      collect (format nil "page ~d: the picture ~a (object ~d)" (1+ (page-index page)) (mark-text mark) (mark-object mark)))
                       :evidence (loop for (nil mark) in these repeat 10 append (nth-value 0 (object-evidence pdf revision (mark-object mark)))))
                out)))
      (when marks
        (let ((pages (sort (remove-duplicates (loop for group in marks append (mapcar #'glyph-page group))) #'<))
              (pictures (remove-duplicates (loop for group in marks append (mapcar #'glyph-picture group)))))
          (push (found :look (format nil "~:d marks on page~p ~{~d~#[~; and ~:;, ~]~} are the same pixel for pixel, with no grain of their own: drawn by a program, or prints the scanner clipped."
                                     (loop for group in marks sum (length group)) (length pages) (mapcar #'1+ pages))
                       :rule "A scanner sets each pixel of a print in part by chance, by the grain of the paper and where the blur falls, and so prints no character twice alike. A mark whose ink strays by a grey level or less (or a fifth of what the page's characters typically stray) has no grain of its own: two of them the same pixel for pixel were drawn by a program, or are prints that the scanner clipped to one grey alike."
                       :revision (revision-index revision) :page (1+ (first pages)) :objects pictures
                       :details (loop for group in marks
                                      collect (format nil "~{~a~^, ~}: the same pixel for pixel, with no grain of their own, the scan's characters of the same shape alike to ~,1f"
                                                      (mapcar (lambda (g) (glyph-words scans g)) group) (reduce #'min group :key #'glyph-alike)))
                       :evidence (loop for p in pictures append (nth-value 0 (object-evidence pdf revision p)))
                       :method :typed-into-scan)
                out)))
      (let ((by-page (make-hash-table)))
        (dolist (entry bare) (push entry (gethash (glyph-page (first (third entry))) by-page)))
        (dolist (page (sort (loop for p being the hash-keys of by-page collect p) #'<))
          (let* ((entries (sort (gethash page by-page) #'< :key (lambda (e) (word-box-y0 (first (second e))))))
                 (lines (count :line entries :key #'first)) (words (count :word entries :key #'first))
                 (pictures (remove-duplicates (loop for e in entries append (mapcar #'glyph-picture (third e))))))
            (push (found :look (format nil "On page ~d, ~@[~a~]~:[~; and ~]~@[~a~] of the scanned page ~:[has~;have~] no grain of ~:[its~;their~] own: drawn into the scan by a program, or in bold or heavier type, or ink, a stamp or coloured ink the scanner clipped."
                                       (1+ page)
                                       (and (plusp lines) (if (= lines 1) "a line" (format nil "~d lines" lines)))
                                       (and (plusp lines) (plusp words))
                                       (and (plusp words) (if (= words 1) "a word" (format nil "~d words" words)))
                                       (> (+ lines words) 1) (> (+ lines words) 1))
                         :rule "A scanner sets each pixel of a print in part by chance, by the grain of the paper and where the blur falls, so that the core of every print (its pixels no lighter than halfway from its darkest to its lightest grey) strays by some grey levels, however wide its strokes. A word or a line whose characters' cores stray by a grey level or less, or by a fifth of what the page's characters typically stray, was drawn into the picture by a program, as a picture editor's text tool draws text; or its ink is one grey for another reason: a scanner that clips dark ink, a stamp or a coloured ink to one level, or a type heavier than the page measures it against."
                         :revision (revision-index revision) :page (1+ page) :objects pictures
                         :details (loop for (kind ws members) in entries
                                        collect (let* ((scan (scan-of-glyph scans (first members))) (m (scan-picture-mark scan))
                                                       (tilt (or (scan-picture-tilt scan) 0)) (first-word (first ws)) (last-word (car (last ws))))
                                                  (destructuring-bind (x0 y0 x1 y1) (word-page-box m first-word (scan-picture-width scan) (scan-picture-height scan) (scan-picture-tilt scan))
                                                    (declare (ignore x1 y1))
                                                    (format nil "~:[a word~;a line of ~:*~d words~] at x ~d, y ~d (pixels ~d,~d to ~d,~d of picture ~d): ~:d characters, none with grain of its own"
                                                            (and (eq kind :line) (length ws)) (round x0) (round y0)
                                                            (word-box-x0 first-word) (round (+ (word-box-y0 first-word) (* tilt (word-box-x0 first-word))))
                                                            (word-box-x1 last-word) (round (+ (word-box-y1 last-word) (* tilt (word-box-x1 last-word))))
                                                            (mark-object m) (length members)))))
                         :evidence (loop for p in pictures append (nth-value 0 (object-evidence pdf revision p)))
                         :method :typed-into-scan)
                  out))))
      (nreverse out))))

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
  "For each way an 8-by-8 block can sit on the picture GLYPH is in, how
flat the highest frequencies are of the most JPEG-like block of GLYPH's
cut (GLYPH-CUT) sitting that way: of the 28 frequencies a JPEG of quality
75 or less sets to nothing (across and down adding up to 8 or more), how
many are within *BLOCK-FLAT-WITHIN* of nothing; a block of which fewer than
two of the four highest are is counted as none, the rest not worked out.
As two values: a vector of 64, the block's left edge's column within every
eighth plus eight times its top row's, -1 for a way no block of the cut
sits but those passed over; and ROWS, room for the work, given again for the next character so
that it is made once. A block more than an eighth of whose pixels are
black or white, the paper or ink clipped, is passed over: it is flat
however it was saved."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)) (down (floor (length pixels) across))
         (starts (- across 7)) (basis *block-basis*) (within (float *block-flat-within* 1d0))
         (scores (make-array 64 :element-type '(signed-byte 8) :initial-element -1)))
    (declare (type grey-pixels pixels) (type (integer 1 100000) across) (type (integer 0 100000) down)
             (type fixnum starts) (type (simple-array double-float (64)) basis) (type double-float within) (optimize speed))
    (when (and (plusp starts) (>= down 8))
      (when (< (length rows) (* down starts 8))
        (setf rows (make-array (* down starts 8 2) :element-type 'double-float)))
      (let ((rows rows) (left (mod (- (the fixnum (glyph-x0 glyph)) 2) 8)) (top (mod (- (the fixnum (glyph-y0 glyph)) 2) 8))
            (stride (* starts 8)))
        (declare (type (simple-array double-float (*)) rows) (type (integer 0 7) left top) (type fixnum stride))
        (flet ((row (j i u)
                 ;; the Uth frequency of the eight pixels of row J from column I, kept at its place in ROWS
                 (let ((at (+ (* j across) i)) (k (* u 8)) (sum 0d0))
                   (declare (type fixnum at k) (type double-float sum))
                   (dotimes (x 8) (incf sum (* (aref basis (+ k x)) (aref pixels (+ at x)))))
                   (setf (aref rows (+ (* j stride) (* i 8) u)) sum)))
               (flat-p (first u v)
                 ;; true when the frequency U across and V down of the block whose first row's work is at FIRST is nothing
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
                     (flat (+ (if (flat-p first 7 7) 1 0) (if (flat-p first 6 7) 1 0) (if (flat-p first 7 6) 1 0) (if (flat-p first 6 6) 1 0)))
                     (way (+ (mod (+ left i0) 8) (* 8 (mod (+ top j0) 8)))))
                (declare (type fixnum first flat) (type (integer 0 63) way))
                (when (<= (loop for y below 8
                                sum (let ((at (+ (* (+ j0 y) across) i0)))
                                      (declare (type fixnum at))
                                      (loop for x below 8 count (let ((g (aref pixels (+ at x)))) (or (= g 0) (= g 255)))))
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
    (values scores rows)))

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
  "A region of a scanned picture whose JPEG traces are unlike the rest of
the picture's: PAGE and PICTURE, the page index and picture object it is
on; X0 Y0 X1 Y1 its pixels in the picture (the last excluded), from the top
left; KIND, :OWN when it carries JPEG blocks of its own and the picture
none, :OFF-GRID when it lacks the blocks the picture carries on its grid;
STRENGTH, :STRONG or :LOOK (JPEG-PIECES); FLAT, how many of the 28 finest
frequencies are flat, of its flattest block on its own grid (:OWN) or of
its blocks on the picture's grid (:OFF-GRID); ABOVE, for :OWN, how many
more than at its other placements; USUAL, the same of the picture's
characters typically; WAY, where its blocks sit (:OWN) or the picture's do
\(:OFF-GRID), as (COLUMN ROW) within every eighth; CHARACTERS, how many
characters it holds; SHOWING, how many of them carry blocks there; LAID,
true when it lies at the same place on two pages or more, or is taller
by half than the taller type of its page, as a stamp, a logo or a
signature laid on the pages by software is (LAID-PIECES); BACKING, for an
:OWN region, what else on it agrees (BACKED-PIECES): :COPIES, characters
within it copies of others the same pixel for pixel; :WORDS, a word within
it unlike the rest both in how it looks and where it sits; :ALONE, nothing,
and it is worth a look though its blocks alone would make it strong."
  page picture x0 y0 x1 y1 kind strength flat (above 0) usual way (characters 1) (showing 1) laid backing)

(defun counted-middle (counts)
  "The middle of the values COUNTS counts, a vector whose Kth holds how many
are K, as MEDIAN-OF finds it; NIL for none."
  (let ((n (reduce #'+ counts)))
    (when (plusp n)
      (let ((wanted (1+ (floor n 2))) (seen 0))
        (dotimes (k (length counts))
          (incf seen (aref counts k))
          (when (>= seen wanted) (return k)))))))

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
GLYPH-BLOCK-SCORES, as four values: the placement (0 to 63) whose blocks
are typically flattest, or WAY when given (a JPEG's own, 0); how many of
its 28 finest frequencies typically are; whether the picture carries a JPEG
grid there: typically *BLOCK-GRID-LEAST* flat or more, *BLOCK-GRID-ABOVE*
more than the placements typically are; and how far the characters' blocks
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
           (best (or way (let ((top (reduce #'max (substitute -1 nil typicals)))) (and (>= top 0) (position top typicals)))))
           (typical (or (and best (nth best typicals)) -1))
           (middle (median-of (or (remove nil typicals) '(0))))
           (spread (if (and best (>= typical 0))
                       (let ((strays (make-array 29 :initial-element 0)))
                         (loop for v from 0 below 29 do (incf (aref strays (abs (- v typical))) (aref (svref counts best) v)))
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
                  (> (* 2 w h) (min (* (- (block-piece-x1 a) (block-piece-x0 a)) (- (block-piece-y1 a) (block-piece-y0 a)))
                                    (* (- (block-piece-x1 b) (block-piece-x0 b)) (- (block-piece-y1 b) (block-piece-y0 b)))))))))
    (dolist (p pieces pieces)
      (let ((upper (gethash (cons (block-piece-picture p) (block-piece-page p)) heights)))
        (setf (block-piece-laid p)
              (or (and upper (> (- (block-piece-y1 p) (block-piece-y0 p) 4) (* 3/2 upper)))
                  (some (lambda (o) (and (not (eql (block-piece-page o) (block-piece-page p))) (same-place-p o p))) pieces)))))))

(defun glyph-in-piece-p (piece glyph)
  "True when the middle of GLYPH lies within PIECE (a BLOCK-PIECE), on its page and picture."
  (let ((x (/ (+ (glyph-x0 glyph) (glyph-x1 glyph)) 2)) (y (/ (+ (glyph-y0 glyph) (glyph-y1 glyph)) 2)))
    (and (eql (glyph-page glyph) (block-piece-page piece)) (eql (glyph-picture glyph) (block-piece-picture piece))
         (<= (block-piece-x0 piece) x) (< x (block-piece-x1 piece)) (<= (block-piece-y0 piece) y) (< y (block-piece-y1 piece)))))

(defun copies-on-piece-grid (copies pieces)
  "COPIES (groups of COPIED-GLYPHS), each strong group whose characters lie
each a whole number of blocks of 8 pixels from the others across and down,
and all within one region of PIECES that carries JPEG blocks of its own, or
carrying a JPEG's block at one placement (SUPPORTED-WAYS), two of them at
least and every one whose blocks could be measured, made worth a look, on
the JPEG's own grid (WHY :GRID): a JPEG keeps the same character drawn
twice on its grid the same pixel for pixel, as the repeated figures of a
date stamp kept as a JPEG and laid into a page kept without loss are.
COPIES."
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
                     (or (some (lambda (p) (and (eq (block-piece-kind p) :own) (every (lambda (g) (glyph-in-piece-p p g)) group))) pieces)
                         (let ((measured (loop for g in group
                                               for (ways seen) = (multiple-value-list (ways g))
                                               when seen collect ways)))
                           (and (>= (count-if #'identity measured) 2)
                                (every #'identity measured)
                                (some (lambda (w) (every (lambda (ws) (some (lambda (v) (near-ways-p v w)) ws)) measured))
                                      (find-if #'identity measured))))))
            (setf (fifth c) :look (sixth c) :grid)))))))

(defparameter *stamp-ink-apart* 60
  "How far apart, at least, the colour of a stamp's ink lies from the
typical colour of its page's characters (the root of the summed squares
of the differences of red, green and blue, each 0 to 255), and four times
as far as those characters' colours typically lie from it: red, blue or
green ink on black type, or a grey far lighter than the type's.")

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
                 (apart (max *stamp-ink-apart* (* 4 (median-of (mapcar (lambda (c) (colour-distance c middle)) colours)))))
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
                      (setf (gethash g stamp-of) (or (gethash (root g) numbers) (setf (gethash (root g) numbers) (incf stamps)))))))))))))
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
  "True when CHARACTERS, those of a stamp, are as a program draws them: the
middle of how far their cores stray (GLYPH-CORE-SPREAD, the median
deviation of their pixels no lighter than halfway from the darkest to the
lightest of their boxes) is under *STAMP-GRAIN-MOST* of how far their
page's characters' cores typically stray, as no stamp pressed on paper and
scanned, carrying the page's grain, is; the stamp holds two shapes or more
repeated, and every character with another
of its shape in the stamp is in the same group of copies as all the others
of that shape (GROUP-OF, a character's group of copies: the same pixel for
pixel, or nearer one another than prints of a character are, as where the
paper shows through a stamp laid over it). A program draws every print of a
shape alike, and a JPEG it is saved as keeps them so, its losses the same
on each; no two prints of a rubber stamp pressed on paper and scanned are,
the scanner's grain apart in each, so a figure copied over another in it
leaves the others of its shape unlike it. Of the same shape are characters of one size, a pixel either
way, alike to three times their page's characters of one shape (twenty grey
levels at least); a stamp of more than 200 characters is no stamp."
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
                          (progn (unless (glyph-darker a) (glyph-measured a)) (unless (glyph-darker b) (glyph-measured b))
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
lines, in degrees, as a hash table by WORD-BOX: the line through the
middles of its characters' bottoms, for a word of four characters or more
\(fewer, a letter's tail below the line could turn it), against the
page's, the middle of its words' (none for a word of fewer characters). A stamp laid turned turns its characters' line with it,
however each is cut."
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
                           (* (/ 180 pi) (atan (/ (reduce #'+ (mapcar (lambda (x y) (* (- x mx) (- y my))) xs ys)) across))))))))
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
:OWN one kept strong only when another sign agrees on its region: a
character within it one of a group of COPIES (COPIED-GLYPHS) the same pixel
for pixel, a strong sign, whose others lie outside it (copies all within it
may be a stamp's repeated figures); else a word within it unlike the rest
both in how it looks and where it sits, on a picture of SCANS whose grain
was judged.
One that nothing else agrees on is worth a look, its BACKING :ALONE: a
stamp, a date or a patch kept as a JPEG and laid into a page kept without
loss by honest software carries the same blocks. PIECES."
  (flet ((inside-p (p page picture x0 y0 x1 y1)
           ;; the middle of the box X0 Y0 X1 Y1 of PAGE's PICTURE within P
           (let ((x (/ (+ x0 x1) 2)) (y (/ (+ y0 y1) 2)))
             (and (eql page (block-piece-page p)) (eql picture (block-piece-picture p))
                  (<= (block-piece-x0 p) x) (< x (block-piece-x1 p)) (<= (block-piece-y0 p) y) (< y (block-piece-y1 p))))))
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
                                                 (inside-p p (page-index (scan-picture-page scan)) (mark-object (scan-picture-mark scan))
                                                           (word-box-x0 word) (word-box-y0 word) (word-box-x1 word) (word-box-y1 word)))))
                                        (scan-picture-odd scan))))
                           scans)
                     :words)
                    (t :alone)))
        (when (eq (block-piece-backing p) :alone)
          (setf (block-piece-strength p) :look))))))

(defun jpeg-pieces (glyphs &key jpeg)
  "The regions of a scanned picture, from its characters GLYPHS, whose JPEG
traces are unlike the rest of the picture's (BLOCK-PIECEs); JPEG true for
a picture kept as a JPEG, its grid its first pixel's.
On a picture that carries a JPEG grid (PICTURE-BLOCK-GRID): characters
whose blocks on it have *BLOCK-OFF-GRID-MOST* or fewer of their 28 finest
frequencies flat, and six times the characters' spread fewer than they
typically do, laid in after the picture was saved as a JPEG: worth a look,
for a region of 3 characters or more; none when more than a twentieth of
the characters are so.
On a picture kept without loss, its blocks typically far from flat: the
characters that carry a JPEG block at a placement (SUPPORTED-WAYS),
pieces saved as JPEGs of their own and laid back. A region is named when 3
or more of its characters, and half of those with a block there, carry
blocks at one placement (or a pixel from it), and no more than a quarter
of the characters within 40 pixels about it, whose blocks there are
typically *BLOCK-PIECE-ABOVE* less flat than the region's: strong, kept
so only when another sign agrees on it (BACKED-PIECES), when fewer than
one in fifty of the picture's other characters do and
half of them or more have blocks measured (a page drawn on white has its
blocks passed over), else worth a look. A character or two alone are not told, and nothing is when more than
a twentieth of the picture's characters carry such blocks: a scan smooth
in places, or a JPEG made larger, smaller or turned.
A picture smooth at every placement, or a JPEG without a grid of its
blocks, tells nothing. Characters within 16 pixels of each other across
\(a letter between them, as in a word cut out) and 3 down are one
region. NIL for a picture of fewer than 40
characters, too few to tell its blocks."
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
                    (regions (join-block-pieces (loop for g in glyphs for w in ways
                                                      when w collect (piece g :kind :own :strength :look :flat 0 :usual usual))
                                                16)))
               (when (> marked (* 1/20 n))
                 (return-from jpeg-pieces nil))
               (dolist (r regions (remove-if-not #'block-piece-strength regions))
                 (let ((inside '()) (outside '()) (around '()) (measured 0))
                   ;; every character of the region's box, marked or not; those within 40 pixels of it; and
                   ;; the marked ones of the rest of the picture
                   (flet ((within (g margin)
                            (and (< (- (glyph-x0 g) 2 margin) (block-piece-x1 r)) (> (+ (glyph-x1 g) 2 margin) (block-piece-x0 r))
                                 (< (- (glyph-y0 g) 2 margin) (block-piece-y1 r)) (> (+ (glyph-y1 g) 2 margin) (block-piece-y0 r)))))
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
                     (let ((showing (count-if (lambda (e) (loop for v below 64 thereis (and (near-ways-p v best) (>= (aref (first e) v) 0))))
                                              inside))
                           (elsewhere (count-if (lambda (ws) (some (lambda (v) (near-ways-p v best)) ws)) outside))
                           ;; the characters about it: how many have a block there, and how many carry one
                           (near (count-if (lambda (e) (loop for v below 64 thereis (and (near-ways-p v best) (>= (aref (first e) v) 0)))) around))
                           (near-carrying (count-if (lambda (e) (some (lambda (v) (near-ways-p v best)) (second e))) around))
                           ;; how flat the blocks about it typically are there
                           (near-flat (median-of (or (loop for (s) in around
                                                           for v = (loop for w below 64 when (near-ways-p w best) maximize (aref s w))
                                                           when (>= v 0) collect v)
                                                     '(0)))))
                       (setf (block-piece-way r) (list (mod best 8) (floor best 8))
                             (block-piece-characters r) (length inside)
                             (block-piece-showing r) most
                             (block-piece-flat r) (loop for (s w) in inside
                                                        maximize (or (loop for v in w when (near-ways-p v best) maximize (aref s v)) 0))
                             (block-piece-above r) (loop for (s w) in inside
                                                         maximize (or (loop for v in w when (near-ways-p v best) maximize (block-support s v)) 0)))
                       (setf (block-piece-strength r)
                             (cond ((or (< most 3) (< (* 2 most) showing) (> (* 4 near-carrying) near)) nil)
                                   ((> near-flat (- (block-piece-flat r) *block-piece-above*)) nil)
                                   ;; the rest of the picture measured, not passed over as black and white
                                   ((and (< elsewhere (* 1/50 (- n (length inside)))) (>= (* 2 measured) (- n (length inside)))) :strong)
                                   (t :look)))))))))))))))

(defun join-block-pieces (pieces &optional (across 3))
  "PIECES (BLOCK-PIECEs of one picture, of one kind) with those whose boxes
touch or are within ACROSS pixels of each other across and three down made
one, its box round theirs, its FLAT the most of theirs (the fewest for
:OFF-GRID ones); the rest of what it says of them JPEG-PIECES works out
for the region."
  (flet ((near-p (a b)
           (and (<= (block-piece-x0 a) (+ (block-piece-x1 b) across)) (<= (block-piece-x0 b) (+ (block-piece-x1 a) across))
                (<= (block-piece-y0 a) (+ (block-piece-y1 b) 3)) (<= (block-piece-y0 b) (+ (block-piece-y1 a) 3))))
         (into (a b)
           (setf (block-piece-x0 a) (min (block-piece-x0 a) (block-piece-x0 b)) (block-piece-y0 a) (min (block-piece-y0 a) (block-piece-y0 b))
                 (block-piece-x1 a) (max (block-piece-x1 a) (block-piece-x1 b)) (block-piece-y1 a) (max (block-piece-y1 a) (block-piece-y1 b))
                 (block-piece-flat a) (funcall (if (eq (block-piece-kind a) :own) #'max #'min) (block-piece-flat a) (block-piece-flat b))
                 (block-piece-characters a) (+ (block-piece-characters a) (block-piece-characters b)))))
    (let ((out '()))
      (dolist (p pieces)
        ;; P joins every region it is near, and they one another through it
        (let ((near (remove-if-not (lambda (o) (near-p p o)) out)))
          (setf out (set-difference out near))
          (dolist (o near) (into p o))
          (push p out)))
      (sort out (lambda (a b) (or (< (block-piece-y0 a) (block-piece-y0 b))
                                  (and (= (block-piece-y0 a) (block-piece-y0 b)) (< (block-piece-x0 a) (block-piece-x0 b)))))))))


(defun block-piece-page-box (mark piece width height)
  "The box (X0 Y0 X1 Y1) on the page of PIECE (a BLOCK-PIECE), found in the
picture MARK draws, WIDTH by HEIGHT pixels (PICTURE-PAGE-BOX)."
  (picture-page-box mark (block-piece-x0 piece) (block-piece-y0 piece) (block-piece-x1 piece) (block-piece-y1 piece) width height))

(defun picture-jpeg-p (pdf revision number)
  "True when the picture object NUMBER of REVISION is kept as a JPEG: its
filter, or one of its filters, is /DCTDecode."
  (let* ((object (revision-object pdf revision number))
         (filter (and object (deref pdf revision (pdf-get (pdf-indirect-value object) "/Filter")))))
    (some (lambda (f) (and (stringp f) (member f '("/DCTDecode" "/DCT") :test #'string=)))
          (if (consp filter) (pdf-array-items filter) (list filter)))))

(defparameter *block-piece-words*
  '(((:own :strong) "carries JPEG blocks of its own, which the rest of the scan lacks, and another sign of editing on the same place: a piece saved apart and laid in"
     "carry JPEG blocks of their own, which the rest of the scan lacks, and another sign of editing on the same places: pieces saved apart and laid in"
     "A JPEG keeps a picture as blocks of 8 by 8 pixels and sets most of each block's finest detail to nothing; a scan kept without loss keeps its grain in every block. Several characters of one region whose blocks are flat at one placement, and far less flat a pixel or more from it, carry a JPEG's grid of their own, which no other part of the page does: they were saved as a JPEG on their own and laid back. Here another sign agrees on the same region: characters within it are copies of others the same pixel for pixel, or a word within it is unlike the rest both in how it looks and where it sits. Together they tell of a piece cut out to be changed or moved. A smooth or blurred patch of a scan is flat however a block sits on it.")
    ((:own :alone) "carries JPEG blocks of its own, which the rest of the scan lacks: a piece saved apart and laid in, or a stamp, a date or a patch kept as a JPEG and laid into the page by the software that made it"
     "carry JPEG blocks of their own, which the rest of the scan lacks: pieces saved apart and laid in, or stamps, dates or patches kept as JPEGs and laid into the page by the software that made it"
     "A JPEG keeps a picture as blocks of 8 by 8 pixels and sets most of each block's finest detail to nothing; a scan kept without loss keeps its grain in every block. Several characters of one region whose blocks are flat at one placement, and far less flat a pixel or more from it, carry a JPEG's grid of their own, which no other part of the page does: they were saved as a JPEG on their own and laid into the page. A piece cut out to be changed or moved is saved so; so is a stamp, a date or a patch of a scan that an office's software keeps as a JPEG and lays into a page kept without loss, honestly, and the pixels do not tell the two apart. Alone, it is worth a look; it is a strong sign only when another sign agrees on the same region, characters within it copies of others the same pixel for pixel, or a word within it unlike the rest both in how it looks and where it sits.")
    ((:own :look) "has a block flatter at one placement than at the others, as a JPEG's are: a piece saved apart and laid in, or the scan smoother there by chance"
     "have blocks flatter at one placement than at the others, as a JPEG's are: pieces saved apart and laid in, or the scan smoother there by chance"
     "A character whose block is flat at one placement and far less flat a pixel or more from it may carry a JPEG's block of its own. Alone, or with few others of its region at that placement, it is a sign worth a look: a single character saved as a JPEG is told only so far.")
    ((:off-grid :look) "lacks the JPEG blocks the rest of the scan carries on its grid: a piece laid in after the scan was saved as a JPEG"
     "lack the JPEG blocks the rest of the scan carries on its grid: pieces laid in after the scan was saved as a JPEG"
     "A scan saved as a JPEG has its blocks of 8 by 8 pixels on one grid across the whole page, and the finest detail of every block set to nothing. A region whose blocks on that grid keep their finest detail was put there after the page was saved: a piece laid in from elsewhere or from the page itself, or text a scanner's software stamped on afterwards."))
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
          (concatenate 'string rule " These stand at the same place on two pages or more, or are taller than the type of their pages: a stamp, a logo or a signature kept as a picture and laid on each page by a program carries the JPEG blocks of the picture it was kept as, as offices do honestly, as well as a piece laid to deceive. Whether such a mark belongs there is for the examiner to judge."))))

(defun check-scan-pieces (pdf revision)
  "Regions of a scanned page whose JPEG traces are unlike the rest of it
\(JPEG-PIECES): pieces cut out of the scan and saved as JPEGs of their own
before they were laid back, their 8-by-8 blocks on a grid of their own
where the page has none, strong only when another sign agrees on the same
region (BACKED-PIECES); or pieces laid in after the scan was saved as a
JPEG, lacking the blocks the rest of the page carries on its grid."
  (multiple-value-bind (scans copies alike pieces) (scans-read pdf revision)
    (declare (ignore copies alike))
    (let ((out '()) (groups '()))
      (dolist (p pieces)
        (let* ((key (list (block-piece-page p) (block-piece-picture p) (block-piece-kind p) (block-piece-strength p) (block-piece-laid p)
                          (eq (block-piece-backing p) :alone)))
               (entry (assoc key groups :test #'equal)))
          (if entry (push p (cdr entry)) (push (list key p) groups))))
      (loop for ((page-index picture kind strength laid alone) . some) in (reverse groups)
            do (let* ((some (reverse some))
                      (scan (find-if (lambda (scan) (and (eql (mark-object (scan-picture-mark scan)) picture)
                                                         (eql (page-index (scan-picture-page scan)) page-index)))
                                     scans))
                      (m (scan-picture-mark scan)) (width (scan-picture-width scan)) (height (scan-picture-height scan))
                      (words (let ((words (cdr (assoc (if alone (list kind :alone) (list kind strength)) *block-piece-words* :test #'equal))))
                               (cond ((not laid) words)
                                     ;; the honest software that lays a piece is named already: the rule says more
                                     (alone (list (first words) (second words) (third (laid-piece-words words))))
                                     (t (laid-piece-words words))))))
                 (labels ((spot (p)
                            ;; where P is on the page
                            (destructuring-bind (x0 y0 x1 y1) (block-piece-page-box m p width height)
                              (declare (ignore x1 y1))
                              (format nil "at x ~d, y ~d" (round x0) (round y0))))
                          (at (p)
                            ;; where P is on the page, and in its picture
                            (format nil "~a (pixels ~d,~d to ~d,~d of picture ~d)" (spot p)
                                    (block-piece-x0 p) (block-piece-y0 p) (block-piece-x1 p) (block-piece-y1 p) picture)))
                   (push (found strength
                                (if (rest some)
                                    (format nil "~d regions of page ~d, in the scanned picture ~a (object ~d), ~a." (length some) (1+ page-index) (mark-text m) picture
                                            (second words))
                                    (let ((p (first some)))
                                      (format nil "A region of page ~d, about ~d by ~d pixels ~a, in the scanned picture ~a, ~a." (1+ page-index)
                                              (- (block-piece-x1 p) (block-piece-x0 p)) (- (block-piece-y1 p) (block-piece-y0 p)) (spot p) (mark-text m)
                                              (first words))))
                                :rule (third words)
                                :revision (revision-index revision) :page (1+ page-index) :objects (list picture)
                                :details (loop for p in some
                                               collect (if (eq kind :own)
                                                           (format nil "~a, ~d character~:p, ~d with a block at column ~d, row ~d of every eight pixels: up to ~d of its 28 finest frequencies flat, ~d more than at its other placements; the scan's characters' flattest typically ~d"
                                                                   (at p) (block-piece-characters p) (block-piece-showing p)
                                                                   (first (block-piece-way p)) (second (block-piece-way p))
                                                                   (block-piece-flat p) (block-piece-above p) (block-piece-usual p))
                                                           (format nil "~a, ~d character~:p: its blocks on the scan's grid with ~d of their 28 finest frequencies flat, the scan's characters' typically ~d"
                                                                   (at p) (block-piece-characters p) (block-piece-flat p) (block-piece-usual p))))
                                :evidence (nth-value 0 (object-evidence pdf revision picture))
                                :method :piece-laid-in)
                         out))))
      (nreverse out))))
