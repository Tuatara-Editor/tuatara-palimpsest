;;;; pixels.lisp --- The pixels of a scanned page: its grey levels, lines and words.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A page scanned in one go has one ink, one blur and one paper throughout.
;;;; Its picture is read as grey levels, made black and white at the level
;;;; that best parts ink from paper, cut into lines and the lines into words;
;;;; each word's ink, edges and paper are measured, and the words unlike all
;;;; the rest named: pasted into the scan, or printed over it.

(in-package #:palimpsest)

(define-package-setting *palimpsest-scan-pixels* (* 60 1000 1000)
  "The most pixels a picture may have for its words to be measured.
A page scanned at 600 dots an inch is some 35 million. Reading a picture
takes about a dozen octets of memory for each of its pixels while it is read,
some 700 MB at the most allowed by default.
- a whole number, 1 or more: the pixels, 60 million by default."
  :values (integer 1 *))

(deftype grey-pixels () '(simple-array (unsigned-byte 8) (*)))

;;; Grey levels

(defun colour-space-of (pdf revision value &key (palette t))
  "The colour space VALUE names, as three values: :grey, :rgb, :cmyk or
:indexed; for :indexed (BASE HIGHEST TABLE), BASE the kind of its colours and
TABLE their octets; and, for a space not read here (NIL), why not in words.
With PALETTE false, as for the colours a palette is made of, a palette is not
read."
  (let ((space (deref pdf revision value)))
    (cond ((null space) nil)
          ((stringp space)
           (cond ((member space '("/DeviceGray" "/G" "/CalGray") :test #'string=) :grey)
                 ((member space '("/DeviceRGB" "/RGB" "/CalRGB") :test #'string=) :rgb)
                 ((member space '("/DeviceCMYK" "/CMYK") :test #'string=) :cmyk)))
          ((consp space)
           (let* ((items (pdf-array-items space)) (name (first items)))
             (cond ((name-p name "/ICCBased")
                    (let* ((object (and (pdf-ref-p (second items))
                                        (revision-object pdf revision (second (second items)))))
                           (n (and object (pdf-get (pdf-indirect-get object :value) "/N"))))
                      (case n (1 :grey) (3 :rgb) (4 :cmyk))))
                   ((name-p name "/CalGray") :grey)
                   ((name-p name "/CalRGB") :rgb)
                   ((member name '("/Separation" "/DeviceN") :test #'equal)
                    (values nil nil (format nil "its colours are the named inks of a ~a space" (subseq name 1))))
                   ((and palette (member name '("/Indexed" "/I") :test #'equal))
                    (palette-of pdf revision items))))))))

(defun palette-of (pdf revision items)
  "The palette the ITEMS of an /Indexed colour space make, as COLOUR-SPACE-OF
gives it: :indexed and (BASE HIGHEST TABLE); or NIL, NIL and why not, in
words. Its base is never a palette itself, and a palette written as a string
must hold an octet for each part of each of its colours."
  (let* ((base (colour-space-of pdf revision (second items) :palette nil))
         (highest (deref pdf revision (third items)))
         (lookup (deref pdf revision (fourth items)))
         (parts (case base (:grey 1) (:rgb 3) (:cmyk 4)))
         (table (cond ((not (and parts (typep highest '(integer 0 255)))) nil)
                      ((stringp lookup)
                       (and (>= (length lookup) (* parts (1+ highest)))
                            (every (lambda (c) (< (char-code c) 256)) lookup)
                            (map '(simple-array (unsigned-byte 8) (*)) #'char-code lookup)))
                      ((and (pdf-ref-p (fourth items)) (revision-object pdf revision (second (fourth items))))
                       (multiple-value-bind (data why)
                           (stream-data pdf (revision-object pdf revision (second (fourth items))))
                         (and (null why) data))))))
    (if table
        (values :indexed (list base highest table))
        (values nil nil "its palette cannot be read"))))

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
             (let ((value (case bits
                            (8 (aref data (+ (* y row) index)))
                            (16 (aref data (+ (* y row) (* index 2))))
                            (t (let ((bit (* index bits)))
                                 (ldb (byte bits (- 8 bits (mod bit 8))) (aref data (+ (* y row) (ash bit -3)))))))))
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

(defun picture-sample-bits (pdf revision dict)
  "The bits each pixel of the picture whose dictionary is DICT takes in its
samples, as its depth and colour space give them: 64, four colours of 16
bits, when they do not say."
  (if (pdf-true-p (pdf-get dict "/ImageMask"))
      1
      (let ((bits (pdf-get dict "/BitsPerComponent")))
        (* (if (member bits '(1 2 4 8 16)) bits 16)
           (case (colour-space-of pdf revision (pdf-get dict "/ColorSpace"))
             ((:grey :indexed) 1) (:rgb 3) (t 4))))))

(defvar *picture-file* nil
  "The picture file examined as one scanned page while its findings are
made, a PICTURE-FILE; NIL while a PDF's are. The page's checks then read its
pixels, call it the picture and name no object or picture number.")

(defparameter *picture-kinds* '((:png . "PNG") (:jpeg . "JPEG") (:tiff . "TIFF"))
  "The kinds of picture file examined as one scanned page, each with its name.")

(defstruct (picture-file (:copier nil) (:predicate nil))
  "A picture file examined as one scanned page. KIND is its kind, a key of
*PICTURE-KINDS*. WIDTH and HEIGHT are its pixels, GREY their grey levels, a
row after another from the top, and COLOURS, for one in colours, its pixels
as (OCTETS . CHANNELS) as PICTURE-GREY gives them (NIL for one in greys).
REFUSAL is why its pixels were not read, in words, or NIL. SECTIONS is what
it says of itself, the sections of a note (OCTETS-METADATA). TABLES is, for
a JPEG, the octets of its quantisation tables, the quality its first stands
for and that table (JPEG-TABLES)."
  kind (width 0) (height 0) grey colours refusal (sections '()) tables)

(defun picture-grey (pdf revision number)
  "The grey levels of the picture object NUMBER of REVISION, as four values:
the levels, a row after another from the top, its width and its height,
and for a picture in colours its colours as (OCTETS . CHANNELS), each
pixel's red, green and blue first of its CHANNELS octets (NIL for one in
greys); or NIL and why not, in words. Raw samples are read here; a JPEG or
a JPEG 2000 is turned into pixels by Tuatara, in the sandbox
\(DECODE-PICTURE). For a picture file examined as a page, those of the
picture file, read once (*PICTURE-FILE*)."
  (let ((file *picture-file*))
    (when file
      (return-from picture-grey
        (if (picture-file-grey file)
            (values (picture-file-grey file) (picture-file-width file) (picture-file-height file)
                    (picture-file-colours file))
            (values nil (picture-file-refusal file))))))
  (let* ((object (revision-object pdf revision number))
         (dict (and object (pdf-indirect-get object :value)))
         (width (and dict (pdf-get dict "/Width"))) (height (and dict (pdf-get dict "/Height"))))
    (unless (and (integerp width) (integerp height) (plusp width) (plusp height))
      (return-from picture-grey (values nil "it gives no size")))
    (when (> (* width height) *palimpsest-scan-pixels*)
      (return-from picture-grey (values nil
                                        (format nil "it has ~:d pixels, more than *palimpsest-scan-pixels* allows"
                                                (* width height)))))
    ;; no more octets than the picture's samples take, by its depth and colours as it gives them (four
    ;; colours of 16 bits when it does not say): a stream that inflates further is not read on
    (multiple-value-bind (data why)
        (stream-data pdf object
                     :limit (+ 1024 (* height (ceiling (* width (picture-sample-bits pdf revision dict)) 8))))
      (cond ((null data) (values nil (or why "its data cannot be read")))
            ((member why '("/DCTDecode" "/DCT" "/JPXDecode") :test #'equal)
             ;; a JPEG 2000 kept in a PDF is a bare codestream or a JP2 file: either is read as :j2k
             (multiple-value-bind (image notes)
                 (decode-picture data (if (equal why "/JPXDecode") :j2k :jpeg) width height)
               (if image
                   (values (image-grey image) (image-width image) (image-height image)
                           (and (>= (image-channels image) 3)
                                (not (eq (colour-space-of pdf revision (pdf-get dict "/ColorSpace")) :grey))
                                (cons (image-pixels image) (image-channels image))))
                   (values nil
                           (string-trim ", "
                                        (or (car (first notes))
                                            (format nil
                                                    "the ~:[JPEG~;JPEG 2000~] could not be decoded"
                                                    (equal why "/JPXDecode"))))))))
            (why (values nil (format nil "its pixels are kept as ~@[~a, ~]~a"
                                     (cdr (assoc why *picture-filter-names* :test #'equal)) why)))
            (t
             (let ((mask (pdf-true-p (pdf-get dict "/ImageMask")))
                   (bits (let ((b (pdf-get dict "/BitsPerComponent"))) (if (integerp b) b 1)))
                   (decode (deref pdf revision (pdf-get dict "/Decode"))))
               (multiple-value-bind (kind table refused)
                   (if mask :grey (colour-space-of pdf revision (pdf-get dict "/ColorSpace")))
                 (if (and kind (member (if mask 1 bits) '(1 2 4 8 16)))
                     (let* ((colours (and (not mask)
                                          (not (eq kind :grey))
                                          (not (and (eq kind :indexed) (eq (first table) :grey)))
                                          (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
                            (grey (raw-grey data width height (if mask 1 bits) kind table
                                            ;; a stencil paints where its samples are 0, unless its Decode says 1
                                            (let ((first-decode (and (consp decode) (first (pdf-array-items decode)))))
                                              (if mask (not (eql first-decode 1)) (eql first-decode 1)))
                                            colours)))
                       (if grey
                           (values grey width height (and colours (cons colours 3)))
                           (values nil "its data is shorter than its size")))
                     (values nil (or refused "its colours are none of grey, RGB, CMYK or a palette of them"))))))))))

;;; Black and white, lines and words

(defun grey-histogram (grey &key (start 0) (end (length grey)) width skipped into)
  "How many of GREY's pixels from START below END are of each grey level, a
vector of 256 whose Kth counts grey K. With SKIPPED, a bit for each row of
GREY, WIDTH wide, the rows whose bit is 1 are left out. The pixels are
counted into INTO when it is given, added to what it holds, else into a new
vector."
  (declare (type grey-pixels grey) (type fixnum start end))
  (let ((counts (or into (make-array 256 :element-type 'fixnum :initial-element 0))))
    (declare (type (simple-array fixnum (256)) counts) (optimize speed))
    (if skipped
        (dotimes (y (length skipped))
          (when (zerop (sbit skipped y))
            (loop for i of-type fixnum from (max start (* y (the fixnum width)))
                    below (min end (* (1+ y) (the fixnum width)))
                  do (incf (aref counts (aref grey i))))))
        (loop for i of-type fixnum from start below end do (incf (aref counts (aref grey i)))))
    counts))

(defun otsu-level (counts)
  "The grey level that best parts the pixels COUNTS counts (GREY-HISTOGRAM)
into dark and light: the one that makes the two groups most unlike (Otsu's
way)."
  (let ((total (reduce #'+ counts)) (sum (loop for i below 256 sum (* i (aref counts i))))
        (below 0) (below-sum 0) (best 0) (level 128))
    (dotimes (i 256 level)
      (incf below (aref counts i))
      (incf below-sum (* i (aref counts i)))
      (let ((above (- total below)))
        (when (and (plusp below) (plusp above))
          (let* ((dark (/ below-sum below)) (light (/ (- sum below-sum) above))
                 (between (* below above (expt (- dark light) 2))))
            (when (> between best) (setf best between level i))))))))

(defparameter *band-paper-share* 19/20
  "Where in a row's grey levels, from the darkest, its paper is read
\(PAPER-BANDS): the level this share of the row's pixels across the text
reach or stay under.")

(declaim (inline counted-level))
(defun counted-level (counts wanted)
  "The least value at or below which WANTED of the values COUNTS counts lie,
a vector whose Kth holds how many are K (of the pixels of a picture, how
many are of grey K); the last when fewer are counted."
  (declare (type vector counts) (type fixnum wanted))
  (let ((seen 0) (last (1- (length counts))))
    (declare (type fixnum seen last))
    (dotimes (k last last)
      (incf seen (aref counts k))
      (when (>= seen wanted) (return k)))))

(defun page-level (grey width height)
  "The grey level that parts the ink of the scanned picture GREY (WIDTH by
HEIGHT) from its paper: Otsu's (OTSU-LEVEL), taken again without some
rows. Those rows have paper (read as PAPER-BANDS reads it,
*BAND-PAPER-SHARE*) that falls below the level yet nearer to it than to
the darkest fiftieth of the picture, where there are any. They are rows of
paper darker than the rest, as a band printed again or a shaded row
brings. Left in, they would draw the level up to their paper and leave the
rest of the page read as it would not be without them. Rows that reach the
picture's top or foot are not taken out: they are what lies about the
page. A row of text, however blurred, has paper between its letters, and a
row of a rule or a bar is ink. As two values: the level, and those rows, a
bit for each row of GREY (NIL for none). As a third value, the pixels of the
rows not taken out counted by grey level (GREY-HISTOGRAM)."
  (declare (type grey-pixels grey) (type fixnum width height))
  (let* ((whole (grey-histogram grey))
         (level (otsu-level whole))
         (core (counted-level whole (max 1 (floor (length grey) 50))))
         (counts (make-array 256 :element-type 'fixnum :initial-element 0))
         (skipped (make-array height :element-type 'bit :initial-element 0)))
    (dotimes (y height)
      (fill counts 0)
      (grey-histogram grey :start (* y width) :end (* (1+ y) width) :into counts)
      (when (< (/ (+ core level) 2) (counted-level counts (ceiling (* *band-paper-share* width))) level)
        (setf (sbit skipped y) 1)))
    ;; rows of darker paper at the picture's top or foot are what lies about the page, a table or a scanner's
    ;; lid, and are read as they are
    (loop for y from 0 below height while (= 1 (sbit skipped y)) do (setf (sbit skipped y) 0))
    (loop for y from (1- height) downto 0 while (= 1 (sbit skipped y)) do (setf (sbit skipped y) 0))
    (if (and (find 1 skipped) (find 0 skipped))
        (let ((kept (grey-histogram grey :width width :skipped skipped)))
          (values (otsu-level kept) skipped kept))
        (values level nil whole))))

(defstruct (word-box (:copier nil))
  "A word found in a scanned picture. X0 Y0 X1 Y1 are its pixels (the last
excluded), from the top left. INK is the grey level of the core of its
strokes (MEASURE-WORD). EDGES is how much of it is half-tone, between ink and
paper, for each pixel of ink (a scan's blur, which a crisp print lacks).
PAPER and NOISE are the mean grey of the paper inside its box and how far the
paper strays from that. BASELINE is the row its letters stand on
\(WORD-BASELINE). SLOPE is how far that row falls across its width, for each
pixel across (NIL for a word too short to tell). OFFSET is how far, in
pixels, it stands below (above, when less than 0) its line, for a word
UNLIKE-WORDS sets apart by where it sits. TURN is how far, in degrees, it is
turned clockwise against its line, for one it sets apart so. Both are 0 for
any other, and both are what its report says. CUT is true when its line was
cut from lines set so close they touched (SPLIT-LINES), its baseline and turn
then being no measure of where it sits. LONE is true for a mark narrower than
its line is tall, standing apart from the words about it (a figure alone in a
table's cell, an initial, a list's number, a bullet). Its characters are set
beside others for copies, but it is no word set beside its line
\(PICTURE-WORDS). COLUMN is the left edge of the column of the page it was
read in, 0 for a page read whole."
  x0 y0 x1 y1 (ink 0) (edges 0) (paper 0) (noise 0) (baseline 0) slope (offset 0) (turn 0) cut lone (column 0))

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
  "A character of a scanned picture, a piece of letters joined by the scanner's
blur (SPLIT-JOINED), or such letters left whole where no cut is told. X0 Y0 X1
Y1 are its ink's tight box (the last excluded) in the picture's grey levels as
read (not made level, so that a character copied within the picture is found
where it was put). PIXELS are those grey levels from two pixels above and left
of the box to two below and right, WIDTH wide, a cut of its own so that the
picture need not be kept. INK is its pixels of ink. WORD is the WORD-BOX it is
in. PICTURE and PAGE are the picture object and the page index it is on
\(SCAN-PICTURES). INKS and DARKER are what GLYPH-DISTANCE sets it beside others
by, worked out once (GLYPH-MEASURED). CLIPPED is the grey level at or below
which its page's scanner clipped its ink, the darkest any character of its page
reaches, or for one without grain of its own two greys above its own darkest (-1
for none). BARE is true when it has no grain of its own (GRAIN-LEVELS sets
both). SPLIT is NIL for a mark as it was found, :SURE for a piece cut from
letters joined by the blur that is letter-sized, and :UNSURE for a piece or a
mark still wider than letters are (SPLIT-JOINED). TYPICAL is how far the cores
of its page's characters typically stray (GRAIN-LEVELS). SPREAD is how far its
own core strays (GLYPH-CORE-SPREAD, NIL for too small a core). ALIKE is how
alike its picture's characters of the same shape typically are, NIL when its
picture's grain is too faint for them to be set beside others (COPIED-GLYPHS).
COLOUR is the colour of its ink, (RED GREEN BLUE) each 0 to 255, on a picture in
colours, else NIL (GLYPH-INK-COLOUR). BLOCKS is how flat its blocks are at each
way a JPEG's block can sit, kept once worked out (GLYPH-BLOCK-SCORES)."
  pixels
  width
  x0
  y0
  x1
  y1
  (ink 0)
  word
  picture
  page
  inks
  darker
  (clipped -1)
  bare
  split
  (typical 0)
  spread
  alike
  colour
  blocks)

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
  "GLYPH, with what GLYPH-DISTANCE sets it beside others by worked out
once. That is its GLYPH-INK-LEVEL over its box and a pixel round it, with
and without the last row (a character of its shape a pixel shorter or
taller). It also holds how many of its pixels are darker than each
sixteenth of the grey levels. So those darker than any level are told at
once, a few too many at most."
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
      (replace cut
               grey
               :start1 (* j across)
               :start2 (+ (* (+ top j) width) left)
               :end2 (+ (* (+ top j) width) left across)))))

(defun word-glyphs (original unruled level slope straight width height x0 top x1 bottom)
  "The characters (GLYPHs) of the word from X0 TOP below X1 BOTTOM of the
picture made level, STRAIGHT (WIDTH by HEIGHT), its lines having fallen
SLOPE for each pixel across in ORIGINAL. Its columns of ink are cut apart
where a column holds none. Each one's rows are found in UNRULED, ORIGINAL
with its rules taken out (WITHOUT-RULES), as ink darker than LEVEL, so that
a frame round a stamp or a line under a word is no part of a character. Its
pixels are cut from ORIGINAL. Those too small to compare (a dot, a comma:
under 6 rows tall, unless twice as wide as tall and 6 columns wide or more,
as a hyphen, a minus sign or a dash is) or too near the picture's edge for
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
            (push (glyph-measured (make-glyph :pixels cut :width across :x0 gx0 :y0 y0 :x1 gx1 :y1 y1 :ink ink))
                  out)))))))

(defun glyph-piece (glyph from to level split)
  "The piece of GLYPH between its box's columns FROM and TO (from its box's
left, TO excluded), as a GLYPH of its own marked SPLIT. Its rows are those
of GLYPH's box holding ink darker than LEVEL in those columns, and its
pixels are cut from GLYPH's own with two pixels round it. It is NIL when it
is too small to compare, as WORD-GLYPHS leaves such marks out."
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
  "How tall the taller characters of GLYPHS are: capitals, figures and the
letters that rise above the others (b, d, l). It is the height a quarter of
them reach or pass, however many commas, dots and bits of rules the page also
holds. Or, when more, it is how far the lines of their words reach above
their baselines, the middle over the words. That holds for a page whose
letters are mostly small ones, where fewer than a quarter rise, as text set
in a book face often is. Nearly every line has a capital, a figure or a tall
letter in it, and the middle is told by many lines, not by the few a rule or
a frame makes taller. That reach is taken as far as eight fifths of the
quarter's height, no more: in the faces measured the letters that rise stand
1.3 to 1.5 times as tall as the small ones, and a page whose lines run
together with marks above them would reach further. NIL for no GLYPHS."
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
  "How wide a character of GLYPHS typically is: the middle width of those
no wider than they are tall. Nearly every letter is so and letters joined
by the blur seldom are, so a page set so tight that most of its letters
touch still gives the width of one. It is the middle of them all when
none is so, and NIL for no GLYPHS."
  (flet ((width (g) (- (glyph-x1 g) (glyph-x0 g))))
    (median-of (mapcar #'width
                       (or (remove-if (lambda (g) (> (width g) (- (glyph-y1 g) (glyph-y0 g)))) glyphs) glyphs)))))

;;; Letters joined by the blur
;;;
;;; A scanner's blur fills the gap between two letters set close with
;;; half-tones, so that their ink runs together and no column of paper parts
;;; them; but where one letter ends and the next begins, a column holds none of
;;; a stroke's core, only the half-tones the blur spread there. Letters touch
;;; over a few rows, where their serifs or curves come near, while the
;;; half-toned edge of a stroke runs its whole height. At a small size the
;;; hairline of a wide letter (the arch of an m) may not be as dark as a core,
;;; and the letter is cut there too, into pieces each compared as any
;;; character is. Half as wide again as a typical character parts no letter
;;; from a pair: wide letters are cut only where they have a neck, and two
;;; narrow letters run together are left whole and compared as one mark, in
;;; which honest prints still differ. A piece told a copy by its likeness alone
;;; is worth a look at most, the same letters cut at the same neck being nearer
;;; each other than prints of a letter are; pieces the same pixel for pixel are
;;; told as any characters are, wherever the cut falls.

(defun split-joined (glyphs level deep)
  "GLYPHS, the characters of a scanned page, with marks of letters joined by the
scanner's blur cut into letter-sized pieces (GLYPH-SPLIT), each in the place of
the mark it was cut from. A mark more than half as wide again as the page's
typical character (TYPICAL-WIDTH) is cut at a neck: of its columns holding no
pixel as dark as DEEP (halfway between the darkest of the page's ink and LEVEL,
the grey that parts ink from paper) and ink in no more than a third of its
rows, the one holding the least ink, with half a typical character's width (3
pixels at least) left on either side; each side is cut again while it is still
that wide. A piece as wide as a letter is :SURE; one still half as wide again
as a typical character, and a mark that wide with no neck, is :UNSURE. The cut
column itself is in no piece's box, but in the paper about each."
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
                                   for piece = (glyph-piece g
                                                            from
                                                            to
                                                            level
                                                            (if (> (- to from) (* 3/2 typical)) :unsure :sure))
                                   when piece collect piece)
                             (progn (setf (glyph-split g) :unsure) (list g))))
              else collect g))))

(defun page-tilt (grey width height level &optional (inked (count-if (lambda (g) (< g level)) grey)))
  "How far the lines of the scanned picture GREY (WIDTH by HEIGHT, ink darker
than LEVEL, INKED pixels of it) fall for each pixel across. It is the slope at which its ink,
summed along rows of that slope, is heaped most sharply into lines. Slopes
are tried from -3 to 3 degrees by quarters and then by fiftieths of a degree
about the best. Of slopes alike, the levellest is taken, for rows heaped
alike at slopes too slight to move a line a whole row across the picture."
  (declare (type grey-pixels grey) (type fixnum width height level inked))
  (let* ((step (max 1 (floor inked 200000)))
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
                           do (let ((v (sharpness d)))
                                (when (or (> v most) (and (= v most) (< (abs d) (abs best)))) (setf most v best d)))
                           finally (return best)))
             (fine (loop with best = coarse and most = -1
                         for d from (- coarse 1/4) to (+ coarse 1/4) by 1/50
                         do (let ((v (sharpness d)))
                              (when (or (> v most) (and (= v most) (< (abs d) (abs best)))) (setf most v best d)))
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
  "The columns of text of the picture GREY (WIDTH by HEIGHT, its rules
taken out, ink darker than LEVEL), as runs (LEFT . RIGHT) of its columns
of pixels: the page parted at each gutter of paper running down it. A
gutter is a run of columns holding ink in no more than a hundredth of the
rows, a sixtieth of the page wide and 12 pixels at least, between a sixth
and five sixths of the way across, with ink on either side. The whole page
is taken when there is none."
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
quarter of its rows that hold ink having that much or more). Each run is
at least two fifths as tall as the band's tall runs (a quarter of its runs
being as tall or taller), so that a mark running across several lines,
such as a signature, does not hide the cores of the others. That third is
the least ink a core's rows hold."
  (let* ((inked (sort (loop for y from top below bottom when (plusp (aref counts y)) collect (aref counts y)) #'>))
         (core (max 1 (if inked (floor (nth (floor (length inked) 4) inked) 3) 1)))
         (zones (ink-runs (subseq counts top bottom) core 1))
         (heights (sort (mapcar (lambda (z) (- (cdr z) (car z))) zones) #'>))
         (tall (if heights (nth (floor (length heights) 4) heights) 0)))
    (values (remove-if (lambda (z) (< (* 5 (- (cdr z) (car z))) (* 2 tall))) zones) core)))

(defun split-lines (rows bands)
  "The lines of a picture, as BANDS (each (TOP . BOTTOM), the runs of
ROWS, each row's count of ink, that hold ink) cut where lines set close
touch: as three values, the lines, those of them made by cutting, and how
many bands were left unread. In every line most ink lies in the band of
its small letters, between the baseline and the top of an x; above and
below it only ascenders and descenders reach, and where lines are set so
close that those of one touch those of the next, two lines run into one
band. A band is cut only when it is clearly taller than one line. It must
be more than half again as tall as the page's bands of one line (those
holding one core, LINE-CORES; or, where no band holds one, the middle
distance between cores). So a row of figures, whose top and bottom strokes
are inkier than their middle, is never cut through. Such a band is cut
between two cores at the row of least ink, measured against the band's own
ink, where that row holds no more than half of the least a core's rows
hold: there only the ascenders and descenders of two lines meet. Where the
strokes of two lines overlap more, or a mark such as a signature runs
across them, the two are left together, and a band still holding two cores
and too tall to be a line is left unread on its own, the rest of the page
read."
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
                                                 when (< (aref rows (+ top y)) (aref rows (+ top best)))
                                                 do (setf best y)
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
two values: the copy, and how many pixels of ink of rules it took out. A rule is
a run of ink (darker than LEVEL) straight across or down longer than a letter
can be, a twentieth of the picture's height and 80 pixels at least (a table's
borders, a line to sign on, an underline), so that words are not run together
with them. It is one unbroken, or one a pixel wide whose grey the scanner left
lighter here and there, broken by gaps of 6 pixels or less, with ink along four
fifths of it. And it is thin: paper 3 pixels to both sides of it along half its
length, and to one side along nine tenths (figures set against a rule touch it
on one side; a row of letters is crossed by their strokes, and so is never so).
Or it is one inked along nine tenths of it, as no row through letters is, though
figures set close in a cell touch it on both sides. A rule down the page need
not be thin, figures in narrow cells touching it on both sides, but it must be
inked along nine tenths of its length: letters standing one over another in
lines set close leave more paper between them than that. With SLANTED, the
default, rules traced at a slant are taken out too (WITHOUT-SLANTED-RULES). GREY
is not changed."
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
                              (let ((before (not (ink-p k (- across 3) horizontal)))
                                    (after (not (ink-p k (+ across 3) horizontal))))
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

(defvar *trace-scratch* nil
  "Where the pictures of a file being read keep the array the slanted rules
are traced in, from page to page (TRACE-PIXELS): a cons whose car is the
array, bound while a file's scanned pictures are read (SCAN-PICTURES), else
NIL, the array made afresh.")

(defun trace-pixels (size)
  "An array of SIZE two-bit elements or more to trace slanted rules in,
reused from the last picture of the file being read when it is as large,
else made (*TRACE-SCRATCH*); its contents are what the last use left."
  (let ((kept (car *trace-scratch*)))
    (if (and kept (>= (length kept) size))
        kept
        (let ((made (make-array size :element-type '(unsigned-byte 2))))
          (when *trace-scratch* (setf (car *trace-scratch*) made))
          made))))

(defun without-slanted-rules (grey out width height level paper longest &optional changed)
  "OUT, GREY (WIDTH by HEIGHT) as WITHOUT-RULES has left it, with the
rules that slant taken out too, PAPER where they were; how many pixels of
ink it took out. A rule photographed at a slant or bowed runs along a row
for a while and then steps to the next. A path of ink (darker than LEVEL)
across the page, or down it, goes on from each pixel to the next column
\(or row) in the same row or one beside it, LONGEST pixels or more. Every
pixel of it has paper 3 pixels to one side of it at least, and half of
them have paper 3 pixels to both sides. Such a path is a rule. A line of
letters is crossed by their strokes and parted by the paper between them,
and is never so. The rule is taken out across its breadth, but where a
stroke crosses or touches it (the ink straight across it there broader
than *RULE-BREADTH*) it is left, so that letters set on a rule stay whole."
  (declare (type grey-pixels grey out) (type fixnum width height level paper longest) (optimize speed))
  (let ((from (trace-pixels (* width height))) (taken 0))
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
               (declare (type (simple-array fixnum (*)) before-length before-thin now-length now-thin)
                        (type fixnum reach))
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
                                       (setf best (aref before-length (1- across))
                                             thin (aref before-thin (1- across))
                                             way 1))
                                     (when (and (< (1+ across) span) (> (aref before-length (1+ across)) best))
                                       (setf best (aref before-length (1+ across))
                                             thin (aref before-thin (1+ across))
                                             way 3)))
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
                                                                (< (aref grey (- k (* (- c (1- first)) across-step)))
                                                                   level))
                                                     do (decf first))
                                               (loop while (and (< (1+ last) span) (< (- last c) *rule-breadth*)
                                                                (< (aref grey (+ k (* (- (1+ last) c) across-step)))
                                                                   level))
                                                     do (incf last))
                                               (when (< (- last first) *rule-breadth*)
                                                 (loop for e fixnum from first to last
                                                       for j fixnum = (+ k (* (- e c) across-step))
                                                       do (when (/= (aref out j) paper)
                                                            (when (< (aref grey j) level) (incf taken))
                                                            (when changed
                                                              (vector-push-extend (+ (* j 256) (aref out j)) changed))
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

(defun changed-from (changed place)
  "Where in CHANGED, pixels each kept as its place times 256 plus its grey
level and sorted, the first pixel at PLACE or after it lies (the length of
CHANGED when none does)."
  (declare (type (simple-array fixnum (*)) changed) (type fixnum place))
  (let ((low 0) (high (length changed)) (key (* place 256)))
    (declare (type fixnum low high key))
    (loop while (< low high)
          do (let ((middle (ash (+ low high) -1)))
               (if (< (aref changed middle) key) (setf low (1+ middle)) (setf high middle))))
    low))

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
  "The least part of the characters a scanned page is reckoned to hold (its
ink over its characters' middle ink) that may be judged before the report
says how few were. The reckoning strays. Honest pages of small print
scanned at 200 dots an inch are read at three fifths of it, and pages of
ordinary print at four fifths to all of it. A page on which a band of lines
run together was left unread is named whatever part was read.")

(defparameter *scan-characters-in-all* 300000
  "The most characters of all the scanned pictures of a file set beside
others for copies; the pictures past them have none set beside them. A
bound of the work, not of judgement: some sixty pages of small print.")

(defstruct (picture-reading (:copier nil) (:predicate nil))
  "The words of a scanned picture read (PICTURE-WORDS). WORDS is the words
\(WORD-BOX) and TILT the slope the page's lines fall by for each pixel across
\(PAGE-TILT). GLYPHS is the words' characters (WORD-GLYPHS) and those of the
marks standing alone, those joined by the blur cut apart (SPLIT-JOINED). PAPER
is how far its blank paper strays (BLANK-PAPER-NOISE), RULES how many pixels
of rules were taken out of it (WITHOUT-RULES), INK how many pixels of ink are
left once they are, and TALL-BANDS how many bands of rows were too tall to be
lines and left unread."
  words tilt glyphs paper rules ink tall-bands)

(defun picture-words (grey width height &optional level skipped counts)
  "The words of the scanned picture GREY, WIDTH by HEIGHT, measured, as a
PICTURE-READING. Lines are found across the page and cut where they
touch (SPLIT-LINES); where that leaves bands unread, they are found in each
column of the page apart (PAGE-COLUMNS) instead when that leaves fewer unread,
as two columns of text set at different leading do. The words are found and
measured on the picture made level (STRAIGHTENED), their rows those of the level
picture: a word's row in GREY is its row plus the tilt times its column. A mark
narrower than its line is tall, standing apart from the words about it (a figure
alone in a table's cell, an initial, a list's number), is LONE: its characters
are among the characters, set beside others for copies as any are, but it is no
word, neither among the words nor set beside its line. Ink is darker than LEVEL,
and the page's paper and darkest ink are read from its rows but those SKIPPED (a
bit for each row), as PAGE-LEVEL gives both unless they are given, with
COUNTS, the pixels of the rows not skipped counted by grey level, so that a
band printed darker does not make the rest of the page's words read as marks too
faint to be letters. NIL when it holds too little or too much ink to be a page
of text, or more than *SCAN-WORDS-MOST* words and marks standing alone."
  (declare (type grey-pixels grey) (type fixnum width height))
  (unless level
    (multiple-value-setq (level skipped counts) (page-level grey width height)))
  (let* ((level level)
         (counts (or counts (grey-histogram grey :width width :skipped skipped)))
         (ink (+ (loop for g below level sum (aref counts g))
                 (if skipped
                     (loop for y below height
                           when (= 1 (sbit skipped y))
                             sum (count-if (lambda (g) (< g level)) grey :start (* y width) :end (* (1+ y) width)))
                     0))))
    (declare (type fixnum level ink))
    (when (or (< ink (* 0.002 (length grey))) (> ink (* 0.4 (length grey))))
      (return-from picture-words nil))
    ;; the paper the middle of what is lighter than LEVEL, the ink the darkest fiftieth
    (let* ((paper (let ((half (floor (loop for g from level below 256 sum (aref counts g)) 2)) (seen 0))
                    (loop for g from level below 256
                          do (incf seen (aref counts g))
                          when (>= seen half)
                          return g
                          finally (return 255))))
           (core (let ((wanted (max 1 (floor (loop for g below level sum (aref counts g)) 50))) (seen 0))
                   (loop for g below 256
                         do (incf seen (aref counts g))
                         when (>= seen wanted)
                         return g
                         finally (return 0))))
           (tilt (page-tilt grey width height level ink))
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
           (measured (progn
                       (incf rules (without-slanted-rules level-grey grey width height level paper
                                                          (max 80 (floor height 20)) changed))
                       grey))
           (changed (sort (coerce changed '(simple-array fixnum (*))) #'<))
           ;; the rules at a slant put back into the rows FROM to TO, or taken out again, where characters
           ;; are cut or the page's columns and blank paper read with the straight rules alone taken out
           (slanted (lambda (from to back)
                      (loop for k from (changed-from changed (* from width)) below (length changed)
                            for entry = (aref changed k)
                            for at = (floor entry 256)
                            while (< at (* to width))
                            do (setf (aref grey at) (if back (mod entry 256) paper)))))
           (rows (make-array height :element-type 'fixnum :initial-element 0))
           (words '())
           (glyphs '())
           (skipped 0)
           (cut-lines '()))
      (declare (type grey-pixels grey))
      (dotimes (y height)
        (job-check-stop)
        (let ((at (* y width)) (n 0))
          (declare (type fixnum at n))
          (dotimes (x width) (when (< (aref measured (+ at x)) level) (incf n)))
          (setf (aref rows y) n)))
      (multiple-value-bind (placed unread made)
          (flet ((lines-of (columns)
                   ;; each line as (LEFT RIGHT TOP . BOTTOM), how many bands were left unread, and the lines made by
                   ;; cutting
                   (let ((placed '()) (unread 0) (made '()))
                     (loop for (left . right) in columns
                           for counts = (if (and (zerop left) (= right width))
                                            rows
                                            (let ((c (make-array height :element-type 'fixnum :initial-element 0)))
                                              (dotimes (y height c)
                                                (loop for x from left below right
                                                      when (< (aref measured (+ (* y width) x)) level)
                                                      do (incf (aref c y))))))
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
                                         (prog1 (page-columns grey width height level)
                                           (funcall slanted 0 height nil))))))
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
                      (setf (word-box-column word) left)
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
                        (dolist (glyph (word-glyphs original
                                                    unruled
                                                    level
                                                    slope
                                                    grey
                                                    width
                                                    height
                                                    (car run)
                                                    top
                                                    (cdr run)
                                                    bottom))
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
        (make-picture-reading :words words :tilt tilt
                              :glyphs (let ((kept (make-hash-table :test 'eq)))
                                        (dolist (word marks) (setf (gethash word kept) t))
                                        (remove-if-not (lambda (g) (gethash (glyph-word g) kept)) glyphs))
                              :paper (blank-paper-noise grey width height rows level)
                              :rules rules
                              :ink (reduce #'+ rows)
                              :tall-bands skipped)))))

(defun blank-paper-noise (grey width height rows level)
  "How far the blank paper of GREY (WIDTH by HEIGHT) strays from its
mean, the root of its mean squared difference from it. The paper is that
of the rows holding no ink, nor any row within 3 of them (ROWS counts
each row's ink, darker than LEVEL), between the lines and in the margins
above and below them, where no ink's blur reaches. It is NIL when no such
row is left."
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
rows from the top, to a fraction of a row. Each row's ink is counted as how
far each pixel is from the picture's PAPER grey to its INK's, a half-tone at
the edge of a stroke in part. The baseline is where that falls below two
fifths of the fullest row's, between the last row above it and the first
below. So the tails of g, p and y below do not count. NIL for no ink."
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
  "How far the letters in GREY (WIDTH wide) from X0 Y0 below X1 Y1,
standing on BASELINE, fall for each pixel across. It is the line best
through the foot of each column of ink that stands on the baseline. A
column counts if it is within 3 pixels of the baseline, so that a word
turned a little keeps its ends and the tails of g, p and y below do not
count. NIL for a word less than five times as wide as tall, or with too
few such columns to tell, across too little of it."
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
  "The WORD-BOX of the pixels of GREY (WIDTH wide) from X0 Y0 below X1 Y1, ink
being darker than LEVEL, its baseline found between the picture's PAPER-GREY and
INK-GREY (WORD-BASELINE). Its ink is the mean of its darkest quarter (the core
of its strokes, alike in thin and bold type). Its paper is only what is clearly
paper (lighter than three quarters of the way from ink to paper)."
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
                     :noise (if (plusp paper)
                                (float (sqrt (max 0 (- (/ paper-squares paper) (expt (/ paper-sum paper) 2)))))
                                0.0))))))
