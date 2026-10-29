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

(defun raw-grey (data width height bits kind table invert)
  "The grey levels of a picture's samples DATA, WIDTH by HEIGHT, BITS to a
sample, its colours of KIND (:grey, :rgb, :cmyk, or :indexed with TABLE as
COLOUR-SPACE-OF gives it), each row starting on an octet; INVERT for a
Decode array reversing them. NIL when DATA is too short."
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
      (dotimes (y height grey)
        (dotimes (x width)
          (setf (aref grey (+ (* y width) x))
                (if (eq kind :indexed)
                    (destructuring-bind (base highest lookup) table
                      (let* ((n (ecase base (:grey 1) (:rgb 3) (:cmyk 4)))
                             (at (* n (min (sample y x) highest))))
                        (if (<= (+ at n) (length lookup))
                            (grey-of base (aref lookup at) (if (> n 1) (aref lookup (+ at 1)) 0)
                                     (if (> n 2) (aref lookup (+ at 2)) 0) (if (> n 3) (aref lookup (+ at 3)) 0))
                            255)))
                    (let ((k (* x parts)))
                      (grey-of kind (sample y k) (if (> parts 1) (sample y (+ k 1)) 0)
                               (if (> parts 2) (sample y (+ k 2)) 0) (if (> parts 3) (sample y (+ k 3)) 0))))))))))

(defun image-grey (image)
  "The grey levels of IMAGE, a picture Tuatara decoded (DECODE-PICTURE)."
  (let* ((width (image-width image)) (height (image-height image)) (channels (image-channels image))
         (pixels (image-pixels image))
         (grey (make-array (* width height) :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) pixels grey) (type fixnum width height channels))
    (dotimes (i (* width height) grey)
      (let ((at (* i channels)))
        (setf (aref grey i) (grey-of :rgb (aref pixels at) (aref pixels (+ at 1)) (aref pixels (+ at 2)) 0))))))

(defun picture-grey (pdf revision number)
  "The grey levels of the picture object NUMBER of REVISION, as three values:
the levels, a row after another from the top, its width and its height; or
NIL and why not, in words. Raw samples are read here; a JPEG is turned into
pixels by Tuatara, in the sandbox (DECODE-PICTURE)."
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
            ((member why '("/DCTDecode" "/DCT") :test #'equal)
             (multiple-value-bind (image notes) (decode-picture data :jpeg width height)
               (if image
                   (values (image-grey image) (image-width image) (image-height image))
                   (values nil (string-trim ", " (or (car (first notes)) "the JPEG could not be decoded"))))))
            (why (values nil (format nil "its pixels are kept as ~a, not read here" why)))
            (t
             (let ((mask (eq (pdf-get dict "/ImageMask") t))
                   (bits (let ((b (pdf-get dict "/BitsPerComponent"))) (if (integerp b) b 1)))
                   (decode (deref pdf revision (pdf-get dict "/Decode"))))
               (multiple-value-bind (kind table) (if mask :grey (colour-space-of pdf revision (pdf-get dict "/ColorSpace")))
                 (if (and kind (member (if mask 1 bits) '(1 2 4 8 16)))
                     (let ((grey (raw-grey data width height (if mask 1 bits) kind table
                                           ;; a stencil paints where its samples are 0, unless its Decode says 1
                                           (let ((first-decode (and (consp decode) (first (pdf-array-items decode)))))
                                             (if mask (not (eql first-decode 1)) (eql first-decode 1))))))
                       (if grey (values grey width height) (values nil "its data is shorter than its size")))
                     (values nil "its colours are of a kind not read here")))))))))

;;; Black and white, lines and words

(defun otsu-level (grey)
  "The grey level that best parts GREY's pixels into dark and light: the one
that makes the two groups most unlike (Otsu's way)."
  (declare (type grey-pixels grey))
  (let ((counts (make-array 256 :element-type 'fixnum :initial-element 0)) (total (length grey)))
    (loop for g across grey do (incf (aref counts g)))
    (let ((sum (loop for i below 256 sum (* i (aref counts i)))) (below 0) (below-sum 0) (best 0) (level 128))
      (dotimes (i 256 level)
        (incf below (aref counts i))
        (incf below-sum (* i (aref counts i)))
        (let ((above (- total below)))
          (when (and (plusp below) (plusp above))
            (let* ((dark (/ below-sum below)) (light (/ (- sum below-sum) above))
                   (between (* below above (expt (- dark light) 2))))
              (when (> between best) (setf best between level i)))))))))

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
than 0) the baseline of its line, the page's tilt allowed for; TURN, how
far, in degrees, it is turned clockwise against the page (ALIGN-WORDS)."
  x0 y0 x1 y1 (ink 0) (edges 0) (paper 0) (noise 0) (baseline 0) slope (offset 0) (turn 0))

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
  "A character of a scanned picture, or letters joined by the scanner's blur:
X0 Y0 X1 Y1 its ink's tight box (the last excluded) in the picture's grey
levels as read (not made level, so that a character copied within the
picture is found where it was put); PIXELS, those grey levels from two
pixels above and left of the box to two below and right, WIDTH wide, a cut
of its own so that the picture need not be kept; INK, its pixels of ink;
WORD, the WORD-BOX it is in; PICTURE and PAGE, the picture object and the
page index it is on (SCAN-PICTURES); INKS and DARKER, what GLYPH-DISTANCE
sets it beside others by, worked out once (GLYPH-MEASURED)."
  pixels width x0 y0 x1 y1 (ink 0) word picture page inks darker)

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
        (darker (make-array 17 :element-type '(unsigned-byte 16) :initial-element 0)))
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

(defun word-glyphs (original level slope straight width height x0 top x1 bottom)
  "The characters (GLYPHs) of the word from X0 TOP below X1 BOTTOM of the
picture made level, STRAIGHT (WIDTH by HEIGHT), its lines having fallen
SLOPE for each pixel across in ORIGINAL: its columns of ink cut apart where
a column holds none, each one's rows found in ORIGINAL, ink darker than
LEVEL. Those too small to compare (a dot, a comma) or too near the
picture's edge for paper round them are left out."
  (declare (type grey-pixels original straight) (type fixnum level width height x0 top x1 bottom))
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
                       when (< (aref original (+ (* y width) x)) level)
                         do (incf ink) (unless y0 (setf y0 y)) (setf y1 (1+ y))))
        (when (and y0 (>= (- gx1 gx0) 3) (>= (- y1 y0) 6) (>= ink 20)
                   (>= gx0 2) (<= (+ gx1 2) width) (>= y0 2) (<= (+ y1 2) height))
          (multiple-value-bind (cut across) (glyph-cut original width gx0 y0 gx1 y1)
            (push (glyph-measured (make-glyph :pixels cut :width across :x0 gx0 :y0 y0 :x1 gx1 :y1 y1 :ink ink)) out)))))))

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

(defun without-rules (grey width height level paper)
  "GREY (WIDTH by HEIGHT) with its rules taken out, PAPER where they were:
every run of ink (darker than LEVEL) straight across or down longer than a
letter can be, a twentieth of the picture's height and 80 pixels at least
(a table's borders, a line to sign on, an underline), so that words are not
run together with them. A copy; GREY is not changed."
  (declare (type grey-pixels grey) (type fixnum width height level))
  (let ((out (copy-seq grey)) (longest (max 80 (floor height 20))))
    (declare (type grey-pixels out))
    (dotimes (y height)
      (let ((start nil))
        (loop for x from 0 to width
              do (if (and (< x width) (< (aref grey (+ (* y width) x)) level))
                     (unless start (setf start x))
                     (when start
                       (when (> (- x start) longest)
                         (loop for k from start below x do (setf (aref out (+ (* y width) k)) paper)))
                       (setf start nil))))))
    (dotimes (x width out)
      (let ((start nil))
        (loop for y from 0 to height
              do (if (and (< y height) (< (aref grey (+ (* y width) x)) level))
                     (unless start (setf start y))
                     (when start
                       (when (> (- y start) longest)
                         (loop for k from start below y do (setf (aref out (+ (* k width) x)) paper)))
                       (setf start nil))))))))

(defparameter *scan-words-most* 20000
  "The most words a scanned picture may hold to be read as a page of text: a
page holds a few hundred, and a picture of more is no page of text but
something made to keep the examination busy.")

(defparameter *scan-characters-most* 40000
  "The most characters of a scanned picture set beside others for copies
(COPIED-GLYPHS); a picture of more has none set beside them.")

(defparameter *scan-characters-in-all* 300000
  "The most characters of all the scanned pictures of a file set beside
others for copies; the pictures past them have none set beside them.")

(defun picture-words (grey width height)
  "The words of the scanned picture GREY, WIDTH by HEIGHT, measured (WORD-BOX),
as three values: the words, the slope the page's lines fall by for each
pixel across (PAGE-TILT), and the words' characters (WORD-GLYPHS). The
words are found and measured on the
picture made level (STRAIGHTENED), their rows those of the level picture: a
word's row in GREY is its row plus the tilt times its column. NIL when it
holds too little or too much ink to be a page of text."
  (declare (type grey-pixels grey) (type fixnum width height))
  (let* ((level (otsu-level grey))
         (ink (loop for g across grey count (< g level))))
    (when (or (< ink (* 0.002 (length grey))) (> ink (* 0.4 (length grey))))
      (return-from picture-words nil))
    (let* ((counts (let ((h (make-array 256 :element-type 'fixnum :initial-element 0)))
                     (loop for g across grey do (incf (aref h g)))
                     h))
           ;; the paper the middle of what is lighter than LEVEL, the ink the darkest fiftieth
           (paper (let ((half (floor (loop for g from level below 256 sum (aref counts g)) 2)) (seen 0))
                    (loop for g from level below 256 do (incf seen (aref counts g)) when (>= seen half) return g finally (return 255))))
           (core (let ((wanted (max 1 (floor ink 50))) (seen 0))
                   (loop for g below 256 do (incf seen (aref counts g)) when (>= seen wanted) return g finally (return 0))))
           (tilt (page-tilt grey width height level))
           (original grey)
           (slope (if (< (abs tilt) 0.0003) 0 tilt))
           (grey (if (zerop slope) grey (straightened grey width height tilt paper)))
           (grey (without-rules grey width height level paper))
           (rows (make-array height :element-type 'fixnum :initial-element 0)) (words '()) (glyphs '()))
      (declare (type grey-pixels grey))
      (dotimes (y height)
        (job-check-stop)
        (let ((at (* y width)) (n 0))
          (declare (type fixnum at n))
          (dotimes (x width) (when (< (aref grey (+ at x)) level) (incf n)))
          (setf (aref rows y) n)))
      (dolist (line (ink-runs rows (max 2 (floor width 500)) 1))
        (destructuring-bind (top . bottom) line
          (let ((tall (- bottom top)))
            (when (<= 6 tall (floor height 8))
              (let ((columns (make-array width :element-type 'fixnum :initial-element 0)))
                (loop for y from top below bottom
                      do (let ((at (* y width)))
                           (dotimes (x width) (when (< (aref grey (+ at x)) level) (incf (aref columns x))))))
                (dolist (run (ink-runs columns 1 (max 2 (floor tall 3))))
                  (when (and (>= (- (cdr run) (car run)) tall)
                             ;; letters, not a dash or an underline: ink in two fifths of the line's rows at least
                             (>= (loop for y from top below bottom
                                       count (loop for x from (car run) below (cdr run)
                                                   thereis (< (aref grey (+ (* y width) x)) level)))
                                 (* 0.4 tall)))
                    (let ((word (measure-word grey width level (car run) top (cdr run) bottom paper core)))
                      (push word words)
                      (dolist (glyph (word-glyphs original level slope grey width height (car run) top (cdr run) bottom))
                        (setf (glyph-word glyph) word)
                        (push glyph glyphs))))))))))
      ;; marks too faint to be letters (what is left at the edges of a rule taken out) are no words
      (let ((words (remove-if (lambda (w) (> (word-box-ink w) (/ (+ core level) 2))) (nreverse words))))
        (when (> (length words) *scan-words-most*)
          (return-from picture-words nil))
        (when (> (length glyphs) *scan-characters-most*)
          (setf glyphs '()))
        (values (align-words words) tilt
                (let ((kept (make-hash-table :test 'eq)))
                  (dolist (word words) (setf (gethash word kept) t))
                  (remove-if-not (lambda (g) (gethash (glyph-word g) kept)) (nreverse glyphs))))))))

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

(defun align-words (words)
  "WORDS, measured, with each one's OFFSET and TURN set: the page's tilt the
middle of its words' slopes, each line's baseline the line of that tilt
through the middle of its words' baselines (allowing for the tilt), so that
a slightly crooked scan sets no word apart; in a line of two words, as a
label and its figure are, the right one set beside the left. WORDS."
  (let* ((slopes (remove nil (mapcar #'word-box-slope words)))
         (tilt (if slopes (median-of slopes) 0)))
    (flet ((centre (word) (/ (+ (word-box-x0 word) (word-box-x1 word)) 2)))
      (dolist (word words)
        (when (word-box-slope word)
          (setf (word-box-turn word) (float (* (/ 180 pi) (atan (- (word-box-slope word) tilt)))))))
      (let ((lines (make-hash-table)))
        (dolist (word words) (push word (gethash (word-box-y0 word) lines)))
        (loop for line being the hash-values of lines
              do (cond ((>= (length line) 3)
                        (let ((through (median-of (mapcar (lambda (w) (- (word-box-baseline w) (* tilt (centre w)))) line))))
                          (dolist (word line)
                            (when (wide-enough-p word)
                              (setf (word-box-offset word) (float (- (word-box-baseline word) (+ through (* tilt (centre word))))))))))
                       ;; two words, as a label and its figure: the right one set beside the left
                       ((= (length line) 2)
                        (destructuring-bind (left right) (sort (copy-list line) #'< :key #'word-box-x0)
                          (when (wide-enough-p right)
                            (setf (word-box-offset right)
                                (float (- (word-box-baseline right)
                                          (+ (word-box-baseline left) (* tilt (- (centre right) (centre left)))))))))))))))
  words)

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

(defun median-of (values)
  "The middle of VALUES, a list of numbers."
  (let ((sorted (sort (copy-list values) #'<)))
    (nth (floor (length sorted) 2) sorted)))

(defun medians-without (values)
  "For each of VALUES, a list of numbers, the middle of all the others, as
MEDIAN-OF finds it: the values sorted once, each one's left out by where
it stands among them."
  (let* ((sorted (sort (coerce values 'vector) #'<)) (n (length sorted)) (k (floor (1- n) 2)))
    (mapcar (lambda (v)
              (let ((at (position v sorted)))
                (aref sorted (if (< k at) k (1+ k)))))
            values)))

(defparameter *scan-measures*
  '((:ink word-box-ink 30 6 :line "~a ink" "darker" "lighter")
    (:edges word-box-edges 0.25 6 :line "~a edges" "sharper" "softer")
    (:noise word-box-noise 4 6 :line "~a paper" "cleaner" "noisier")
    (:offset word-box-offset 1.5 5 :page "a ~a baseline" "higher" "lower")
    (:turn word-box-turn 1.5 4 :page "turned ~a" "anticlockwise" "clockwise"))
  "What is measured of each word of a scan: its key, the measure, the least
difference from the usual that counts, how many times as far from it as
words typically are a word must be, what it is set beside, and its words:
how a word unlike the rest is said to be, ~a standing for what a word with
less, and with more, of it is. What it is set beside: :LINE, the other
words of its line (a heading or a footnote is a line of its own style);
:PAGE, the page's words, for a measure already taken against the word's
line (its baseline, its turn). In a line of two words, a label and its figure, the
right one is set beside the left.")

(defun unlike-words (words)
  "Those of WORDS (WORD-BOXes) whose ink, edges or paper stand apart from the
other words of their line, or that sit off their line or are turned against
the page, each (WORD (KEY OWN USUAL MORE-OR-LESS-WORDS)...): set beside what
*SCAN-MEASURES* says, several times as far from it as the page's words
typically are from theirs, and by more than the least that counts. NIL for
a page of fewer than 12 words."
  (when (>= (length words) 12)
    (let ((out '()) (lines (make-hash-table)))
      (dolist (word words) (push word (gethash (word-box-y0 word) lines)))
      (loop for (key measure least times beside nil fewer more) in *scan-measures*
            do (job-check-stop)
               (let ((usuals
                       ;; each word with what it is set beside, NIL where nothing is
                       (if (eq beside :page)
                           (mapcar #'cons words (medians-without (mapcar measure words)))
                           (mapcar (lambda (word)
                                     (cons word
                                           (let* ((line (gethash (word-box-y0 word) lines))
                                                  (peers (remove word line)))
                                             (cond ((>= (length peers) 2) (median-of (mapcar measure peers)))
                                                   ;; two words: the right one beside the left
                                                   ((and (= (length line) 2) (= (length peers) 1)
                                                         (> (word-box-x0 word) (word-box-x0 (first peers))))
                                                    (funcall measure (first peers)))))))
                                   words))))
                 (let* ((offs (loop for (word . usual) in usuals when usual collect (- (funcall measure word) usual)))
                        (spread (max (if offs (median-of (mapcar #'abs offs)) 0) (/ least 6))))
                   (loop for (word . usual) in usuals
                         when usual
                           do (let* ((own (funcall measure word)) (off (- own usual)))
                                (when (and (> (abs off) (* times spread)) (> (abs off) least))
                                  (let ((entry (or (assoc word out) (car (push (list word) out)))))
                                    (push (list key own usual (if (minusp off) fewer more)) (cdr entry)))))))))
      (nreverse out))))

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
(KEY OWN USUAL WORDS)...); GLYPHS its characters (WORD-GLYPHS)."
  page mark width height tilt odd glyphs)

(defun scan-pictures (pdf revision)
  "The scanned pictures of the pages of REVISION (SCAN-PICTURE-P), each read
once however many pages draw it, as SCAN-PICTUREs; a picture that is no page
of text, or cannot be read, left out; the characters of those past
*SCAN-CHARACTERS-IN-ALL* not kept."
  (let ((out '()) (characters 0))
    (loop for (m page) in (pictures-of pdf revision :keep #'scan-picture-p)
          do (job-check-stop)
             (multiple-value-bind (grey width height) (picture-grey pdf revision (mark-object m))
               (when grey
                 (multiple-value-bind (words tilt glyphs) (picture-words grey width height)
                   (when words
                     (if (> (incf characters (length glyphs)) *scan-characters-in-all*)
                         (setf glyphs '())
                         (dolist (g glyphs) (setf (glyph-picture g) (mark-object m) (glyph-page g) (page-index page))))
                     (push (make-scan-picture :page page :mark m :width width :height height :tilt tilt
                                              :odd (unlike-words words) :glyphs glyphs)
                           out))))))
    (nreverse out)))

(defun scan-picture-p (page mark)
  "True when MARK, a picture PAGE draws, covers a third of the page or more, as a scan of it does."
  (and (eq (mark-kind mark) :image) (mark-object mark)
       (>= (* (- (mark-x1 mark) (mark-x0 mark)) (- (mark-y1 mark) (mark-y0 mark)))
           (* 0.3 (- (third (page-media page)) (first (page-media page))) (- (fourth (page-media page)) (second (page-media page)))))))

(defun unlike-words-words (ways)
  "How a word unlike the rest is, from its WAYS ((KEY OWN USUAL WORDS)...), in
words: darker ink, a lower baseline."
  (remove-duplicates (mapcar (lambda (w) (format nil (sixth (assoc (first w) *scan-measures*)) (fourth w))) ways)
                     :test #'string=))

(defparameter *scan-looks* '(:ink :edges :noise)
  "The measures of *SCAN-MEASURES* that tell how a word looks; the others
tell where it sits.")

(defun looks-and-sits-unlike-p (ways)
  "True when the WAYS ((KEY OWN USUAL WORDS)...) a word is unlike the rest
hold both a way it looks (*SCAN-LOOKS*) and a way it sits: two measures
apart from each other agree."
  (and (some (lambda (w) (member (first w) *scan-looks*)) ways)
       (some (lambda (w) (not (member (first w) *scan-looks*))) ways)))

;;; Characters copied within a scan

(defun glyph-distance (a b limit)
  "How unlike the characters A and B (GLYPHs) are in their ink: the mean of
the squared differences of their grey levels over the pixels inked in
either (darker than halfway from A's lightest to its darkest), the
narrower set along the wider, a pixel higher and lower too (their tops
alike within a pixel, as one character's are), for the closest (a character
copied with a sliver of its neighbour is found in its copy). The paper
about them is left out: a copy laid with a Darken blend keeps its ink, not
its paper. NIL when no way of setting them together comes under LIMIT."
  (when (> (- (glyph-x1 a) (glyph-x0 a)) (- (glyph-x1 b) (glyph-x0 b))) (rotatef a b))
  (let* ((w (+ (- (glyph-x1 a) (glyph-x0 a)) 2))
         (h (+ (min (- (glyph-y1 a) (glyph-y0 a)) (- (glyph-y1 b) (glyph-y0 b))) 2))
         (wider (- (- (glyph-x1 b) (glyph-x0 b)) (- (glyph-x1 a) (glyph-x0 a))))
         (pa (glyph-pixels a)) (pb (glyph-pixels b)) (wa (glyph-width a)) (wb (glyph-width b))
         (ink 0) (most 0) (best nil))
    (declare (type grey-pixels pa pb) (type fixnum w h wider wa wb ink most)
             (optimize speed))
    ;; within each one's cut (GLYPH-CUT) its box begins two pixels in; A's window from a pixel before it
    (unless (<= (* (+ 1 h) wa) (length pa))
      (return-from glyph-distance nil))
    (let* ((k (- h (- (glyph-y1 a) (glyph-y0 a)) 1))
           (level (if (<= 0 k 1) (svref (glyph-inks a) k) (glyph-ink-level a h))))
      (setf ink (car level) most (cdr level)))
    ;; no comparison counts more pixels than both hold ink: a bound to stop early by
    (incf most (aref (the (simple-array (unsigned-byte 16) (17)) (glyph-darker b)) (min 16 (1+ (ash ink -4)))))
    (when (plusp most)
      (let ((bound (ceiling (* limit most))))
        (declare (type fixnum bound))
        (dolist (dy '(0 -1 1))
          (declare (type fixnum dy))
          (loop for dx fixnum from 0 to (+ 2 wider)
                do (let ((bx (+ 1 (if (> dx wider) -1 dx))) (by (+ 1 dy)) (sum 0) (n 0))
                     (declare (type fixnum bx by sum n))
                     (when (and (>= bx 0) (>= by 0) (<= (+ bx w) wb) (<= (* (+ by h) wb) (length pb)))
                       (block rows
                         (dotimes (j h)
                           (let ((ra (+ (* (+ 1 j) wa) 1)) (rb (+ (* (+ by j) wb) bx)))
                             (declare (type fixnum ra rb))
                             (dotimes (i w)
                               (let ((ga (aref pa (+ ra i))) (gb (aref pb (+ rb i))))
                                 (when (or (< ga ink) (< gb ink))
                                   (incf n)
                                   (let ((d (- ga gb))) (declare (type fixnum d)) (incf sum (* d d))))))
                             (when (>= sum bound) (return-from rows))))
                         (when (plusp n)
                           (let ((mean (/ sum n)))
                             (when (and (< mean limit) (or (null best) (< mean best)))
                               (setf best mean bound (min bound (ceiling (* mean most))))))))))))))
    best))

(defparameter *glyph-peers-most* 300
  "The most characters one is set beside for copies (GLYPH-PEERS): those
nearest it in ink, a page holding seldom more prints of one character.")

(defun glyph-peers (glyphs)
  "A function of a GLYPH giving those of GLYPHS that could be the same
character: as tall within a pixel, as wide within three, and of as much ink
within an eighth or so (a copy keeps its ink, give or take a few pixels),
the *GLYPH-PEERS-MOST* nearest it in ink, on its page or two pages either side (a character is copied from near at
hand; and so a long scan takes time as its pages do, not as their pairs)."
  (let ((sizes (make-hash-table :test 'equal)))
    (dolist (g glyphs)
      (push g (gethash (list (- (glyph-x1 g) (glyph-x0 g)) (- (glyph-y1 g) (glyph-y0 g)) (glyph-page g)) sizes)))
    (lambda (g)
      (let ((w (- (glyph-x1 g) (glyph-x0 g))) (h (- (glyph-y1 g) (glyph-y0 g))) (out '()))
        (loop for dw from -3 to 3
              do (loop for dh from -1 to 1
                       do (loop for page from (- (glyph-page g) 2) to (+ (glyph-page g) 2)
                                do (dolist (p (gethash (list (+ w dw) (+ h dh) page) sizes))
                                     (unless (or (eq p g) (> (abs (- (glyph-ink p) (glyph-ink g))) (max 3 (* 0.12 (glyph-ink g)))))
                                       (push p out))))))
        ;; those of the nearest ink first, as many as *GLYPH-PEERS-MOST*
        (let ((out (sort out #'< :key (lambda (p) (abs (- (glyph-ink p) (glyph-ink g)))))))
          (if (> (length out) *glyph-peers-most*) (subseq out 0 *glyph-peers-most*) out))))))

(defun alike-characters (glyphs peers)
  "How alike characters of the same shape typically are among GLYPHS: the
middle, over up to 400 of them spread through, of the root of the mean
squared difference to the nearest of its PEERS (GLYPH-PEERS), each counted
when under 30 grey levels. NIL for fewer than 40 such characters, too few to
tell."
  (let* ((n (length glyphs)) (step (max 1 (floor n 400)))
         (nearest (loop for g in glyphs for k from 0
                        when (zerop (mod k step))
                          do (job-check-stop)
                          and append (let ((best nil))
                                       (dolist (p (funcall peers g))
                                         (let ((d (glyph-distance g p (or best 900))))
                                           (when (and d (or (null best) (< d best))) (setf best d))))
                                       (and best (list (sqrt best)))))))
    (and (>= (length nearest) 40) (median-of nearest))))

(defun nearest-glyph (glyph peers limit &optional but)
  "The one of PEERS nearest GLYPH, BUT apart, as two values: it and the root
of its GLYPH-DISTANCE; NIL when none is nearer than LIMIT (a root too)."
  (let ((best nil) (bound (expt limit 2)))
    (dolist (p peers)
      (unless (eq p but)
        (let ((d (glyph-distance glyph p bound)))
          (when (and d (< d bound)) (setf best p bound d)))))
    (and best (values best (sqrt bound)))))

(defun copied-glyphs (glyphs)
  "The characters of GLYPHS, those of all the scanned pictures of a file,
that are copies of one another: as two values, the groups of copies, each
((GLYPH...) DIFFERENCE), DIFFERENCE the root of the mean squared difference
of its closest pair; and how alike characters of the same shape typically
are (ALIKE-CHARACTERS). Two characters are copies when they are alike to
half of that in their ink, its grain and all (to a quarter, pixel for
pixel); or when each is the other's nearest, nearer than three quarters of
it, and at least twice as near each other as either is to the next of its shape (there being one
within twice what is typical): two
prints of a character are as far from each other as from the rest, but a
copy saved again as a JPEG is near its source alone. Copies are told only on
scans with grain, where alike characters differ by 4 grey levels or more:
on a picture drawn rather than scanned every e is the same. Marks twice as
tall as the middle character, pieces of a table's border, are no characters."
  (let* ((tall (* 2 (or (median-of (mapcar (lambda (g) (- (glyph-y1 g) (glyph-y0 g))) glyphs)) 0)))
         ;; a piece of a table's border, too short to be taken out as a rule, is no character
         (glyphs (remove-if (lambda (g) (> (- (glyph-y1 g) (glyph-y0 g)) tall)) glyphs))
         (peers (glyph-peers glyphs))
         (alike (alike-characters glyphs peers)))
    (when (and alike (>= alike 4))
      (let* ((index (make-hash-table :test 'eq)) (parent (make-array (length glyphs)))
             (nearest (make-hash-table :test 'eq)) (closest (make-hash-table)))
        ;; each one's nearest, when nearer than three quarters of what is typical; the next, for those
        (loop for g in glyphs for k from 0
              do (when (zerop (mod k 50)) (job-check-stop))
                 (setf (gethash g index) k (aref parent k) k)
                 (let ((near (funcall peers g)))
                   (multiple-value-bind (p d) (nearest-glyph g near (* 3/4 alike))
                     (when p
                       (setf (gethash g nearest)
                             (list p d (nth-value 1 (nearest-glyph g near (* 2 alike) p))))))))
        (labels ((root (k) (if (= (aref parent k) k) k (setf (aref parent k) (root (aref parent k)))))
                 (join (a b d)
                   (let ((ra (root (gethash a index))) (rb (root (gethash b index))))
                     (unless (= ra rb) (setf (aref parent rb) ra))
                     (setf (gethash ra closest) (min d (or (gethash ra closest) d) (or (gethash rb closest) d))))))
          (loop for g being the hash-keys of nearest using (hash-value (p d next))
                do (let ((theirs (gethash p nearest)))
                     (when (or (<= d (/ alike 2))
                               (and theirs (eq (first theirs) g) next (third theirs)
                                    (<= (* 2 d) next) (<= (* 2 d) (third theirs))))
                       (join g p d))))
          (let ((groups (make-hash-table)))
            (loop for g in glyphs for k from 0 do (push g (gethash (root k) groups)))
            (values (loop for r being the hash-keys of groups using (hash-value members)
                          when (cdr members) collect (list (nreverse members) (gethash r closest)))
                    alike)))))))

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
  "Where GLYPH is, in words: on the page and in its picture."
  (let* ((scan (scan-of-glyph scans glyph)))
    (destructuring-bind (x0 y0 x1 y1) (glyph-page-box (scan-picture-mark scan) glyph (scan-picture-width scan) (scan-picture-height scan))
      (declare (ignore x1 y1))
      (format nil "page ~d at x ~d, y ~d (pixels ~d,~d to ~d,~d of picture ~d)"
              (1+ (glyph-page glyph)) (round x0) (round y0)
              (glyph-x0 glyph) (glyph-y0 glyph) (glyph-x1 glyph) (glyph-y1 glyph) (glyph-picture glyph)))))

(defun scans-read (pdf revision)
  "The scanned pictures of REVISION's pages read (SCAN-PICTURES) and the
characters copied among them (COPIED-GLYPHS), as three values: the
SCAN-PICTUREs, the groups of copies and how alike characters typically are.
Read once and kept with PDF, its characters' pixels let go but for the
copies', for the drawing (OVERLAY-OPERATORS) to find them again."
  (let ((kept (or (getf (pdf-cache pdf) :scans) (setf (getf (pdf-cache pdf) :scans) (make-hash-table :test 'eq)))))
    (values-list
     (or (gethash revision kept)
         (setf (gethash revision kept)
               (let ((scans (scan-pictures pdf revision)))
                 (multiple-value-bind (copies alike) (copied-glyphs (loop for scan in scans append (scan-picture-glyphs scan)))
                   (dolist (scan scans) (setf (scan-picture-glyphs scan) nil))
                   (list scans copies alike))))))))

(defun check-scans (pdf revision)
  "Pictures of a page whose words are unlike each other: words whose ink,
edges or paper stand apart from the rest of the scan, or that sit off their
line's baseline or are turned against the page. A word unlike the rest both
in how it looks and in where it sits is a strong sign; one unlike it in one
of them alone is worth a look, as a real scan has such words too. And
characters that are copies of one another, on one scanned page or across
them (COPIED-GLYPHS)."
  (multiple-value-bind (scans copies alike) (scans-read pdf revision)
    (let ((out '()))
      (dolist (scan scans)
        (let ((page (scan-picture-page scan)) (m (scan-picture-mark scan)) (odd (scan-picture-odd scan))
              (width (scan-picture-width scan)) (height (scan-picture-height scan)) (tilt (scan-picture-tilt scan)))
          (labels ((row (word y) (round (+ y (* (or tilt 0) (word-box-x0 word)))))
                   (report (strength odd both)
                     (when odd
                       (push (found strength (format nil "On page ~d, the scanned picture ~a (object ~d) has ~:d word~:p unlike the rest of it~a, ~{~a~^, ~}: pasted into the scan, or printed over it."
                                                     (1+ (page-index page)) (mark-text m) (mark-object m) (length odd)
                                                     (cond ((not both) "")
                                                           ((= (length odd) 1) " both in how it looks and where it sits")
                                                           (t " both in how they look and where they sit"))
                                                     (remove-duplicates (loop for (nil . ways) in odd append (unlike-words-words ways)) :test #'string=))
                                    :rule (if both
                                              "A page scanned in one go has one ink, one blur and one paper throughout, and its words stand on their lines; a word whose ink, edges or paper are unlike the rest and that also sits off its line or turned was put there another way: two measures apart from each other agree, which a real scan's unevenness seldom makes."
                                              "A page scanned in one go has one ink, one blur and one paper throughout; words whose ink is darker or lighter, whose edges are sharper or softer, or whose paper is cleaner or noisier than all the rest were put there another way.")
                                    :revision (revision-index revision) :page (1+ (page-index page)) :objects (list (mark-object m))
                                    :details (loop for (word . ways) in odd
                                                   collect (destructuring-bind (x0 y0 x1 y1) (word-page-box m word width height tilt)
                                                             (declare (ignore x1 y1))
                                                             (format nil "at x ~d, y ~d (pixels ~d,~d to ~d,~d of picture ~d): ~{~a~^; ~}"
                                                                     (round x0) (round y0)
                                                                     (word-box-x0 word) (row word (word-box-y0 word)) (word-box-x1 word) (row word (word-box-y1 word))
                                                                     (mark-object m)
                                                                     (loop for (key own usual nil) in ways
                                                                           collect (case key
                                                                                     (:ink (format nil "ink grey ~d, the page's ~d" (round own) (round usual)))
                                                                                     (:edges (format nil "half-tones ~,2f of its ink, the page's ~,2f" own usual))
                                                                                     (:noise (format nil "paper straying ~,1f, the page's ~,1f" own usual))
                                                                                     (:offset (format nil "its baseline ~,1f pixels ~:[above~;below~] its line's" (abs own) (plusp own)))
                                                                                     (:turn (format nil "turned ~,1f° ~:[anticlockwise~;clockwise~] against the page" (abs own) (plusp own))))))))
                                    :evidence (nth-value 0 (object-evidence pdf revision (mark-object m)))
                                    ;; a word unlike its line in how it looks was put there; one only off its line was moved
                                    :method (if (some (lambda (o) (some (lambda (w) (member (first w) *scan-looks*)) (cdr o))) odd)
                                                :pasted-on-scan :moved-in-scan))
                             out))))
            (report :strong (remove-if-not (lambda (o) (looks-and-sits-unlike-p (cdr o))) odd) t)
            (report :look (remove-if (lambda (o) (looks-and-sits-unlike-p (cdr o))) odd) nil))))
      (flet ((report (strength copies words rule)
               (when copies
                 (let ((pages (sort (remove-duplicates (loop for (group) in copies append (mapcar #'glyph-page group))) #'<))
                       (pictures (remove-duplicates (loop for (group) in copies append (mapcar #'glyph-picture group)))))
                   (push (found strength (format nil words (loop for (group) in copies sum (length group)) (length pages))
                                :rule rule
                                :revision (revision-index revision) :page (1+ (first pages)) :objects pictures
                                :details (loop for (group difference) in copies
                                               collect (format nil "~{~a~^, ~}: alike to ~,1f grey levels, the scan's characters of the same shape to ~,1f"
                                                               (mapcar (lambda (g) (glyph-words scans g)) group) difference alike))
                                :evidence (loop for p in pictures append (nth-value 0 (object-evidence pdf revision p)))
                                :method (if (eq strength :strong) :characters-composed :characters-composed-resaved))
                         out)))))
        (report :strong (remove-if (lambda (c) (> (second c) (/ alike 4))) copies)
                "~:d characters of the scanned page~p are copies of one another, pixel for pixel, the grain of their ink too: copied within the scan, or typed onto it."
                "No scanner draws a character twice alike: each print of it differs in the grain of its ink and in where the blur falls on the pixels. Two characters whose ink is alike to a quarter of how alike the scan's characters of the same shape typically are are one character copied, or both drawn by a program, as typed text is.")
        (report :look (remove-if-not (lambda (c) (> (second c) (/ alike 4))) copies)
                "~:d characters of the scanned page~p are nearer one another than prints of one character are: copied within the scan and saved again, or alike by chance."
                "A copy saved again as a JPEG is no longer alike pixel for pixel, but stays nearer its source than prints of a character are to one another: two characters whose ink is alike to half of how alike the scan's characters of the same shape typically are, or each the other's nearest and twice as near as to the next of its shape. Two prints of a character now and then come as near."))
      (nreverse out))))
