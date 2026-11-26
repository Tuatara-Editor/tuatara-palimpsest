;;;; header.lisp --- A JPEG's header and EXIF thumbnail set beside the device it names.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A camera, phone or scanner writes its JPEGs in a layout of its own: the
;;;; segments in its order, its EXIF first, a baseline frame, its tables.
;;;; A program that saves the picture again writes its own: Adobe's segment,
;;;; a progressive frame, often a JFIF segment first, which scanner drivers
;;;; and phones write too and so is given only for the record. A picture
;;;; whose EXIF names a device while its header is a program's was saved
;;;; again by one, though its EXIF was kept. The small picture the device
;;;; keeps in its EXIF, the thumbnail, is set beside the picture itself: an
;;;; edit that keeps the old EXIF leaves the old thumbnail. The header and
;;;; the EXIF are read as data, within limits; the thumbnail is turned into
;;;; pixels in the sandbox, as every picture is.

(in-package #:palimpsest)

;;; The header

(defparameter *thumbnail-most-pixels* 320
  "The most pixels across or down an EXIF thumbnail is set beside its
picture at: the EXIF standard's thumbnail is 160 by 120, and a larger one
would make the picture's reduced copies (THUMBNAIL-LIKENESS) large too. One
that is larger, up to *THUMBNAIL-LARGEST-PIXELS*, is made this small first.")

(defparameter *thumbnail-largest-pixels* 1024
  "The most pixels across or down an EXIF thumbnail may have to be judged.
Some phones keep one of 512 by 384; it is decoded at a half or a quarter of
its size and made no larger than *THUMBNAIL-MOST-PIXELS*. One whose frame
claims more is refused before it is decoded: a JPEG of 64 KB can claim
millions of pixels.")

(defun marker-name (marker)
  "The name of the JPEG segment MARKER: APP0 to APP15, DQT, DHT, SOF0 to
SOF15, DRI, COM, or its number in hex."
  (cond ((<= #xe0 marker #xef) (format nil "APP~d" (- marker #xe0)))
        ((= marker #xdb) "DQT")
        ((= marker #xc4) "DHT")
        ((= marker #xdd) "DRI")
        ((= marker #xfe) "COM")
        ((<= #xc0 marker #xcf) (format nil "SOF~d" (- marker #xc0)))
        (t (format nil "~2,'0x" marker))))

(defun layout-words (layout tables)
  "LAYOUT, a JPEG-LAYOUT, in words for the record, with TABLES, the JPEG's
quantisation tables as JPEG-TABLES gives them in a list: its segments, its
frame, its sampling, its Huffman tables and its steps, libjpeg's standard
ones at a quality or its own."
  (destructuring-bind (&optional octets quality table) tables
    (declare (ignore octets))
    (let ((standard (and table (standard-table-quality table))))
      (format nil "segments ~{~a~^ ~}; ~a frame~@[, sampling ~{~{~dx~d~}~^ ~}~]~@[; ~a Huffman tables~]~@[; ~a~]"
              (mapcar #'marker-name (jpeg-layout-markers layout))
              (case (jpeg-layout-frame layout)
                ((nil) "no") (#xc0 "a baseline") (#xc1 "an extended") (#xc2 "a progressive")
                (t (format nil "a ~a" (marker-name (jpeg-layout-frame layout)))))
              (jpeg-layout-sampling layout)
              (case (jpeg-layout-huffman layout) (:standard "the standard's") (:own "its own"))
              (cond (standard (format nil "libjpeg's standard steps at quality ~d" standard))
                    (quality (format nil "steps of its own, about quality ~d" quality)))))))

(defun layout-program-signs (layout)
  "The signs in LAYOUT, a JPEG-LAYOUT, that a program wrote the JPEG, not a
camera, phone or scanner, each in words: Adobe's APP14 segment, which
Adobe's programs write; a frame other than the baseline one the EXIF
standard has devices write. A JFIF segment before the EXIF, which the EXIF
standard does not allow, is no such sign: scanner drivers and phones write
it as programs built on libjpeg do."
  (append (and (jpeg-layout-adobe layout)
               (list "it holds Adobe's APP14 segment, which Adobe's programs write"))
          (let ((frame (jpeg-layout-frame layout)))
            (and frame (/= frame #xc0)
                 (list (format nil "its frame is ~a (~a), not the baseline one a device writes"
                               (case frame (#xc2 "progressive") (#xc1 "extended") (t "another kind"))
                               (marker-name frame)))))))

(defun header-findings (layout tables fields name &key place)
  "What the header of a JPEG, LAYOUT (a JPEG-LAYOUT) with TABLES (JPEG-TABLES'
values in a list), says beside the device and programs FIELDS name (as
PICTURE-FIELDS gives them); NAME is how the findings name the picture, and
PLACE the keys and values of FOUND that place them. Nothing when FIELDS
name no device. Worth a look, a device named while the header is a
program's (LAYOUT-PROGRAM-SIGNS) and no picture editor named; for the
record, the header beside the editor named, or a header as a device writes
it, a JFIF segment before the EXIF said then."
  (let ((device (picture-device fields)))
    (when device
      (let* ((editors (mapcar #'second (remove :editor (picture-programs fields) :key #'third :test-not #'eq)))
             (signs (layout-program-signs layout))
             (words (layout-words layout tables))
             (opening (format nil "~a~a" (char-upcase (char name 0)) (subseq name 1)))
             (rule (concatenate 'string "A camera, phone or scanner writes its JPEGs baseline, its EXIF first "
                                        "and without Adobe's segment (the EXIF standard, CIPA DC-008); a "
                                        "program that saves the picture again writes its own header, though it "
                                        "keeps the EXIF. A JFIF segment first, steps and Huffman tables are given "
                                        "for the record: many devices write these as libjpeg does.")))
        (list (cond (editors
                     (apply #'found :record
                            (format nil "~a names ~a as its maker and ~{~a~^, ~} as a program it went through; ~
                                         its header: ~a."
                                    opening device editors words)
                            :rule rule place))
                    (signs
                     (apply #'found :look
                            (format nil "~a names ~a as its maker, but a program wrote it: ~{~a~^; ~}. A ~
                                         device's own app that saves it again, a gallery, messaging or scanning ~
                                         app, or an honest crop or turn does the same."
                                    opening device signs)
                            :rule rule :details (list (format nil "its header: ~a" words)) :method :picture-editor
                            place))
                    (t
                     (apply #'found :record
                            (format nil "~a names ~a as its maker, and its header is laid out as a device ~
                                         writes it~:[~;, though a JFIF segment comes before its EXIF, as scanner ~
                                         drivers and phones write it as well as programs built on libjpeg~]: ~a."
                                    opening device (jpeg-layout-jfif-first layout) words)
                            :rule rule place))))))))

;;; The thumbnail

(defun grey-reduced (grey width height across down)
  "GREY, the grey levels of a picture WIDTH by HEIGHT a row after another,
made ACROSS by DOWN pixels, each the mean of the pixels that fall in it, as
double floats. ACROSS and DOWN are at most WIDTH and HEIGHT."
  (declare (type (simple-array (unsigned-byte 8) (*)) grey) (type fixnum width height across down))
  (let ((sums (make-array (* across down) :element-type 'fixnum :initial-element 0))
        (counts (make-array (* across down) :element-type 'fixnum :initial-element 0))
        (columns (make-array width :element-type 'fixnum))
        (out (make-array (* across down) :element-type 'double-float)))
    (dotimes (x width) (setf (aref columns x) (floor (* x across) width)))
    (dotimes (y height)
      (let ((row (* (floor (* y down) height) across)) (from (* y width)))
        (declare (type fixnum row from))
        (dotimes (x width)
          (let ((at (+ row (aref columns x))))
            (incf (aref sums at) (aref grey (+ from x)))
            (incf (aref counts at))))))
    (dotimes (k (length out) out)
      (setf (aref out k) (/ (float (aref sums k) 1d0) (max 1 (aref counts k)))))))

(defun levels-reduced (levels width height across down)
  "LEVELS, double floats of a picture WIDTH by HEIGHT a row after another,
made ACROSS by DOWN as GREY-REDUCED makes a picture's."
  (declare (type (simple-array double-float (*)) levels) (type fixnum width height across down))
  (let ((sums (make-array (* across down) :element-type 'double-float :initial-element 0d0))
        (counts (make-array (* across down) :element-type 'fixnum :initial-element 0)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((at (+ (the fixnum (* (floor (* y down) height) across)) (floor (* x across) width))))
          (incf (aref sums at) (aref levels (+ (* y width) x)))
          (incf (aref counts at)))))
    (dotimes (k (length sums) sums)
      (setf (aref sums k) (/ (aref sums k) (max 1 (aref counts k)))))))

(defun turned-point (u v quarter mirror)
  "Where the point U, V (across and down, each from 0 to 1) of a picture
turned QUARTER quarters clockwise, then mirrored left to right with MIRROR,
lies in the picture as it was, as two values."
  (let ((u (if mirror (- 1 u) u)))
    (ecase quarter
      (0 (values u v))
      (1 (values v (- 1 u)))
      (2 (values (- 1 u) (- 1 v)))
      (3 (values (- 1 v) u)))))

(defun levels-turned (levels width height quarter mirror)
  "LEVELS, of a picture WIDTH by HEIGHT a row after another, turned QUARTER
quarters clockwise, then mirrored left to right with MIRROR, as
TURNED-POINT turns a point; as three values: the levels, the width and the
height."
  (let* ((across (if (oddp quarter) height width)) (down (if (oddp quarter) width height))
         (out (make-array (* across down) :element-type 'double-float)))
    (dotimes (j down)
      (dotimes (i across)
        (let ((m (if mirror (- across 1 i) i)))
          (multiple-value-bind (x y) (ecase quarter
                                       (0 (values m j))
                                       (1 (values j (- height 1 m)))
                                       (2 (values (- width 1 m) (- height 1 j)))
                                       (3 (values (- width 1 j) m)))
            (setf (aref out (+ (* j across) i)) (aref levels (+ (* y width) x)))))))
    (values out across down)))

(defun levels-blurred (levels width height)
  "LEVELS, of a picture WIDTH by HEIGHT, each the mean of the 3 by 3 about it
within the picture: the small differences of how two programs make a
picture smaller set aside."
  (let ((out (make-array (length levels) :element-type 'double-float)))
    (dotimes (y height out)
      (dotimes (x width)
        (let ((sum 0d0) (count 0))
          (loop for yy from (max 0 (1- y)) to (min (1- height) (1+ y))
                do (loop for xx from (max 0 (1- x)) to (min (1- width) (1+ x))
                         do (incf sum (aref levels (+ (* yy width) xx))) (incf count)))
          (setf (aref out (+ (* y width) x)) (/ sum count)))))))

(defparameter *thumbnail-cell-least* 20
  "The mean difference in grey levels, out of 255, from which a cell of an
EXIF thumbnail differs from the picture in its place (THUMBNAIL-LIKENESS).
See doc/measures.org.")

(defparameter *thumbnail-cell-share* 2/5
  "The share of a cell's spread of grey levels, the thumbnail's and the
picture's added and 4 more, that its mean difference reaches for it to
differ (THUMBNAIL-LIKENESS). See doc/measures.org.")

(defparameter *thumbnail-alike-least* 4/5
  "The likeness, how far the thumbnail's grey levels follow the picture's
\(a correlation, 1 for the same), under which an EXIF thumbnail is unlike
its picture as a whole. See doc/measures.org.")

(defparameter *thumbnail-flat* 4
  "The spread of grey levels (a standard deviation) under which an EXIF
thumbnail shows too little to be set beside its picture.")

(defun thumbnail-fits (across down width height)
  "The places in a thumbnail ACROSS by DOWN pixels where a picture WIDTH by
HEIGHT may have been drawn, each (X0 Y0 X1 Y1 KIND): the whole of it,
stretched (:filling), and the largest part of its shape at the middle,
with bars about it (:within), when that differs."
  (let* ((scale (min (/ across width) (/ down height)))
         (w (max 1 (round (* width scale)))) (h (max 1 (round (* height scale))))
         (x0 (floor (- across w) 2)) (y0 (floor (- down h) 2)))
    (cons (list 0 0 across down :filling)
          (unless (and (= w across) (= h down)) (list (list x0 y0 (+ x0 w) (+ y0 h) :within))))))

(defun thumbnail-likeness (thumbnail across down grey width height)
  "How far THUMBNAIL, the grey levels of an EXIF thumbnail ACROSS by DOWN,
is like GREY, those of its picture, WIDTH by HEIGHT. The picture is made
the thumbnail's size in each of its eight turns and mirrorings and in each
place the thumbnail may draw it (THUMBNAIL-FITS), both are blurred a little
\(LEVELS-BLURRED), and the thumbnail's levels are fitted to the picture's
by a straight line, a lightening or a change of contrast set aside; the
best fit is kept. As a list (LIKENESS CELL SHARE BOX TURN): LIKENESS, the
correlation of the two, 1 for the same; CELL, the mean difference in grey
levels of the cell of the thumbnail (a tenth of the picture across, at
least 8 pixels) that differs most, among those that differ by
*THUMBNAIL-CELL-LEAST* or more, and SHARE, that difference over the cell's
spread of levels (both 0 when none differs so); BOX, that cell's place in
the picture as (X0 Y0 X1 Y1) in its pixels; TURN, how the picture was
fitted, in words. NIL and why not, in words, when it cannot be judged."
  (when (> (max across down) *thumbnail-most-pixels*)
    (return-from thumbnail-likeness (values nil (format nil "not a thumbnail's size (~d by ~d pixels)" across down))))
  (when (or (< (min across down) 16) (< (min width height) (min across down)))
    (return-from thumbnail-likeness (values nil "the thumbnail or the picture is too small")))
  (let* ((long (* 4 (max across down)))
         (scale (min 1 (/ long (max width height))))
         (w (max 1 (round (* width scale)))) (h (max 1 (round (* height scale))))
         (reduced (grey-reduced grey width height w h))
         (best nil))
    (dotimes (quarter 4)
      (dolist (mirror '(nil t))
        (multiple-value-bind (turned tw th) (levels-turned reduced w h quarter mirror)
          (loop for (x0 y0 x1 y1 kind) in (thumbnail-fits across down tw th)
                for rw = (- x1 x0) for rh = (- y1 y0)
                when (and (>= rw 16) (>= rh 16) (<= rw tw) (<= rh th))
                  do (let* ((a (levels-blurred (levels-reduced turned tw th rw rh) rw rh))
                            (b (let ((cut (make-array (* rw rh) :element-type 'double-float)))
                                 (dotimes (j rh) (dotimes (i rw)
                                                   (setf (aref cut (+ (* j rw) i))
                                                         (float (aref thumbnail (+ (* (+ y0 j) across) x0 i)) 1d0))))
                                 (levels-blurred cut rw rh)))
                            (n (* rw rh))
                            (ma (/ (reduce #'+ a) n)) (mb (/ (reduce #'+ b) n))
                            (sab 0d0) (saa 0d0) (sbb 0d0))
                       (dotimes (k n)
                         (let ((da (- (aref a k) ma)) (db (- (aref b k) mb)))
                           (incf sab (* da db)) (incf saa (* da da)) (incf sbb (* db db))))
                       (when (and (plusp saa) (>= (sqrt (/ sbb n)) *thumbnail-flat*))
                         (let* ((gain (/ sab saa))
                                (residual (map '(simple-array double-float (*))
                                               (lambda (va vb) (- vb (+ mb (* gain (- va ma))))) a b))
                                (misfit (/ (/ (reduce #'+ residual :key #'abs) n) (sqrt (/ sbb n)))))
                           (when (or (null best) (< misfit (first best)))
                             (setf best (list misfit (/ sab (sqrt (* saa sbb))) residual a b gain rw rh
                                              (list x0 y0 quarter mirror kind)))))))))))
    (unless best
      (return-from thumbnail-likeness (values nil "the thumbnail shows too little to be set beside the picture")))
    (destructuring-bind (misfit likeness residual a b gain rw rh (x0 y0 quarter mirror kind)) best
      (declare (ignore misfit))
      (let ((size (max 8 (floor rw 10))) (worst (list 0 0 nil)))
        (loop for cy from 0 to (- rh size) by size
              do (loop for cx from 0 to (- rw size) by size
                       do (let ((sum 0d0) (sa 0d0) (sa2 0d0) (sb 0d0) (sb2 0d0) (n (* size size)))
                            (dotimes (j size)
                              (dotimes (i size)
                                (let ((k (+ (* (+ cy j) rw) cx i)))
                                  (incf sum (abs (aref residual k)))
                                  (incf sa (aref a k)) (incf sa2 (expt (aref a k) 2))
                                  (incf sb (aref b k)) (incf sb2 (expt (aref b k) 2)))))
                            (let* ((cell (/ sum n))
                                   (spread (+ (sqrt (max 0d0 (- (/ sb2 n) (expt (/ sb n) 2))))
                                              (* (abs gain) (sqrt (max 0d0 (- (/ sa2 n) (expt (/ sa n) 2)))))
                                              4))
                                   (share (/ cell spread)))
                              (when (and (>= cell *thumbnail-cell-least*) (> share (second worst)))
                                (setf worst (list cell share (list cx cy (+ cx size) (+ cy size)))))))))
        (list likeness (first worst) (second worst)
              (and (third worst)
                   ;; the cell's corners in the turned picture, then in the picture as it is
                   (destructuring-bind (cx0 cy0 cx1 cy1) (third worst)
                     (multiple-value-bind (ua va) (turned-point (/ cx0 rw) (/ cy0 rh) quarter mirror)
                       (multiple-value-bind (ub vb) (turned-point (/ cx1 rw) (/ cy1 rh) quarter mirror)
                         (list (floor (* (min ua ub) width)) (floor (* (min va vb) height))
                               (ceiling (* (max ua ub) width)) (ceiling (* (max va vb) height)))))))
              (format nil "~:[as it is~;~:*turned ~d degrees clockwise~]~:[~;, mirrored~], ~a~@[ at ~d,~d~]"
                      (and (plusp quarter) (* 90 quarter)) mirror
                      (if (eq kind :filling) "filling the thumbnail" "within bars")
                      (and (eq kind :within) x0) (and (eq kind :within) y0)))))))

(defun thumbnail-findings (layout pixels name where &key place)
  "What the EXIF thumbnail LAYOUT (a JPEG-LAYOUT) keeps says beside the
picture whose grey levels, width and height PIXELS, a function, gives as
three values (NIL and why not when it cannot). NAME is how the findings
name the picture, WHERE how they name what the thumbnail's offset counts
from, and PLACE the keys and values of FOUND that place them. Worth a
look, a thumbnail unlike the picture as a whole or in a region
\(THUMBNAIL-LIKENESS); for the record, one alike, or one that could not be
judged, as one larger than *THUMBNAIL-LARGEST-PIXELS* across or down, refused
by the size its frame gives before it is decoded. A thumbnail larger than
*THUMBNAIL-MOST-PIXELS* is decoded made that small, so that it is set beside
the picture at that size."
  (let ((thumbnail (jpeg-layout-thumbnail layout))
        (rule (concatenate 'string "A camera, phone or scanner keeps a small copy of the picture in its EXIF; a "
                                   "program that changes the picture and keeps the EXIF keeps the old copy. The "
                                   "thumbnail is set beside the picture made as small, in each of its turns.")))
    (flet ((not-judged (why)
             (list (apply #'found :record (format nil "The EXIF thumbnail of ~a was not set beside it: ~a." name why)
                          :rule rule place))))
      (cond ((jpeg-layout-thumbnail-why layout) (not-judged (jpeg-layout-thumbnail-why layout)))
            ((null thumbnail) nil)
            ((let ((size (jpeg-layout thumbnail)))
               (and (jpeg-layout-width size)
                    (> (max (jpeg-layout-width size) (jpeg-layout-height size)) *thumbnail-largest-pixels*)
                    (not-judged (format nil "not a thumbnail's size (~d by ~d pixels)"
                                        (jpeg-layout-width size) (jpeg-layout-height size))))))
            (t
             (multiple-value-bind (image notes)
                 (decode-picture thumbnail :jpeg *thumbnail-most-pixels* *thumbnail-most-pixels*)
               (if (null image)
                   (not-judged (decode-refusal notes "it could not be decoded"))
                   (multiple-value-bind (grey width height) (funcall pixels)
                     (if (null grey)
                         (not-judged (format nil "the picture's pixels could not be read: ~a" width))
                         (multiple-value-bind (measured why)
                             (thumbnail-likeness (image-grey image) (image-width image) (image-height image)
                                                 grey width height)
                           (if (null measured)
                               (not-judged why)
                               (destructuring-bind (likeness cell share box turn) measured
                                 (let ((details (list (format nil "the thumbnail: ~d by ~d pixels, ~:d octets from ~
                                                                   octet ~:d of ~a"
                                                              (image-width image) (image-height image)
                                                              (length thumbnail) (jpeg-layout-thumbnail-at layout)
                                                              where)
                                                      (format nil "the picture fitted to it ~a" turn)
                                                      (format nil "likeness ~,3f; the cell that differs most ~
                                                                   ~,1f grey levels, ~,2f of its spread"
                                                              likeness cell share)))
                                       (steps (list (format nil "Cut the thumbnail out of ~a: ~:d octets from ~
                                                                 octet ~:d (dd bs=1 skip=~d count=~d), and open it ~
                                                                 beside the picture."
                                                            where (length thumbnail)
                                                            (jpeg-layout-thumbnail-at layout)
                                                            (jpeg-layout-thumbnail-at layout) (length thumbnail)))))
                                   (list
                                    (cond ((< likeness *thumbnail-alike-least*)
                                           (apply #'found :look
                                                  (format nil "The EXIF thumbnail of ~a is unlike the picture as a ~
                                                               whole (likeness ~,2f, under ~,2f): the picture was ~
                                                               changed or replaced after the thumbnail was made. An ~
                                                               honest crop by a program that keeps the old ~
                                                               thumbnail does the same."
                                                          name likeness *thumbnail-alike-least*)
                                                  :rule rule :details details :steps steps :method :picture-editor
                                                  place))
                                          ((>= share *thumbnail-cell-share*)
                                           (apply #'found :look
                                                  (format nil "The EXIF thumbnail of ~a differs from the picture at ~
                                                               pixels ~{~d,~d to ~d,~d~} (~,1f grey levels, ~,2f of ~
                                                               the spread there, ~,2f or more being worth a look): ~
                                                               changed there after the thumbnail was made. A ~
                                                               program's local retouching, or a crop or straightening ~
                                                               that keeps the old thumbnail, does the same."
                                                          name box cell share (float *thumbnail-cell-share*))
                                                  :rule rule :details details :steps steps :method :picture-editor
                                                  place))
                                          (t
                                           (apply #'found :record
                                                  (format nil "The EXIF thumbnail of ~a is like the picture ~
                                                               (likeness ~,2f), nowhere differing as an edit made ~
                                                               after it would. An edit whose program writes a new ~
                                                               thumbnail leaves no such sign."
                                                          name likeness)
                                                  :rule rule :details details :steps steps place)))))))))))))))))

;;; The checks

(defun check-jpeg-header-file (file)
  "What the header and EXIF thumbnail of FILE, a JPEG PICTURE-FILE, say beside
the device it names (HEADER-FINDINGS, THUMBNAIL-FINDINGS)."
  (let ((layout (picture-file-layout file)))
    (when layout
      (append (header-findings layout (picture-file-tables file) (picture-fields (picture-file-sections file))
                               "the picture")
              (thumbnail-findings layout
                                  (lambda () (if (picture-file-grey file)
                                                 (values (picture-file-grey file) (picture-file-width file)
                                                         (picture-file-height file))
                                                 (values nil (picture-file-refusal file))))
                                  "the picture" "the file")))))

(defun check-jpeg-headers (pdf revision)
  "What the header and EXIF thumbnail of each JPEG picture REVISION of PDF
draws say beside the device its EXIF names (HEADER-FINDINGS,
THUMBNAIL-FINDINGS)."
  (let ((out '()))
    (loop for (m page) in (jpeg-pictures pdf revision)
          do (job-check-stop)
             (guarding-item (out "the header of the picture ~a (object ~d)" (mark-text m) (mark-object m))
               (let ((head (jpeg-picture-head pdf revision m)))
                 (when head
                   (let* ((layout (jpeg-layout head))
                          (fields (picture-fields (octets-metadata head :jpeg)))
                          (name (format nil "the JPEG picture ~a (object ~d) on page ~d"
                                        (mark-text m) (mark-object m) (1+ (page-index page))))
                          (place (list :revision (revision-index revision) :page (1+ (page-index page))
                                       :objects (list (mark-object m))
                                       :evidence (nth-value 0 (object-evidence pdf revision (mark-object m))))))
                     (setf out (append (reverse
                                        (append (header-findings layout (jpeg-layout-tables layout)
                                                                 fields name :place place)
                                                (thumbnail-findings
                                                 layout (lambda () (picture-grey pdf revision (mark-object m)))
                                                 name (format nil "the data of object ~d with the filters before ~
                                                                   the JPEG's undone"
                                                              (mark-object m))
                                                 :place place)))
                                       out)))))))
    (nreverse out)))
