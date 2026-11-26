;;;; jpeg.lisp --- A JPEG's segments before its picture data, read in one walk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Everything palimpsest reads of a JPEG before its picture data comes from
;;;; one walk of its segments (JPEG-LAYOUT): the order of the segments, the
;;;; frame, the Huffman and quantisation tables, Adobe's and JFIF's segments,
;;;; the resolution it says it was made at, and its EXIF with the thumbnail it
;;;; keeps, read by Tuatara's EXIF reader. All of it is read as data, every
;;;; length checked against the octets.

(in-package #:palimpsest)

(defparameter *standard-huffman-lengths*
  '((0 0 1 5 1 1 1 1 1 1 0 0 0 0 0 0 0)
    (0 0 3 1 1 1 1 1 1 1 1 1 0 0 0 0 0)
    (1 0 2 1 3 3 2 4 3 5 5 4 4 0 0 1 125)
    (1 0 2 1 2 4 4 3 4 7 5 4 4 0 1 2 119))
  "The Huffman tables of the JPEG standard's examples (ITU-T T.81, Annex K.3)
that libjpeg and most devices write unless told to make their own, each its
class (0 for the blocks' means, 1 for their other frequencies) and how many
codes it has of each length from 1 to 16 bits: the luminance and the
chrominance table of each class.")

(defparameter *jpeg-luminance-table*
  #(16 11 12 14 12 10 16 14 13 14 18 17 16 19 24 40 26 24 22 22 24 49 35 37 29 40 58 51 61 60 57 51
    56 55 64 72 92 78 64 68 87 69 55 56 80 109 81 87 95 98 103 104 103 62 77 113 121 112 100 120 92 101 103 99)
  "The luminance table of the JPEG standard's example, in the order a JPEG
keeps it, that the usual encoders scale for each quality.")

(defstruct (jpeg-layout (:copier nil) (:predicate nil))
  "How a JPEG is laid out before its picture data (JPEG-LAYOUT). MARKERS is
its segments' markers in order, as numbers. FRAME is its frame's marker:
#xC0 for a baseline frame, #xC2 for a progressive one; NIL when none was
read. WIDTH and HEIGHT are the size in pixels the frame gives. SAMPLING
is, for each component of the frame, how many samples it keeps across and
down for each of the coarsest one's, as (ACROSS DOWN). ADOBE is true when
it holds Adobe's APP14 segment. JFIF-FIRST is true when a JFIF segment
\(APP0) comes before its EXIF. HUFFMAN is :standard when every Huffman
table read before the picture data is one of the standard's
\(*STANDARD-HUFFMAN-LENGTHS*), :own when any is not, NIL when none was
read. TABLES is its quantisation tables as JPEG-TABLES gives them, in a
list. DENSITY is the resolution it says it was made at as JPEG-DENSITY
gives it, in a list; NIL when it says none. EXIF is the tags of its EXIF
\(EXIF-TAGS). THUMBNAIL is the octets of the thumbnail its EXIF keeps, a
JPEG of its own, and THUMBNAIL-AT where they begin among the JPEG's octets;
NIL when it keeps none that can be read, and THUMBNAIL-WHY then says why
when it keeps one."
  (markers '()) frame width height (sampling '()) adobe jfif-first huffman (tables '()) density (exif '())
  thumbnail thumbnail-at thumbnail-why)

(defparameter *thumbnail-most-octets* 65536
  "The most octets an EXIF thumbnail may take: an EXIF lies in one segment of
at most 64 KB, its thumbnail within it.")

(defun start-of-frame-p (marker)
  "True when MARKER, a JPEG segment's, begins a frame (SOF0 to SOF15): not
DHT, JPG nor DAC, which share its range."
  (and (<= #xc0 marker #xcf) (not (member marker '(#xc4 #xc8 #xcc)))))

(defun huffman-standard-p (octets start end)
  "True when every Huffman table of the DHT segment of OCTETS from START to
END has the code lengths of one of the standard's of its class
\(*STANDARD-HUFFMAN-LENGTHS*); NIL also when a table is cut short."
  (loop with at = start
        while (< at end)
        do (when (> (+ at 17) end) (return nil))
           (let ((lengths (cons (ldb (byte 4 4) (aref octets at))
                                (loop for k from 1 to 16 collect (aref octets (+ at k))))))
             (unless (member lengths *standard-huffman-lengths* :test #'equal) (return nil))
             (incf at (+ 17 (reduce #'+ (rest lengths)))))
        finally (return (= at end))))

(defun exif-thumbnail (octets start end tags)
  "The thumbnail the EXIF whose TIFF structure lies in OCTETS from START to
END keeps, its TAGS as EXIF-TAGS gives them: its octets and where they
begin among OCTETS, as two values. NIL and why not, in words, when its
thumbnail is not a JPEG lying wholly within the EXIF in at most
*THUMBNAIL-MOST-OCTETS*; NIL alone when it names none."
  (multiple-value-bind (offset length compression) (exif-thumbnail-place tags)
    (cond ((null offset) nil)
          ((not (member compression '(nil 6))) (values nil "it is kept without JPEG compression"))
          ((not (< 0 length *thumbnail-most-octets*)) (values nil (format nil "it says it takes ~:d octets" length)))
          ((> (+ start offset length) end) (values nil "it says it lies past the end of the EXIF"))
          ((not (octets-at-p octets '(#xff #xd8) (+ start offset))) (values nil "it does not begin as a JPEG does"))
          (t (values (subseq octets (+ start offset) (+ start offset length)) (+ start offset))))))

(defun tables-quality (table)
  "The quality from 1 to 100 the luminance TABLE, its 64 steps in the order
a JPEG keeps them, stands for when made by scaling the standard's
\(*JPEG-LUMINANCE-TABLE*)."
  (let ((scale (/ (loop for q in table for s across *jpeg-luminance-table* sum (/ (* 100 q) s)) 64)))
    (max 1 (min 100 (round (if (<= scale 100) (/ (- 200 scale) 2) (/ 5000 scale)))))))

(defun jpeg-layout (octets)
  "How the JPEG in OCTETS is laid out before its picture data, as a
JPEG-LAYOUT: its segments in order, its frame and sampling, its Huffman and
quantisation tables, Adobe's and JFIF's segments, the resolution it says it
was made at, its EXIF's tags and thumbnail. Read in one walk of its
segments, as data, every length checked against the octets (JPEG-SEGMENTS).
The luminance table is the one its frame's first component uses, whatever
its number and wherever it lies among the others; with no frame read, the
first table. A DQT segment that says it is longer than the octets hold is
read as far as they go; a table cut short is not read."
  (let ((layout (make-jpeg-layout)) (exif-seen nil) (huffman '()) (jfif nil) (exif-density nil)
        (tables (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (numbered (make-array 4 :initial-element nil)) (first-read nil) (luminance nil))
    (jpeg-segments
     octets
     (lambda (marker start end)
       (push marker (jpeg-layout-markers layout))
       (cond ((and (= marker #xe0) (octets-at-p octets (format nil "JFIF~c" #\Nul) start))
              (unless exif-seen (setf (jpeg-layout-jfif-first layout) t))
              (when (and (null jfif) (>= (- end start) 12))
                (let ((unit (aref octets (+ start 7)))
                      (x (octets-integer octets (+ start 8) 2 :big-endian t))
                      (y (octets-integer octets (+ start 10) 2 :big-endian t)))
                  (when (and (member unit '(1 2)) (plusp x) (plusp y))
                    (setf jfif (if (= unit 1) (list x y) (list (* x 2.54) (* y 2.54))))))))
             ((and (= marker #xe1) (not exif-seen) (octets-at-p octets (format nil "Exif~c~c" #\Nul #\Nul) start))
              (setf exif-seen t)
              (let ((tags (exif-tags octets :start (min end (+ start 6)) :end end)))
                (setf (jpeg-layout-exif layout) tags
                      exif-density (exif-resolution tags))
                (multiple-value-bind (thumbnail at) (exif-thumbnail octets (+ start 6) end tags)
                  (if thumbnail
                      (setf (jpeg-layout-thumbnail layout) thumbnail (jpeg-layout-thumbnail-at layout) at)
                      (setf (jpeg-layout-thumbnail-why layout) at)))))
             ((and (= marker #xee) (octets-at-p octets "Adobe" start))
              (setf (jpeg-layout-adobe layout) t))
             ((= marker #xc4)
              (push (huffman-standard-p octets start end) huffman))
             ((= marker #xdb)
              (loop for k from start below end do (vector-push-extend (aref octets k) tables))
              ;; a segment may hold several tables, each its precision and number, then its steps
              (loop with at = start
                    while (< at end)
                    do (let* ((wide (plusp (ldb (byte 4 4) (aref octets at))))
                              (number (ldb (byte 2 0) (aref octets at)))
                              (size (if wide 128 64)))
                         (when (> (+ at 1 size) end) (loop-finish))
                         (let ((table (loop for k below 64
                                            collect (if wide
                                                        (octets-integer octets (+ at 1 (* 2 k)) 2 :big-endian t)
                                                        (aref octets (+ at 1 k))))))
                           (setf (svref numbered number) table)
                           (unless first-read (setf first-read table)))
                         (incf at (1+ size)))))
             ((start-of-frame-p marker)
              (when (and (null (jpeg-layout-frame layout)) (<= (+ start 6) end))
                (setf (jpeg-layout-frame layout) marker
                      (jpeg-layout-height layout) (octets-integer octets (+ start 1) 2 :big-endian t)
                      (jpeg-layout-width layout) (octets-integer octets (+ start 3) 2 :big-endian t)
                      (jpeg-layout-sampling layout)
                      (loop for k below (min 4 (aref octets (+ start 5)))
                            for at = (+ start 6 (* 3 k))
                            while (<= (+ at 3) end)
                            collect (let ((factors (aref octets (1+ at))))
                                      (list (ldb (byte 4 4) factors) (ldb (byte 4 0) factors))))))
              ;; the frame's first component's table number is its ninth octet
              (when (and (null luminance) (<= (+ start 9) end))
                (setf luminance (or (svref numbered (ldb (byte 2 0) (aref octets (+ start 8)))) first-read)))))
       nil))
    (let ((first-table (or luminance first-read)))
      (setf (jpeg-layout-markers layout) (nreverse (jpeg-layout-markers layout))
            (jpeg-layout-huffman layout) (cond ((null huffman) nil) ((every #'identity huffman) :standard) (t :own))
            (jpeg-layout-tables layout) (list (and (plusp (length tables))
                                                   (coerce tables '(simple-array (unsigned-byte 8) (*))))
                                              (and first-table (tables-quality first-table))
                                              first-table)
            (jpeg-layout-density layout) (cond (jfif (list (first jfif) (second jfif) "JFIF"))
                                               (exif-density (list (first exif-density) (second exif-density) "EXIF"))
                                               (t nil))))
    layout))

(defun jpeg-tables (octets)
  "The quantisation tables a JPEG's OCTETS hold (its DQT segments), as three
values: their octets together; the quality from 1 to 100 its luminance
table stands for when made by scaling the standard's (NIL for none); and
that table, its 64 steps in the order the JPEG keeps them (JPEG-LAYOUT says
which table that is). One who has the JPEG's layout reads them from it
\(JPEG-LAYOUT-TABLES) instead."
  (values-list (jpeg-layout-tables (jpeg-layout octets))))

(defun jpeg-density (octets)
  "The pixels to the inch across and down the JPEG in OCTETS says it was made
at, as two values: by its JFIF header when that gives inches or centimetres,
else by the resolution of its EXIF; NIL when it says none (a JFIF header
giving only how its pixels are shaped says none). A third value says which,
\"JFIF\" or \"EXIF\". One who has the JPEG's layout reads it from it
\(JPEG-LAYOUT-DENSITY) instead."
  (values-list (jpeg-layout-density (jpeg-layout octets))))

(defun jpeg-tables-words (tables)
  "TABLES, a JPEG's quantisation tables as JPEG-LAYOUT-TABLES gives them, in
words for a fingerprint: a short name for their octets (FNV-HEX) and the
quality they stand for; NIL when there are none."
  (destructuring-bind (&optional octets quality table) tables
    (declare (ignore table))
    (and octets (format nil "tables ~a~@[, about quality ~d~]" (fnv-hex octets) quality))))

(defun picture-head-layout (pdf revision mark)
  "The layout (JPEG-LAYOUT) of the JPEG picture MARK of REVISION of PDF
draws when it is kept with that filter alone, read from the first 64 KB of
its stream, which hold its segments before the picture data; NIL for any
other picture, or one whose layout cannot be read."
  (let ((filters (mapcar #'car (pdf-stream-filters (mark-actual mark)))))
    (when (and (= 1 (length filters)) (jpeg-filter-p (first filters)))
      (let ((object (revision-object pdf revision (mark-object mark))))
        (and object (handler-case (jpeg-layout (raw-stream pdf object 65536)) (error () nil)))))))
