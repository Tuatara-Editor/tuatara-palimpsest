;;;; generated.lisp --- Signs in a scanned page's pixels that an image generator drew it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An image generator works on a picture eight times smaller than the one
;;;; it gives, and its decoder draws each cell of 8 by 8 pixels from it. That
;;;; leaves a faint pattern repeating every 8 pixels across and down in the
;;;; flat parts of the picture, a pattern that changes along both at once. A
;;;; JPEG's blocks repeat every 8 pixels too, but their steps run along whole
;;;; rows and columns of the blocks' edges, as a scanner's sensor runs along
;;;; its columns; so the pattern is weighed by what is left once each row's
;;;; and each column's share is taken out.

(in-package #:palimpsest)

(defparameter *generator-grid-least* 4
  "How far above chance the pattern repeating every 8 by 8 pixels in a
picture's flat parts must stand, once what runs along its rows and columns
is taken out, for it to be weighed at all (GENERATOR-GRID): the mean of each
place's squared offset over its standard error, about 1 for a picture with
no such pattern. Under chance 49 times it falls as a chi-square of 49
degrees of freedom, so 4 (196) comes by chance less than once in a million
million pictures, however large. It says the pattern is there, not how
strong it is: that is *GENERATOR-GRID-EFFECT*.")

(defparameter *generator-grid-effect* '(0.075 . 0.15)
  "The least and the most the pattern repeating every 8 by 8 pixels may be,
as the root of the mean of its squared offsets (four times a pixel less its
four neighbours, averaged at each place of the grid, the rows' and columns'
share taken out) over the root of the mean of the flat pixels' own squared
offsets, for it to be taken for an image generator's (GENERATOR-GRID-P): a
measure of the pattern against the picture's own noise, the same at every
size of page. The generator's pictures kept without loss gave 0.016 to
0.109, 46 of 76 named at 0.075 or more (45 with the floors of flat pixels
and noise). Of 431 honest pictures kept without
loss and measured, JPEG pages cropped and saved again among them, none came
above 0.054, and none above 0.049 where the pattern changed along both at
once as a generator's does. Pages made up for the measuring with even
stripes of text, cropped and saved again, gave 0.08 to 0.098, with at most
60 % of it along both at once. Ordered dither and a half-tone screen on grey
paper gave 0.255 to 17, far stronger than a decoder's.")

(defparameter *generator-grid-share* 4/5
  "How much of the pattern repeating every 8 by 8 pixels in a picture's flat
parts must change along rows and columns at once, as a share of the whole
pattern's strength measured alike, for it to be taken for an image
generator's (GENERATOR-GRID). The generator's pictures named gave 0.86 to
1.24. A JPEG's blocks run along rows and columns: JPEGs and pictures once
JPEGs gave mostly under 0.6, but a JPEG cropped by a few pixels and saved
again, its two grids out of step, up to 1.0, which is why a picture once a
JPEG is not measured at all.")

(defparameter *generator-grid-flat-least* 2000
  "The fewest flat pixels each of the 64 places of the grid of 8 by 8 pixels
must hold for a picture's pattern to be measured (GENERATOR-GRID). Below a
few thousand, chance alone moves the pattern's size against the noise
\(*GENERATOR-GRID-EFFECT*) as far as a decoder's: at 300, a pattern just
above chance comes to 0.1. Of the generator's pictures, 69 of 82 hold 2,000
or more, and one of the 46 named with 300 is lost.")

(defparameter *generator-grid-noise-least* 1.5
  "The least noise of a picture's flat pixels, the root of the mean of their
squared offsets (four times a pixel less its four neighbours), for its
pattern to be judged (GENERATOR-GRID-P). A JPEG smooths the noise of plain
grey paper down to 0.15 to 1.06, and two saves out of step then leave a
pattern that, set over so little noise, passes for a decoder's; a picture
whose first save left no history to tell is so named. The generator's
pictures gave 1.29 and up, those named 2.17 and up; honest scans 0.05 to
9.")

(defun generator-grid (grey width height)
  "How strongly the flat parts of the picture GREY (WIDTH by HEIGHT) carry a
pattern repeating every 8 by 8 pixels, as four values: its strength once
what runs along its rows and columns is taken out, the strength of the whole
pattern, the root of the mean of its squared offsets, and the root of the
mean of the flat pixels' own squared offsets, its noise. A pixel is flat when the 3 by 3
pixels about it lie within 6 grey levels, none clipped to black or white.
Each flat pixel's offset is four times its grey less its four neighbours';
the offsets are averaged at each of the 64 places of the grid, and each
place's mean, less the mean of all, is set over its standard error. The
strengths are the mean of those squared, over the 63 places free to vary, and
over the 49 once each row's and column's mean is taken out too. NIL when a
place holds fewer than *GENERATOR-GRID-FLAT-LEAST* flat pixels, or its
offsets do not vary."
  (declare (type grey-pixels grey) (type fixnum width height) (optimize speed))
  (let ((counts (make-array 64 :element-type 'fixnum :initial-element 0))
        (sums (make-array 64 :element-type 'fixnum :initial-element 0))
        (squares (make-array 64 :element-type 'double-float :initial-element 0d0)))
    (loop for y fixnum from 1 below (- height 1)
          do (let ((row (* y width)) (place (* 8 (logand y 7))))
               (declare (type fixnum row place))
               (loop for x fixnum from 1 below (- width 1)
                     do (let ((g (aref grey (+ row x))))
                          (when (< 2 g 253)
                            (let ((lo g) (hi g))
                              (declare (type fixnum lo hi))
                              (loop for at fixnum from (- (+ row x) width 1) by width
                                    repeat 3
                                    do (loop for i fixnum from at to (+ at 2)
                                             do (let ((v (aref grey i)))
                                                  (when (< v lo) (setf lo v))
                                                  (when (> v hi) (setf hi v)))))
                              (when (and (<= (- hi lo) 6) (< 2 lo) (< hi 253))
                                (let ((offset (- (* 4 g) (aref grey (- (+ row x) width)) (aref grey (+ row x width))
                                                 (aref grey (+ row x -1)) (aref grey (+ row x 1))))
                                      (k (+ place (logand x 7))))
                                  (declare (type fixnum offset k))
                                  (incf (aref counts k))
                                  (incf (aref sums k) offset)
                                  (incf (aref squares k) (float (* offset offset) 1d0))))))))))
    (when (every (lambda (n) (>= n *generator-grid-flat-least*)) counts)
      (let ((means (make-array 64 :element-type 'double-float))
            (errors (make-array 64 :element-type 'double-float)))
        (dotimes (k 64)
          (let* ((n (float (aref counts k) 1d0)) (mean (/ (aref sums k) n))
                 (variance (- (/ (aref squares k) n) (* mean mean))))
            (when (<= variance 0d0) (return-from generator-grid nil))
            (setf (aref means k) mean (aref errors k) (/ variance n))))
        (let ((all (/ (reduce #'+ means) 64)) (whole 0d0) (across 0d0) (faint 0d0) (noise 0d0)
              (rows (make-array 8 :element-type 'double-float :initial-element 0d0))
              (columns (make-array 8 :element-type 'double-float :initial-element 0d0)))
          (dotimes (k 64)
            (decf (aref means k) all)
            (incf (aref rows (floor k 8)) (/ (aref means k) 8))
            (incf (aref columns (mod k 8)) (/ (aref means k) 8)))
          (dotimes (k 64)
            (let ((left (- (aref means k) (aref rows (floor k 8)) (aref columns (mod k 8)))))
              (incf whole (/ (expt (aref means k) 2) (aref errors k)))
              (incf across (/ (* left left) (aref errors k)))
              (incf faint (* left left))
              (incf noise (* (aref errors k) (aref counts k)))))
          (values (/ across 49) (/ whole 63) (sqrt (/ faint 64)) (sqrt (/ noise 64))))))))

(defun generator-grid-p (strength &optional whole faint noise)
  "True when a picture's pattern repeating every 8 by 8 pixels, measured as
GENERATOR-GRID gives STRENGTH, WHOLE, FAINT and NOISE (NIL for one not
measured), is an image generator's: above chance by *GENERATOR-GRID-LEAST*,
changing along rows and columns at once for *GENERATOR-GRID-SHARE* of the
whole or more, NOISE no less than *GENERATOR-GRID-NOISE-LEAST*, and FAINT
over NOISE within *GENERATOR-GRID-EFFECT*."
  (and strength (>= strength *generator-grid-least*) (plusp whole)
       (>= noise *generator-grid-noise-least*)
       (>= (/ strength whole) *generator-grid-share*)
       (<= (car *generator-grid-effect*) (/ faint noise) (cdr *generator-grid-effect*))))

(defun picture-lossy-p (pdf revision mark)
  "True when the picture MARK draws, or one of its strips, is kept with loss
\(LOSSY-FILTER-P), as a JPEG or a JPEG 2000 rounds its pixels in blocks of
its own; or, for a picture file examined as a page, when it keeps its pixels
as JPEG data: a JPEG, or a TIFF compressed as one (PICTURE-FILE-JPEG-TIFF)."
  (some (lambda (number)
          (if *picture-file*
              (or (picture-jpeg-p pdf revision number) (picture-file-jpeg-tiff *picture-file*))
              (some #'lossy-filter-p (picture-filters pdf revision number))))
        (mark-objects mark)))

(defun never-jpeg-p (pdf revision mark once)
  "True when the scanned picture MARK draws in REVISION of PDF was never saved
as a JPEG: it is not kept with loss (PICTURE-LOSSY-P) and ONCE, its
SCAN-PICTURE-ONCE-JPEG, is NIL."
  (not (or once (picture-lossy-p pdf revision mark))))

(defun scan-generator-grid (pdf revision mark grey width height once)
  "The measure of the pattern an image generator leaves (GENERATOR-GRID) in the
scanned picture MARK draws in REVISION of PDF, whose grey levels are GREY
\(WIDTH by HEIGHT), as a list (STRENGTH WHOLE FAINT NOISE); NIL when it is
kept as a JPEG or a JPEG 2000, or was ONCE (NEVER-JPEG-P), whose blocks
hide such a pattern and, saved twice on grids out of step, make one; or
when it cannot be measured."
  (when (never-jpeg-p pdf revision mark once)
    (let ((measured (multiple-value-list (generator-grid grey width height))))
      (and (first measured) measured))))

(defun check-generator-grid (pdf revision)
  "The scanned pages of REVISION of PDF kept without loss whose flat parts
carry the pattern an image generator's decoder leaves (GENERATOR-GRID-P),
one finding, worth a look, for all of them. Never strong: a picture tiled
with a faint pattern of 8 by 8 pixels, or a JPEG saved twice on grids out of
step whose first save left no trace to tell, carries one too."
  (let ((scans (remove-if-not (lambda (scan)
                                (and (not (scan-picture-twin scan)) (scan-picture-grid scan)
                                     (apply #'generator-grid-p (scan-picture-grid scan))))
                              (scan-reading-scans (scans-read pdf revision)))))
    (when scans
      (let ((pages (sort (remove-duplicates (mapcar (lambda (s) (1+ (page-index (scan-picture-page s)))) scans)) #'<)))
        (list (found :look (format nil "~a in ~:[its~;their~] flat parts a faint pattern repeating every 8 by 8 ~
                                        pixels, as the decoder of an image generator leaves it: ~:[it~;they~] may ~
                                        have been made by one. A picture tiled with a faint pattern of that size, ~
                                        or a JPEG cropped and saved again whose first save left no trace to ~
                                        tell, carries one too."
                                   (cond (*picture-file* "The picture carries")
                                         ((rest scans)
                                          (format nil "The scanned pictures of ~a carry" (pages-words pages)))
                                         (t (format nil "The scanned picture of ~a carries" (pages-words pages))))
                                   (rest scans) (rest scans))
                     :rule (format nil "An image generator draws its picture from one eight times smaller, each ~
                                        cell of 8 by 8 pixels by its decoder, which leaves a faint pattern of that ~
                                        size changing along rows and columns at once. A scanner, a camera and a ~
                                        JPEG leave none such: a JPEG's blocks, and a scanner's sensor, run along ~
                                        whole rows and columns. Named when that pattern, measured in the flat parts, ~
                                        stands far above chance, ~d % or more of it changes along both at once, ~
                                        and it is ~d % to ~d % of the flat pixels' own noise, itself enough to ~
                                        weigh by: a decoder's is faint, a dither's or a screen's far stronger. ~
                                        Measured only on pictures kept without loss and never saved as a JPEG, ~
                                        with flat pixels enough: a JPEG's own blocks hide it, ~
                                        as a print scanned again does, and two JPEG saves out of step make one."
                                   (round (* 100 *generator-grid-share*)) (round (* 100 (car *generator-grid-effect*)))
                                   (round (* 100 (cdr *generator-grid-effect*))))
                     :revision (revision-index revision) :page (first pages) :tally (list nil nil pages)
                     :objects (remove-duplicates (mapcar (lambda (s) (mark-object (scan-picture-mark s))) scans))
                     :details (loop for s in scans
                                    collect (destructuring-bind (strength whole faint noise) (scan-picture-grid s)
                                              (format nil "page ~d: ~a, ~:d by ~:d pixels, the pattern ~,1f times ~
                                                           chance, ~d % of it along both at once, ~,1f % of the ~
                                                           noise"
                                                      (1+ (page-index (scan-picture-page s)))
                                                      (scan-named (scan-picture-mark s))
                                                      (scan-picture-width s) (scan-picture-height s)
                                                      strength (round (* 100 (/ strength whole)))
                                                      (* 100 (/ faint noise)))))
                     :evidence (pictures-evidence pdf revision
                                                  (loop for s in scans repeat 10
                                                        append (ends-objects (scan-picture-mark s))))
                     :method :generated))))))
