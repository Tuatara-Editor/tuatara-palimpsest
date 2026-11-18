;;;; marks.lisp --- The marks of a scanned page as large as a signature or a stamp.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A signature, a stamp or a seal on a scanned page is ink larger than the
;;;; page's type that is neither its words nor its rules. Each scanned page's
;;;; ink is joined into marks so, while its pixels are at hand, and each mark
;;;; kept with its grey levels, to be set beside the others (repeats.lisp).

(in-package #:palimpsest)

;;; Marks of a scanned page larger than its type

(defparameter *mark-cell* 4
  "How many pixels square a cell is in which SCAN-INK-MARKS finds the marks
of a scanned page: a cell is ink when any of its pixels is, and cells a cell
apart or nearer are one mark, so that the strokes of a signature or the
rings and letters of a stamp hold together.")

(defparameter *mark-least-type* 2
  "How many times the height of its page's type (UPPER-HEIGHT) a mark must be
across and down to be set beside others as a signature or a stamp
\(SCAN-INK-MARKS). Smaller marks are characters, which the search for
copied characters sets beside one another (COPIED-GLYPHS).")

(defparameter *mark-pixels-most* 600000
  "The most pixels the box of a mark may hold for it to be set beside others
\(SCAN-INK-MARKS); a larger one is a picture or a drawing, not a signature
or a stamp.")

(defparameter *marks-page-most* 16
  "The most marks of one scanned page kept to be set beside others, those
holding the most ink first (SCAN-INK-MARKS).")

(defparameter *marks-pixels-in-all* (* 40 1000 1000)
  "The most pixels the marks of the scanned pages of a file may keep among
them to be set beside one another; past it, a page's marks are let go
\(MARKS-KEPT).")

(defparameter *mark-margin* 4
  "How many pixels of paper about the box of its ink a mark keeps, so that two
marks may be laid over each other up to 3 pixels apart.")

(defparameter *mark-ring* '(64 . 256)
  "Where a mark's paper is sampled to tell two places of one scan from a mark
copied (SAME-SCAN-PLACE-P), as (APART . SAMPLES): SAMPLES grey levels evenly
round the box APART pixels outside its ink. A copy pasted with a patch of its
paper seldom reaches so far.")

(defstruct (ink-mark (:copier nil) (:predicate nil))
  "A mark of a scanned picture larger than its type, as a signature, a stamp
or a seal is (SCAN-INK-MARKS). SCAN is the SCAN-PICTURE it is on. X0 Y0 X1
Y1 is the box of its ink, in the picture's pixels (the last excluded).
PIXELS is the grey levels from *MARK-MARGIN* pixels above and left of the
box to as far below and right of it, ACROSS wide, the ink of words and
rules within it (INK-CELLS) made white (255) and the pixels off the picture
too. LEVEL is the
grey its ink is darker than, INK how many of its pixels are ink, MIDDLE
the middle of its ink in PIXELS, as (X . Y). COARSE is
how much of each of 8 by 8 parts of its box is ink, 0 to 255 each. RING is
grey levels of its paper round it (*MARK-RING*), -1 off the picture. LOSSY
is true when its picture is kept as a JPEG or a JPEG 2000, or was once saved
as a JPEG. SOURCE is the name of the file it is in, for the marks of several
files set beside one another (COMPARED-FILES)."
  scan x0 y0 x1 y1 pixels (across 0) (level 0) (ink 0) (middle '(0 . 0)) coarse ring lossy source)

(defun ink-cells (grey width height level words type)
  "The ink of the scanned picture GREY (WIDTH by HEIGHT, its ink darker than
LEVEL) in cells *MARK-CELL* pixels square, as a bit vector of rows of cells,
with how many cells across and down as more values. A cell is ink when any
of its pixels is. The cells of WORDS no taller than twice TYPE, the height of
the page's type, are left out, as are runs of cells across or down six times
as long as TYPE or longer and a fifth of the picture's width or height or
longer: the rules of a form or a table, longer than the strokes of a
signature."
  (declare (type grey-pixels grey) (type fixnum width height level))
  (let* ((cell *mark-cell*) (across (ceiling width cell)) (down (ceiling height cell))
         (cells (make-array (* across down) :element-type 'bit :initial-element 0))
         (rules (make-array (* across down) :element-type 'bit :initial-element 0))
         (long (max 12 (ceiling (* 6 type) cell))))
    (declare (type fixnum cell across down long) (type simple-bit-vector cells rules))
    (dotimes (y height)
      (let ((row (* (floor y cell) across)) (at (* y width)))
        (declare (type fixnum row at) (optimize speed))
        (dotimes (x width)
          (when (< (aref grey (+ at x)) level) (setf (sbit cells (+ row (floor x cell))) 1)))))
    (dolist (w words)
      (when (<= (- (word-box-y1 w) (word-box-y0 w)) (* 2 type))
        (loop for cy from (max 0 (floor (word-box-y0 w) cell)) below (min down (ceiling (word-box-y1 w) cell))
              do (loop for cx from (max 0 (floor (word-box-x0 w) cell))
                       below (min across (ceiling (word-box-x1 w) cell))
                       do (setf (sbit cells (+ (* cy across) cx)) 0)))))
    (flet ((runs (count length at &aux (long (max long (ceiling length 5))))
             ;; the runs of ink cells along COUNT lines of LENGTH cells, AT giving a cell's place, marked as rules
             (declare (type fixnum count length long) (type function at))
             (dotimes (k count)
               (let ((start -1))
                 (declare (type fixnum start))
                 (loop for i of-type fixnum from 0 to length
                       for ink = (and (< i length) (= 1 (sbit cells (funcall at k i))))
                       do (cond ((and ink (minusp start)) (setf start i))
                                ((and (not ink) (>= start 0))
                                 (when (>= (- i start) long)
                                   (loop for j from start below i do (setf (sbit rules (funcall at k j)) 1)))
                                 (setf start -1))))))))
      (runs down across (lambda (k i) (+ (* k across) i)))
      (runs across down (lambda (k i) (+ (* i across) k))))
    (bit-andc2 cells rules t)
    (values cells across down)))

(defun cell-marks (cells across down)
  "The marks the ink CELLS make (ACROSS by DOWN cells), cells a cell apart or
nearer joined into one, each a vector #(X0 Y0 X1 Y1 COUNT ROOT) in cells,
the last excluded, COUNT its ink cells; and as a second value a vector
giving each ink cell the ROOT of its mark."
  (declare (type simple-bit-vector cells) (type fixnum across down))
  (let ((parent (make-array (* across down) :element-type '(unsigned-byte 32))))
    (dotimes (i (length parent)) (setf (aref parent i) i))
    (labels ((root (i)
               (declare (type fixnum i))
               (loop until (= (aref parent i) i)
                     do (setf (aref parent i) (aref parent (aref parent i)) i (aref parent i)))
               i))
      (dotimes (cy down)
        (dotimes (cx across)
          (let ((i (+ (* cy across) cx)))
            (when (= 1 (sbit cells i))
              (loop for dy from 0 to 2
                    for ny = (+ cy dy)
                    while (< ny down)
                    do (loop for dx from (if (zerop dy) 1 -2) to 2
                             for nx = (+ cx dx)
                             do (when (and (< -1 nx across) (= 1 (sbit cells (+ (* ny across) nx))))
                                  (let ((a (root i)) (b (root (+ (* ny across) nx))))
                                    (unless (= a b) (setf (aref parent (max a b)) (min a b)))))))))))
      (let ((boxes (make-hash-table)))
        (dotimes (cy down)
          (dotimes (cx across)
            (let ((i (+ (* cy across) cx)))
              (when (= 1 (sbit cells i))
                (let* ((r (root i)) (box (gethash r boxes)))
                  (setf (aref parent i) r)
                  (if box
                      (setf (svref box 0) (min (svref box 0) cx) (svref box 1) (min (svref box 1) cy)
                            (svref box 2) (max (svref box 2) (1+ cx)) (svref box 3) (max (svref box 3) (1+ cy))
                            (svref box 4) (1+ (svref box 4)))
                      (setf (gethash r boxes) (vector cx cy (1+ cx) (1+ cy) 1 r))))))))
        (values (loop for box being the hash-values of boxes collect box) parent)))))

(defun mark-cut (grey width height level cells parent across box)
  "The INK-MARK of the mark BOX (CELL-MARKS) of the scanned picture GREY
\(WIDTH by HEIGHT, its ink darker than LEVEL), whose ink CELLS (ACROSS a
row) lead to their mark's root by PARENT: its box drawn tight about its ink,
its pixels cut, the ink of other marks within them made white, its coarse
parts and its ring measured. NIL when it holds no ink of its own."
  (declare (type grey-pixels grey) (type fixnum width height level across) (type simple-bit-vector cells)
           (type (simple-array (unsigned-byte 32) (*)) parent) (type simple-vector box))
  (let* ((cell *mark-cell*) (root (svref box 5))
         (px0 (* cell (the fixnum (svref box 0)))) (py0 (* cell (the fixnum (svref box 1))))
         (px1 (min width (* cell (the fixnum (svref box 2))))) (py1 (min height (* cell (the fixnum (svref box 3)))))
         (x0 px1) (y0 py1) (x1 px0) (y1 py0) (ink 0) (across-sum 0) (down-sum 0))
    (declare (type fixnum cell root px0 py0 px1 py1 x0 y0 x1 y1 ink across-sum down-sum))
    (flet ((own-p (x y)
             (declare (type fixnum x y) (optimize speed))
             (let ((c (+ (* (floor y cell) across) (floor x cell))))
               (and (= 1 (sbit cells c)) (= (aref parent c) root)))))
      (declare (inline own-p))
      (loop for y of-type fixnum from py0 below py1
            do (let ((at (* y width)))
                 (declare (type fixnum at) (optimize speed))
                 (loop for x of-type fixnum from px0 below px1
                       do (when (and (< (aref grey (+ at x)) level) (own-p x y))
                            (incf ink) (incf across-sum x) (incf down-sum y)
                            (setf x0 (min x0 x) y0 (min y0 y) x1 (max x1 (1+ x)) y1 (max y1 (1+ y)))))))
      (when (plusp ink)
        (let* ((m *mark-margin*) (w (+ (- x1 x0) m m)) (h (+ (- y1 y0) m m)) (bw (- x1 x0)) (bh (- y1 y0))
               (pixels (make-array (* w h) :element-type '(unsigned-byte 8) :initial-element 255))
               (coarse (make-array 64 :element-type '(unsigned-byte 8)))
               (counts (make-array 64 :element-type 'fixnum :initial-element 0)))
          (declare (type fixnum m w h bw bh))
          (dotimes (j h)
            (let ((y (+ y0 j (- m))))
              (declare (type fixnum y) (optimize speed))
              (when (< -1 y height)
                (dotimes (i w)
                  (let ((x (+ x0 i (- m))))
                    (declare (type fixnum x))
                    (when (< -1 x width)
                      (let ((g (aref grey (+ (* y width) x))))
                        (if (< g level)
                            (if (own-p x y)
                                (progn (setf (aref pixels (+ (* j w) i)) g)
                                       (incf (aref counts (+ (* 8 (floor (* 8 (- y y0)) bh))
                                                             (floor (* 8 (- x x0)) bw)))))
                                (setf (aref pixels (+ (* j w) i)) 255))
                            (setf (aref pixels (+ (* j w) i)) g)))))))))
          (dotimes (k 64)
            (let ((part (max 1 (* (/ bw 8) (/ bh 8)))))
              (setf (aref coarse k) (min 255 (round (* 255 (aref counts k)) part)))))
          (make-ink-mark :x0 x0 :y0 y0 :x1 x1 :y1 y1 :pixels pixels :across w :level level :ink ink
                         :middle (cons (float (+ (- (/ across-sum ink) x0) m) 1.0)
                                       (float (+ (- (/ down-sum ink) y0) m) 1.0))
                         :coarse coarse :ring (mark-ring grey width height x0 y0 x1 y1)))))))

(defun mark-ring (grey width height x0 y0 x1 y1)
  "Grey levels of the scanned picture GREY (WIDTH by HEIGHT) sampled evenly
round the box X0 Y0 X1 Y1, as far outside it as *MARK-RING* says, -1 for a
sample off the picture."
  (destructuring-bind (apart . samples) *mark-ring*
    (let* ((left (- x0 apart)) (top (- y0 apart)) (right (+ x1 apart)) (bottom (+ y1 apart))
           (w (- right left)) (h (- bottom top)) (around (* 2 (+ w h)))
           (ring (make-array samples :element-type '(signed-byte 16))))
      (dotimes (k samples ring)
        (let ((d (floor (* k around) samples)) (x 0) (y 0))
          (cond ((< d w) (setf x (+ left d) y top))
                ((< d (+ w h)) (setf x right y (+ top (- d w))))
                ((< d (+ w w h)) (setf x (- right (- d w h)) y bottom))
                (t (setf x left y (- bottom (- d w w h)))))
          (setf (aref ring k) (if (and (< -1 x width) (< -1 y height)) (aref grey (+ (* y width) x)) -1)))))))

(defun scan-ink-marks (grey width height level words glyphs lossy)
  "The marks of the scanned picture GREY (WIDTH by HEIGHT, its ink darker
than LEVEL, WORDS and GLYPHS read from it) that may be a signature, a stamp
or a seal, as INK-MARKs, LOSSY saying whether the picture is or was kept as
a JPEG: the ink left once its words no taller than twice its type and its
rules are taken out (INK-CELLS), joined where it lies a cell apart or nearer
\(CELL-MARKS). Kept are the marks *MARK-LEAST-TYPE* times as tall as the
type (UPPER-HEIGHT) and as wide, no taller than a third of the picture, no
wider than two thirds of it, their box of *MARK-PIXELS-MOST* or fewer and
no more than half of its cells ink (a photograph or a block of colour is
more), at most *MARKS-PAGE-MOST* of them, the most ink first. NIL for a picture without
type to measure by."
  (let ((type (upper-height glyphs)))
    (when (and type (plusp type))
      (multiple-value-bind (cells across down) (ink-cells grey width height level words type)
        (multiple-value-bind (boxes parent) (cell-marks cells across down)
          (let* ((cell *mark-cell*) (least (* *mark-least-type* type))
                 (kept (sort (remove-if-not
                              (lambda (box)
                                (let ((w (* cell (- (svref box 2) (svref box 0))))
                                      (h (* cell (- (svref box 3) (svref box 1)))))
                                  (and (>= w least) (>= h least) (<= (* 3 h) height) (<= (* 3 w) (* 2 width))
                                       (<= (* w h) *mark-pixels-most*)
                                       ;; strokes, not a photograph or a block of colour
                                       (<= (* 2 (svref box 4))
                                           (* (- (svref box 2) (svref box 0)) (- (svref box 3) (svref box 1)))))))
                              boxes)
                             #'> :key (lambda (box) (svref box 4)))))
            (loop for box in kept
                  for mark = (mark-cut grey width height level cells parent across box)
                  when mark
                    do (setf (ink-mark-lossy mark) lossy)
                    and collect mark into marks
                  while (< (length marks) *marks-page-most*)
                  finally (return marks))))))))
