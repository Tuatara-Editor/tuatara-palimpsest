;;;; resolution.lisp --- How finely a scanned page was scanned, where its pixels tell.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A scanned page's pixels to the inch are read from how large the page
;;;; draws its picture; what its JPEG says is read beside them. Neither
;;;; tells how much detail the pixels hold: a page scanned at 150 dots an
;;;; inch and made four times larger has the pixels of 600 and the detail
;;;; of 150. The edges of its ink tell that. A scanner blurs an edge over a
;;;; pixel or two at the resolution it scans at; a picture made larger
;;;; spreads that blur over as many more pixels as it was made larger by.

(in-package #:palimpsest)

(defparameter *edge-sharpest* 0.6
  "The least width, in pixels, an edge of ink can have between a fifth and
four fifths of the way from paper to ink: a step from paper to ink within a
single pixel, as a page drawn by a program or a black and white scan has.
A page made smaller keeps its edges as sharp (0.6 to 1.0 on honest pages
made half as large), so the least resolution a page can have been scaled up
from is told by this width over its own edges' width.")

(defparameter *edge-scanned-widest* 2.0
  "The widest the ink's edges of a page scanned at the resolution of its
pixels are taken to be, in pixels between a fifth and four fifths of the way
from paper to ink (INK-EDGE-WIDTH). On 209 honest pages with 1,000 edges or
more, scanned on flatbeds and office machines at 150 to 600 dots an inch,
the middle width was 0.8 to 1.9 on 206 and 2.3 to 4.4 on three, of two
documents made larger or blurred before they were put out. On 389 honest
pages photographed with phones it was 0.7 to 2.9, 2.0 in the middle: a
phone's camera blurs more than a scanner, and the 164 of them wider than
this are said to hold the detail of a lower resolution, as they do. Honest pages made larger came
to 1.4 to 1.7 times their width made half as large again, 1.6 to 2.5 times
made twice as large (with bicubic, Lanczos or linear blending, kept without
loss or as JPEG of quality 75 or 92), and 3.4 to 3.8 times made four times as
large; made a quarter as large and four times larger again, to 1.9 to 4.0
times. Blurred by a Gaussian of 1.5 pixels, they came to 1.7 to 3.8 times
it, as wide as pages made twice or four times larger: the edges cannot tell
a page made larger from a page blurred, and the wording names both. Nor from
a scanner whose optics are softer than its pixels: honest pages at 400 and
600 dots an inch blurred by a Gaussian of 1.2 pixels, as such a scanner
blurs, came to 2.1 to 2.3, so that is named too, and the span of
resolutions given for a page made larger does not hold for it.")

(defparameter *edge-resolution-spread* 1.9
  "The widest middle width of the ink's edges, in pixels, of the honest pages
scanned at the resolution of their pixels (1.9, on 206 of 209 pages scanned
on flatbeds and office machines). A page whose edges are wider has the detail of a
resolution from its pixels' times *EDGE-SHARPEST* over its edges' width to
its pixels' times this over it. A page whose edges are narrower was scanned
at the resolution of its pixels, or made larger from no less than its pixels'
times *EDGE-SHARPEST* over its edges' width. On 257 honest pages made larger,
smaller or larger again in the ways *EDGE-SCANNED-WIDEST* names, that held
for the resolution they were made from on all but one, a page made twice as
large by repeating its pixels, which REPEATED-PIXELS tells.")

(defparameter *edges-least* 1000
  "The fewest edges of ink a scanned page must have for the width of its edges
to tell its resolution (INK-EDGE-WIDTH). Honest pages of text have 30,000 or
more; a page of a few words, a photograph or a drawing has from none to a few
hundred, whose width is that of a few strokes.")

(defparameter *repeat-share* 9/10
  "The share of the changes from one pixel to the next, of 8 grey levels or
more, that must fall on one place in every K pixels for a picture to be taken
as made K times larger by repeating its pixels (REPEATED-PIXELS). A scan's
changes fall anywhere, a K-th of them on each place.")

(defparameter *paper-sizes*
  '(("A3" 297 420) ("A4" 210 297) ("A5" 148 210) ("Letter" 215.9 279.4) ("Legal" 215.9 355.6))
  "The paper sizes a page drawn at the size of its paper is told by, each
\(NAME WIDTH HEIGHT) in millimetres.")

(defparameter *default-densities* '(72 96)
  "The pixels to the inch programs write into a JPEG when they are given
none: a JPEG saying so says nothing of how it was scanned.")

(defun ink-edge-width (grey width height words)
  "How wide the edges of the ink of WORDS (WORD-BOXes) are in the picture GREY,
WIDTH by HEIGHT, in pixels: the middle of the widths of every edge across a
row or down a column of each word, between a fifth and four fifths of the way
from its paper to its ink, to a fraction of a pixel. Only words whose ink is
60 grey levels or more darker than their paper count, and only edges that run
from paper to ink without turning back. As a second value, how many edges
were measured; NIL and 0 for none."
  (declare (type grey-pixels grey) (type fixnum width height))
  (let ((widths (make-array 1024 :element-type 'single-float :adjustable t :fill-pointer 0)))
    (dolist (w words)
      (unless (word-box-lone w)
        (let* ((paper (word-box-paper w)) (ink (word-box-ink w)) (span (- paper ink)))
          (when (>= span 60)
            (let ((lo (+ ink (* 0.2 span))) (hi (- paper (* 0.2 span)))
                  (x0 (max 0 (1- (word-box-x0 w)))) (x1 (min width (1+ (word-box-x1 w))))
                  (y0 (max 0 (1- (word-box-y0 w)))) (y1 (min height (1+ (word-box-y1 w)))))
              (flet ((walk (start step count)
                       ;; the COUNT pixels from START, STEP apart: each edge from paper (HI or lighter) to ink (LO or
                       ;; darker), or back, its two crossings found between pixels
                       (flet ((at (k) (aref grey (+ start (* k step)))))
                         (loop for i from 0 below (1- count)
                               do (let* ((a (at i)) (b (at (1+ i)))
                                         (falling (and (>= a hi) (< b hi))) (rising (and (<= a lo) (> b lo))))
                                    (when (or falling rising)
                                      (loop for j from (1+ i) below (min count (+ i 24))
                                            for g = (at j)
                                            do (cond ((if falling (<= g lo) (>= g hi))
                                                      (let* ((c (at (1- j)))
                                                             (from (+ i (/ (abs (- a (if falling hi lo)))
                                                                           (max 1 (abs (- a b))))))
                                                             (to (+ j -1 (/ (abs (- c (if falling lo hi)))
                                                                            (max 1 (abs (- c g)))))))
                                                        (vector-push-extend (float (- to from) 1.0) widths))
                                                      (return))
                                                     ((if falling (>= g hi) (<= g lo)) (return))))))))))
                (loop for y from y0 below y1 do (walk (+ (* y width) x0) 1 (- x1 x0)))
                (loop for x from x0 below x1 do (walk (+ (* y0 width) x) width (- y1 y0)))))))))
    (if (zerop (length widths))
        (values nil 0)
        (values (let ((sorted (sort widths #'<))) (aref sorted (floor (length sorted) 2))) (length widths)))))

(defun repeated-pixels (grey width height)
  "How many times larger the picture GREY, WIDTH by HEIGHT, was made by
repeating its pixels, each pixel of the picture it was made from become a
block of K by K alike: the K from 2 to 4 at whose one place in every K
pixels, across and down, at least *REPEAT-SHARE* of the changes of 8 grey
levels or more fall; NIL for none, or for a picture with fewer than 1,000
such changes."
  (declare (type grey-pixels grey) (type fixnum width height))
  (let ((across (make-array 12 :element-type 'fixnum :initial-element 0))
        (down (make-array 12 :element-type 'fixnum :initial-element 0))
        (changes-across 0) (changes-down 0))
    (declare (type fixnum changes-across changes-down))
    (flet ((count-place (places n)
             ;; the place of a change in every 2, 3 and 4 pixels, each counted apart
             (incf (aref places (mod n 2))) (incf (aref places (+ 2 (mod n 3)))) (incf (aref places (+ 5 (mod n 4))))))
      (dotimes (y height)
        (let ((at (* y width)))
          (dotimes (x width)
            (let ((g (aref grey (+ at x))))
              (when (and (plusp x) (>= (abs (- g (aref grey (+ at x -1)))) 8))
                (incf changes-across)
                (count-place across x))
              (when (and (plusp y) (>= (abs (- g (aref grey (- (+ at x) width)))) 8))
                (incf changes-down)
                (count-place down y)))))))
    (when (and (>= changes-across 1000) (>= changes-down 1000))
      (loop for k in '(4 3 2) for base in '(5 2 0)
            when (and (>= (loop for p below k maximize (aref across (+ base p))) (* *repeat-share* changes-across))
                      (>= (loop for p below k maximize (aref down (+ base p))) (* *repeat-share* changes-down)))
              return k))))

(defstruct (scan-detail (:copier nil) (:predicate nil))
  "How much detail a scanned picture's pixels hold (PICTURE-DETAIL). EDGE is
how wide the edges of its ink are, in pixels (INK-EDGE-WIDTH), NIL when none
was measured, and EDGES how many were. REPEATED is how many times larger it
was made by repeating its pixels (REPEATED-PIXELS), or NIL. GREYS is how
many grey levels it holds."
  edge (edges 0) repeated (greys 0))

(defun picture-detail (grey width height words)
  "How much detail the scanned picture GREY, WIDTH by HEIGHT, holds, read
from the edges of the ink of its WORDS (WORD-BOXes) and from whether its
pixels repeat in blocks, as a SCAN-DETAIL."
  (multiple-value-bind (edge edges) (ink-edge-width grey width height words)
    (make-scan-detail :edge edge :edges edges :repeated (repeated-pixels grey width height)
                      :greys (count-if #'plusp (grey-histogram grey)))))

(defun placed-resolution (mark width height)
  "The pixels to the inch at which MARK, a picture WIDTH by HEIGHT pixels,
is drawn on its page, across and down the picture, as two values. A picture
drawn turned a quarter is told by its sides: its width set beside the side
of the box it is drawn in that it fits best. NIL for a box too small to
tell."
  (let ((across (/ (- (mark-x1 mark) (mark-x0 mark)) 72)) (down (/ (- (mark-y1 mark) (mark-y0 mark)) 72)))
    (when (and (> across 1/72) (> down 1/72))
      (let ((straight (list (/ width across) (/ height down))) (turned (list (/ width down) (/ height across))))
        (flet ((apart (pair) (abs (log (/ (first pair) (second pair))))))
          (values-list (if (<= (apart straight) (apart turned)) straight turned)))))))

(defun paper-size (page)
  "The name of the paper size PAGE is drawn at (*PAPER-SIZES*), either way
up and within 3 % of it, or NIL for none."
  (destructuring-bind (x0 y0 x1 y1) (page-media page)
    (let ((w (* (abs (- x1 x0)) 25.4 1/72)) (h (* (abs (- y1 y0)) 25.4 1/72)))
      (loop for (name pw ph) in *paper-sizes*
            when (or (and (<= (abs (- w pw)) (* 0.03 pw)) (<= (abs (- h ph)) (* 0.03 ph)))
                     (and (<= (abs (- w ph)) (* 0.03 ph)) (<= (abs (- h pw)) (* 0.03 pw))))
              return name))))

(defun stated-density (pdf revision mark)
  "The pixels to the inch across and down that the JPEG picture MARK draws
says it was made at (JPEG-DENSITY), as two values; NIL for a picture that
is no JPEG or says none."
  (let* ((filters (mapcar #'car (pdf-stream-filters (mark-actual mark))))
         (object (and (= 1 (length filters)) (jpeg-filter-p (first filters))
                      (revision-object pdf revision (mark-object mark)))))
    ;; the JFIF header and the EXIF come before the picture's data: its first 64 KB hold them
    (and object (jpeg-density (raw-stream pdf object 65536)))))

(defun round-resolution (dpi)
  "DPI, pixels to the inch, rounded as it is told: to the nearest 10 from
100, to the nearest 5 below."
  (if (>= dpi 100) (* 10 (round dpi 10)) (max 5 (* 5 (round dpi 5)))))

(defun scan-resolution (pdf revision scan)
  "What the scanned picture SCAN (a SCAN-PICTURE) of REVISION of PDF tells of
the resolution it was scanned at, as two values: what it says, a list whose
first is :fine, :coarse, :repeated, :drawn or :unknown, and lines saying how
it was worked out. DPI below is the pixels to the inch it is drawn at,
rounded (ROUND-RESOLUTION).

- (:fine DPI LEAST PAPER): its edges are as sharp as a scan's at the
  resolution of its pixels (*EDGE-SCANNED-WIDEST*), so it was scanned at DPI,
  made smaller from a finer scan (which keeps its edges as sharp) or made
  larger from no less than LEAST; PAPER is true when its page is of a paper
  size (PAPER-SIZE), so that its pixels to the inch are the
  paper's.
- (:coarse DPI ABOUT LEAST MOST): its detail is that of a lower resolution,
  about ABOUT, from LEAST to MOST.
- (:repeated DPI K): it was made K times larger by repeating its pixels.
- (:drawn DPI): it was drawn by a program, not scanned (SCANS-READ).
- (:unknown WHY): its resolution cannot be told, WHY :unread, :few,
  :black-and-white or :unplaced (drawn in a box too small to tell)."
  (let* ((mark (scan-picture-mark scan)) (page (scan-picture-page scan))
         (width (scan-picture-width scan)) (height (scan-picture-height scan))
         (detail (scan-picture-detail scan))
         (paper (paper-size page))
         (lines '()))
    (multiple-value-bind (across down) (placed-resolution mark width height)
      (let ((dpi (and across (sqrt (* across down)))))
        (push (format nil "~:d by ~:d pixels drawn on ~:[a page of no paper size (~,1f by ~,1f cm), so that they ~
                           are to the inch of the page as drawn, not of the paper~;~:*~a page~2*~]~@[: ~a~]"
                      width height (and paper (format nil "~:[a~;an~] ~a" (char= (char paper 0) #\A) paper))
                      (/ (abs (- (third (page-media page)) (first (page-media page)))) (/ 72 2.54))
                      (/ (abs (- (fourth (page-media page)) (second (page-media page)))) (/ 72 2.54))
                      (cond ((null dpi) nil)
                            ((> (abs (log (/ across down))) 0.05)
                             (format nil "~d to the inch across, ~d down" (round across) (round down)))
                            (t (format nil "~d to the inch" (round dpi)))))
              lines)
        (multiple-value-bind (x y source) (stated-density pdf revision mark)
          (when x
            (push (cond ((and (= x y) (member (round x) *default-densities*))
                         (format nil "its JPEG (~a) says ~d to the inch, what programs write when given none: it ~
                                      tells nothing" source (round x)))
                        ((and dpi (<= (abs (log (/ (sqrt (* x y)) dpi))) 0.05))
                         (format nil "its JPEG (~a) says the same, ~d to the inch" source (round (sqrt (* x y)))))
                        (t (format nil "its JPEG (~a) says ~d to the inch~:[, ~d down~;~*~]: not what the page ~
                                        draws it at, as a picture placed at another size has"
                                   source (round x) (= (round x) (round y)) (round y))))
                  lines)))
        (let ((rounded (and dpi (round-resolution dpi)))
              (edge (scan-detail-edge detail)) (edges (scan-detail-edges detail)))
          (when edge
            (push (format nil "the edges of its ink ~,1f pixels wide in the middle (~:d measured), where a scan's are ~
                               ~,1f to ~,1f at the resolution it is scanned at"
                          edge edges *edge-sharpest* *edge-resolution-spread*)
                  lines))
          (values (cond ((and (scan-picture-drawn scan) rounded) (list :drawn rounded))
                        ((and (scan-detail-repeated detail) rounded)
                         (list :repeated rounded (scan-detail-repeated detail)))
                        ((<= (scan-detail-greys detail) 4) (list :unknown :black-and-white))
                        ((scan-picture-unread scan) (list :unknown :unread))
                        ((< edges *edges-least*) (list :unknown :few))
                        ((null dpi) (list :unknown :unplaced))
                        ((<= edge *edge-scanned-widest*)
                         (list :fine rounded (round-resolution (* dpi (/ *edge-sharpest* edge))) (and paper t)))
                        (t (list :coarse rounded
                                 (round-resolution (* dpi (/ (sqrt (* *edge-sharpest* *edge-resolution-spread*)) edge)))
                                 (round-resolution (* dpi (/ *edge-sharpest* edge)))
                                 (round-resolution (min dpi (* dpi (/ *edge-resolution-spread* edge)))))))
                  (nreverse lines)))))))

(defun resolution-conclusion (said)
  "What SAID, the first value of SCAN-RESOLUTION, concludes for one page, in words."
  (destructuring-bind (kind &optional dpi a b c) said
    (ecase kind
      (:fine (format nil "its ink's edges as sharp as a scan's at about ~d ~:[to the inch of the page as drawn~;dpi~]: ~
                          scanned at that, made smaller from a finer scan, or made larger from no less than about ~d"
                     dpi b a))
      (:coarse (format nil "its detail that of about ~d dpi (~d to ~d), its pixels ~d to the inch; or that of its ~
                            pixels from a scanner softer than them" a b c dpi))
      (:repeated (format nil "made ~d times larger by repeating its pixels, from about ~d dpi" a
                         (round-resolution (/ dpi a))))
      (:drawn (format nil "drawn by a program at about ~d pixels to the inch, not scanned" dpi))
      (:unknown (ecase dpi
                  (:unread "no words could be read, whose edges would tell")
                  (:black-and-white "black and white, whose edges are a pixel wide at any resolution")
                  (:few (format nil "fewer than ~:d edges of ink to measure" *edges-least*))
                  (:unplaced "drawn too small for its pixels to the inch to be told"))))))

(defun resolution-findings (pdf revision scans)
  "The findings, for the record, of the resolution the scanned pictures SCANS
of REVISION of PDF were scanned at (SCAN-RESOLUTION): one for each conclusion, the
pages it holds for named and how it was worked out for each. Never more than
for the record: a page made larger, a page blurred, out of focus or
printed soft, and a page from a scanner softer than its pixels cannot be
told apart by their edges, and honest pages are
made larger by the programs that put them in a PDF."
  (let ((groups '()))
    (dolist (scan scans)
      (multiple-value-bind (said lines) (scan-resolution pdf revision scan)
        (let* ((key (case (first said)
                      (:fine (list :fine (second said) (fourth said)))
                      (:coarse (list :coarse))
                      (t said)))
               (group (assoc key groups :test #'equal)))
          (unless group (setf group (list key)) (push group groups))
          (push (list scan said lines) (cdr group)))))
    (loop for (key . members) in (nreverse groups)
          for these = (reverse members)
          for pages = (remove-duplicates (mapcar (lambda (m) (1+ (page-index (scan-picture-page (first m))))) these))
          collect (found :record
                         (destructuring-bind (kind &optional a b &rest more) (cadr (first these))
                           (declare (ignore more))
                           (ecase kind
                             (:fine (if (fourth (cadr (first these)))
                                        (format nil "The ink's edges of the scanned picture~:[~;s~] on ~a are as ~
                                                     sharp as a scan's at about ~d dpi: scanned at that, made smaller ~
                                                     from a finer scan, or made larger from a lower resolution."
                                                (rest these) (pages-words pages) a)
                                        (format nil "The ink's edges of the picture~:[~;s~] on ~a are as sharp as ~
                                                     a scan's at about ~d to the inch of the page as drawn: scanned ~
                                                     at that, made smaller from a finer scan, or made larger from a ~
                                                     lower resolution. The page is of no paper size, so how large ~
                                                     the paper was is not known."
                                                (rest these) (pages-words pages) a)))
                             (:coarse (format nil "The scanned picture~:[~;s~] on ~a hold~:[s~;~] the detail of a ~
                                                   lower resolution than ~:[its~;their~] pixels: made larger, ~
                                                   blurred (out of focus, smoothed or printed soft), or scanned by ~
                                                   a scanner whose optics are softer than its pixels."
                                              (rest these) (pages-words pages) (rest these) (rest these)))
                             (:repeated (format nil "The scanned picture~:[~;s~] on ~a ~:[was~;were~] made ~d times ~
                                                     larger by repeating ~:[its~;their~] pixels."
                                                (rest these) (pages-words pages) (rest these) b (rest these)))
                             (:drawn (format nil "The picture~:[~;s~] on ~a, drawn by a program rather than ~
                                                  scanned, ~:[is~;are~] drawn at about ~d pixels to the inch."
                                             (rest these) (pages-words pages) (rest these) a))
                             (:unknown (format nil "The resolution the picture~:[~;s~] on ~a ~:[was~;were~] scanned at ~
                                                    cannot be told: ~a."
                                               (rest these) (pages-words pages) (rest these)
                                               (resolution-conclusion (cadr (first these)))))))
                         :rule (concatenate 'string
                                            "A scanned page's pixels to the inch are read from the size its page draws "
                                            "it at, and what its JPEG says set beside them; how much detail they hold "
                                            "from the edges of its ink. A scanner blurs an edge over "
                                            (format nil "~,1f to ~,1f" *edge-sharpest* *edge-resolution-spread*)
                                            " pixels at the resolution it scans at; a page made larger spreads its "
                                            "edges over as many more pixels as it was made larger by, and so does a "
                                            "page blurred, which the edges cannot tell apart. A page made larger by "
                                            "repeating its pixels is told by its changes from pixel to pixel all "
                                            "falling on one place in every two, three or four pixels. A black and "
                                            "white page has edges a pixel wide at any resolution, and a page of few "
                                            "words too few edges to tell.")
                         :revision (revision-index revision) :page (first pages)
                         :objects (remove-duplicates (mapcar (lambda (m) (mark-object (scan-picture-mark (first m))))
                                                             these))
                         :details (loop for (scan said lines) in these
                                        collect (format nil "page ~d: ~{~a~^; ~}: ~a"
                                                        (1+ (page-index (scan-picture-page scan))) lines
                                                        (resolution-conclusion said)))))))
