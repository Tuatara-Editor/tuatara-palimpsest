;;;; covers.lisp --- What filled shapes hide: the text drawn before them, character by character.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A filled shape hides what was drawn before it where it lies. Its
;;;; outline, its curves cut into straight pieces, is filled row by row as a
;;;; printer fills it, by its own fill rule and within the outlines it is
;;;; clipped to. Each character of a run of text drawn before it is then
;;;; tested in its own frame, along its baseline and up its height, against
;;;; all the shapes of one colour laid over it together.

(in-package #:palimpsest)

(defparameter *coverage-row* 0.5
  "The height, in points, of the rows the coverage of a filled shape is
worked out in, from the page's foot: each row is covered across where the
shape fills it at the row's middle height.")

(defparameter *run-bucket* 36
  "The size, in points, of the squares runs of text are filed under by where
they lie, so that a shape is set beside only the runs near it.")

(defstruct (coverage (:constructor make-coverage (shape)) (:copier nil) (:predicate nil))
  "What the filled SHAPE (a mark with an OUTLINE) covers: ROWS, a list of
\(ROW . SPANS), a row's number (*COVERAGE-ROW*) and the spans across it the
shape covers, worked out when first asked for."
  shape (rows '()))

(defun outline-spans (outline y)
  "The spans across, at height Y, that OUTLINE (OUTLINE-OF) fills by its fill
rule, as a list (X0 X1 X0 X1 ...) from left to right."
  (let ((crossings '()))
    (dolist (sub (rest outline))
      (let ((n (length sub)))
        (declare (type (simple-array single-float (*)) sub) (type fixnum n))
        (loop for k fixnum from 0 below n by 2
              do (let* ((j (if (= (+ k 2) n) 0 (+ k 2)))
                        (y0 (aref sub (1+ k))) (y1 (aref sub (1+ j))))
                   (when (or (and (<= y0 y) (< y y1)) (and (<= y1 y) (< y y0)))
                     (let ((x0 (aref sub k)) (x1 (aref sub j)))
                       (push (cons (+ x0 (* (- y y0) (/ (- x1 x0) (- y1 y0)))) (if (< y0 y1) 1 -1)) crossings)))))))
    (let ((spans '()) (winding 0) (count 0) (from nil) (even-odd (first outline)))
      (dolist (c (sort crossings #'< :key #'car))
        (incf winding (cdr c))
        (incf count)
        (let ((inside (if even-odd (oddp count) (/= winding 0))))
          (cond ((and inside (null from)) (setf from (car c)))
                ((and (not inside) from) (push from spans) (push (car c) spans) (setf from nil)))))
      (nreverse spans))))

(defun spans-intersection (a b)
  "Where the spans A and B, each (X0 X1 X0 X1 ...) from left to right, overlap, as such a list."
  (let ((out '()))
    (loop while (and a b)
          do (let ((low (max (first a) (first b))) (high (min (second a) (second b))))
               (when (< low high) (push low out) (push high out))
               (if (< (second a) (second b)) (setf a (cddr a)) (setf b (cddr b)))))
    (nreverse out)))

(defun coverage-spans (coverage row)
  "The spans across ROW (a row's number) that the shape of COVERAGE covers:
filled by its outline within every outline it is clipped to."
  (let ((kept (assoc row (coverage-rows coverage))))
    (if kept
        (cdr kept)
        (let* ((shape (coverage-shape coverage))
               (y (* (+ row 0.5d0) *coverage-row*))
               (spans (outline-spans (mark-outline shape) y)))
          (dolist (clip (mark-clips shape))
            (when spans (setf spans (spans-intersection spans (outline-spans clip y)))))
          (push (cons row spans) (coverage-rows coverage))
          spans))))

(defun covered-point-p (coverages x y)
  "True when the point X Y lies where one of COVERAGES covers."
  (declare (type double-float x y))
  (some (lambda (c)
          (let ((s (coverage-shape c)))
            (and (<= (mark-x0 s) x (mark-x1 s)) (<= (mark-y0 s) y (mark-y1 s))
                 (loop for (a b) on (coverage-spans c (floor y *coverage-row*)) by #'cddr
                       thereis (<= a x b)))))
        coverages))

(declaim (inline frame-point))
(defun frame-point (frame along up)
  "The point on the page ALONG the baseline of a run of text and UP its
height, each a fraction, from its FRAME (MARK-FRAME), as two values."
  (declare (type (simple-array double-float (6)) frame) (type double-float along up))
  (values (+ (aref frame 0) (* along (aref frame 2)) (* up (aref frame 4)))
          (+ (aref frame 1) (* along (aref frame 3)) (* up (aref frame 5)))))

(defun frame-box (frame from to low high)
  "The box (x0 y0 x1 y1) round the part of a run of text from FROM to TO along
its baseline and from LOW to HIGH up its height, fractions, by its FRAME."
  (let ((xs '()) (ys '()))
    (dolist (along (list from to))
      (dolist (up (list low high))
        (multiple-value-bind (x y) (frame-point frame (float along 1d0) (float up 1d0)) (push x xs) (push y ys))))
    (list (reduce #'min xs) (reduce #'min ys) (reduce #'max xs) (reduce #'max ys))))

(defun run-middle (run)
  "The middle of RUN, a run of text, on the page, as two values: halfway
along its baseline, a third of its height up, where its letters have ink."
  (frame-point (mark-frame run) 0.5d0 0.35d0))

(defun character-hidden-p (run k coverages)
  "True when the middle of the Kth character of RUN lies wholly where
COVERAGES cover: from a fifth to four fifths of its width along the
baseline, from a tenth to six tenths of the run's height above it, where
the letters of a line all have ink, in the character's own frame however
the text is turned. Points across it, no further apart than a row
\(*COVERAGE-ROW*), are each tested."
  (let* ((frame (mark-frame run)) (stops (mark-stops run))
         (s0 (float (aref stops (* 2 k)) 1d0)) (s1 (float (aref stops (1+ (* 2 k))) 1d0))
         (from (+ s0 (* 0.2d0 (- s1 s0)))) (to (+ s0 (* 0.8d0 (- s1 s0))))
         (along (* (abs (- to from)) (sqrt (+ (expt (aref frame 2) 2) (expt (aref frame 3) 2)))))
         (up (* 0.5d0 (sqrt (+ (expt (aref frame 4) 2) (expt (aref frame 5) 2)))))
         (across-steps (max 1 (min 10000 (ceiling along *coverage-row*))))
         (up-steps (max 1 (min 10000 (ceiling up *coverage-row*)))))
    (declare (type (simple-array double-float (6)) frame) (type double-float s0 s1 from to) (type fixnum across-steps up-steps))
    (and (> s1 s0)
         (loop for i fixnum from 0 to across-steps
               always (loop for j fixnum from 0 to up-steps
                            always (multiple-value-bind (x y)
                                       (frame-point frame (+ from (/ (* (- to from) i) across-steps)) (+ 0.1d0 (/ (* 0.5d0 j) up-steps)))
                                     (covered-point-p coverages x y)))))))

(defun hidden-characters (run coverages)
  "Which characters of RUN, a run of text, COVERAGES hide together
\(CHARACTER-HIDDEN-P), as a vector of booleans, one for each character of its
text, or NIL when they hide none. Spaces are never hidden."
  (let* ((text (mark-text run)) (n (length text)) (hidden nil)
         (reach (let ((boxes (mapcar (lambda (c) (let ((s (coverage-shape c))) (list (mark-x0 s) (mark-y0 s) (mark-x1 s) (mark-y1 s))))
                                     coverages)))
                  (list (reduce #'min boxes :key #'first) (reduce #'min boxes :key #'second)
                        (reduce #'max boxes :key #'third) (reduce #'max boxes :key #'fourth)))))
    (when (and (mark-frame run) (mark-stops run))
      (dotimes (k n)
        (unless (char= (char text k) #\Space)
          (let ((box (frame-box (mark-frame run) (aref (mark-stops run) (* 2 k)) (aref (mark-stops run) (1+ (* 2 k))) 0.1 0.6)))
            (when (and (<= (first reach) (first box)) (<= (third box) (third reach))
                       (<= (second reach) (second box)) (<= (fourth box) (fourth reach))
                       (character-hidden-p run k coverages))
              (unless hidden (setf hidden (make-array n :initial-element nil)))
              (setf (aref hidden k) t))))))
    hidden))

(defun hidden-part (text hidden)
  "The characters of TEXT that HIDDEN (HIDDEN-CHARACTERS) names, each stretch
of them with the spaces between its characters, stretches apart joined by
three dots: \"Total\" or \"Tot ... 0\"."
  (let ((pieces '()) (from nil))
    (dotimes (k (1+ (length text)))
      (let ((in (and (< k (length text)) (or (aref hidden k) (and from (char= (char text k) #\Space))))))
        (cond ((and in (null from)) (setf from k))
              ((and (not in) from)
               (let ((piece (string-trim " " (subseq text from k))))
                 (when (plusp (length piece)) (push piece pieces)))
               (setf from nil)))))
    (format nil "~{~a~^ ... ~}" (nreverse pieces))))

(defun cover-shape-p (mark)
  "True when MARK is a filled shape that can hide what is beneath it: its
outline kept (at least 0.9 opaque) and its colour known."
  (and (eq (mark-kind mark) :fill) (mark-outline mark) (mark-colour mark) (>= (mark-alpha mark) 0.9)))

(defun cover-class (colour)
  "The kind of cover COLOUR makes, shapes of one kind hiding text together:
:white (WHITE-P*), :black (each of red, green and blue at most 0.05), else
the colour to a fiftieth."
  (cond ((white-p* colour) :white)
        ((every (lambda (c) (<= c 0.05)) colour) :black)
        (t (mapcar (lambda (c) (round c 0.02)) colour))))

(defun run-index (runs)
  "RUNS, runs of text, filed by the squares of *RUN-BUCKET* points their
frames lie over, for RUNS-NEAR: a table from a square to its runs, and a
list of those too large to file, as two values."
  (let ((table (make-hash-table)) (large '()))
    (dolist (run runs)
      (destructuring-bind (x0 y0 x1 y1) (frame-box (mark-frame run) 0 1 0 1)
        (let ((bx0 (floor x0 *run-bucket*)) (by0 (floor y0 *run-bucket*))
              (bx1 (floor x1 *run-bucket*)) (by1 (floor y1 *run-bucket*)))
          (if (or (> (* (1+ (- bx1 bx0)) (1+ (- by1 by0))) 256)
                  (> (max (abs bx0) (abs bx1) (abs by0) (abs by1)) 100000))
              (push run large)
              (loop for bx from bx0 to bx1
                    do (loop for by from by0 to by1
                             do (push run (gethash (+ (* bx 1000000) by) table))))))))
    (values table large)))

(defun runs-near (table large x0 y0 x1 y1)
  "The runs filed in TABLE (RUN-INDEX) under the squares the box X0 Y0 X1 Y1
lies over, and LARGE, those too large to file; a run may come more than once."
  (let ((bx0 (max -100000 (floor x0 *run-bucket*))) (by0 (max -100000 (floor y0 *run-bucket*)))
        (bx1 (min 100000 (floor x1 *run-bucket*))) (by1 (min 100000 (floor y1 *run-bucket*)))
        (out large))
    (if (> (* (1+ (- bx1 bx0)) (1+ (- by1 by0))) (* 4 (hash-table-count table)))
        (maphash (lambda (key runs) (declare (ignore key)) (dolist (run runs) (push run out))) table)
        (loop for bx from bx0 to bx1
              do (loop for by from by0 to by1
                       do (dolist (run (gethash (+ (* bx 1000000) by) table)) (push run out)))))
    out))

(defun page-covers (marks)
  "What the filled shapes among MARKS (a page's, in the order drawn) hide of
the text drawn before them, seen (not in render mode 3 or 7): a list of
entries, each a list (KIND SHAPES RUNS PART HIDDEN), KIND :covered when the
shapes of one kind (COVER-CLASS) laid over RUNS hide every character of
them but spaces, :partly when they hide some of the one run in RUNS, a
letter or figure among them: PART those characters (HIDDEN-PART), HIDDEN
which (HIDDEN-CHARACTERS). SHAPES are those of that kind
over the hidden characters. Runs covered by the same shapes come in one
entry. A second value is a function giving, for a shape, the runs whose
middle (RUN-MIDDLE) it covers, drawn before or after it."
  (let* ((runs (remove-if-not (lambda (m) (and (eq (mark-kind m) :text) (mark-frame m) (mark-stops m) (not (member (mark-mode m) '(3 7)))))
                              (coerce marks 'list)))
         (shapes (remove-if-not #'cover-shape-p (coerce marks 'list)))
         (coverages (make-hash-table :test 'eq))
         (under (make-hash-table :test 'eq))
         (entries '()))
    (flet ((coverage (shape) (or (gethash shape coverages) (setf (gethash shape coverages) (make-coverage shape)))))
      (multiple-value-bind (table large) (run-index runs)
        (when shapes
          (dolist (shape shapes)
            (dolist (run (runs-near table large (mark-x0 shape) (mark-y0 shape) (mark-x1 shape) (mark-y1 shape)))
              (when (< (mark-order run) (mark-order shape))
                (pushnew shape (gethash run under) :test #'eq))))
          (dolist (run runs)
            (let ((over (reverse (gethash run under))) (classes '()))
              (dolist (shape over)
                (let ((entry (assoc (cover-class (mark-colour shape)) classes :test #'equal)))
                  (if entry (push shape (cdr entry)) (push (list (cover-class (mark-colour shape)) shape) classes))))
              (dolist (class (reverse classes))
                (let* ((group (reverse (rest class)))
                       (hidden (hidden-characters run (mapcar #'coverage group))))
                  (when hidden
                    (let* ((text (mark-text run))
                           (first (position t hidden)) (last (position t hidden :from-end t))
                           (reach (frame-box (mark-frame run) (aref (mark-stops run) (* 2 first)) (aref (mark-stops run) (1+ (* 2 last))) 0.1 0.6))
                           (involved (remove-if-not (lambda (s) (and (< (mark-x0 s) (third reach)) (< (first reach) (mark-x1 s))
                                                                    (< (mark-y0 s) (fourth reach)) (< (second reach) (mark-y1 s))))
                                                    group)))
                      (cond ((every (lambda (h c) (or h (char= c #\Space))) hidden text)
                             (let ((same (find-if (lambda (e) (and (eq (first e) :covered) (equal (second e) involved))) entries)))
                               (if same
                                   (push run (third same))
                                   (push (list :covered involved (list run) nil hidden) entries))))
                            ((loop for k from first to last thereis (and (aref hidden k) (alphanumericp (char text k))))
                             (push (list :partly involved (list run) (hidden-part text hidden) hidden) entries))))))))))
        (values (mapcar (lambda (e) (list* (first e) (second e) (sort (copy-list (third e)) #'< :key #'mark-order) (cdddr e)))
                        (nreverse entries))
                (lambda (shape)
                  (let ((c (list (coverage shape))))
                    (remove-duplicates
                     (remove-if-not (lambda (run) (multiple-value-bind (x y) (run-middle run) (covered-point-p c x y)))
                                    (runs-near table large (mark-x0 shape) (mark-y0 shape) (mark-x1 shape) (mark-y1 shape)))))))))))

;;; Honest causes of text under a shape

(defun light-p (colour)
  "True when COLOUR is light: red, green and blue each 0.6 or more, a pale
grey or tint as watermarks and background words are set in."
  (and colour (every (lambda (c) (>= c 0.6)) colour)))

(defun run-angle (run)
  "The angle, in degrees, the baseline of RUN, a run of text, is turned by from across the page."
  (let ((frame (mark-frame run))) (* (/ 180 pi) (atan (aref frame 3) (aref frame 2)))))

(defun page-text-usual (runs)
  "The angle (RUN-ANGLE, to a degree) and size most of the characters of RUNS
are set at, as two values."
  (values (line-usual runs (lambda (m) (round (run-angle m))))
          (line-usual runs #'mark-size)))

(defparameter *background-turn* 10
  "The least, in degrees, a background word is turned by from the page's
text to be told by its turn: the watermarks read are turned by 30 and 45
degrees, the lines of a page lie within a degree of one another.")

(defparameter *background-size* 1.5
  "The least a background word's size is, as a share of the size most of the
page's text is set at, to be told by its size: the watermarks read are 2.5
to 18.4 times the size of the text, headings 1.2 to 1.44 times.")

(defun background-run-p (run runs angle size)
  "True when RUN, a run of text among RUNS (a page's), is a watermark or
background word: light (LIGHT-P), drawn before every text of the page that
is not light, so under all of it, and turned from the page's text (ANGLE,
PAGE-TEXT-USUAL) by *BACKGROUND-TURN* degrees or more, set at
*BACKGROUND-SIZE* times its usual SIZE or more, or written on the page three
times or more."
  (and (light-p (mark-colour run)) (mark-frame run)
       (every (lambda (m) (or (light-p (mark-colour m)) (> (mark-order m) (mark-order run)))) runs)
       (or (and angle (>= (abs (- (mod (+ (- (run-angle run) angle) 180) 360) 180)) *background-turn*))
           (and size (plusp size) (>= (mark-size run) (* *background-size* size)))
           (>= (count (mark-text run) runs :key #'mark-text :test #'string=) 3))))

(defun outlined-p (shape)
  "True when the filled SHAPE is outlined as it is filled in a colour unlike
white, so that it is seen against the page: a callout or a label meant to
be seen, not a patch the colour of the paper."
  (let ((stroke (mark-stroke shape))) (and stroke (not (white-p* stroke)))))

(defun label-p (run shape marks)
  "True when RUN, a run of text the filled SHAPE lies over, is a label of its
own under another: RUN drawn straight after a box of its own, a shape of
SHAPE's kind (COVER-CLASS) lying under all of it and no more than RUN's
height beyond it each way, and SHAPE followed straight away by text of its
own, as a drawing lays labels, each a box and its text, one over another.
MARKS are the page's, in the order drawn."
  (let ((before (and (> (mark-order run) 1) (aref marks (- (mark-order run) 2))))
        (after (and (< (mark-order shape) (length marks)) (aref marks (mark-order shape)))))
    (and before after (mark-frame run)
         (cover-shape-p before)
         (equal (cover-class (mark-colour before)) (cover-class (mark-colour shape)))
         (eq (mark-kind after) :text) (mark-frame after)
         (let ((reach (frame-box (mark-frame run) 0 1 0 1)) (height (mark-size run)))
           (and (<= (mark-x0 before) (first reach)) (<= (third reach) (mark-x1 before))
                (<= (mark-y0 before) (+ (second reach) (* 0.1 height))) (<= (- (fourth reach) (* 0.4 height)) (mark-y1 before))
                (<= (- (first reach) height) (mark-x0 before)) (<= (mark-x1 before) (+ (third reach) height))
                (<= (- (second reach) height) (mark-y0 before)) (<= (mark-y1 before) (+ (fourth reach) height))))
         (multiple-value-bind (x y) (run-middle after)
           (covered-point-p (list (make-coverage shape)) x y)))))

(defun stamp-p (run shape marks)
  "True when the filled SHAPE lying over RUN, a run of text, is a stamp or a
note laid across it: followed straight away by text of its own, whose
middle lies within its box (in a ring's hole, as a round stamp's), turned
from RUN by *BACKGROUND-TURN* degrees or more, as a stamp is set at a slant
over what it marks; new text set in place of old lies along the old line.
MARKS are the page's, in the order drawn."
  (let ((after (and (< (mark-order shape) (length marks)) (aref marks (mark-order shape)))))
    (and after (eq (mark-kind after) :text) (mark-frame after) (mark-frame run)
         (>= (abs (- (mod (+ (- (run-angle after) (run-angle run)) 180) 360) 180)) *background-turn*)
         (multiple-value-bind (x y) (run-middle after)
           (and (<= (mark-x0 shape) x (mark-x1 shape)) (<= (mark-y0 shape) y (mark-y1 shape)))))))
