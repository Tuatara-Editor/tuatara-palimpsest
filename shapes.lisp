;;;; shapes.lisp --- The characters of a scanned page set side by side by their shapes, for the examiner to look at.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; For when a scan shows no copies but something still feels off: each
;;;; scanned page's characters of one size, as palimpsest cuts them (the
;;;; same characters the search for copies sets beside each other), made
;;;; black and white, scaled to their lines and grouped by their shape,
;;;; those that match no other shape on their page last, each with its
;;;; place. No reading of the text and no finding: honest type differs too
;;;; (bold, italic, headings, figures, forms filled in, logos), and the
;;;; examiner looks and decides. Written as a copy of the revision with one
;;;; save more, its pages of shapes first, each character a link to its
;;;; place on the scanned page that follows.

(in-package #:palimpsest)

;;; A character's shape

(defparameter *shape-rows* 32
  "How many rows a character's shape is scaled to for the height of its
line's taller characters (LINE-SIZE): the capitals and tall letters of
any size of type come out this tall.")

(defparameter *shape-widest* 54
  "The most columns a character's shape keeps once scaled; a mark wider than
that (letters run together) is cut at its right.")

(defparameter *shape-unlike* 0.3
  "How unlike two characters' shapes may be in their weight and still be
grouped as one: the part of the pixels inked in either that are inked in
one only (SHAPE-DISTANCE). The same letter set bold comes over it.")

(defparameter *shape-strays* 0.025
  "How unlike two characters' shapes may be in their form and still be
grouped as one: the part of the pixels inked in either that lie more than a
pixel from any ink of the other (SHAPE-DISTANCE). Two prints of a letter of
one typeface stray at their edges by a pixel, within it; the same letter
in another typeface or in italic has serifs, bowls or strokes elsewhere,
over it.")

(defstruct (shape (:copier nil) (:predicate nil))
  "A character of a scanned page as it is set beside the others: GLYPH, the
character; LINE, its line on the page counted from the top, and POSITION,
its place in that line from the left, both from 1, among the characters
palimpsest reads (WORD-GLYPHS); ROWS, its pixels made black and white and
scaled (GLYPH-SHAPE), each row a whole number whose bits are its pixels,
the leftmost the lowest; GROWN, the same with each pixel of ink grown by a
pixel every way, a row more above and below (GROWN-ROWS); ACROSS, how many
pixels wide ROWS are; SCALE, what it was scaled by; DROP, how far below its
line's baseline its foot is, in the scan's pixels; UNLIKE, how unlike the
first of its group it is (SHAPE-DISTANCE)."
  glyph (line 0) (position 0) rows grown (across 0) (scale 1) (drop 0) (unlike 0))

(defun glyph-shape (glyph scale)
  "GLYPH's ink made black and white and scaled by SCALE, as two values: its
rows, a vector of whole numbers whose bits are the pixels of each, the
leftmost the lowest, and how many pixels wide they are (no more than
*SHAPE-WIDEST*). A pixel is ink when it is darker than halfway from the
lightest to the darkest of GLYPH's box and a pixel round it; a scaled
pixel is ink when most of the nine points spread over it fall on ink."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (w (- (glyph-x1 glyph) (glyph-x0 glyph))) (h (- (glyph-y1 glyph) (glyph-y0 glyph)))
         (level (car (svref (glyph-inks glyph) 1)))
         (wide (max 1 (min *shape-widest* (round (* w scale)))))
         (tall (max 1 (round (* h scale))))
         (rows (make-array tall)))
    (declare (type grey-pixels pixels) (type fixnum across w h level wide tall))
    (dotimes (j tall (values rows wide))
      (let ((row 0))
        (dotimes (i wide)
          (let ((inked 0))
            (dotimes (b 3)
              (dotimes (a 3)
                (let ((x (min (1- w) (floor (/ (+ i (/ (+ a 0.5) 3)) scale))))
                      (y (min (1- h) (floor (/ (+ j (/ (+ b 0.5) 3)) scale)))))
                  ;; within its cut (GLYPH-CUT) the box begins two pixels in
                  (when (< (aref pixels (+ (* (+ 2 y) across) 2 x)) level) (incf inked)))))
            (when (>= inked 5) (setf row (logior row (ash 1 i))))))
        (setf (svref rows j) row)))))

(defun grown-rows (rows)
  "ROWS (GLYPH-SHAPE) with each pixel of ink grown by a pixel every way: a
vector two longer, its first row the one above ROWS' first."
  (let* ((tall (length rows)) (grown (make-array (+ tall 2) :initial-element 0)))
    (dotimes (j tall grown)
      (let* ((r (svref rows j)) (wide (logior r (ash r 1) (ash r -1))))
        (loop for k from j to (+ j 2) do (setf (svref grown k) (logior (svref grown k) wide)))))))

(defun shape-distance (a b)
  "How unlike the shapes A and B (SHAPEs) are, 1 or more for two that are
not grouped as one: the greater of two parts of the pixels inked in either,
each over the most it may be, set together the way that makes it least,
their middles together or a pixel apart across and down. The part inked in
one only, over *SHAPE-UNLIKE*, tells their weight apart; the part lying
more than a pixel from any ink of the other, over *SHAPE-STRAYS*, their
form. 2 when their sizes are too far apart for them to be one character:
their heights by more than a sixth of the taller (2 pixels at least), their
widths by more than a fifth of the wider."
  (let* ((ra (shape-rows a)) (rb (shape-rows b)) (ga (shape-grown a)) (gb (shape-grown b))
         (ha (length ra)) (hb (length rb)) (wa (shape-across a)) (wb (shape-across b))
         (unlike (float *shape-unlike*)) (strays (float *shape-strays*))
         (best 2.0))
    (declare (type simple-vector ra rb ga gb) (type fixnum ha hb wa wb) (type single-float unlike strays best))
    (when (or (> (abs (- ha hb)) (max 2 (floor (max ha hb) 6)))
              (> (abs (- wa wb)) (max 2 (floor (max wa wb) 5))))
      (return-from shape-distance 2.0))
    (let ((ox (floor (- wb wa) 2)) (oy (floor (- hb ha) 2)))
      (declare (type fixnum ox oy))
      (loop for dy fixnum from -1 to 1
            do (loop for dx fixnum from -1 to 1
                     do (let* ((down (+ oy dy)) (shift (+ ox dx)) (only 0) (either 0) (away 0))
                          (declare (type fixnum down shift only either away))
                          ;; A's row J lies on B's row J+DOWN, its pixels SHIFT to the right
                          (flet ((placed (row) (declare (type (unsigned-byte 62) row)) (if (>= shift 0) (ash row shift) row))
                                 (other (row) (declare (type (unsigned-byte 62) row)) (if (>= shift 0) row (ash row (- shift)))))
                            (declare (inline placed other))
                            (loop for j fixnum from (min -1 (1- down)) below (max (1+ hb) (+ ha down 1))
                                  do (let* ((k (- j down))
                                            (p (placed (if (< -1 k ha) (svref ra k) 0)))
                                            (q (other (if (< -1 j hb) (svref rb j) 0)))
                                            (pg (placed (if (< -1 (1+ k) (+ ha 2)) (svref ga (1+ k)) 0)))
                                            (qg (other (if (< -1 (1+ j) (+ hb 2)) (svref gb (1+ j)) 0))))
                                       (declare (type (unsigned-byte 62) p q pg qg))
                                       (incf only (logcount (logxor p q)))
                                       (incf either (logcount (logior p q)))
                                       (incf away (+ (logcount (logandc2 p qg)) (logcount (logandc2 q pg)))))))
                          (when (plusp either)
                            (setf best (min best (max (/ (/ only (float either)) unlike) (/ (/ away (float either)) strays)))))))))
    best))

;;; A page's characters, by size and shape

(defun line-baseline (words)
  "The baseline of a line of WORDS (WORD-BOXes) on the picture made level:
the middle of their baselines."
  (median-of (mapcar #'word-box-baseline words)))

(defun line-size (glyphs)
  "How tall the taller characters of a line of GLYPHS are: the height a
tenth of them reach or pass, the tallest of a line of fewer than ten, so
that a line of few capitals or tall letters, or a short one, is still told
by them."
  (let ((heights (sort (mapcar (lambda (g) (- (glyph-y1 g) (glyph-y0 g))) glyphs) #'>)))
    (nth (floor (length heights) 10) heights)))

(defun page-shapes (scan)
  "The characters of SCAN, a SCAN-PICTURE read (SCAN-PICTURE-READ), as
groups of one size: a list of (SIZE . SHAPES), SIZE how tall the taller
characters of their lines are in the scan's pixels (LINE-SIZE), SHAPES
in reading order, each made black and white and scaled so that SIZE comes
to *SHAPE-ROWS* rows (GLYPH-SHAPE), with its line and its place in it. A
line's size is that of its own characters; the size most characters are
of, with the lines within a fifth of it, is the first, then the same of the
lines left, and so on."
  (let* ((glyphs (scan-picture-glyphs scan))
         (tilt (or (scan-picture-tilt scan) 0))
         (slope (if (< (abs tilt) 0.0003) 0 tilt))
         (by-line (make-hash-table :test 'equal)))
    ;; a line is the words of one band of rows (PICTURE-WORDS)
    (dolist (g glyphs)
      (let ((word (glyph-word g)))
        (push g (gethash (cons (word-box-y0 word) (word-box-y1 word)) by-line))))
    (let* ((lines (sort (loop for key being the hash-keys of by-line using (hash-value members)
                              collect (list key (sort (copy-list members) #'< :key #'glyph-x0)))
                        (lambda (a b) (or (< (car (first a)) (car (first b)))
                                          (and (= (car (first a)) (car (first b))) (< (cdr (first a)) (cdr (first b))))))))
           (numbered (loop for (nil members) in lines for number from 1
                           collect (list number (line-size members) members)))
           (out '()))
      ;; the size most characters are of, and the lines within a fifth of it, then the same of those left
      (loop while numbered
            do (let* ((counts (let ((table (make-hash-table)))
                                (loop for (nil size members) in numbered do (incf (gethash size table 0) (length members)))
                                table))
                      (size (loop with best = nil
                                  for size being the hash-keys of counts using (hash-value count)
                                  when (or (null best) (> count (gethash best counts)) (and (= count (gethash best counts)) (< size best)))
                                    do (setf best size)
                                  finally (return best)))
                      (scale (/ *shape-rows* (float (max 1 size))))
                      (shapes '()))
                 (flet ((member-p (line) (<= (/ size 1.2) (second line) (* size 1.2))))
                   (loop for (number nil line-glyphs) in (remove-if-not #'member-p numbered)
                         do (let ((baseline (line-baseline (remove-duplicates (mapcar #'glyph-word line-glyphs)))))
                              (loop for g in line-glyphs for position from 1
                                    do (multiple-value-bind (rows across) (glyph-shape g scale)
                                         (push (make-shape :glyph g :line number :position position :rows rows :grown (grown-rows rows)
                                                           :across across :scale scale
                                                           :drop (- (- (glyph-y1 g) (* slope (/ (+ (glyph-x0 g) (glyph-x1 g)) 2))) baseline))
                                               shapes)))))
                   (setf numbered (remove-if #'member-p numbered)))
                 (push (cons size (nreverse shapes)) out)))
      (nreverse out))))

(defun shape-groups (shapes)
  "SHAPES (of one size, in reading order) grouped by their shape, as two
values: the groups, each a list of shapes, the largest first (of as many,
the first met first), its characters the least like its first ones first;
and the shapes that match no other, in reading order. Each shape joins the
group whose first shape it is nearest, when under 1 (SHAPE-DISTANCE), else
begins one; then each shape left alone joins the group of any other shape
it comes under 1 of. The same shapes
give the same groups."
  (let ((groups (make-array 0 :adjustable t :fill-pointer 0)))
    ;; each group (FIRST . MEMBERS), MEMBERS backwards
    (dolist (s shapes)
      (job-check-stop)
      (let ((best nil) (nearest 1.0))
        (loop for group across groups
              do (let ((d (shape-distance s (car group))))
                   (when (< d nearest) (setf best group nearest d))))
        (if best
            (progn (setf (shape-unlike s) nearest) (push s (cdr best)))
            (progn (setf (shape-unlike s) 0) (vector-push-extend (list s s) groups)))))
    ;; a shape alone joins the group of any shape it is like
    (let ((owner (make-hash-table :test 'eq)))
      (loop for group across groups do (dolist (s (cdr group)) (setf (gethash s owner) group)))
      (loop for group across groups
            when (null (cddr group))
              do (let ((s (car group)))
                   (job-check-stop)
                   (dolist (other shapes)
                     (unless (eq other s)
                       (let ((d (shape-distance s other)))
                         (when (< d 1.0)
                           (let ((into (gethash other owner)))
                             (setf (shape-unlike s) d (cdr group) '() (gethash s owner) into)
                             (push s (cdr into)))
                           (return)))))))
      (let* ((kept (remove-if (lambda (group) (null (cdr group))) (coerce groups 'list)))
             (alone (remove-if-not (lambda (group) (null (cddr group))) kept))
             (together (remove-if (lambda (group) (null (cddr group))) kept)))
        (values (mapcar (lambda (group) (stable-sort (reverse (cdr group)) #'> :key #'shape-unlike))
                        (stable-sort together #'> :key (lambda (group) (length (cdr group)))))
                (sort (mapcar #'car alone) (lambda (a b) (or (< (shape-line a) (shape-line b))
                                                              (and (= (shape-line a) (shape-line b))
                                                                   (< (shape-position a) (shape-position b)))))))))))

;;; The pages of shapes

(defparameter *shapes-shown-most* 32
  "The most characters of one group drawn; how many more it holds is said.")

(defparameter *shape-sheet* '(595 842 40)
  "The pages of shapes: their width and height and the margin round them, in points.")

(defparameter *shape-line-points* 14
  "How tall, in points, a line's taller characters are drawn on the pages of shapes.")

(defstruct (shape-sheet (:copier nil) (:predicate nil))
  "The pages of shapes being laid out: PAGES, those finished, each (PIECES
LINKS), the latest first; PIECES, the operators of the page being laid out,
the latest first, each text or a function of the number of pages of
shapes giving text; LINKS, its links, each (X0 Y0 X1 Y1 PAGE X Y), a box of
it leading to the place X Y of the scanned PAGE; OUT, where its operators
are being written; Y, how far down it has been filled, from its foot."
  (pages '()) (pieces '()) (links '()) (out (make-string-output-stream)) (y 0))

(defun sheet-flush (sheet)
  "Move what SHEET's page has written to its pieces."
  (let ((text (get-output-stream-string (shape-sheet-out sheet))))
    (when (plusp (length text)) (push text (shape-sheet-pieces sheet)))))

(defun sheet-new-page (sheet)
  "End SHEET's page, when it has one begun, and begin another."
  (sheet-flush sheet)
  (when (or (shape-sheet-pieces sheet) (shape-sheet-links sheet))
    (push (list (reverse (shape-sheet-pieces sheet)) (reverse (shape-sheet-links sheet))) (shape-sheet-pages sheet)))
  (destructuring-bind (width height margin) *shape-sheet*
    (declare (ignore width))
    (setf (shape-sheet-pieces sheet) '() (shape-sheet-links sheet) '() (shape-sheet-y sheet) (- height margin))))

(defun sheet-room (sheet height)
  "Make room for HEIGHT points more on SHEET's page, beginning another when it has not got them."
  (when (< (- (shape-sheet-y sheet) height) (third *shape-sheet*))
    (sheet-new-page sheet)))

(defun sheet-text (sheet x y size text &key (grey 0))
  "Write TEXT on SHEET's page at X Y, SIZE points tall, in GREY (0 black)."
  (format (shape-sheet-out sheet) "BT ~,2f g /PalimpsestLabel ~,1f Tf ~,2f ~,2f Td ~a Tj ET~%"
          grey size x y (pdf-literal-text text)))

(defun text-width (text size)
  "About how wide TEXT is set in Helvetica SIZE points tall, a little more rather than less."
  (* 0.55 size (length text)))

(defun wrapped-lines (text size width)
  "TEXT broken into lines of words no wider than WIDTH points, set SIZE points tall."
  (let ((lines '()) (line nil))
    (dolist (word (uiop:split-string text :separator " ") (nreverse (if line (cons line lines) lines)))
      (cond ((null line) (setf line word))
            ((<= (text-width (format nil "~a ~a" line word) size) width) (setf line (format nil "~a ~a" line word)))
            (t (push line lines) (setf line word))))))

(defun sheet-paragraph (sheet text size &key (grey 0) (after 0))
  "Write TEXT on SHEET as a paragraph of lines SIZE points tall in GREY,
across the page between its margins, leaving AFTER points below it."
  (destructuring-bind (width height margin) *shape-sheet*
    (declare (ignore height))
    (dolist (line (wrapped-lines text size (- width (* 2 margin))))
      (sheet-room sheet (* 1.3 size))
      (decf (shape-sheet-y sheet) (* 1.3 size))
      (sheet-text sheet margin (shape-sheet-y sheet) size line :grey grey))
    (decf (shape-sheet-y sheet) after)))

(defun glyph-picture-operators (glyph x y width height)
  "The operators drawing GLYPH's grey pixels, its box and a pixel round it,
as a picture of its own in the page's content (sixteen greys, written in
hex), filling the box X Y WIDTH HEIGHT."
  (let* ((pixels (glyph-pixels glyph)) (across (glyph-width glyph))
         (w (+ 2 (- (glyph-x1 glyph) (glyph-x0 glyph)))) (h (+ 2 (- (glyph-y1 glyph) (glyph-y0 glyph)))))
    (with-output-to-string (s)
      (format s "q ~,3f 0 0 ~,3f ~,2f ~,2f cm BI /W ~d /H ~d /BPC 4 /CS /G /F /AHx ID~%" width height x y w h)
      (dotimes (j h)
        (dotimes (i w)
          ;; the box and a pixel round it begin a pixel into the cut (GLYPH-CUT)
          (write-char (char "0123456789ABCDEF" (ash (aref pixels (+ (* (+ 1 j) across) 1 i)) -4)) s))
        (when (oddp w) (write-char #\0 s))
        (terpri s))
      (format s ">~%EI Q~%"))))

(defun shape-size (shape)
  "How wide and how tall SHAPE's character is drawn on a page of shapes, in
points, its box and a pixel round it, as two values."
  (let* ((g (shape-glyph shape))
         (points (* (shape-scale shape) (/ *shape-line-points* *shape-rows*))))
    (values (* points (+ 2 (- (glyph-x1 g) (glyph-x0 g)))) (* points (+ 2 (- (glyph-y1 g) (glyph-y0 g)))))))

(defun sheet-shapes (sheet shapes place &key labelled (more 0) count)
  "Draw SHAPES on SHEET side by side, standing on their baselines, on as
many rows as they take, each a link to its place: PLACE, a function of a
shape giving its scanned page and its place there (PAGE X Y). With
LABELLED, its line and place in it under each (12:4). MORE, how many of
the group are not drawn, said after them; COUNT, how many it holds, said
at the left of its first row."
  (destructuring-bind (width height margin) *shape-sheet*
    (declare (ignore height))
    (let* ((points (/ *shape-line-points* *shape-rows*))
           (above (* 1.3 *shape-line-points*)) (below (if labelled 12 7))
           (left (+ margin 36)) (x left) (baseline nil))
      (flet ((new-row ()
               (sheet-room sheet (+ above below))
               (setf baseline (- (shape-sheet-y sheet) above) x left)
               (decf (shape-sheet-y sheet) (+ above below))))
        (new-row)
        (when count
          (let ((text (format nil "~:d" count)))
            (sheet-text sheet (- (- left 6) (text-width text 6)) baseline 6 text :grey 0.35)))
        (dolist (s shapes)
          (multiple-value-bind (w h) (shape-size s)
            (let* ((label (and labelled (format nil "~d:~d" (shape-line s) (shape-position s))))
                   (cell (max w (if label (text-width label 5) 0))))
              (when (> (+ x cell) (- width margin)) (new-row))
              (let ((foot (- baseline (* points (shape-scale s) (+ 1 (shape-drop s))))))
                (write-string (glyph-picture-operators (shape-glyph s) x foot w h) (shape-sheet-out sheet))
                (let ((to (funcall place s)))
                  (when to (push (list* x foot (+ x w) (+ foot h) to) (shape-sheet-links sheet))))
                (when label (sheet-text sheet x (- baseline 9) 5 label :grey 0.35)))
              (incf x (+ cell (if label 5 3))))))
        (when (plusp more)
          (let ((words (format nil "and ~:d more" more)))
            (when (> (+ x (text-width words 6)) (- width margin)) (new-row))
            (sheet-text sheet x baseline 6 words :grey 0.35)))))))

;;; The copy with its pages of shapes

(defun shapes-copy (analysis revision target &key (cancel (list nil)) (progress (constantly nil)))
  "Write to TARGET the revision REVISION of ANALYSIS's PDF with one save more
setting pages of shapes before its own: for each scanned page, its
characters of one size side by side (PAGE-SHAPES), grouped by shape
\(SHAPE-GROUPS), each a link to its place, those that match no other last,
each with its line and place in it; on the scanned page, those framed and
labelled so. PROGRESS, a function of the part done from 0 to 1. Returns
TARGET; :NONE when REVISION has no scanned page; an error saying why when
no copy can be made."
  (let* ((pdf (analysis-pdf analysis))
         (refusal (copy-refusal revision))
         (root (pdf-get (revision-trailer revision) "/Root"))
         (catalogue (catalogue pdf revision))
         (tree (and catalogue (pdf-get catalogue "/Pages")))
         (top (and (pdf-ref-p tree) (deref pdf revision tree))))
    (when refusal (error refusal))
    (unless (and (pdf-ref-p root) (pdf-dictionary-p top) (listp (pdf-get top "/Kids")))
      (error "its page tree cannot be read: no pages could be set before its own"))
    (let ((pictures (pictures-of pdf revision :keep #'scan-picture-p)))
      (when (null pictures) (return-from shapes-copy :none))
      (let* ((next (first-free-number pdf))
             (font next) (open-state (+ next 1)) (close-state (+ next 2)) (info (+ next 3)) (new-top (+ next 4))
             (objects '()) (sheet (make-shape-sheet)) (overlays (make-hash-table))
             (pages (pages-of pdf revision)))
        (setf next (+ next 5))
        (flet ((add (number text) (push (cons number text) objects)))
          (sheet-new-page sheet)
          (destructuring-bind (width height margin) *shape-sheet*
            (declare (ignore width height))
            (sheet-paragraph sheet (format nil "The shapes of the characters of ~a, ~a" (analysis-name analysis) (revision-name revision))
                             12 :after 4)
            (sheet-paragraph sheet (format nil "No finding and no judgement: palimpsest claims nothing here, and reads no text. ~
Each scanned page's characters of one size are set side by side, grouped by their shape made black and white and scaled to their lines; ~
each group's least like its first come first; those that match no other shape on their page come last, with their line and place in it ~
under each (12:4: line 12, the 4th character, as palimpsest reads them; dots and commas are too small to be read). ~
Honest type differs too: bold, italic, headings, figures, forms filled in, logos, letters run together. Look, and decide. ~
Each character leads to its place on its scanned page, which follow these pages, those that match nothing framed there.")
                             8 :grey 0.2 :after 6)
            (loop for (mark page) in pictures for k from 1
                  do (when (car cancel) (return-from shapes-copy nil))
                     (job-check-stop)
                     (funcall progress (/ (1- k) (length pictures)))
                     (let ((scan (scan-picture-read pdf revision mark page))
                           (number (1+ (page-index page))))
                       (sheet-room sheet 60)
                       (decf (shape-sheet-y sheet) 14)
                       (sheet-flush sheet)
                       (let ((x margin) (y (shape-sheet-y sheet)))
                         ;; its page in this copy is known once the pages of shapes are counted
                         (push (lambda (shape-pages)
                                 (with-output-to-string (s)
                                   (format s "BT 0 g /PalimpsestLabel 10 Tf ~,2f ~,2f Td ~a Tj ET~%" x y
                                           (pdf-literal-text (format nil "Page ~d (page ~d of this copy)~@[: ~a~]"
                                                                     number (+ shape-pages number)
                                                                     (cond ((null scan) "its picture could not be read")
                                                                           ((scan-picture-unread scan) "no lines of characters could be read from it")
                                                                           ((null (scan-picture-glyphs scan)) "no characters could be read from it")))))))
                               (shape-sheet-pieces sheet)))
                       (decf (shape-sheet-y sheet) 4)
                       (when (and scan (scan-picture-glyphs scan))
                         (let ((width-px (scan-picture-width scan)) (height-px (scan-picture-height scan)))
                           (flet ((place (s)
                                    (and (page-number page)
                                         (destructuring-bind (x0 y0 x1 y1) (glyph-page-box mark (shape-glyph s) width-px height-px)
                                           (declare (ignore y0 x1))
                                           (list (page-number page) x0 y1)))))
                             (dolist (size (page-shapes scan))
                               (multiple-value-bind (groups alone) (shape-groups (cdr size))
                                 (sheet-room sheet 40)
                                 (sheet-paragraph sheet (format nil "~:d character~:p of one size (their lines' taller characters ~d pixels tall): ~:d shape~:p more than one shares, ~:d character~:p matching no other"
                                                                (length (cdr size)) (car size) (length groups) (length alone))
                                                  8 :after 2)
                                 (dolist (group groups)
                                   (job-check-stop)
                                   (sheet-shapes sheet (subseq group 0 (min (length group) *shapes-shown-most*)) #'place
                                                 :more (max 0 (- (length group) *shapes-shown-most*)) :count (length group)))
                                 (when alone
                                   (sheet-room sheet 40)
                                   (sheet-paragraph sheet (format nil "Matching no other shape of this size on page ~d: ~:d" number (length alone)) 8 :after 2)
                                   (sheet-shapes sheet alone #'place :labelled t)
                                   ;; framed on the scanned page too
                                   (let ((framed (or (gethash page overlays)
                                                     (setf (gethash page overlays) (let ((out (make-string-output-stream))) (cons (page-framer page out) out))))))
                                     (dolist (s alone)
                                       (destructuring-bind (x0 y0 x1 y1) (glyph-page-box mark (shape-glyph s) width-px height-px)
                                         (funcall (car framed) (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5)
                                                  (format nil "~d:~d" (shape-line s) (shape-position s)) '(0.1 0.35 0.85)))))))
                               (decf (shape-sheet-y sheet) 6))))))))
          (sheet-new-page sheet)
          (funcall progress 1)
          ;; the pages of shapes, before the document's own, each character a link to its place
          (let* ((sheets (reverse (shape-sheet-pages sheet)))
                 (count (length sheets))
                 (kids '()))
            (add font *label-font*)
            (add info (copy-information analysis revision "its scanned pages' characters set side by side by their shapes"))
            (loop for (pieces links) in sheets
                  do (let ((page-object next) (content (+ next 1)) (annots '()))
                       (incf next 2)
                       (dolist (link links)
                         (destructuring-bind (x0 y0 x1 y1 to x y) link
                           (push (list :ref next 0) annots)
                           (add next (pdf-value-text (list (cons "/Type" "/Annot") (cons "/Subtype" "/Link")
                                                           (cons "/Rect" (list x0 y0 x1 y1)) (cons "/Border" (list 0 0 0))
                                                           (cons "/Dest" (list (list :ref to 0) "/XYZ" x y "null")))))
                           (incf next)))
                       (add content (pdf-stream-text (apply #'concatenate 'string
                                                            (mapcar (lambda (piece) (if (functionp piece) (funcall piece count) piece)) pieces))))
                       (add page-object (pdf-value-text (list (cons "/Type" "/Page") (cons "/Parent" (list :ref new-top 0))
                                                              (cons "/MediaBox" (list 0 0 (first *shape-sheet*) (second *shape-sheet*)))
                                                              (cons "/Resources" (list (cons "/Font" (list (cons "/PalimpsestLabel" (list :ref font 0))))))
                                                              (cons "/Contents" (list :ref content 0))
                                                              (cons "/Annots" (reverse annots)))))
                       (push (list :ref page-object 0) kids)))
            ;; the document's own pages after them, under a new top of the page tree
            (add new-top (pdf-value-text (list (cons "/Type" "/Pages")
                                               (cons "/Kids" (append (reverse kids) (list tree)))
                                               (cons "/Count" (+ count (length pages))))))
            (add (second tree) (pdf-value-text (append (remove "/Parent" top :key #'car :test #'string=)
                                                       (list (cons "/Parent" (list :ref new-top 0))))))
            (add (second root) (pdf-value-text (append (remove "/Pages" catalogue :key #'car :test #'string=)
                                                       (list (cons "/Pages" (list :ref new-top 0))))))
            ;; the characters that match nothing framed on their scanned pages
            (when (plusp (hash-table-count overlays))
              (add open-state (pdf-stream-text "q"))
              (add close-state (pdf-stream-text "Q"))
              (dolist (page pages)
                (let ((framed (gethash page overlays)))
                  (when (and framed (page-number page))
                    (add next (pdf-stream-text (get-output-stream-string (cdr framed))))
                    (add (page-number page) (overlaid-page-text pdf revision page font open-state close-state next))
                    (incf next)))))
            (write-saved-copy pdf revision target (reverse objects) next info)))))))

;;; The commands

(define-command palimpsest-show-shapes ()
  "Show beside the report, in a window of its own that \\`q' there closes,
the shapes of the characters of the PDF's scanned pages: each page's
characters of one size side by side, grouped by their shape (made black and
white and scaled to their lines), the least like their group's first
first, and last those that match no other shape on their page, each with
its line and its place in it. For when a scan shows no copies but something
still feels off. No finding and no judgement: palimpsest reads no text and
claims nothing of them; honest type differs too (bold, italic, headings,
figures, forms filled in, logos), and you look and decide. Each character
leads to its place on its scanned page, which follow the pages of shapes,
those matching nothing framed there. The revision whose part of the report
point is in, else the latest: that revision with one save more, made in
the background and written to the report's own folder for you alone, never
beside the PDF examined; the PDF itself is never changed. Shown already,
\\<palimpsest-mode>\\[palimpsest-show-shapes] closes it again; the report stays selected throughout."
  (show-copy t))

(define-command palimpsest-write-shapes ()
  "Write the shapes of the characters of the PDF's scanned pages
\(\\<palimpsest-mode>\\[palimpsest-show-shapes]) to a PDF of its own, to keep or show to others: of the
revision whose part of the report point is in, else the latest. Asked
where, offered in the folder last saved to, else in the home folder; never
beside the PDF examined nor in the package's own folder, nothing written
over. The shapes are set side by side afresh, the PDF examined never
changed."
  (write-copy t))
