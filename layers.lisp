;;;; layers.lisp --- What copying a page gives set against what it shows: a scan's text layer, a font's map.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A scanned page made searchable holds the words text recognition (OCR)
;;;; read from it as invisible text over its picture. Each word of that layer
;;;; is set on the picture's pixels: a word over blank paper, or a word of ink
;;;; between words the layer holds with none of its own, tells of pixels
;;;; changed after the layer was made. A born-digital page holds two accounts
;;;; of each character: the glyph its encoding names, which is drawn, and the
;;;; text its ToUnicode map gives, which is copied and searched. A figure or
;;;; letter whose two accounts disagree shows one thing and copies as another.

(in-package #:palimpsest)

;;; A scan's text layer against its picture

(defstruct (layer-word (:copier nil) (:predicate nil))
  "A word of a scanned page's invisible text layer. TEXT is the word, MARK the
run of text it is in. X0 Y0 X1 Y1 are the pixels of the picture it covers,
the last excluded, from the top left of the picture as it is kept, its
characters' feet below the baseline taken in. HEIGHT is its characters'
height in pixels. INK is how many pixels within it are darker than the paper
about it, by the measure LAYER-INK-LEVEL gives."
  text mark x0 y0 x1 y1 height (ink 0))

(defparameter *layer-feet* 0.3
  "How far below its baseline a word of a text layer is taken to reach, in
heights of its characters: the feet of g, p and y, and the room text
recognition leaves under a line.")

(defun upright-picture-p (mark)
  "True when the picture MARK draws is set upright in its box, neither turned
nor flipped, so that its pixels map onto the page by scaling alone."
  (destructuring-bind (&optional (a 1) (b 0) (c 0) (d 1)) (mark-matrix mark)
    (and (plusp a) (plusp d) (< (abs b) (* 1d-3 a)) (< (abs c) (* 1d-3 d)))))

(defun layer-words (marks picture width height)
  "The words of the invisible text (render mode 3 or 7) among MARKS whose
middle lies on the picture PICTURE draws, WIDTH by HEIGHT pixels, each a
LAYER-WORD with its box in the picture's pixels; NIL for a picture not set
upright (UPRIGHT-PICTURE-P)."
  (when (upright-picture-p picture)
    (let* ((px0 (mark-x0 picture)) (py1 (mark-y1 picture))
           (sx (/ width (max 1d-6 (- (mark-x1 picture) px0))))
           (sy (/ height (max 1d-6 (- py1 (mark-y0 picture)))))
           (box (list (mark-x0 picture) (mark-y0 picture) (mark-x1 picture) (mark-y1 picture)))
           (out '()))
      (loop for m across marks
            for frame = (mark-frame m)
            when (and (eq (mark-kind m) :text) (member (mark-mode m) '(3 7)) frame
                      (inside-box-p box (* 1/2 (+ (mark-x0 m) (mark-x1 m))) (* 1/2 (+ (mark-y0 m) (mark-y1 m))) 0))
              do (let ((ox (aref frame 0)) (oy (aref frame 1)) (ax (aref frame 2)) (ay (aref frame 3))
                       (ux (aref frame 4)) (uy (aref frame 5)))
                   (loop for (word first last) in (mark-placed-words m t)
                         for start = (mark-character-along m first)
                         for end = (mark-character-along m (1- last) t)
                         do (let ((xs '()) (ys '()))
                              (dolist (along (list start end))
                                (dolist (up (list 1 (- *layer-feet*)))
                                  (push (+ ox (* along ax) (* up ux)) xs)
                                  (push (+ oy (* along ay) (* up uy)) ys)))
                              (let ((x0 (floor (* sx (- (reduce #'min xs) px0))))
                                    (x1 (ceiling (* sx (- (reduce #'max xs) px0))))
                                    (y0 (floor (* sy (- py1 (reduce #'max ys)))))
                                    (y1 (ceiling (* sy (- py1 (reduce #'min ys))))))
                                (when (and (< x0 x1) (< y0 y1) (< x0 width) (< y0 height) (plusp x1) (plusp y1))
                                  (push (make-layer-word :text word :mark m
                                                         :x0 (max 0 x0) :y0 (max 0 y0)
                                                         :x1 (min width x1) :y1 (min height y1)
                                                         :height (* sy (sqrt (+ (* ux ux) (* uy uy)))))
                                        out)))))))
      (nreverse out))))

(defun dark-count (grey width x0 y0 x1 y1 dark)
  "How many pixels of GREY, WIDTH wide, from X0,Y0 to X1,Y1 (the last
excluded) are darker than DARK."
  (declare (type grey-pixels grey) (type fixnum width x0 y0 x1 y1 dark) (optimize speed))
  (let ((n 0))
    (declare (type fixnum n))
    (loop for y of-type fixnum from y0 below y1
          do (loop for i of-type fixnum from (+ (* y width) x0) below (+ (* y width) x1)
                   when (< (aref grey i) dark) do (incf n)))
    n))

(defparameter *layer-margin* 0.1
  "How far about a word of a text layer ink is looked for, in heights of its
characters, for the word to stand on ink: honest layers from the scans
measured put 99.9 % of their words on ink within their own box, and all but
single letters on a rule or a speck within this.")

(defparameter *layer-contrast-least* 40
  "How many grey levels the paper of a scanned page must be lighter than its
level of ink (PAGE-LEVEL) for its text layer to be set against its pixels:
a page darker than that, or of light text on dark, is not judged.")

(defparameter *layer-word-height-least* 10
  "The least height, in pixels, of a word of a text layer set against the
picture: smaller words are what text recognition makes of specks and rules.")

(defun layer-ink-level (paper level)
  "The grey a pixel of a scanned page whose paper is PAPER and whose level of
ink is LEVEL must be darker than to count as ink under its text layer: a
third of the way from the paper to that level, so that faint or grey type
text recognition reads counts as ink too."
  (round (- paper (/ (- paper level) 3))))

(defun picture-pixel-box (picture mark width height)
  "The box (X0 Y0 X1 Y1) of the pixels of the picture PICTURE draws, WIDTH by
HEIGHT pixels, that MARK's box covers, the last excluded."
  (let ((across (max 1d-6 (- (mark-x1 picture) (mark-x0 picture))))
        (down (max 1d-6 (- (mark-y1 picture) (mark-y0 picture)))))
    (list (floor (* width (/ (- (mark-x0 mark) (mark-x0 picture)) across)))
          (floor (* height (/ (- (mark-y1 picture) (mark-y1 mark)) down)))
          (ceiling (* width (/ (- (mark-x1 mark) (mark-x0 picture)) across)))
          (ceiling (* height (/ (- (mark-y1 picture) (mark-y0 mark)) down))))))

(defun boxes-overlap-p (a b)
  "True when the boxes A and B, (X0 Y0 X1 Y1) each, their far edges
excluded, share any area (BOX-INTERSECTION)."
  (destructuring-bind (x0 y0 x1 y1) (box-intersection a b)
    (and (< x0 x1) (< y0 y1))))

(defun masked-picture-p (mark)
  "True when the picture MARK draws, or any strip of it, is drawn through a
mask of its own (/SMask or /Mask): a page kept in layers may keep its
text's ink in the mask, leaving the picture itself blank paper."
  (some (lambda (m)
          (let ((dict (mark-actual m)))
            (and (pdf-dictionary-p dict) (or (pdf-get dict "/SMask") (pdf-get dict "/Mask")) t)))
        (cons mark (mapcar #'first (mark-strips mark)))))

(defparameter *layer-gap-most* 1.5
  "How far apart, in heights of the text layer's characters, a word of ink
with no word of the layer may stand from the words on either side of it in
its line that the layer holds, for it to be named: a word space, not the
gap to a drawing or a logo beside the text.")

(defstruct (layer-reading (:copier nil) (:predicate nil))
  "A scanned page's text layer set against its picture (SCAN-LAYER). WORDS is
how many of the layer's words were set against the picture, BLANK the
LAYER-WORDs over blank paper, UNREAD the words of ink (WORD-BOX, rows of
the picture made level) between words the layer holds with none of their
own. UNJUDGED, when the page was not judged, says why, in words."
  (words 0) blank unread unjudged)

(defun layer-word-box (w)
  "The box (X0 Y0 X1 Y1) of the layer word W in its picture's pixels."
  (list (layer-word-x0 w) (layer-word-y0 w) (layer-word-x1 w) (layer-word-y1 w)))

(defun level-word-box (word tilt)
  "The box (X0 Y0 X1 Y1) in the picture as it is kept of WORD, a WORD-BOX
whose rows are those of the picture made level, its lines falling TILT for
each pixel across."
  (let ((x0 (word-box-x0 word)) (x1 (word-box-x1 word)))
    (list x0 (round (+ (word-box-y0 word) (* tilt (if (plusp tilt) x0 x1))))
          x1 (round (+ (word-box-y1 word) (* tilt (if (plusp tilt) x1 x0)))))))

(defun unread-words (words lw tilt)
  "The words of ink of WORDS (WORD-BOXes of a scanned picture whose lines
fall TILT) that no word of the layer LW covers, between two words of their
line that the layer does cover, each no further off than *LAYER-GAP-MOST*.
The layer's words are kept by bands of rows a type height tall, so that each
word of ink is set beside those of its own rows alone."
  (let* ((heights (sort (mapcar #'layer-word-height lw) #'<))
         (band (max 1 (ceiling (nth (floor (length heights) 2) heights))))
         (reach (* *layer-gap-most* band))
         (bands (make-hash-table)))
    (dolist (w lw)
      (loop for k from (floor (layer-word-y0 w) band) to (floor (1- (layer-word-y1 w)) band)
            do (push (layer-word-box w) (gethash k bands))))
    (flet ((covered-p (word)
             (let ((box (level-word-box word tilt)))
               (loop for k from (floor (second box) band) to (floor (1- (fourth box)) band)
                     thereis (some (lambda (b) (boxes-overlap-p box b)) (gethash k bands))))))
      (loop for line across (scan-lines (remove-if (lambda (w) (or (word-box-lone w) (word-box-faint w))) words))
            for covered = (map 'vector #'covered-p line)
            append (loop for k from 1 below (1- (length line))
                         for word = (svref line k)
                         when (and (not (svref covered k)) (svref covered (1- k)) (svref covered (1+ k))
                                   (<= (- (word-box-x0 word) (word-box-x1 (svref line (1- k)))) reach)
                                   (<= (- (word-box-x0 (svref line (1+ k))) (word-box-x1 word)) reach))
                           collect word)))))

(defun scan-layer (marks mark grey width height level counts words tilt)
  "The invisible text layer among MARKS, the marks of the page the scanned
picture MARK draws, set against the picture's pixels GREY (WIDTH by HEIGHT),
LEVEL its level of ink and COUNTS its pixels counted by grey (PAGE-LEVEL),
WORDS its words read (PICTURE-WORDS) on its lines falling TILT: a
LAYER-READING, NIL when the page holds no layer over the picture. A word of
the layer over another picture the page draws is not judged, as a page kept
in layers (a background and the masks of its text) keeps its ink there; a
page whose paper is not lighter than its ink by *LAYER-CONTRAST-LEAST*, or
whose picture is drawn through a mask of its own (MASKED-PICTURE-P), is not
judged at all."
  (let ((lw (layer-words marks mark width height)))
    (when lw
      (let* ((others (loop for m across marks
                           when (and (eq (mark-kind m) :image) (not (eq m mark))
                                     (not (member m (mark-strips mark) :key #'first)))
                             collect (picture-pixel-box mark m width height)))
             (lw (remove-if (lambda (w) (some (lambda (b) (boxes-overlap-p (layer-word-box w) b)) others)) lw))
             (paper (counted-level counts (floor (reduce #'+ counts) 2))))
        (cond ((null lw) nil)
              ((masked-picture-p mark)
               (make-layer-reading :unjudged "the picture is drawn through a mask of its own, where its text's ink ~
                                              may be kept"))
              ((< (- paper level) *layer-contrast-least*)
               (make-layer-reading :unjudged (format nil "its paper, grey ~d, is not lighter than its ink's level, ~
                                                          ~d, by ~d grey levels" paper level *layer-contrast-least*)))
              (t
               (let ((dark (layer-ink-level paper level)))
                 (make-layer-reading
                  :words (length lw)
                  :blank (loop for w in lw
                               for h = (layer-word-height w)
                               for grow = (round (* *layer-margin* h))
                               for x0 = (max 0 (- (layer-word-x0 w) grow))
                               for y0 = (max 0 (- (layer-word-y0 w) grow))
                               for x1 = (min width (+ (layer-word-x1 w) grow))
                               for y1 = (min height (+ (layer-word-y1 w) grow))
                               when (and (>= (count-if #'alphanumericp (layer-word-text w)) 2)
                                         (>= h *layer-word-height-least*)
                                         (<= (setf (layer-word-ink w) (dark-count grey width x0 y0 x1 y1 dark))
                                             (max 2 (/ (* (- x1 x0) (- y1 y0)) 1000))))
                                 collect w)
                  :unread (unread-words words lw tilt)))))))))

(defun layer-findings (pdf revision scans)
  "The findings of the text layers of SCANS (SCAN-PICTUREs of REVISION of
PDF) set against their pictures (SCAN-LAYER): for each page whose layer
holds words over blank paper, or whose picture holds words of ink between
the layer's words with none of their own, one finding worth a look, with
the words the layer holds there and where; for a page whose layer could not
be judged, why, for the record."
  (loop for scan in scans
        for layer = (scan-picture-layer scan)
        for page = (1+ (page-index (scan-picture-page scan)))
        for m = (scan-picture-mark scan)
        for width = (scan-picture-width scan)
        for height = (scan-picture-height scan)
        when (and layer (layer-reading-unjudged layer))
          collect (found :record (format nil "The text layer of page ~d was not set against ~a: ~a."
                                         page (scan-named m) (layer-reading-unjudged layer))
                         :revision (revision-index revision) :page page)
        when (and layer (or (layer-reading-blank layer) (layer-reading-unread layer)))
          collect (let ((blank (layer-reading-blank layer)) (unread (layer-reading-unread layer)))
                    (flet ((place (box &optional tilt)
                             (destructuring-bind (x0 y0 x1 y1) (picture-page-box m box width height tilt)
                               (declare (ignore x1 y1))
                               (format nil "at x ~d, y ~d (~a)" (round x0) (round y0)
                                       (if tilt
                                           (pixels-words m (word-box-x0 box) (scan-row scan (word-box-x0 box)
                                                                                       (word-box-y0 box))
                                                         (word-box-x1 box) (scan-row scan (word-box-x0 box)
                                                                                     (word-box-y1 box)))
                                           (apply #'pixels-words m box))))))
                      (found :look
                             (format nil "On page ~d the invisible text layer (text recognition, OCR) disagrees with ~
                                          ~a: ~@[~a~]~:[~;; ~]~@[~a~]. The picture was changed after its text was ~
                                          recognised, or the layer written apart from it."
                                     page (scan-named m)
                                     (and blank (format nil "~d word~:p of the layer lie~:[~;s~] over blank paper ~
                                                             (~{~s~^, ~})"
                                                        (length blank) (= (length blank) 1)
                                                        (loop for w in blank repeat 8 collect (layer-word-text w))))
                                     (and blank unread)
                                     (and unread (format nil "~d word~:p of ink between the layer's words ~
                                                              ha~:[ve~;s~] none of ~:*~:[their~;its~] own"
                                                         (length unread) (= (length unread) 1))))
                             :rule (format nil "Text recognition writes each word it reads where it read it, as ~
                                                invisible text over the picture. On ~:d words of honest layers over ~
                                                60 scanned pages, none of two letters or more ~d pixels high or ~
                                                more lay over paper with no ink within a tenth of its height, and no ~
                                                word of ink the layer passed over stood a word space from layer ~
                                                words on both sides. A word erased or painted over after the text ~
                                                was recognised leaves its layer word over blank paper; a word put ~
                                                into the picture after it stands among the layer's words with none ~
                                                of its own. A figure changed in place keeps its ink and is not told ~
                                                so, and a layer whose words were changed but not moved is not told ~
                                                either; text recognition that left out a word, or a stamp or a mark ~
                                                in a line, gives the second sign honestly."
                                           30940 *layer-word-height-least*)
                             :revision (revision-index revision) :page page
                             :objects (list (mark-object m))
                             :details (append (loop for w in blank
                                                    collect (format nil "~s ~a: no ink under it"
                                                                    (layer-word-text w)
                                                                    (place (layer-word-box w))))
                                              (loop for word in unread
                                                    collect (format nil "a word of ink ~a: no word of the layer"
                                                                    (place word (scan-picture-tilt scan)))))
                             :evidence (append (pictures-evidence pdf revision (list (mark-object m)))
                                               (loop for w in blank
                                                     repeat 3
                                                     append (nth-value 0 (mark-evidence pdf revision (layer-word-mark w)
                                                                                        (format nil "the layer's ~s"
                                                                                                (layer-word-text w))))))
                             :method :scan-changed-after-ocr)))))

;;; Characters that copy as others than they show

(defparameter *misnamed-runs-shown* 6
  "The most runs of text a finding of characters copying as others quotes,
each with its page.")

(defun misnamed-words (misnamed)
  "The codes MISNAMED (FONT-MISNAMED) in words: each code, the glyph its
encoding names and what its map copies it as."
  (format nil "~{~a~^; ~}"
          (loop for (code drawn copied name) in misnamed
                collect (format nil "code ~d draws ~a (~:[its glyph ~a~;~a~]) and copies as ~a"
                                code drawn (member name '("WinAnsiEncoding" "MacRomanEncoding") :test #'string=)
                                name copied))))

(defun check-font-maps (pdf revision)
  "Text whose figures or letters copy as other figures or letters than its
page shows: a font whose encoding names a code's glyph as one figure or
letter (by /Differences, or by the WinAnsi or MacRoman encoding of a font
not symbolic) while its ToUnicode map gives another (MISNAMED-CODES), one
finding for each such font drawn on a page, worth a look."
  (let ((runs (make-hash-table :test 'eq)) (fonts '()))
    (dolist (page (pages-of pdf revision))
      (job-check-stop)
      (loop for m across (marks-of pdf revision page)
            when (and (eq (mark-kind m) :text) (mark-drawn m))
              do (unless (gethash (mark-font m) runs) (push (mark-font m) fonts))
                 (push (cons page m) (gethash (mark-font m) runs))))
    (loop for font in (nreverse fonts)
          for shown = (reverse (gethash font runs))
          for pages = (remove-duplicates (mapcar (lambda (r) (1+ (page-index (car r)))) shown))
          collect (found :look
                         (format nil "Font ~a~@[ (object ~d)~] draws figures or letters that copy as others: on ~a ~
                                      the page shows ~s where copying and searching give ~s."
                                 (font-base font) (font-number font) (pages-words pages)
                                 (shorten-string (mark-drawn (cdr (first shown))) 80 :one-line t)
                                 (shorten-string (mark-text (cdr (first shown))) 80 :one-line t))
                         :rule (concatenate 'string "A PDF keeps two accounts of each character: the glyph its font's "
                                            "encoding names, which is drawn, and the text its ToUnicode map gives, "
                                            "which is copied, searched and read by programs. Nothing ties them. "
                                            "Honest programs write imperfect maps (ligatures, symbols in private "
                                            "places, glyphs renumbered by a subset), so only a glyph named as one "
                                            "figure copied as another figure, or one letter copied as another "
                                            "letter, is named. The map or the encoding was written so that the "
                                            "text reads otherwise than it shows; a broken font program can give "
                                            "it too.")
                         :revision (revision-index revision)
                         :page (first pages)
                         :details (cons (misnamed-words (font-misnamed font))
                                        (loop for (page . m) in shown
                                              repeat *misnamed-runs-shown*
                                              collect (format nil "page ~d, ~a: shows ~s, copies as ~s"
                                                              (1+ (page-index page)) (mark-words page m)
                                                              (shorten-string (mark-drawn m) 100 :one-line t)
                                                              (shorten-string (mark-text m) 100 :one-line t))))
                         :objects (remove nil (cons (font-number font)
                                                    (remove-duplicates (mapcar (lambda (r) (mark-source (cdr r)))
                                                                               shown))))
                         :evidence (append (and (font-number font)
                                                (key-evidence pdf revision (font-number font) "/ToUnicode"))
                                           (mark-evidence pdf revision (cdr (first shown))
                                                          (mark-words (car (first shown)) (cdr (first shown)))))
                         :method :text-remapped))))
