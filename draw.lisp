;;;; draw.lisp --- A copy of a revision with each thing a page draws framed and named by its object.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The copy is the revision as it was, to its %%EOF, and one save more that
;;;; lays a drawing over each page: a frame round each line of text, shape,
;;;; picture and annotation, and beside it the number of the object that
;;;; draws it, each content stream in a colour of its own. It is written to
;;;; the report's own directory, never beside the PDF examined, and shown in
;;;; Tuatara's PDF view beside the report.

(in-package #:palimpsest)

;;; PDF values written

(defun pdf-name-text (name)
  "NAME, text with a / first as PDF-OBJECT-AT reads a name, written as a PDF
writes it: every character but the plain ones as # and its two hex digits,
so that no delimiter, space or other character in it ends it or begins
something else."
  (with-output-to-string (s)
    (write-char #\/ s)
    (loop for c across (subseq name 1)
          for code = (logand (char-code c) 255)
          do (if (and (< 32 code 127) (not (find c "()<>[]{}/%#")))
                 (write-char c s)
                 (format s "#~2,'0x" code)))))

(defun pdf-literal-text (text)
  "TEXT written as a PDF literal string: in brackets, those and the
backslash escaped, every character but the printable ones as \\ and its
three octal digits, and one past Latin-1 as a question mark."
  (with-output-to-string (s)
    (write-char #\( s)
    (loop for c across text
          do (cond ((find c "()\\") (write-char #\\ s) (write-char c s))
                   ((<= 32 (char-code c) 126) (write-char c s))
                   ((< (char-code c) 256) (format s "\\~3,'0o" (char-code c)))
                   (t (write-char #\? s))))
    (write-char #\) s)))

(defun pdf-text-string (text)
  "TEXT written as a PDF text string: a literal string (PDF-LITERAL-TEXT) when
each of its characters is Latin-1, else in hex as UTF-16 big-endian after
its byte order mark, so that no character is lost."
  (if (every (lambda (c) (< (char-code c) 256)) text)
      (pdf-literal-text text)
      (with-output-to-string (s)
        (write-string "<FEFF" s)
        (loop for c across text
              for code = (char-code c)
              do (if (< code #x10000)
                     (format s "~4,'0X" code)
                     (let ((v (- code #x10000)))
                       (format s "~4,'0X~4,'0X" (+ #xD800 (ash v -10)) (+ #xDC00 (logand v #x3FF))))))
        (write-string ">" s))))

(defun pdf-value-text (value)
  "VALUE, as PDF-OBJECT-AT reads one, written as a PDF writes it: a name (text
with a / first, PDF-NAME-TEXT), true, false and null as words, other text as
a literal string (PDF-LITERAL-TEXT), a number, a reference, an array or a
dictionary."
  (cond ((pdf-ref-p value) (format nil "~d ~d R" (second value) (or (third value) 0)))
        ((pdf-dictionary-p value)
         (format nil
                 "<< ~{~a~^ ~} >>"
                 (loop for (key . v) in value collect (format nil "~a ~a" (pdf-name-text key) (pdf-value-text v)))))
        ((integerp value) (format nil "~d" value))
        ((realp value) (let ((text (format nil "~,4f" value))) (string-right-trim "." (string-right-trim "0" text))))
        ((stringp value)
         (cond ((and (plusp (length value)) (char= (char value 0) #\/)) (pdf-name-text value))
               ((member value '("true" "false" "null") :test #'string=) value)
               (t (pdf-literal-text value))))
        ((null value) "null")
        ((listp value) (format nil "[~{~a~^ ~}]" (mapcar #'pdf-value-text value)))
        (t "null")))

;;; The drawing laid over a page

(defparameter *draw-colours*
  '((0.8 0.45 0.0) (0.55 0.15 0.7) (0.6 0.4 0.2) (0.6 0.55 0.0) (0.4 0.4 0.75))
  "The colours the objects drawing a page are framed in, one after another:
each content stream, form and picture its own. Red, blue and dark green are
left out: they are the strengths of signs (*STRENGTH-COLOURS*).")

(defparameter *shape-frame-colour* '(0.55 0.15 0.7)
  "The colour the characters matching no other shape are framed in on their
scanned page (PALIMPSEST-SHOW-SHAPES), (red green blue): red, blue and dark
green are for the strengths of signs (*STRENGTH-COLOURS*).")

(defparameter *shape-note-grey* 0.35
  "The grey of the small words of the pages of shapes: counts, places and
how many more (0 black, 1 white).")

(defparameter *shape-text-grey* 0.2
  "The grey of the words at the top of the pages of shapes (0 black, 1 white).")

(defun page-framer (page out &key (label-size 6))
  "A function of X0 Y0 X1 Y1 LABEL COLOUR and :DASHED writing to OUT the
operators of a frame on PAGE round that box, in COLOUR (red green blue).
LABEL is on white above its top left (inside it when that would be off the
page), moved right past any label it wrote there already. DASHED is true
for dashes, or the dash pattern itself. A frame at the page's edge is drawn
just inside it. Labels are set LABEL-SIZE points tall."
  (let ((labels-placed '()))
    (lambda (x0 y0 x1 y1 label colour &key dashed)
      (destructuring-bind (px0 py0 px1 py1) (or (page-crop page) (page-media page))
        (setf x0 (max x0 (+ px0 1)) y0 (max y0 (+ py0 1)) x1 (min x1 (- px1 1)) y1 (min y1 (- py1 1))))
      (destructuring-bind (r g b) colour
        (format out "q ~,3f ~,3f ~,3f RG 0.6 w ~a d ~,2f ~,2f ~,2f ~,2f re S Q~%"
                r g b (cond ((stringp dashed) dashed) (dashed "[2 1] 0") (t "[] 0")) x0 y0 (- x1 x0) (- y1 y0))
        (let* ((tall (+ label-size 1)) (width (+ 2 (* label-size 0.56 (length label)))) (x x0)
               (y (if (> (+ y1 tall 1) (fourth (or (page-crop page) (page-media page)))) (- y1 tall 1) (+ y1 0.5))))
          (loop while (some (lambda (placed) (destructuring-bind (px py pw) placed
                                               (and (< (abs (- py y)) tall) (< x (+ px pw)) (< px (+ x width)))))
                            labels-placed)
                do (incf x 3))
          (push (list x y width) labels-placed)
          (format out "q 1 1 1 rg ~,2f ~,2f ~,2f ~d re f ~,3f ~,3f ~,3f rg BT /PalimpsestLabel ~d Tf ~,2f ~,2f Td ~a ~
                       Tj ET Q~%"
                  x y width tall r g b label-size (+ x 1) (+ y (/ label-size 4)) (pdf-literal-text label)))))))

(define-package-setting *palimpsest-strong-colour* "#e60000"
  "The colour a strong sign is framed in on a scanned picture drawn by d.
The frame is drawn whole, so it can be told from the others printed in black
and white too.
- a colour: \"#rrggbb\", a name such as :red, or a number of the 256 colours;
  red, \"#e60000\", by default."
  :values (or string keyword (integer 0 255)))

(define-package-setting *palimpsest-look-colour* "#1a4ce6"
  "The colour a sign worth a look is framed in on a scanned picture drawn by d.
The frame is dashed.
- a colour: \"#rrggbb\", a name such as :blue, or a number of the 256 colours;
  blue, \"#1a4ce6\", by default."
  :values (or string keyword (integer 0 255)))

(define-package-setting *palimpsest-not-judged-colour* "#007326"
  "The colour a character not judged is framed in on a scanned picture drawn by
d. The frame is dotted; dark green stands out on a grey or a black and white
scan where grey would not.
- a colour: \"#rrggbb\", a name such as :green, or a number of the 256
  colours; dark green, \"#007326\", by default."
  :values (or string keyword (integer 0 255)))

(define-package-setting *palimpsest-strong-line* :whole
  "How the frame of a strong sign is drawn on a scanned picture drawn by d.
- :whole, by default, :dashed or :dotted."
  :values (member :whole :dashed :dotted))

(define-package-setting *palimpsest-look-line* :dashed
  "How the frame of a sign worth a look is drawn on a scanned picture drawn by d.
- :whole, :dashed, by default, or :dotted."
  :values (member :whole :dashed :dotted))

(define-package-setting *palimpsest-not-judged-line* :dotted
  "How the frame of a character not judged is drawn on a scanned picture drawn
by d.
- :whole, :dashed or :dotted, by default."
  :values (member :whole :dashed :dotted))

(defparameter *strength-colours*
  '((:strong *palimpsest-strong-colour* "#e60000" *palimpsest-strong-line* :whole)
    (:look *palimpsest-look-colour* "#1a4ce6" *palimpsest-look-line* :dashed)
    (:not-judged *palimpsest-not-judged-colour* "#007326" *palimpsest-not-judged-line* :dotted))
  "How a sign on a scanned picture is framed by its strength, each (STRENGTH
COLOUR-SETTING STANDARD-COLOUR LINE-SETTING STANDARD-LINE): the settings naming
its colour and its line, and what is used when a setting holds neither.")

(defparameter *line-dashes* '((:whole . "[] 0") (:dashed . "[2 1] 0") (:dotted . "[0.4 0.8] 0"))
  "The PDF dash pattern of each kind of line a frame is drawn in.")

(defun strength-colour (strength)
  "The colour STRENGTH (:strong, :look or :not-judged) is framed in, as (red
green blue), each 0 to 1: its setting's colour, or the standard one when the
setting holds no colour."
  (destructuring-bind (setting standard &rest line) (rest (assoc strength *strength-colours*))
    (declare (ignore line))
    (or (strength-colour-of (symbol-value setting)) (strength-colour-of standard))))

(defun strength-colour-of (colour)
  "COLOUR, as the strength settings take it, as (red green blue), each 0 to 1;
NIL when it is no colour."
  (let ((components (colour-components colour)))
    (and components (mapcar (lambda (c) (/ c 255.0)) components))))

(defun strength-dash (strength)
  "The PDF dash pattern STRENGTH (:strong, :look or :not-judged) is framed in:
that of its line setting, or of the standard line when the setting holds none."
  (destructuring-bind (setting standard) (cdddr (assoc strength *strength-colours*))
    (or (cdr (assoc (symbol-value setting) *line-dashes*)) (cdr (assoc standard *line-dashes*)))))

(defun strength-frame (frame x0 y0 x1 y1 label strength)
  "Frame the box X0 Y0 X1 Y1 with FRAME (PAGE-FRAMER), LABEL beside it, in
the colour and line of STRENGTH (STRENGTH-COLOUR, STRENGTH-DASH): :strong,
:look or :not-judged."
  (funcall frame x0 y0 x1 y1 label (strength-colour strength) :dashed (strength-dash strength)))

(defparameter *annotation-colour* '(0.9 0.3 0.6)
  "The colour annotations are framed in, dashed, (red green blue).")

(defun text-line-parts (line)
  "The stretches of LINE, the marks of one line of text, that one stream or
form draws without a gap wider than one and a half times their size (a
column apart), left to right. Each is a list of the stream's object number,
the stretch's X0 Y0 X1 Y1, where in the stream's decoded octets its
operators begin, and the numbers of the font objects it is set in."
  (let ((parts '()))
    (dolist (m line)
      (when (mark-source m)
        (let ((part (first parts))
              (font (and (mark-font m) (font-number (mark-font m)))))
          (if (and part (eql (first part) (mark-source m))
                   (<= (- (mark-x0 m) (fourth part)) (* 1.5 (max 1 (mark-size m)))))
              (destructuring-bind (x0 y0 x1 y1 from fonts) (cdr part)
                (setf (cdr part)
                      (list (min x0 (mark-x0 m)) (min y0 (mark-y0 m)) (max x1 (mark-x1 m)) (max y1 (mark-y1 m))
                            (if (and from (mark-from m)) (min from (mark-from m)) (or from (mark-from m)))
                            (if font (adjoin font fonts) fonts))))
              (push (list (mark-source m) (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m) (mark-from m)
                          (and font (list font)))
                    parts)))))
    (nreverse parts)))

(defun glyph-on-mark-p (glyph mark page)
  "True when GLYPH was cut from the picture MARK draws on PAGE."
  (and (eql (glyph-picture glyph) (mark-object mark))
       (eql (glyph-page glyph) (page-index page))))

(defun frame-glyph (frame mark glyph width height label strength)
  "Frame GLYPH, cut from the picture MARK draws (WIDTH by HEIGHT pixels),
half a point outside its box on the page, with FRAME (PAGE-FRAMER) as
STRENGTH is framed (STRENGTH-FRAME), LABEL beside it."
  (destructuring-bind (x0 y0 x1 y1) (picture-page-box mark glyph width height)
    (strength-frame frame (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5) label strength)))

(defun frame-scan-signs (frame scan mark page &key copies pieces bare)
  "Frame on PAGE, each as its strength is framed (STRENGTH-FRAME), the signs
on SCAN, the picture MARK draws: its words unlike the rest with how they are
unlike and its characters in BARE (groups of characters each), worth a look;
its copies in COPIES (COPIED-GLYPHS, each with its strength) and its PIECES
\(JPEG-PIECES), as strong as their findings. True when it framed any."
  (let ((width (scan-picture-width scan)) (height (scan-picture-height scan)) (drawn nil))
    (loop for (word . ways) in (scan-picture-odd scan)
          do (destructuring-bind (x0 y0 x1 y1) (picture-page-box mark word width height (scan-picture-tilt scan))
               (setf drawn t)
               (strength-frame frame (- x0 1) (- y0 1) (+ x1 1) (+ y1 1)
                               (format nil "~{~a~^, ~}" (unlike-words-description ways)) :look)))
    (flet ((frame-groups (groups label strength-of)
             (loop for entry in groups
                   do (dolist (glyph (first entry))
                        (when (glyph-on-mark-p glyph mark page)
                          (setf drawn t)
                          (frame-glyph frame mark glyph width height label (funcall strength-of entry)))))))
      (frame-groups copies "copy" (lambda (entry) (if (eq (fifth entry) :strong) :strong :look)))
      (dolist (piece pieces)
        (when (and (eql (block-piece-picture piece) (mark-object mark))
                   (eql (block-piece-page piece) (page-index page)))
          (destructuring-bind (x0 y0 x1 y1) (picture-page-box mark piece width height)
            (setf drawn t)
            (strength-frame frame (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5) "JPEG piece"
                            (if (eq (block-piece-strength piece) :strong) :strong :look)))))
      (frame-groups bare "no grain" (constantly :look)))
    drawn))

(defun hatch-clipped-ink (out glyph mark width height)
  "Write to OUT a grey hatch over each pixel of GLYPH, cut from the picture
MARK draws (WIDTH by HEIGHT pixels), that is ink the scanner clipped. True
when there was any."
  (let ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)) (any nil))
    (dotimes (j (- (glyph-y1 glyph) (glyph-y0 glyph)) any)
      (dotimes (i (- (glyph-x1 glyph) (glyph-x0 glyph)))
        (when (<= (aref pixels (+ (* (+ 2 j) across) 2 i)) (glyph-clipped glyph))
          (setf any t)
          (let ((x (+ (glyph-x0 glyph) i)) (y (+ (glyph-y0 glyph) j)))
            (destructuring-bind (px0 py0 px1 py1) (picture-page-box mark (list x y (1+ x) (1+ y)) width height)
              (format out "q 0.6 0.6 0.6 RG 0.1 w ~,3f ~,3f m ~,3f ~,3f l S Q~%" px0 py0 px1 py1))))))))

(defun frame-not-judged (out frame scan mark page &key copies bare not-judged)
  "Frame on PAGE, dark green and dotted (STRENGTH-FRAME), the characters of
NOT-JUDGED cut from SCAN, the picture MARK draws, and hatch the clipped ink
the characters of COPIES and BARE were judged without. True when anything was drawn, so that the page
needs the key saying what it means."
  (let ((width (scan-picture-width scan)) (height (scan-picture-height scan)) (keyed nil))
    (dolist (glyph not-judged)
      (when (glyph-on-mark-p glyph mark page)
        (setf keyed t)
        (frame-glyph frame mark glyph width height "not judged: clipped" :not-judged)))
    (let ((here (loop for (group) in (append copies bare)
                      append (remove-if-not (lambda (glyph) (glyph-on-mark-p glyph mark page)) group))))
      (dolist (glyph (remove-duplicates here))
        (when (and (>= (glyph-clipped glyph) 0) (hatch-clipped-ink out glyph mark width height))
          (setf keyed t))))
    keyed))

(defun write-signs-key (out page not-judged &key shapes)
  "Write to OUT, at the foot of PAGE, what the frames of the signs on its
scanned pictures mean, each line begun by a piece of its frame in its colour
and line (STRENGTH-COLOUR, STRENGTH-DASH); with NOT-JUDGED true, what the
frames of characters not judged and the grey hatch mean too. With SHAPES
true, for a page of the shapes of characters, which holds no signs and
no hatch: only the line for the characters not judged, and it says no more
than that they are not judged."
  (destructuring-bind (px0 py0 &rest more) (or (page-crop page) (page-media page))
    (declare (ignore more))
    (let ((lines (append (and (not shapes)
                              (list (list "A strong sign." :strong)
                                    (list "A sign worth a look." :look)))
                         (and not-judged
                              (list (list (concatenate 'string "Not judged: too few pixels left once the ink the "
                                                       "scanner clipped is set aside."
                                                       (if shapes "" " Grey hatch: clipped ink set aside."))
                                          :not-judged))))))
      (loop for (key strength) in (reverse lines)
            for y from (+ py0 2) by 8
            do (destructuring-bind (r g b) (strength-colour strength)
                 (format out "q 1 1 1 rg ~,2f ~,2f ~,2f 8 re f ~,3f ~,3f ~,3f RG 0.6 w ~a d ~,2f ~,2f m ~,2f ~,2f l S ~
                              ~,3f ~,3f ~,3f rg BT /PalimpsestLabel 6 Tf ~,2f ~,2f Td ~a Tj ET Q~%"
                         (+ px0 2) y (+ 20 (* 6 0.5 (length key))) r g b (strength-dash strength)
                         (+ px0 3) (+ y 4) (+ px0 17) (+ y 4)
                         r g b (+ px0 20) (+ y 2) (pdf-literal-text key)))))))

(defun frame-annotations (frame pdf revision page)
  "Frame each annotation of PAGE but popups, dashed, named by its object
number and subtype."
  (dolist (value (page-annots page))
    (let* ((annot (deref pdf revision value))
           (subtype (and (pdf-dictionary-p annot) (pdf-get annot "/Subtype")))
           (box (and (pdf-dictionary-p annot) (box-of (deref pdf revision (pdf-get annot "/Rect"))))))
      (when (and box (stringp subtype) (not (string= subtype "/Popup")))
        (destructuring-bind (x0 y0 x1 y1) box
          (funcall frame x0 y0 x1 y1 (format nil "~@[~d ~]~a" (and (pdf-ref-p value) (second value)) (subseq subtype 1))
                   *annotation-colour* :dashed t))))))

(defun scan-of-mark (scans page mark)
  "The scan of SCANS (SCAN-PICTURES) read from the picture MARK draws on
PAGE, or NIL."
  (find-if (lambda (scan)
             (and (eq (scan-picture-page scan) page) (eql (mark-object (scan-picture-mark scan)) (mark-object mark))))
           scans))

(defun overlay-operators (pdf revision page &key scans copies pieces bare not-judged signs-only)
  "The operators of the drawing laid over PAGE of REVISION: a frame round each
line of text (each part of it a stream or form of its own draws), each
shape and each picture, and round each annotation, with the number of the
object that draws it beside it. For text, also where in that stream's
decoded octets its operators begin (@312) and the font objects it is set
in. On a scanned picture, framed by their strength (red for strong, blue
dashed for worth a look), its words unlike the rest, its characters
copied, its regions whose JPEG traces are unlike the rest of it, and its
characters without the scan's grain, from SCANS (SCAN-PICTURES), COPIES
\(COPIED-GLYPHS), PIECES (JPEG-PIECES) and BARE, groups of characters each
\(GLYPH...). In dark green, dotted, the characters NOT-JUDGED, and a grey
hatch over the ink the scanner clipped that the copies and the characters
without grain were set beside others without. A key at the foot of the page
says what the frames mean. With SIGNS-ONLY true, only those signs are framed:
no line of text, shape, picture or annotation, as on the page that stands for
a picture file."
  (let* ((colours (make-hash-table)) (next 0) (out (make-string-output-stream)) (signed nil) (keyed nil)
         (frame (page-framer page out))
         (marks (marks-of pdf revision page)))
    (flet ((colour (number)
             (or (gethash number colours)
                 (setf (gethash number colours) (nth (mod (1- (incf next)) (length *draw-colours*)) *draw-colours*)))))
      (unless signs-only
        (dolist (line (page-lines-of marks))
          (loop for (number x0 y0 x1 y1 from fonts) in (text-line-parts line)
                do (funcall frame (- x0 0.5) (- y0 1.5) (+ x1 0.5) y1 (place-label number from fonts)
                            (colour number)))))
      (loop for m across marks
            do (case (mark-kind m)
                 (:fill (when (and (not signs-only) (mark-source m)
                                   (> (- (mark-x1 m) (mark-x0 m)) 4) (> (- (mark-y1 m) (mark-y0 m)) 4))
                          (funcall frame (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m)
                                   (format nil "~d shape" (mark-source m)) (colour (mark-source m)) :dashed t)))
                 (:image (when (mark-object m)
                           (unless signs-only
                             (funcall frame (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m)
                                      (format nil "~d picture" (mark-object m)) (colour (mark-object m))))
                           (let ((scan (scan-of-mark scans page m)))
                             (when scan
                               ;; a picture joined from strips is found by its top strip and framed whole
                               (when (frame-scan-signs frame scan (scan-picture-mark scan) page :copies copies
                                                       :pieces pieces :bare bare)
                                 (setf signed t))
                               (when (frame-not-judged out frame scan (scan-picture-mark scan) page
                                                       :copies copies :bare bare :not-judged not-judged)
                                 (setf keyed t)))))))))
    (when (or signed keyed)
      (write-signs-key out page keyed))
    (unless signs-only (frame-annotations frame pdf revision page))
    (get-output-stream-string out)))

;;; The copy

(defparameter *label-font* "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>"
  "The font the labels of a copy's drawing are set in, as a PDF writes it.")

(defun copy-information (analysis revision what)
  "The document information of a copy of REVISION of ANALYSIS's PDF with
WHAT drawn on it, as a PDF writes it: its title saying so, palimpsest its
producer."
  (format nil "<< /Title ~a /Producer ~a >>"
          (pdf-text-string (format nil "~a, ~a, ~a" (analysis-name analysis) (revision-name revision) what))
          (pdf-literal-text "palimpsest, a package of Tuatara")))

(defun overlaid-page-text (pdf revision page font open-state close-state overlay)
  "PAGE's dictionary written anew, as a PDF writes it, with the content
stream OVERLAY drawn over its own drawing, that kept apart between the
streams OPEN-STATE (q) and CLOSE-STATE (Q), and the font object FONT among
its resources as /PalimpsestLabel."
  (let* ((dict (page-dict page))
         (contents (let ((c (pdf-get dict "/Contents")))
                     (if (pdf-ref-p c) (list c) (and (listp c) (remove-if-not #'pdf-ref-p c)))))
         (resources (let ((r (deref pdf revision (page-resources page)))) (if (pdf-dictionary-p r) r '())))
         (fonts (let ((f (deref pdf revision (pdf-get resources "/Font")))) (if (pdf-dictionary-p f) f '())))
         (new-resources (acons "/Font" (append (remove "/PalimpsestLabel" fonts :key #'car :test #'string=)
                                               (list (cons "/PalimpsestLabel" (list :ref font 0))))
                               (remove "/Font" resources :key #'car :test #'string=))))
    (pdf-value-text (append (remove-if (lambda (pair) (member (car pair) '("/Contents" "/Resources") :test #'string=))
                                       dict)
                            (list (cons "/Resources" new-resources)
                                  (cons "/Contents" (append (list (list :ref open-state 0)) contents
                                                            (list (list :ref close-state 0)
                                                                  (list :ref overlay 0)))))))))

(defun first-free-number (pdf)
  "The object number after the highest any save of PDF names."
  (1+ (loop for r across (pdf-revisions pdf)
            maximize (loop for n being the hash-keys of (revision-entries r) maximize n))))

(defstruct (copy-save (:constructor make-copy-save-of (octets fill)) (:copier nil) (:predicate nil))
  "A copy of a revision with one save more, being written: OCTETS, the
revision's octets and those added after them, FILL of them written so far;
ENTRIES, each object added as (NUMBER GENERATION . OFFSET); BASE, where the
PDF counts its offsets from (PDF-OFFSET-BASE), taken from those written."
  (octets (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (fill 0 :type fixnum) (entries '()) (base 0))

(defun make-copy-save (pdf revision)
  "A COPY-SAVE of REVISION of PDF: its octets to its %%EOF line, and a line
end after them when they lack one. The PDF itself is copied once."
  (let* ((octets (pdf-octets pdf)) (end (revision-end revision))
         (save (make-copy-save-of (make-array (max 4096 (+ end (floor end 8))) :element-type '(unsigned-byte 8)) end)))
    (replace (copy-save-octets save) octets :end2 end)
    (setf (copy-save-base save) (pdf-base pdf))
    (unless (member (aref octets (1- end)) '(10 13)) (save-put save (string #\Newline)))
    save))

(defun save-put (save data)
  "Write DATA after what SAVE holds: text, an octet a character (Latin-1, as
PDF-OBJECT-AT reads it; a character past it as ?), or octets."
  (let* ((n (length data)) (fill (copy-save-fill save)) (octets (copy-save-octets save)))
    (when (> (+ fill n) (length octets))
      (let ((larger (make-array (max (+ fill n) (* 2 (length octets))) :element-type '(unsigned-byte 8))))
        (replace larger octets :end2 fill)
        (setf (copy-save-octets save) larger octets larger)))
    (if (stringp data)
        (loop for c across data for i from fill
              do (setf (aref octets i) (let ((code (char-code c))) (if (< code 256) code 63))))
        (replace octets data :start1 fill))
    (setf (copy-save-fill save) (+ fill n))))

(defun save-object (save number body &key (generation 0))
  "Write to SAVE the object NUMBER of GENERATION, its BODY text as a PDF writes it."
  (push (list* number generation (copy-save-fill save)) (copy-save-entries save))
  (save-put save (format nil "~d ~d obj~%" number generation))
  (save-put save body)
  (save-put save (format nil "~%endobj~%")))

(defun save-stream (save number data &optional (dictionary ""))
  "Write to SAVE the stream object NUMBER holding DATA (text, an octet a
character, or octets), its DICTIONARY's other entries (as a PDF writes them)
before its length."
  (push (list* number 0 (copy-save-fill save)) (copy-save-entries save))
  (save-put save
            (format nil
                    "~d 0 obj~%<< ~@[~a ~]/Length ~d >>~%stream~%"
                    number
                    (and (plusp (length dictionary)) dictionary)
                    (length data)))
  (save-put save data)
  (save-put save (format nil "~%endstream~%endobj~%")))

(defun copy-trailer-entries (revision info base)
  "The entries, as written, of the trailer of a copy of REVISION whose octets
count from BASE: INFO its document information, the revision's own root and
identifier, and the revision's cross-reference section as the one before,
when the revision names one."
  (let ((trailer (revision-trailer revision)) (named (revision-startxref revision)))
    (append (list (format nil "/Info ~d 0 R" info))
            (loop for key in '("/Root" "/ID")
                  for value = (pdf-get trailer key)
                  when value
                    collect (format nil "~a ~a" (pdf-name-text key) (pdf-value-text value)))
            (and named (list (format nil "/Prev ~d" (- named base)))))))

(defun finish-copy-save (save revision target size info)
  "End SAVE, a copy of REVISION, with a cross-reference table for the objects
added and a trailer going back to the revision's, of SIZE objects, naming
INFO its document information and the revision's own root and identifier;
and write it to TARGET, a new file. Returns TARGET."
  (let ((xref (- (copy-save-fill save) (copy-save-base save))))
    (save-put save (format nil "xref~%0 1~%0000000000 65535 f~c~c" #\Return #\Newline))
    (dolist (entry (sort (copy-list (copy-save-entries save)) #'< :key #'car))
      (destructuring-bind (number generation . offset) entry
        (save-put save (format nil "~d 1~%~10,'0d ~5,'0d n~c~c" number (- offset (copy-save-base save)) generation
                               #\Return #\Newline))))
    (save-put save (format nil "trailer~%<< /Size ~d ~{~a~^ ~} >>~%startxref~%~d~%%%EOF~%"
                           size (copy-trailer-entries revision info (copy-save-base save)) xref))
    (ensure-directories target)
    (write-new-file target (copy-save-octets save) :end (copy-save-fill save))
    target))

;;; Objects written again as they were, a few values changed

(defun object-source (pdf revision number)
  "Where the object NUMBER of REVISION is written, as two values: the octets
holding it (the file's, or its object stream's decoded octets) and where
its value begins in them; NIL when it cannot be found."
  (let ((object (revision-object pdf revision number)))
    (when object
      (if (pdf-indirect-get object :packed-in)
          (let ((stream (revision-object pdf revision (pdf-indirect-get object :packed-in))))
            (multiple-value-bind (pairs data) (and stream (packed-objects pdf stream))
              (and pairs data (values data (pdf-indirect-get object :at)))))
          (let* ((octets (pdf-octets pdf))
                 (obj (octets-search (load-time-value (string-octets "obj")) octets :start (pdf-indirect-get object :at)
                                     :end (min (length octets) (+ (pdf-indirect-get object :at) 64)))))
            (and obj (values octets (+ obj 3))))))))

(defun dictionary-text-with (octets at changes)
  "The dictionary written in OCTETS from AT, as text an octet a character,
with the value of each key of CHANGES ((KEY . TEXT)...) put in place of its
own, and those it lacks added before its end: everything else as it was
written, its strings, names and spacing kept. NIL when no dictionary
begins there, or it cannot be read to its end; NIL and, as a second value,
the key, when a key of CHANGES is written in it twice, as no honest program
writes one: which of the two a reader takes cannot be told."
  (let ((cuts '()) (left (copy-list changes)) (seen '()))
    (multiple-value-bind (start close)
        (map-pdf-dictionary (lambda (key value after next)
                              (declare (ignore value))
                              (when (assoc key changes :test #'string=)
                                (when (member key seen :test #'string=)
                                  (return-from dictionary-text-with (values nil key)))
                                (push key seen))
                              (let ((change (assoc key left :test #'string=)))
                                (when change
                                  (push (list after next (cdr change)) cuts)
                                  (setf left (remove change left)))))
                            octets at)
      (when start
        (let ((from start) (pieces '()))
          (loop for (after next text) in (nreverse cuts)
                do (push (octets-latin-1 octets :start from :end after) pieces)
                   (push (format nil " ~a" text) pieces)
                   (setf from next))
          (push (octets-latin-1 octets :start from :end close) pieces)
          (dolist (change left) (push (format nil "~a ~a " (car change) (cdr change)) pieces))
          (push ">>" pieces)
          (apply #'concatenate 'string (nreverse pieces)))))))

(defun object-text-with (pdf revision number changes)
  "The dictionary of the object NUMBER of REVISION as it is written, with
CHANGES ((KEY . TEXT)...) made to it (DICTIONARY-TEXT-WITH); NIL when it
cannot be read so, and as a second value the key of CHANGES it writes twice,
when that is why."
  (multiple-value-bind (octets at) (object-source pdf revision number)
    (and octets (dictionary-text-with octets at changes))))

(defun copy-refusal (revision)
  "Why no copy of REVISION with a save more can be made, in words; NIL when
one can: an encrypted file's added streams would be read as encrypted too."
  (and (pdf-get (revision-trailer revision) "/Encrypt")
       "the PDF is encrypted: a drawing added to it would be read as encrypted too"))

(defun save-picture-samples (save file)
  "Write to SAVE the picture object of the page that stands for the picture
file FILE (a PICTURE-FILE with its pixels read, as WITH-PICTURE-PIXELS gives)
again, with its pixels as samples, grey or red, green and blue, packed with
/FlateDecode, the picture turned as its EXIF says: what the page drew blank
before."
  (let* ((width (picture-file-width file)) (height (picture-file-height file))
         (colours (picture-file-colours file)) (grey (picture-file-grey file)))
    (unless grey (error "the picture's pixels were not read"))
    (save-stream save +picture-image-object+
                 (zlib-deflate
                  (if colours
                      (destructuring-bind (octets . channels) colours
                        (let ((out (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
                          (dotimes (i (* width height) out)
                            (dotimes (c 3) (setf (aref out (+ (* 3 i) c)) (aref octets (+ (* channels i) c)))))))
                      grey))
                 (format nil "/Type /XObject /Subtype /Image /Width ~d /Height ~d /ColorSpace ~a ~
                              /BitsPerComponent 8 /Filter /FlateDecode"
                         width height (if colours "/DeviceRGB" "/DeviceGray")))))

(defun marked-copy (analysis revision target &key (cancel (list nil)))
  "Write to TARGET the revision REVISION of ANALYSIS's PDF with one save more
laying the drawing of OVERLAY-OPERATORS over each of its pages (for a picture
file's, only the signs found on its scanned page); the page's
own drawing kept apart between q and Q, and document information of its own
saying what the copy is (the revision's own in the save before). Returns TARGET; an error saying why
it cannot be made (an encrypted file, whose added streams would be read as
encrypted too)."
  (let ((pdf (analysis-pdf analysis)))
    (let ((refusal (copy-refusal revision))) (when refusal (error refusal)))
    (let* ((next (first-free-number pdf))
           (save (make-copy-save pdf revision))
           (font next) (open-state (+ next 1)) (close-state (+ next 2)) (info (+ next 3))
           (reading (scans-read pdf revision))
           (scans (scan-reading-scans reading))
           (copies (scan-reading-copies reading))
           (pieces (scan-reading-pieces reading))
           (bare (append (mapcar #'list (scan-reading-marks reading))
                         (mapcar (lambda (entry) (list (third entry))) (scan-reading-bare reading))))
           (not-judged (scan-reading-not-judged reading)))
      (setf next (+ next 4))
      (save-object save font *label-font*)
      ;; the copy says what it is: the revision's own information is in the save before
      (save-object save info (copy-information analysis revision
                                                (if (analysis-picture analysis)
                                                    "the signs found on its scanned page framed"
                                                    "each object it draws framed and named")))
      (when (analysis-picture analysis) (save-picture-samples save *picture-file*))
      (save-stream save open-state "q")
      (save-stream save close-state "Q")
      (dolist (page (pages-of pdf revision))
        (when (car cancel) (return-from marked-copy nil))
        (job-check-stop)
        (when (page-number page)
          (let ((overlay next))
            (incf next)
            (save-stream save
                         overlay
                         (overlay-operators pdf
                                            revision
                                            page
                                            :scans scans
                                            :copies copies
                                            :pieces pieces
                                            :bare bare
                                            :not-judged not-judged
                                            :signs-only (and (analysis-picture analysis) t)))
            (save-object save
                         (page-number page)
                         (overlaid-page-text pdf revision page font open-state close-state overlay)
                         :generation (page-generation page)))))
      (finish-copy-save save revision target next info))))

;;; The commands

(defun copy-target-name (analysis shapes revision)
  "The name of the copy of REVISION of ANALYSIS's PDF drawn for the report: its
objects drawn, or with SHAPES its pages of shapes. For a picture file, the
page that stands for it with the signs found on it drawn, named after the
picture, and REVISION is not looked at."
  (if (analysis-picture analysis)
      (format nil "~a-drawing.pdf" (native-file-stem (analysis-name analysis)))
      (format nil "~:[objects~;shapes~]-revision-~d.pdf" shapes (1+ (revision-index revision)))))

(defun drawn-revision (analysis)
  "The revision of ANALYSIS's PDF a copy is drawn of: the one whose part of the
current report point is in, else the latest; for a picture file the page that
stands for it."
  (or (and (not (analysis-picture analysis)) (report-revision-at analysis))
      (last-revision (analysis-pdf analysis))))

(defun drawing-refused-p (analysis shapes)
  "True, and why said, when no copy of ANALYSIS's PDF can be drawn: for a
picture file only its drawing (not SHAPES, its pages of shapes) can."
  (and shapes (picture-refused-p analysis "set side by side")))

(defun report-drawing-window (report)
  "The window showing a copy REPORT drew (PALIMPSEST-DRAW-OBJECTS,
PALIMPSEST-SHOW-SHAPES), when one does."
  (let* ((analysis (buffer-local report 'analysis)) (directory (and analysis (analysis-directory analysis))))
    (and directory
         (loop for buffer in (buffer-list)
               for file = (shown-file buffer)
               thereis (and file (path-within-p file directory)
                            (let ((name (native-file-name file)))
                              (or (search "objects-revision-" name)
                                  (search "shapes-revision-" name)
                                  (and (analysis-picture analysis)
                                       (string= name (copy-target-name analysis nil nil)))))
                            (buffer-window buffer))))))

(define-command palimpsest-quit ()
  "In a report: close the copy with its objects drawn
\(\\<palimpsest-mode>\\[palimpsest-draw-objects]) or its pages of shapes (\\[palimpsest-show-shapes]) when it is
shown; else put the window back as \\[quit-window] does, the report kept,
shown again by \\[palimpsest] on the same PDF."
  (let ((drawing (report-drawing-window (current-buffer))))
    (if drawing
        (leave-window drawing)
        (quit-window))))

(defstruct (palimpsest-draw-job (:include job)
                                (:constructor make-palimpsest-draw-job (buffer analysis revision target shapes))
                                (:copier nil))
  "Writing a copy of REVISION of ANALYSIS's PDF as TARGET, for BUFFER, the
report, a job: with its objects framed and named, or with SHAPES its pages
of shapes (SHAPES-COPY). DONE, true once written; NONE, true when it was
not, REVISION having no scanned page to set shapes of; KEPT, true for a
copy written where asked, to keep, rather than shown; PROGRESS, how far it
has got, from 0 to 1."
  analysis revision target shapes done none kept (progress 0))

(setf (documentation 'palimpsest-draw-job-p 'function) "True when OBJECT is a PALIMPSEST-DRAW-JOB.")

(define-job-words palimpsest-draw-job
  :doing (if (palimpsest-draw-job-shapes job) "Setting the shapes side by side" "Drawing the objects")
  :description (format nil "~:[Framing the objects of~;Setting side by side the shapes of the characters of~] ~a"
                       (palimpsest-draw-job-shapes job) (native-file-name (palimpsest-draw-job-target job)))
  :verb (if (palimpsest-draw-job-shapes job) "set the shapes side by side" "draw the objects")
  :left-words (if (palimpsest-draw-job-shapes job) "no shapes shown" "no copy shown"))

(defmethod job-percent ((job palimpsest-draw-job))
  "How far setting the shapes side by side has got; NIL for drawing the objects."
  (and (palimpsest-draw-job-shapes job)
       (running-percent (round (* 1000 (palimpsest-draw-job-progress job))) 1000)))

(defun shown-beside (path)
  "The window showing the copy at PATH, when one does."
  (let ((buffer (find-if (lambda (b) (equal (shown-file b) path)) (buffer-list))))
    (and buffer (buffer-window buffer))))

(defun show-beside (path)
  "Show the PDF at PATH in Tuatara's PDF view in a window of its own down the
right, made for it, which \\`q' there closes; the report kept where it is,
selected. Shown already, it is left where it is."
  (pdf-view-visit path (lambda (buffer)
                         (unless (buffer-window buffer)
                           (or (display-buffer-at buffer :right)
                               (display-buffer buffer :other t))))))

(defmethod job-take-in ((job palimpsest-draw-job))
  "Show the copy written beside the report, and say what it shows; or, for one
written where asked (PALIMPSEST-WRITE-DRAWING, PALIMPSEST-WRITE-SHAPES), say
where; or why it was not made."
  (let* ((analysis (palimpsest-draw-job-analysis job))
         (name (analysis-name analysis))
         (picture (analysis-picture analysis))
         (revision (if picture "scanned page" (revision-name (palimpsest-draw-job-revision job))))
         (shapes (palimpsest-draw-job-shapes job)))
    (cond ((palimpsest-draw-job-none job)
           (message "~a's ~a has no scanned page: no characters to set side by side" name revision))
          ((not (palimpsest-draw-job-done job))
           (message "~:[The objects were not drawn~;The shapes were not set side by side~]: ~a"
                    shapes
                    (job-ending-words job)))
          ((palimpsest-draw-job-kept job)
           (message "Wrote ~a: ~a's ~a with ~a"
                    (abbreviated-path (palimpsest-draw-job-target job)) name revision
                    (cond (picture "the signs found on it framed by their strength")
                          (shapes "its scanned pages' characters set side by side by their shapes")
                          (t "each thing its pages draw framed and named by its object"))))
          (t
           (show-beside (palimpsest-draw-job-target job))
           (cond
             (picture
              (message "~a's ~a with the signs found on it framed by their strength, the key at its foot"
                       name revision))
             (shapes
              (message "~a's ~a: each scanned page's characters grouped by their shape, those matching no other last; ~
                        palimpsest claims nothing of them"
                       name revision))
             (t
              (message "~a's ~a with each thing its pages draw framed, the number of the object drawing it beside it: ~
                        each content stream a colour of its own, annotations dashed"
                       name revision)))))))

(defun copy-work (job)
  "What a job writing a copy does (PALIMPSEST-DRAW-JOB): the copy written,
DONE set only when it was, not stopped short."
  (let* ((analysis (palimpsest-draw-job-analysis job)) (revision (palimpsest-draw-job-revision job))
         (target (palimpsest-draw-job-target job))
         (written (if (palimpsest-draw-job-shapes job)
                      (shapes-copy analysis revision target :cancel (job-cancel job)
                                                            :progress (lambda (part)
                                                                        (setf (palimpsest-draw-job-progress job) part)))
                      (with-picture-pixels (analysis (job-cancel job))
                        (marked-copy analysis revision target :cancel (job-cancel job))))))
    (setf (palimpsest-draw-job-none job) (eq written :none)
          (palimpsest-draw-job-done job) (and written (not (eq written :none)) t))))

(defun show-copy (shapes)
  "Show beside the current report a copy of the revision whose part of it
point is in, else the latest: its objects drawn, or with SHAPES its pages
of shapes; made afresh in the background, in the report's own directory. For a
picture file, the page that stands for it with the signs found on it drawn
\(no shapes). Shown already, it is put away again."
  (let* ((report (current-buffer))
         (analysis (buffer-local report 'analysis)))
    (cond
      ((null analysis)
       (message "Not a report of palimpsest: ~a there first" (substitute-command-keys "\\[palimpsest]")))
      ((drawing-refused-p analysis shapes))
      (t
        (let* ((revision (drawn-revision analysis))
               (target (path-join (analysis-directory analysis) (copy-target-name analysis shapes revision))))
          (cond ((shown-beside target)
                 ;; shown already: the same key puts it away again
                 (leave-window (shown-beside target)))
                ((analysis-busy-p analysis))
                (t
                 ;; made afresh each time, never a copy an earlier palimpsest made, nor its view kept
                 (dolist (buffer (buffer-list))
                   (when (equal (shown-file buffer) target) (kill-buffer buffer)))
                 (when (path-kind target) (remove-file target))
                 (start-analysis-job analysis (make-palimpsest-draw-job report analysis revision target shapes)
                                     #'copy-work
                                     (if shapes "tuatara palimpsest shapes" "tuatara palimpsest objects")))))))))

(defun write-copy (shapes)
  "Write a copy of the revision whose part of the current report point is
in, else the latest, to a PDF of its own, asked where: its objects drawn,
or with SHAPES its pages of shapes; for a picture file, the page that stands
for it with the signs found on it drawn, offered as NAME-drawing.pdf. Offered
in the directory last saved to, else in the home directory; never beside the
PDF or picture examined nor in the package's own directory, nothing written
over."
  (let* ((report (current-buffer))
         (analysis (buffer-local report 'analysis)))
    (cond
      ((null analysis)
       (message "Not a report of palimpsest: ~a there first" (substitute-command-keys "\\[palimpsest]")))
      ((drawing-refused-p analysis shapes))
      (t
        (let* ((revision (drawn-revision analysis))
               (name (copy-target-name analysis shapes revision))
               (target (ask-write-target analysis
                                         (if (analysis-picture analysis)
                                             (format nil "Write the drawing of ~a to: " (analysis-name analysis))
                                             (format nil "Write the ~:[objects~;shapes~] of ~a to: "
                                                     shapes (revision-name revision)))
                                         (if (analysis-picture analysis)
                                             name
                                             (format nil "~a-~a" (native-file-stem (analysis-name analysis)) name)))))
          (when target
            (let ((job (make-palimpsest-draw-job report analysis revision target shapes)))
              (setf (palimpsest-draw-job-kept job) t)
              (start-analysis-job analysis job #'copy-work (if shapes
                                                                "tuatara palimpsest shapes written"
                                                                "tuatara palimpsest objects written")))))))))

(define-command palimpsest-draw-objects ()
  "Show beside the report, in a window of its own that \\`q' there closes, a
copy of the PDF with each thing its pages draw framed and named by the
object that draws it: every line of text (each part of a line a content
stream or form of its own draws, framed apart), every shape and picture,
every annotation. Each content stream is drawn in a colour of its own, so
that one a later save added stands out. The revision whose part of the
report point is in, else the latest. The copy is that revision with one
save more, the drawing, written to the report's own directory for you alone,
never beside the PDF examined, drawn afresh each time; the PDF itself is
never changed. On a scanned page, the words unlike the rest (as the
findings name them) are framed in red with how they are unlike. On a picture
file, the copy is the page that stands for the picture, only the signs found
on it framed, written as NAME-drawing.pdf. Shown already,
\\<palimpsest-mode>\\[palimpsest-draw-objects] closes it again; the report stays selected throughout."
  (show-copy nil))

(define-command palimpsest-write-drawing ()
  "Write the copy with each object its pages draw framed and named
\(\\<palimpsest-mode>\\[palimpsest-draw-objects]) to a PDF of its own, to keep or show to others: of the
revision whose part of the report point is in, else the latest. Asked
where, offered in the directory last saved to, else in the home directory; never
beside the PDF examined nor in the package's own directory, nothing written
over. The copy is drawn afresh, the PDF examined never changed. For a picture
file, the page that stands for it with the signs found on it framed, offered as
NAME-drawing.pdf."
  (write-copy nil))
