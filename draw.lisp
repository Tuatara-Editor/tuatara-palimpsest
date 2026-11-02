;;;; draw.lisp --- A copy of a revision with each thing a page draws framed and named by its object.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The copy is the revision as it was, to its %%EOF, and one save more that
;;;; lays a drawing over each page: a frame round each line of text, shape,
;;;; picture and annotation, and beside it the number of the object that
;;;; draws it, each content stream in a colour of its own. It is written to
;;;; the report's own folder, never beside the PDF examined, and shown in
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
three octal digits."
  (with-output-to-string (s)
    (write-char #\( s)
    (loop for c across text
          do (cond ((find c "()\\") (write-char #\\ s) (write-char c s))
                   ((<= 32 (char-code c) 126) (write-char c s))
                   (t (format s "\\~3,'0o" (logand (char-code c) 255)))))
    (write-char #\) s)))

(defun pdf-value-text (value)
  "VALUE, as PDF-OBJECT-AT reads one, written as a PDF writes it: a name (text
with a / first, PDF-NAME-TEXT), true, false and null as words, other text as
a literal string (PDF-LITERAL-TEXT), a number, a reference, an array or a
dictionary."
  (cond ((pdf-ref-p value) (format nil "~d ~d R" (second value) (or (third value) 0)))
        ((pdf-dictionary-p value)
         (format nil "<< ~{~a~^ ~} >>" (loop for (key . v) in value collect (format nil "~a ~a" (pdf-name-text key) (pdf-value-text v)))))
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
  '((0.85 0.1 0.1) (0.1 0.35 0.85) (0.1 0.6 0.2) (0.8 0.45 0.0) (0.55 0.15 0.7) (0.0 0.55 0.6) (0.6 0.4 0.2))
  "The colours the objects drawing a page are framed in, one after another:
each content stream, form and picture its own.")

(defparameter *shape-frame-colour* '(0.1 0.35 0.85)
  "The colour the characters matching no other shape are framed in on their
scanned page (PALIMPSEST-SHOW-SHAPES): red, (red green blue), is for signs.")

(defparameter *shape-note-grey* 0.35
  "The grey of the small words of the pages of shapes: counts, places and
how many more (0 black, 1 white).")

(defparameter *shape-text-grey* 0.2
  "The grey of the words at the top of the pages of shapes (0 black, 1 white).")

(defun page-framer (page out &key (label-size 6))
  "A function of X0 Y0 X1 Y1 LABEL COLOUR and :DASHED writing to OUT the
operators of a frame on PAGE round that box, in COLOUR (red green blue),
with LABEL on white above its top left (inside it when that would be off
the page), moved right past any label it wrote there already; DASHED true
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
          (format out "q 1 1 1 rg ~,2f ~,2f ~,2f ~d re f ~,3f ~,3f ~,3f rg BT /PalimpsestLabel ~d Tf ~,2f ~,2f Td ~a Tj ET Q~%"
                  x y width tall r g b label-size (+ x 1) (+ y (/ label-size 4)) (pdf-literal-text label)))))))

(defun overlay-operators (pdf revision page &key scans copies pieces bare not-judged)
  "The operators of the drawing laid over PAGE of REVISION: a frame round each
line of text (each part of it a stream or form of its own draws), each shape
and each picture, and round each annotation, with the number of the object
that draws it beside it: for text, also where in that stream's decoded
octets its operators begin (@312) and the font objects it is set in. On a
scanned picture, in red, its words unlike the rest, its characters copied
its regions whose JPEG traces are unlike the rest of it, and its characters
without the scan's grain, from SCANS (SCAN-PICTURES), COPIES
\(COPIED-GLYPHS), PIECES (JPEG-PIECES) and BARE, groups of characters each
\(GLYPH...); in grey, dotted, the characters NOT-JUDGED, and a hatch over
the ink the scanner clipped that the copies and the characters without
grain were set beside others without, with a key at the foot of the page
saying so."
  (let* ((colours (make-hash-table)) (next 0) (out (make-string-output-stream)) (keyed nil)
         (framer (page-framer page out)))
    (labels ((colour (number)
               (or (gethash number colours)
                   (setf (gethash number colours) (nth (mod (1- (incf next)) (length *draw-colours*)) *draw-colours*))))
             (frame (&rest arguments) (apply framer arguments)))
      (let ((marks (marks-of pdf revision page)))
        ;; text, line by line: each stretch of it one stream or form draws without a gap wider than one
        ;; and a half times its size (a column apart) framed on its own, named by the stream, where in its
        ;; decoded octets the stretch's operators begin, and the font objects it is set in
        (dolist (line (page-lines-of marks))
          (let ((parts '()))
            (dolist (m line)
              (when (mark-source m)
                (let ((part (first parts)))
                  (if (and part (eql (first part) (mark-source m))
                           (<= (- (mark-x0 m) (fourth part)) (* 1.5 (max 1 (mark-size m)))))
                      (destructuring-bind (x0 y0 x1 y1 from fonts) (cdr part)
                        (setf (cdr part) (list (min x0 (mark-x0 m)) (min y0 (mark-y0 m)) (max x1 (mark-x1 m)) (max y1 (mark-y1 m))
                                               (if (and from (mark-from m)) (min from (mark-from m)) (or from (mark-from m)))
                                               (let ((n (and (mark-font m) (font-number (mark-font m)))))
                                                 (if n (adjoin n fonts) fonts)))))
                      (push (list (mark-source m) (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m) (mark-from m)
                                  (let ((n (and (mark-font m) (font-number (mark-font m))))) (and n (list n))))
                            parts)))))
            (loop for (number x0 y0 x1 y1 from fonts) in (reverse parts)
                  do (frame (- x0 0.5) (- y0 1.5) (+ x1 0.5) y1
                            (place-label number from fonts)
                            (colour number)))))
        ;; shapes of some size, and pictures
        (loop for m across marks
              do (case (mark-kind m)
                   (:fill (when (and (mark-source m) (> (- (mark-x1 m) (mark-x0 m)) 4) (> (- (mark-y1 m) (mark-y0 m)) 4))
                            (frame (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m) (format nil "~d shape" (mark-source m))
                                   (colour (mark-source m)) :dashed t)))
                   (:image (when (mark-object m)
                             (frame (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m) (format nil "~d picture" (mark-object m))
                                    (colour (mark-object m)))
                             ;; on a scan, the words unlike the rest, in red, with how they are unlike
                             (let ((scan (find-if (lambda (scan) (and (eq (scan-picture-page scan) page)
                                                                      (eql (mark-object (scan-picture-mark scan)) (mark-object m))))
                                                  scans)))
                               (when scan
                                 (let ((width (scan-picture-width scan)) (height (scan-picture-height scan)))
                                   (loop for (word . ways) in (scan-picture-odd scan)
                                         do (destructuring-bind (x0 y0 x1 y1) (word-page-box m word width height (scan-picture-tilt scan))
                                              (frame (- x0 1) (- y0 1) (+ x1 1) (+ y1 1)
                                                     (format nil "~{~a~^, ~}" (unlike-words-words ways)) '(0.9 0.0 0.0))))
                                   (loop for (group) in copies
                                         do (dolist (glyph group)
                                              (when (and (eql (glyph-picture glyph) (mark-object m)) (eql (glyph-page glyph) (page-index page)))
                                                (destructuring-bind (x0 y0 x1 y1) (glyph-page-box m glyph width height)
                                                  (frame (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5) "copy" '(0.9 0.0 0.0))))))
                                   (dolist (piece pieces)
                                     (when (and (eql (block-piece-picture piece) (mark-object m)) (eql (block-piece-page piece) (page-index page)))
                                       (destructuring-bind (x0 y0 x1 y1) (block-piece-page-box m piece width height)
                                         (frame (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5) (if (eq (block-piece-strength piece) :strong) "JPEG piece" "JPEG piece?") '(0.9 0.0 0.0)
                                                :dashed (eq (block-piece-strength piece) :look)))))
                                   (loop for (group) in bare
                                         do (dolist (glyph group)
                                              (when (and (eql (glyph-picture glyph) (mark-object m)) (eql (glyph-page glyph) (page-index page)))
                                                (destructuring-bind (x0 y0 x1 y1) (glyph-page-box m glyph width height)
                                                  (frame (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5) "no grain" '(0.9 0.0 0.0))))))
                                   ;; the characters not judged, and the clipped ink the others named were judged without
                                   (flet ((here-p (glyph) (and (eql (glyph-picture glyph) (mark-object m)) (eql (glyph-page glyph) (page-index page)))))
                                     (dolist (glyph not-judged)
                                       (when (here-p glyph)
                                         (setf keyed t)
                                         (destructuring-bind (x0 y0 x1 y1) (glyph-page-box m glyph width height)
                                           (frame (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5) "not judged: clipped" '(0.5 0.5 0.5) :dashed "[0.4 0.8] 0"))))
                                     (dolist (glyph (remove-duplicates (loop for (group) in (append copies bare) append group)))
                                       (when (and (here-p glyph) (>= (glyph-clipped glyph) 0))
                                         (let ((pixels (glyph-pixels glyph)) (across (glyph-width glyph)))
                                           (dotimes (j (- (glyph-y1 glyph) (glyph-y0 glyph)))
                                             (dotimes (i (- (glyph-x1 glyph) (glyph-x0 glyph)))
                                               (when (<= (aref pixels (+ (* (+ 2 j) across) 2 i)) (glyph-clipped glyph))
                                                 (setf keyed t)
                                                 (let ((x (+ (glyph-x0 glyph) i)) (y (+ (glyph-y0 glyph) j)))
                                                   (destructuring-bind (px0 py0 px1 py1) (picture-page-box m x y (1+ x) (1+ y) width height)
                                                     (format out "q 0.6 0.6 0.6 RG 0.1 w ~,3f ~,3f m ~,3f ~,3f l S Q~%" px0 py0 px1 py1))))))))))))))))))
      ;; what the grey frames and hatch mean, at the foot of the page
      (when keyed
        (destructuring-bind (px0 py0 px1 py1) (or (page-crop page) (page-media page))
          (declare (ignore px1 py1))
          (let ((key "Dotted grey: not judged, too few pixels left once the ink the scanner clipped is set aside. Grey hatch: clipped ink set aside."))
            (format out "q 1 1 1 rg ~,2f ~,2f ~,2f 8 re f 0.4 0.4 0.4 rg BT /PalimpsestLabel 6 Tf ~,2f ~,2f Td ~a Tj ET Q~%"
                    (+ px0 2) (+ py0 2) (+ 2 (* 6 0.5 (length key))) (+ px0 3) (+ py0 4) (pdf-literal-text key)))))
      ;; annotations, laid over the page
      (dolist (value (page-annots page))
        (let* ((annot (deref pdf revision value))
               (subtype (and (pdf-dictionary-p annot) (pdf-get annot "/Subtype")))
               (box (and (pdf-dictionary-p annot) (box-of (deref pdf revision (pdf-get annot "/Rect"))))))
          (when (and box (stringp subtype) (not (string= subtype "/Popup")))
            (destructuring-bind (x0 y0 x1 y1) box
              (frame x0 y0 x1 y1 (format nil "~@[~d ~]~a" (and (pdf-ref-p value) (second value)) (subseq subtype 1))
                     '(0.9 0.3 0.6) :dashed t))))))
    (get-output-stream-string out)))

;;; The copy

(defparameter *label-font* "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>"
  "The font the labels of a copy's drawing are set in, as a PDF writes it.")

(defun copy-information (analysis revision what)
  "The document information of a copy of REVISION of ANALYSIS's PDF with
WHAT drawn on it, as a PDF writes it: its title saying so, palimpsest its
producer."
  (pdf-value-text (list (cons "/Title" (format nil "~a, ~a, ~a" (analysis-name analysis) (revision-name revision) what))
                        (cons "/Producer" "palimpsest, a package of Tuatara"))))

(defun overlaid-page-text (pdf revision page font open-state close-state overlay)
  "PAGE's dictionary written anew, as a PDF writes it, with the content
stream OVERLAY drawn over its own drawing, that kept apart between the
streams OPEN-STATE (q) and CLOSE-STATE (Q), and the font object FONT among
its resources as /PalimpsestLabel."
  (let* ((dict (page-dict page))
         (contents (let ((c (pdf-get dict "/Contents"))) (if (pdf-ref-p c) (list c) (and (listp c) (remove-if-not #'pdf-ref-p c)))))
         (resources (let ((r (deref pdf revision (page-resources page)))) (if (pdf-dictionary-p r) r '())))
         (fonts (let ((f (deref pdf revision (pdf-get resources "/Font")))) (if (pdf-dictionary-p f) f '())))
         (new-resources (acons "/Font" (append (remove "/PalimpsestLabel" fonts :key #'car :test #'string=)
                                               (list (cons "/PalimpsestLabel" (list :ref font 0))))
                               (remove "/Font" resources :key #'car :test #'string=))))
    (pdf-value-text (append (remove-if (lambda (pair) (member (car pair) '("/Contents" "/Resources") :test #'string=)) dict)
                            (list (cons "/Resources" new-resources)
                                  (cons "/Contents" (append (list (list :ref open-state 0)) contents
                                                            (list (list :ref close-state 0) (list :ref overlay 0)))))))))

(defun first-free-number (pdf)
  "The object number after the highest any save of PDF names."
  (1+ (loop for r across (pdf-revisions pdf)
            maximize (loop for n being the hash-keys of (revision-entries r) maximize n))))

(defstruct (copy-save (:constructor make-copy-save-of (octets fill)) (:copier nil) (:predicate nil))
  "A copy of a revision with one save more, being written: OCTETS, the
revision's octets and those added after them, FILL of them written so far;
ENTRIES, each object added as (NUMBER GENERATION . OFFSET)."
  (octets (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (fill 0 :type fixnum) (entries '()))

(defun make-copy-save (pdf revision)
  "A COPY-SAVE of REVISION of PDF: its octets to its %%EOF line, and a line
end after them when they lack one. The PDF itself is copied once."
  (let* ((octets (pdf-octets pdf)) (end (revision-end revision))
         (save (make-copy-save-of (make-array (max 4096 (+ end (floor end 8))) :element-type '(unsigned-byte 8)) end)))
    (replace (copy-save-octets save) octets :end2 end)
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
  (save-put save (format nil "~d 0 obj~%<< ~@[~a ~]/Length ~d >>~%stream~%" number (and (plusp (length dictionary)) dictionary) (length data)))
  (save-put save data)
  (save-put save (format nil "~%endstream~%endobj~%")))

(defun finish-copy-save (save revision target size info)
  "End SAVE, a copy of REVISION, with a cross-reference table for the objects
added and a trailer going back to the revision's, of SIZE objects, naming
INFO its document information and the revision's own root and identifier;
and write it to TARGET, a new file. Returns TARGET."
  (let ((trailer (revision-trailer revision)) (xref (copy-save-fill save)))
    (save-put save (format nil "xref~%0 1~%0000000000 65535 f~c~c" #\Return #\Newline))
    (dolist (entry (sort (copy-list (copy-save-entries save)) #'< :key #'car))
      (destructuring-bind (number generation . offset) entry
        (save-put save (format nil "~d 1~%~10,'0d ~5,'0d n~c~c" number offset generation #\Return #\Newline))))
    (save-put save (format nil "trailer~%<< /Size ~d ~{~a~^ ~} /Prev ~d >>~%startxref~%~d~%%%EOF~%"
                           size
                           (cons (format nil "/Info ~d 0 R" info)
                                 (loop for key in '("/Root" "/ID")
                                       for value = (pdf-get trailer key)
                                       when value collect (format nil "~a ~a" (pdf-name-text key) (pdf-value-text value))))
                           (revision-startxref revision) xref))
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
                 (obj (search #(111 98 106) octets :start2 (pdf-indirect-get object :at)
                                                   :end2 (min (length octets) (+ (pdf-indirect-get object :at) 64)))))
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

(defun marked-copy (analysis revision target &key (cancel (list nil)))
  "Write to TARGET the revision REVISION of ANALYSIS's PDF with one save more
laying the drawing of OVERLAY-OPERATORS over each of its pages; the page's
own drawing kept apart between q and Q, and document information of its own
saying what the copy is (the revision's own in the save before). Returns TARGET; an error saying why
it cannot be made (an encrypted file, whose added streams would be read as
encrypted too)."
  (let ((pdf (analysis-pdf analysis)))
    (let ((refusal (copy-refusal revision))) (when refusal (error refusal)))
    (let* ((next (first-free-number pdf))
           (save (make-copy-save pdf revision))
           (font next) (open-state (+ next 1)) (close-state (+ next 2)) (info (+ next 3))
           (scans (nth-value 0 (scans-read pdf revision)))
           (copies (nth-value 1 (scans-read pdf revision)))
           (pieces (nth-value 3 (scans-read pdf revision)))
           (bare (append (mapcar #'list (nth-value 4 (scans-read pdf revision)))
                         (mapcar (lambda (entry) (list (third entry))) (nth-value 5 (scans-read pdf revision)))))
           (not-judged (nth-value 6 (scans-read pdf revision))))
      (setf next (+ next 4))
      (save-object save font *label-font*)
      ;; the copy says what it is: the revision's own information is in the save before
      (save-object save info (copy-information analysis revision "each object it draws framed and named"))
      (save-stream save open-state "q")
      (save-stream save close-state "Q")
      (dolist (page (pages-of pdf revision))
        (when (car cancel) (return-from marked-copy nil))
        (job-check-stop)
        (when (page-number page)
          (let ((overlay next))
            (incf next)
            (save-stream save overlay (overlay-operators pdf revision page :scans scans :copies copies :pieces pieces :bare bare :not-judged not-judged))
            (save-object save (page-number page) (overlaid-page-text pdf revision page font open-state close-state overlay)
                         :generation (page-generation page)))))
      (finish-copy-save save revision target next info))))

;;; The commands

(defun copy-target-name (shapes revision)
  "The name of the copy of REVISION drawn for the report: its objects drawn,
or with SHAPES its pages of shapes."
  (format nil "~:[objects~;shapes~]-revision-~d.pdf" shapes (1+ (revision-index revision))))

(defun report-drawing-window (report)
  "The window showing a copy REPORT drew (PALIMPSEST-DRAW-OBJECTS,
PALIMPSEST-SHOW-SHAPES), when one does."
  (let ((folder (let ((analysis (buffer-local report 'analysis))) (and analysis (analysis-folder analysis)))))
    (and folder
         (loop for buffer in (buffer-list)
               for file = (shown-file buffer)
               thereis (and file (path-within-p file folder)
                            (or (search "objects-revision-" (native-file-name file))
                                (search "shapes-revision-" (native-file-name file)))
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

(defstruct (palimpsest-draw-job (:include job) (:constructor make-palimpsest-draw-job (buffer analysis revision target shapes)) (:copier nil))
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
  (let ((name (analysis-name (palimpsest-draw-job-analysis job)))
        (revision (revision-name (palimpsest-draw-job-revision job)))
        (shapes (palimpsest-draw-job-shapes job)))
    (cond ((palimpsest-draw-job-none job)
           (message "~a's ~a has no scanned page: no characters to set side by side" name revision))
          ((not (palimpsest-draw-job-done job))
           (message "~:[The objects were not drawn~;The shapes were not set side by side~]: ~a" shapes (job-ending-words job)))
          ((palimpsest-draw-job-kept job)
           (message "Wrote ~a: ~a's ~a with ~:[each thing its pages draw framed and named by its object~;its scanned pages' characters set side by side by their shapes~]"
                    (abbreviated-path (palimpsest-draw-job-target job)) name revision shapes))
          (t
           (show-beside (palimpsest-draw-job-target job))
           (if shapes
               (message "~a's ~a: each scanned page's characters grouped by their shape, those matching no other last; palimpsest claims nothing of them"
                        name revision)
               (message "~a's ~a with each thing its pages draw framed, the number of the object drawing it beside it: each content stream a colour of its own, annotations dashed"
                        name revision))))))

(defun copy-work (job)
  "What a job writing a copy does (PALIMPSEST-DRAW-JOB): the copy written,
DONE set only when it was, not stopped short."
  (let* ((analysis (palimpsest-draw-job-analysis job)) (revision (palimpsest-draw-job-revision job))
         (target (palimpsest-draw-job-target job))
         (written (if (palimpsest-draw-job-shapes job)
                      (shapes-copy analysis revision target :cancel (job-cancel job)
                                                            :progress (lambda (part) (setf (palimpsest-draw-job-progress job) part)))
                      (marked-copy analysis revision target :cancel (job-cancel job)))))
    (setf (palimpsest-draw-job-none job) (eq written :none)
          (palimpsest-draw-job-done job) (and written (not (eq written :none)) t))))

(defun show-copy (shapes)
  "Show beside the current report a copy of the revision whose part of it
point is in, else the latest: its objects drawn, or with SHAPES its pages
of shapes; made afresh in the background, in the report's own folder. Shown
already, it is put away again."
  (let* ((report (current-buffer))
         (analysis (buffer-local report 'analysis)))
    (if (null analysis)
        (message "Not a report of palimpsest: ~a there first" (substitute-command-keys "\\[palimpsest]"))
        (let* ((pdf (analysis-pdf analysis))
               (revision (or (report-revision-at analysis) (last-revision pdf)))
               (target (path-join (analysis-folder analysis) (copy-target-name shapes revision))))
          (cond ((shown-beside target)
                 ;; shown already: the same key puts it away again
                 (leave-window (shown-beside target)))
                (t
                 ;; made afresh each time, never a copy an earlier palimpsest made, nor its view kept
                 (dolist (buffer (buffer-list))
                   (when (equal (shown-file buffer) target) (kill-buffer buffer)))
                 (when (path-kind target) (remove-file target))
                 (start-job-thread (make-palimpsest-draw-job report analysis revision target shapes) #'copy-work
                                   :name (if shapes "tuatara palimpsest shapes" "tuatara palimpsest objects"))))))))

(defun copy-target-refusal (analysis target)
  "Why a copy of ANALYSIS's PDF may not be written to TARGET, in words: its
folder may not be written in (FOLDER-REFUSAL), or something is there
already; NIL when it may."
  (or (folder-refusal analysis (parent-path target))
      (and (path-kind target :follow nil)
           (format nil "~a is there already: nothing is written over" (native-file-name target)))))

(defun write-copy (shapes)
  "Write a copy of the revision whose part of the current report point is
in, else the latest, to a PDF of its own, asked where: its objects drawn,
or with SHAPES its pages of shapes. Offered in the folder last saved to,
else in the home folder; never beside the PDF examined nor in the package's
own folder, nothing written over."
  (let* ((report (current-buffer))
         (analysis (buffer-local report 'analysis)))
    (if (null analysis)
        (message "Not a report of palimpsest: ~a there first" (substitute-command-keys "\\[palimpsest]"))
        (let* ((revision (or (report-revision-at analysis) (last-revision (analysis-pdf analysis))))
               (offered (offered-path analysis (format nil "~a-~a" (native-file-stem (analysis-name analysis)) (copy-target-name shapes revision))))
               (target (read-new-native-path (format nil "Write the ~:[objects~;shapes~] of ~a to: " shapes (revision-name revision)) :initial offered)))
          (when target
            (let ((refusal (copy-target-refusal analysis target)))
              (if refusal
                  (message "Not written to ~a: ~a" (abbreviated-path target) refusal)
                  (let ((job (make-palimpsest-draw-job report analysis revision target shapes)))
                    (setf (palimpsest-draw-job-kept job) t)
                    (start-job-thread job #'copy-work
                                      :name (if shapes "tuatara palimpsest shapes written" "tuatara palimpsest objects written"))))))))))

(define-command palimpsest-draw-objects ()
  "Show beside the report, in a window of its own that \\`q' there closes, a
copy of the PDF with each thing its pages draw
framed and named by the object that draws it: every line of text (each part
of a line a content stream or form of its own draws, framed apart), every
shape and picture, every annotation; each content stream a colour of its
own, so that one a later save added stands out. The revision whose part of
the report point is in, else the latest. The copy is that revision with one
save more, the drawing, written to the report's own folder for you alone,
never beside the PDF examined, drawn afresh each time; the PDF itself is
never changed. On a scanned page, the words unlike the rest (as the
findings name them) are framed in red with how they are unlike. Shown
already, \\<palimpsest-mode>\\[palimpsest-draw-objects] closes it again; the report stays selected throughout."
  (show-copy nil))

(define-command palimpsest-write-drawing ()
  "Write the copy with each object its pages draw framed and named
\(\\<palimpsest-mode>\\[palimpsest-draw-objects]) to a PDF of its own, to keep or show to others: of the
revision whose part of the report point is in, else the latest. Asked
where, offered in the folder last saved to, else in the home folder; never
beside the PDF examined nor in the package's own folder, nothing written
over. The copy is drawn afresh, the PDF examined never changed."
  (write-copy nil))
