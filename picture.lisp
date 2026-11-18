;;;; picture.lisp --- A picture file examined as one scanned page, and what it says of itself.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A PNG, JPEG or TIFF, as a scanner, a phone or a picture editor saves one,
;;;; is examined as a PDF's scanned page is: its pixels, which Tuatara turns
;;;; into pixels in the sandbox, are read for words unlike the rest, characters
;;;; copied and pieces with JPEG blocks of their own. What it says of itself
;;;; (EXIF, XMP, a PNG's text and time, a JPEG's comments, a TIFF's tags) is
;;;; read as data, never run, and a JPEG's steps are measured for a second
;;;; save. The checks of a PDF's pages read a page of the package's own making
;;;; that draws the picture over the whole of it.

(in-package #:palimpsest)

(defun scan-named (mark &key short)
  "How a finding names the scanned picture MARK: its name and object number,
the name alone with SHORT; for one joined from strips (MARK-STRIPS), how
many, and the first's and last's names and object numbers; the picture while
a picture file is examined (*PICTURE-FILE*)."
  (let ((strips (mark-strips mark)))
    (cond (*picture-file* "the picture")
          ((and strips short) (format nil "the scanned page joined from ~d strips" (length strips)))
          (strips (let ((first (first (first strips))) (last (first (first (last strips)))))
                    (format nil "the scanned page joined from ~d strips, ~a (object ~d) to ~a (object ~d)"
                            (length strips) (mark-text first) (mark-object first) (mark-text last)
                            (mark-object last))))
          (short (format nil "the scanned picture ~a" (mark-text mark)))
          (t (format nil "the scanned picture ~a (object ~d)" (mark-text mark) (mark-object mark))))))

(defun picture-number (number)
  "NUMBER, the object number of a scanned picture as a finding's details name
it, or NIL while a picture file is examined (*PICTURE-FILE*), its one
picture the file itself."
  (unless *picture-file* number))

(defun strip-at (mark row)
  "The strip of the picture MARK draws, joined from strips (MARK-STRIPS), that
holds its row ROW, as (MARK ROW HEIGHT): the first for a row above them all,
the last for one below; NIL for a picture of one piece."
  (let ((strips (mark-strips mark)))
    (and strips
         (or (find-if (lambda (s) (< row (+ (second s) (third s)))) strips) (first (last strips))))))

(defun pixels-words (mark x0 y0 x1 y1)
  "Where the pixels from X0,Y0 to X1,Y1 of the picture MARK draws lie, in
words, as a finding's details name them: in its picture, named by its object
number (PICTURE-NUMBER); for a picture joined from strips, in the strip or
strips holding them, by the strips' own rows, and the rows of the joined
picture beside them."
  (let ((top (strip-at mark y0)) (bottom (strip-at mark (max y0 (1- y1)))))
    (cond ((null top)
           (format nil "pixels ~d,~d to ~d,~d~@[ of picture ~d~]" x0 y0 x1 y1 (picture-number (mark-object mark))))
          ((eq top bottom)
           (format nil "pixels ~d,~d to ~d,~d of picture ~d, rows ~d to ~d of the page joined from strips"
                   x0 (- y0 (second top)) x1 (- y1 (second top)) (mark-object (first top)) y0 y1))
          (t (format nil "pixels ~d,~d of picture ~d to ~d,~d of picture ~d, rows ~d to ~d of the page joined from ~
                          strips"
                     x0 (- y0 (second top)) (mark-object (first top)) x1 (- y1 (second bottom))
                     (mark-object (first bottom)) y0 y1)))))

(defun pixels-place (mark x0 y0 x1 y1 width height)
  "Where the pixels from X0,Y0 to X1,Y1 of the picture MARK draws, WIDTH by
HEIGHT pixels, lie, in words: their place on the page in points
\(PICTURE-PAGE-BOX), then the pixels themselves (PIXELS-WORDS)."
  (destructuring-bind (px py &rest far) (picture-page-box mark (list x0 y0 x1 y1) width height)
    (declare (ignore far))
    (format nil "at x ~d, y ~d (~a)" (round px) (round py) (pixels-words mark x0 y0 x1 y1))))

(defun objects-at (mark rows)
  "The object numbers of the pictures that hold ROWS, each (TOP . BOTTOM), of
the picture MARK draws: for one joined from strips (MARK-STRIPS), the strips
holding any of them, in the order of its rows; else its own alone."
  (if (mark-strips mark)
      (loop for strip in (mark-strips mark)
            for (nil row height) = strip
            when (some (lambda (r) (and (< (car r) (+ row height)) (<= row (max (car r) (1- (cdr r)))))) rows)
              collect (mark-object (first strip)))
      (list (mark-object mark))))

(defun ends-objects (mark)
  "The object numbers by which a finding's evidence names the whole of the
picture MARK draws: for one joined from strips (MARK-STRIPS), its first and
last strips; else its own."
  (let ((objects (mark-objects mark)))
    (remove-duplicates (list (first objects) (first (last objects))))))

;;; Reading the picture

(defun picture-kind-of (path)
  "The kind of picture file at PATH, a key of *PICTURE-KINDS*, told by its
first octets whatever its name; NIL for any other file."
  (let ((head (file-head path)))
    (and head (car (assoc (nth-value 1 (octets-kind head)) *picture-kinds*)))))

(defun read-picture-file (octets kind)
  "The picture file of KIND whose contents are OCTETS, read as a
PICTURE-FILE: what it says of itself read as data (OCTETS-METADATA), its
content credentials (PICTURE-CREDENTIALS), a JPEG's tables (JPEG-TABLES)
and layout (JPEG-LAYOUT), and its pixels, which Tuatara turns into pixels
in the sandbox (DECODE-PICTURE), at most *PALIMPSEST-SCAN-PIXELS* of them.
The picture is turned as its EXIF says it is held. When its pixels cannot
be read, its REFUSAL says why; when what it says of itself cannot be read,
even for want of memory, a section says so."
  (let ((file (make-picture-file
               :kind kind
               :jpeg-tiff (and (eq kind :tiff) (tiff-jpeg-p octets))
               ;; a file made to run the reading out of memory loses what it says of itself, not the examining
               :sections (handler-case (remove "" (octets-metadata octets kind) :test #'string=)
                           ((or error storage-condition) (e)
                             (list (format nil "* Not read~%- Why :: ~a~%" (shorten-string (princ-to-string e) 200
                                                                                         :one-line t)))))
               :tables (handler-case (and (eq kind :jpeg) (multiple-value-list (jpeg-tables octets)))
                         ((or error storage-condition) () nil))
               :layout (handler-case (and (eq kind :jpeg) (jpeg-layout octets))
                         ((or error storage-condition) () nil))
               :credentials (handler-case (picture-credentials octets kind)
                              ((or error storage-condition) (e)
                                (list (make-credential :problem (shorten-string (princ-to-string e) 200 :one-line t)
                                                       :active t)))))))
    (multiple-value-bind (image notes) (decode-picture octets kind 16384 16384)
      (cond ((null image)
             (setf (picture-file-refusal file)
                   (decode-refusal notes (format nil "the ~a could not be decoded"
                                                 (cdr (assoc kind *picture-kinds*))))))
            ((> (* (image-width image) (image-height image)) *palimpsest-scan-pixels*)
             (setf (picture-file-refusal file)
                   (format nil "it has ~:d pixels, more than *palimpsest-scan-pixels* allows"
                           (* (image-width image) (image-height image)))))
            (t
             (let* ((pixels (image-pixels image)) (channels (image-channels image))
                    (coloured (loop for at from 0 below (length pixels) by channels
                                    thereis (or (/= (aref pixels at) (aref pixels (+ at 1)))
                                                (/= (aref pixels at) (aref pixels (+ at 2)))))))
               (setf (picture-file-width file) (image-width image)
                     (picture-file-height file) (image-height image)
                     (picture-file-grey file) (image-grey image)
                     (picture-file-colours file) (and coloured (cons pixels channels)))))))
    file))

(defun source-octets (path &optional sum)
  "The octets of the file at PATH, as far as *PALIMPSEST-LARGEST-FILE* allows.
With SUM, a SHA-256, an error when the file's differs: it is not the one
examined any more."
  (let ((octets (file-octets path :limit *palimpsest-largest-file*)))
    (when (and sum (not (equal (octets-sha256 octets) sum)))
      (error "~a has changed since it was examined" (native-file-name path)))
    octets))

(defconstant +picture-image-object+ 4
  "The number of the picture's object in the page that stands for a picture
file (PICTURE-PAGE-OCTETS).")

(defun picture-page-octets (width height)
  "A PDF of one page WIDTH by HEIGHT points that draws one picture of WIDTH by
HEIGHT pixels over the whole of it, the picture's samples left out: the
page a picture file is examined as, its pixels those of *PICTURE-FILE*."
  (let* ((content (format nil "q ~d 0 0 ~d 0 0 cm /Im1 Do Q" width height))
         (bodies (list "<< /Type /Catalog /Pages 2 0 R >>"
                       "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
                       (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~d ~d] /Resources << /XObject << /Im1 ~
                                    ~d 0 R >> >> /Contents 5 0 R >>"
                               width height +picture-image-object+)
                       (format nil "<< /Type /XObject /Subtype /Image /Width ~d /Height ~d /ColorSpace /DeviceGray ~
                                    /BitsPerComponent 8 /Length 0 >>~%stream~%~%endstream"
                               width height)
                       (format nil "<< /Length ~d >>~%stream~%~a~%endstream" (length content) content)))
         (offsets '())
         (text (with-output-to-string (s)
                 (format s "%PDF-1.4~%")
                 (loop for body in bodies for n from 1
                       do (push (file-position s) offsets)
                          (format s "~d 0 obj~%~a~%endobj~%" n body))
                 (let ((xref (file-position s)))
                   (format s "xref~%0 ~d~%0000000000 65535 f ~%" (1+ (length bodies)))
                   (dolist (at (reverse offsets)) (format s "~10,'0d 00000 n ~%" at))
                   (format s "trailer~%<< /Size ~d /Root 1 0 R >>~%startxref~%~d~%%%EOF~%"
                           (1+ (length bodies)) xref)))))
    (map '(simple-array (unsigned-byte 8) (*)) #'char-code text)))

;;; What it says of itself

(defun picture-fields (sections)
  "The fields of SECTIONS, sections of a note as OCTETS-METADATA gives them,
each (HEADING LABEL VALUE): the section's heading, and its line's label and
value as the note writes them, the label ending at the line's first ::,
which the note never writes within a label, whatever the file calls it."
  (loop for section in sections
        for lines = (uiop:split-string section :separator '(#\Newline))
        for heading = (string-left-trim "* " (first lines))
        append (loop for line in (rest lines)
                     for split = (search " :: " line)
                     when (and split (uiop:string-prefix-p "- " line))
                       collect (list heading (subseq line 2 split) (subseq line (+ split 4))))))

(defparameter *picture-program-labels* '("software" "creatortool" "comment" "history" "processing" "producer")
  "Words in the label of a field of a picture's metadata that names the
programs it went through: EXIF's Software, XMP's CreatorTool and edit
history, a JPEG's or PNG's comment (\"Created with GIMP\").")

(defparameter *picture-made-labels*
  '("DateTimeOriginal" "DateTimeDigitized" "xmp:CreateDate" "exif:DateTimeOriginal" "photoshop:DateCreated")
  "The fields of a picture's metadata that say when it was made, as their labels are.")

(defparameter *picture-changed-labels* '("DateTime" "xmp:ModifyDate" "Last changed (tIME)")
  "The fields of a picture's metadata that say when it was last changed, as their labels are.")

(defparameter *picture-dates-apart* 60
  "The most seconds two dates of a picture's metadata, when it was made and
when it was changed, may lie apart and agree, when both are of its EXIF or
both name their zone. A camera or scanner writes both at once; a picture
editor writes the second when it saves.")

(defparameter *picture-zones-apart* (* 26 3600)
  "The most seconds a date of a picture's metadata that names no zone may lie
from one of another kind and agree: the widest the zones of the world lie
apart, 26 hours, as the EXIF's own dates name none and a PNG's time is
universal.")

(defun picture-date (text)
  "TEXT, a date as a picture's metadata says it, written as XMP writes one:
EXIF's 2026:03:02 10:00:00 as 2026-03-02T10:00:00, a PNG's time in
universal time with its zone; else TEXT as it is."
  (cond ((and (>= (length text) 19) (every #'digit-char-p (subseq text 0 4)) (char= (char text 4) #\:)
              (char= (char text 7) #\:) (char= (char text 10) #\Space))
         (format nil "~a-~a-~aT~a" (subseq text 0 4) (subseq text 5 7) (subseq text 8 10) (subseq text 11 19)))
        ((and (>= (length text) 23) (string= (subseq text 19 23) " UTC"))
         (format nil "~aT~aZ" (subseq text 0 10) (subseq text 11 19)))
        (t text)))

(defun picture-dates-disagree-p (made changed)
  "True when MADE and CHANGED, each (LABEL VALUE) of a picture's metadata,
lie further apart than they may (*PICTURE-DATES-APART*,
*PICTURE-ZONES-APART*)."
  (let* ((a (picture-date (second made))) (b (picture-date (second changed)))
         (apart (dates-apart a b))
         (zoned (lambda (text) (nth-value 1 (date-of text))))
         (strict (or (and (not (find #\: (first made))) (not (find #\: (first changed)))
                          (not (search "tIME" (first changed))))
                     (and (funcall zoned a) (funcall zoned b)))))
    (and apart (> apart (if strict *picture-dates-apart* *picture-zones-apart*)))))

(defun dates-among (fields labels)
  "The first *PICTURE-FIELDS-LISTED* different fields of FIELDS (as
PICTURE-FIELDS gives them) whose label is one of LABELS, each (LABEL VALUE)."
  (let ((out '()))
    (loop for (nil label value) in fields
          while (< (length out) *picture-fields-listed*)
          when (and (member label labels :test #'string=) (not (member (list label value) out :test #'equal)))
            do (push (list label value) out))
    (nreverse out)))

(defun check-picture-metadata (file)
  "What FILE, a PICTURE-FILE, says of itself. For the record, the programs,
device and dates it names, with every field in the details. Worth a look, a
picture editor named, the more so beside a camera's or scanner's own name
(an honest crop, turn or lightening is saved so too, and some scanner and
phone apps name such a program); and dates of when it was made and changed
that disagree (an honest picture turned in a phone's gallery or saved again
by a viewer gives that too)."
  (let* ((fields (picture-fields (picture-file-sections file)))
         (name (cdr (assoc (picture-file-kind file) *picture-kinds*)))
         (named (picture-programs fields))
         (programs (nth-value 1 (picture-programs fields)))
         (editors (remove :editor named :key #'third :test-not #'eq))
         (device (picture-device fields))
         ;; each date once, the first of them: a file can hold a great many
         (made (dates-among fields *picture-made-labels*))
         (changed (dates-among fields *picture-changed-labels*))
         (apart (loop for m in made
                      thereis (loop for c in changed
                                    when (picture-dates-disagree-p m c) return (list m c))))
         (details (append (loop for (heading label value) in fields
                                repeat *picture-details-most*
                                collect (format nil "~a: ~a: ~a" heading label value))
                          (and (> (length fields) *picture-details-most*)
                               (list (format nil "and ~:d fields more" (- (length fields) *picture-details-most*))))))
         (out '()))
    (when fields
      (push (found :record (format nil "What the ~a says of itself is listed~:[~;: ~:*~{~a~^; ~}~]."
                                   name
                                   (append (and device (list (format nil "made by ~a" device)))
                                           (and named (list (format nil "naming ~{~a~^, ~}" (mapcar #'second named))))
                                           (mapcar (lambda (d) (format nil "~a ~a" (first d) (second d)))
                                                   (append made changed))))
                   :rule (concatenate 'string "What a picture says of itself (EXIF, XMP, a PNG's text, a JPEG's "
                                              "comments, a TIFF's tags) is read as data, never run, and given "
                                              "whole for the record.")
                   :details details)
            out))
    (when editors
      (push (found :look (format nil "The ~a names ~{~a~^, ~} in what it says of itself (~a): opened in a ~
                                      picture editor and saved again~@[, after ~a made it~]. An honest crop, turn or ~
                                      lightening is saved so too, and some scanner and phone apps name such a program."
                                 name
                                 (mapcar (lambda (e) (format nil "~a (~a)" (second e) (fourth e))) editors)
                                 (listed-words (let ((seen (make-hash-table :test 'equal)))
                                                 (loop for (label value) in programs
                                                       for words = (format nil "~a ~a" label value)
                                                       unless (gethash words seen)
                                                         collect (setf (gethash words seen) words))))
                                 device)
                   :rule (concatenate 'string "A picture editor writes its name into the pictures it saves (EXIF's "
                                              "Software, XMP's CreatorTool and history, a comment). Worth a look, "
                                              "not proof: it tells that the picture was saved by such a program, "
                                              "not what was changed.")
                   :details details
                   :method :picture-editor)
            out))
    (when apart
      (destructuring-bind ((made-label made-value) (changed-label changed-value)) apart
        (push (found :look (format nil "The ~a says it was made ~a (~a) and changed ~a (~a): saved again after it was ~
                                        made. A picture turned or cropped in a phone's gallery, or saved again by a ~
                                        viewer, gives that too."
                                   name made-value made-label changed-value changed-label)
                     :rule (format nil "A camera or scanner writes when a picture was made and when it was changed ~
                                        at once; a program that saves it again writes the second anew. Dates of ~
                                        its EXIF, or both naming their zone, agree within ~d seconds; others, one ~
                                        of which names no zone, within ~d hours, as far as zones lie apart."
                                   *picture-dates-apart* (round *picture-zones-apart* 3600))
                     :details details
                     :method :picture-editor)
              out)))
    (nreverse out)))

(defun picture-device (fields)
  "The camera, phone or scanner FIELDS (as PICTURE-FIELDS gives them) name
as the one that made the picture, its EXIF's make and model in words; NIL
when they name none, or only blanks."
  (flet ((field (label)
           (let ((value (third (find label fields :key #'second :test #'string=))))
             (and value (string/= "" (string-trim " " value)) value))))
    (let ((make (field "Make")) (model (field "Model")))
      (and (or make model) (format nil "~@[~a~]~:[~; ~]~@[~a~]" make (and make model) model)))))

(defparameter *picture-fields-listed* 10
  "The most fields of one kind a finding on what a picture says of itself
names in its words (programs, dates), the rest counted: a file can hold a
great many.")

(defparameter *picture-details-most* 200
  "The most fields of what a picture says of itself given in a finding's
details, the rest counted.")

(defun picture-programs (fields)
  "The entries of *PDF-TOOLS* that those of FIELDS (as PICTURE-FIELDS gives
them) naming the programs a picture went through (*PICTURE-PROGRAM-LABELS*)
name, each once, and those fields, each (LABEL VALUE), as two values. A
value held by many fields is looked into once."
  (let* ((programs (loop for (nil label value) in fields
                         when (some (lambda (word) (search word (string-downcase label))) *picture-program-labels*)
                           collect (list label value)))
         (seen (make-hash-table :test 'equal)))
    (values (remove-duplicates (loop for (nil value) in programs
                                     unless (gethash value seen)
                                       append (progn (setf (gethash value seen) t) (tools-in value)))
                               :key #'second :test #'string= :from-end t)
            programs)))

(defun listed-words (items)
  "ITEMS, strings, as words: the first *PICTURE-FIELDS-LISTED* of them, joined
by semicolons, and how many more there are."
  (format nil "~{~a~^; ~}~@[ and ~:d more~]"
          (subseq items 0 (min (length items) *picture-fields-listed*))
          (and (> (length items) *picture-fields-listed*) (- (length items) *picture-fields-listed*))))

(defun picture-traits (file)
  "The traits of FILE, a PICTURE-FILE, for its fingerprint, each (KIND VALUE)
as FINGERPRINT gives them: each picture editor it names, :editor, each other
program, :made-by, and a JPEG's tables, :jpeg."
  (append (loop for tool in (picture-programs (picture-fields (picture-file-sections file)))
                collect (list (if (eq (third tool) :editor) :editor :made-by) (second tool)))
          (destructuring-bind (&optional octets quality table) (picture-file-tables file)
            (declare (ignore table))
            (and octets (list (list :jpeg (format nil "tables ~a~@[, about quality ~d~]" (fnv-hex octets) quality)))))))

;;; A JPEG saved twice

(defparameter *zigzag*
  #(0 1 8 16 9 2 3 10 17 24 32 25 18 11 4 5 12 19 26 33 40 48 41 34 27 20 13 6 7 14 21 28
    35 42 49 56 57 50 43 36 29 22 15 23 30 37 44 51 58 59 52 45 38 31 39 46 53 60 61 54 47 55 62 63)
  "For each step of a JPEG's quantisation table in the order it keeps them,
the frequency it is of: across plus eight times down.")

(defun standard-table (quality)
  "The luminance table libjpeg, the library most programs save JPEGs with,
makes for QUALITY from 1 to 100 by scaling the standard's: 64 steps in the
order a JPEG keeps them, as a list."
  (let ((scale (if (< quality 50) (floor 5000 quality) (- 200 (* 2 quality)))))
    (map 'list (lambda (base) (max 1 (min 255 (floor (+ (* base scale) 50) 100)))) *jpeg-luminance-table*)))

(defun standard-table-quality (table)
  "The quality from 1 to 100 at which libjpeg makes TABLE (64 steps in the
order a JPEG keeps them) from the standard's luminance table
\(STANDARD-TABLE); NIL when TABLE is none of those, a table of a camera's or
scanner's own."
  (loop for quality from 1 to 100
        when (every #'= table (standard-table quality))
          return quality))

(defparameter *requantised-frequencies* 15
  "How many of a JPEG's lowest frequencies after the first (its blocks' mean),
in the order it keeps them, are measured for a second save: those most
blocks hold enough of to count.")

(defparameter *requantised-largest* 30
  "The most steps from nothing a frequency of a block is counted at, when a
JPEG is measured for a second save.")

(defparameter *requantised-blocks-most* 60000
  "The most blocks of a picture measured for a second save, spread over it.")

(defparameter *requantised-counted-least* 100
  "How many blocks the fuller of a step's two neighbours must hold for the
step to be measured for a second save: fewer count too unsteadily to tell a
gap.")

(defun clipped-pixels (grey colours)
  "Which pixels of GREY, a picture's grey levels, are clipped: black or white,
or, for a picture in COLOURS ((OCTETS . CHANNELS) as PICTURE-GREY gives
them), one of their red, green and blue at its least or most. A JPEG's
decoder clips there, so that no step tells what was saved. A vector of bits,
1 for clipped."
  (let ((clipped (make-array (length grey) :element-type 'bit :initial-element 0)))
    (declare (type grey-pixels grey) (type simple-bit-vector clipped))
    (dotimes (i (length grey))
      (let ((g (aref grey i))) (when (or (= g 0) (= g 255)) (setf (sbit clipped i) 1))))
    (when colours
      (destructuring-bind (octets . channels) colours
        (declare (type grey-pixels octets) (type fixnum channels))
        (dotimes (i (length grey))
          (let ((at (* i channels)))
            (dotimes (c 3)
              (let ((o (aref octets (+ at c)))) (when (or (= o 0) (= o 255)) (setf (sbit clipped i) 1))))))))
    clipped))

(defun block-frequencies (grey clipped width x0 y0 positions out)
  "Into OUT, the frequencies of POSITIONS (across plus eight times down) of
the 8-by-8 block of GREY (WIDTH pixels to a row) whose first pixel is at X0,
Y0, as a JPEG works them out (*BLOCK-BASIS*); NIL, OUT untouched, when the
block holds a pixel CLIPPED says is (CLIPPED-PIXELS), which no JPEG step
tells."
  (declare (type grey-pixels grey) (type simple-bit-vector clipped) (type fixnum width x0 y0)
           (type (simple-array double-float (*)) out) (optimize speed))
  (dotimes (y 8)
    (let ((at (+ (* (+ y0 y) width) x0)))
      (dotimes (x 8)
        (when (= 1 (sbit clipped (+ at x))) (return-from block-frequencies nil)))))
  (let ((basis *block-basis*))
    (declare (type (simple-array double-float (64)) basis))
    (loop for p across positions for k fixnum from 0
          do (let ((u (mod p 8)) (v (floor p 8)) (sum 0d0))
               (declare (type double-float sum))
               (dotimes (y 8)
                 (let ((at (+ (* (+ y0 y) width) x0)) (row 0d0))
                   (declare (type double-float row))
                   (dotimes (x 8) (incf row (* (aref basis (+ (* u 8) x)) (aref grey (+ at x)))))
                   (incf sum (* (aref basis (+ (* v 8) y)) row))))
               (setf (aref out k) sum)))
    out))

(defun jpeg-grid (grey clipped width height positions steps
                  &optional (corners (loop for k below 64 collect k)) (most 20000))
  "Where the 8-by-8 blocks of the JPEG whose pixels are GREY (WIDTH by HEIGHT)
lie, as the column and row of the first one's corner, each from 0 to 7, and
how far from its steps its frequencies lie there: the mean distance from the
nearest whole step of POSITIONS' frequencies over STEPS, those a step or more
from nothing, 0 on the steps, a quarter for none. A picture turned as its
EXIF says may keep its blocks off the corner. At most MOST blocks (20,000
unless given) spread over the picture are tried for each corner, those with
a pixel CLIPPED says is passed over (CLIPPED-PIXELS). Only the CORNERS are
tried, each the column plus eight times the row, all 64 unless given. NIL
when they hold fewer than 200 such frequencies."
  (let ((out (make-array (length positions) :element-type 'double-float))
        (best nil) (best-x 0) (best-y 0)
        (stride (max 1 (floor (* (floor width 8) (floor height 8)) most))))
    (dolist (corner corners)
      (let ((ox (mod corner 8)) (oy (floor corner 8)))
        (let ((sum 0d0) (count 0) (k 0))
          (loop for y0 from oy to (- height 8) by 8
                do (loop for x0 from ox to (- width 8) by 8
                         do (when (zerop (mod (incf k) stride))
                              (when (block-frequencies grey clipped width x0 y0 positions out)
                                (loop for f across out for q across steps
                                      for c = (/ f q)
                                      when (>= (abs c) 0.75d0)
                                        do (incf sum (abs (- c (fround c)))) (incf count))))))
          (when (>= count 200)
            (let ((mean (/ sum count)))
              (when (or (null best) (< mean best)) (setf best mean best-x ox best-y oy)))))))
    (and best (values best-x best-y best))))

;;; A JPEG kept without loss

(defparameter *history-blocks* 2000
  "The most blocks of 8 by 8 pixels of a picture measured for a JPEG's
traces at each of the 64 places its grid can begin (JPEG-HISTORY), spread
over the picture.")

(defparameter *history-rational* '(0 4 32 36)
  "The frequencies (across plus eight times down) left out when a picture's
frequencies are set beside whole numbers (JPEG-HISTORY): the blocks' mean
and those whose cosines are all a half or the root of a half, which any
picture of whole grey levels puts on multiples of an eighth.")

(defun block-all-frequencies (grey width x0 y0 rows out)
  "Into OUT (64), every frequency of the 8-by-8 block of GREY (WIDTH pixels
to a row) whose first pixel is at X0, Y0, as a JPEG works them out
\(*BLOCK-BASIS*), at across plus eight times down; ROWS (64) is room for the
work. NIL, OUT untouched, when the block's pixels are all one grey level,
which tells nothing of how it was saved."
  (declare (type grey-pixels grey) (type fixnum width x0 y0)
           (type (simple-array double-float (64)) rows out) (optimize speed))
  (let ((basis *block-basis*) (first (aref grey (+ (* y0 width) x0))) (flat t))
    (declare (type (simple-array double-float (64)) basis))
    (dotimes (y 8)
      (let ((at (+ (* (+ y0 y) width) x0)))
        (declare (type fixnum at))
        (dotimes (x 8) (unless (= (aref grey (+ at x)) first) (setf flat nil)))
        (dotimes (u 8)
          (let ((sum 0d0))
            (declare (type double-float sum))
            (dotimes (x 8) (incf sum (* (aref basis (+ (* u 8) x)) (aref grey (+ at x)))))
            (setf (aref rows (+ (* y 8) u)) sum)))))
    (unless flat
      (dotimes (v 8 out)
        (dotimes (u 8)
          (let ((sum 0d0))
            (declare (type double-float sum))
            (dotimes (y 8) (incf sum (* (aref basis (+ (* v 8) y)) (aref rows (+ (* y 8) u)))))
            (setf (aref out (+ (* v 8) u)) sum)))))))

(defparameter *history-least* 2d0
  "How far from nothing, at least, a frequency of a block must lie to be set
beside whole numbers (HISTORY-LATTICE). Nearer, the few pixels a level from
the paper of a clean page make sums of a cosine or two, which fall near
whole numbers more often than not: from 1, made-up scans of grain 0.5 never
saved as JPEGs gave 0.09, as much as a JPEG of quality 30 measured on the
wrong grid; from 2, at most 0.040.")

(defun history-blocks (grey width height)
  "The places of the blocks of GREY (WIDTH by HEIGHT) that JPEG-HISTORY
measures, as a list of (X0 . Y0), the corners of blocks on a grid from the
picture's corner, at most *HISTORY-BLOCKS* spread over those whose 15 by 15
pixels from the corner, the block moved by up to 7 across and down, hold
more than one grey level. So every place a grid can begin is measured on
the same part of the picture, and blank paper is passed over."
  (declare (type grey-pixels grey) (type fixnum width height))
  (let ((usable '()))
    (loop for y0 of-type fixnum from 0 to (- height 15) by 8
          do (loop for x0 of-type fixnum from 0 to (- width 15) by 8
                   do (let ((first (aref grey (+ (* y0 width) x0))))
                        (unless (loop for j below 15
                                      always (let ((at (+ (* (+ y0 j) width) x0)))
                                               (loop for i below 15 always (= (aref grey (+ at i)) first))))
                          (push (cons x0 y0) usable)))))
    (let ((stride (max 1 (ceiling (length usable) *history-blocks*))))
      (loop for place in (nreverse usable) for k from 0 when (zerop (mod k stride)) collect place))))

(defun block-new-p (grey width x0 y0 seen)
  "True when the 8-by-8 block of GREY (WIDTH pixels to a row) whose first
pixel is at X0, Y0 is unlike every block SEEN holds (a table by EQUAL),
which it is then added to."
  (declare (type grey-pixels grey) (type fixnum width x0 y0))
  (let ((key (make-array 64 :element-type '(unsigned-byte 8))))
    (dotimes (y 8) (replace key grey :start1 (* y 8) :start2 (+ (* (+ y0 y) width) x0) :end1 (+ (* y 8) 8)))
    (let ((key (map 'string #'code-char key)))
      (unless (gethash key seen) (setf (gethash key seen) t)))))

(defun history-lattice (grey clipped width places ox oy &optional kept)
  "How near whole numbers the frequencies of the blocks at PLACES of GREY
\(WIDTH pixels to a row, HISTORY-BLOCKS) lie, each block moved by OX and OY,
as two values: the sum of the cosines of a whole turn times each frequency
*HISTORY-LEAST* or more from nothing, and how many were summed. Spread
evenly, the sum stays within about the root of half their number of
nothing; near whole numbers, it grows with it. Blocks holding a pixel
CLIPPED says is (CLIPPED-PIXELS), or of one grey level, are passed over, as
are the frequencies of *HISTORY-RATIONAL*. A block the same pixel for pixel
as one measured already is passed over too: a page drawn by a program with
rules or a grid every 8 pixels repeats its blocks exactly, and their few
frequencies, whole numbers or not by chance, would count a thousand times.
With KEPT, a vector of 64 lists, each block's frequencies are pushed on the
list of their place."
  (declare (type grey-pixels grey) (type simple-bit-vector clipped) (type fixnum width ox oy))
  (let ((rows (make-array 64 :element-type 'double-float)) (out (make-array 64 :element-type 'double-float))
        (sum 0d0) (count 0) (least (float *history-least* 1d0)) (seen (make-hash-table :test 'equal))
        (left (let ((mask (make-array 64 :element-type 'bit :initial-element 0)))
                (dolist (k *history-rational* mask) (setf (sbit mask k) 1)))))
    (declare (type double-float sum least) (type fixnum count) (type simple-bit-vector left)
             (optimize speed))
    (loop for (x . y) in places
          for x0 of-type fixnum = (+ x ox) for y0 of-type fixnum = (+ y oy)
          unless (loop for j below 8
                       thereis (let ((at (+ (* (+ y0 j) width) x0)))
                                 (loop for i below 8 thereis (= 1 (sbit clipped (+ at i))))))
            do (when (and (block-new-p grey width x0 y0 seen) (block-all-frequencies grey width x0 y0 rows out))
                 (dotimes (k 64)
                   (when (zerop (sbit left k))
                     (let ((c (aref out k)))
                       (when kept (push c (svref kept k)))
                       (when (>= (abs c) least)
                         (incf sum (cos (* 2d0 pi c)))
                         (incf count)))))))
    (values sum count)))

(defun history-step (values)
  "The step a JPEG rounded the frequencies VALUES (of one place, across the
blocks) to, as the largest whole step from 2 to 120 on whose multiples those
of half a step or more from nothing lie: their mean cosine of a whole turn
times each over the step 0.6 or more, of 30 or more. NIL when there is
none."
  (let ((values (coerce values '(simple-array double-float (*)))))
    (declare (type (simple-array double-float (*)) values) (optimize speed))
    (loop for q of-type fixnum from 120 downto 2
          do (let ((sum 0d0) (count 0) (half (/ q 2d0)) (turn (/ (* 2d0 pi) q)))
               (declare (type double-float sum half turn) (type fixnum count))
               (loop for c across values
                     when (>= (abs c) half) do (incf sum (cos (* turn c))) (incf count))
               (when (and (>= count 30) (>= (/ sum count) 0.6d0)) (return q))))))

(defparameter *history-sigmas-least* 5
  "How many standard deviations, at least, the frequencies of a picture must
stand off spread evenly on the grid where they lie nearest whole numbers
\(JPEG-HISTORY) for its JPEG traces to be told at all: fewer, a picture
with too few blocks to measure, a page of little ink or one of few grey
levels, cannot be told either way.")

(defparameter *history-lattice-least* 0.08d0
  "The mean cosine of a whole turn times a picture's frequencies, on the grid
where they lie nearest whole numbers (JPEG-HISTORY), from which it was once
saved as a JPEG. Pictures never saved so gave at most 0.040: made-up scans
of grain 0 to 3, born digital or blurred, posterised to 4 to 16 levels,
1-bit, JPEG 2000, and real scans kept without loss, but for one colour page
at 0.09 whose lowest frequencies lie on steps, most likely once a JPEG;
pages drawn on a grid or ruled every 8 pixels, at most 0.020. Saved as
JPEGs of quality 30 to 98, decoded and kept without loss, a third cropped by
1 to 7 pixels after, they gave 0.153 to 0.204, 0.187 in the middle
\(doc/measures.org).")

(defun jpeg-history (grey width height &optional colours)
  "Whether the picture GREY (WIDTH by HEIGHT, in COLOURS as PICTURE-GREY gives
them) was once saved as a JPEG, though it is kept without loss now. A JPEG
rounds each frequency of its blocks of 8 by 8 pixels to a whole step of its
table; decoded, the pixels made whole grey levels again move each by a
quarter of a level or so. So every frequency lies near a whole number on the
grid the JPEG had, wherever that now begins, however fine its quality, and
the lower frequencies on multiples of their steps; a picture never saved so
has frequencies spread evenly. The grid is taken where the sum of
HISTORY-LATTICE stands furthest above nothing for its number: of the 64
places it can begin, the four best on a quarter of the blocks (all of them,
400 or fewer) are measured again on all of them. It was once saved as a
JPEG when the sum stands *HISTORY-SIGMAS-LEAST* standard deviations or more
above nothing and the mean cosine there is *HISTORY-LATTICE-LEAST* or more.
As six values: whether it was; the mean cosine; how many standard
deviations; the grid's corner, (COLUMN ROW) from 0 to 7; for the record,
the steps found there for the lowest 20 frequencies but two, in the order
a JPEG keeps them, NIL where none is (HISTORY-STEP); and how many
frequencies were measured. NIL when fewer than 50 can be."
  (let* ((clipped (clipped-pixels grey colours))
         (places (history-blocks grey width height))
         (few (if (> (length places) 400)
                  (loop for place in places for k from 0 when (zerop (mod k 4)) collect place)
                  places))
         (best nil) (best-sum 0d0) (best-x 0) (best-y 0) (best-count 0))
    (flet ((sigmas (places ox oy)
             (multiple-value-bind (sum count) (history-lattice grey clipped width places ox oy)
               (values (and (>= count 50) (/ sum (sqrt (/ count 2d0)))) sum count))))
      ;; every place a grid can begin, on a quarter of the blocks; the four best again on all of them
      (dolist (corner (subseq (sort (loop for k below 64 collect (cons (or (sigmas few (mod k 8) (floor k 8)) -1d6) k))
                                    #'> :key #'car)
                              0 4))
        (let ((ox (mod (cdr corner) 8)) (oy (floor (cdr corner) 8)))
          (multiple-value-bind (sigmas sum count) (sigmas places ox oy)
            (when (and sigmas (or (null best) (> sigmas best)))
              (setf best sigmas best-sum sum best-x ox best-y oy best-count count))))))
    (when best
      (let* ((kept (make-array 64 :initial-element '()))
             (mean (progn (history-lattice grey clipped width places best-x best-y kept) (/ best-sum best-count)))
             (steps (loop for k from 1 to 20
                          for p = (aref *zigzag* k)
                          unless (member p *history-rational*) collect (history-step (svref kept p)))))
        (values (and (>= best *history-sigmas-least*) (>= mean *history-lattice-least*))
                mean best (list best-x best-y) steps best-count)))))

(defun requantised-measure (grey width height table &optional colours)
  "How far the frequencies of the JPEG whose pixels are GREY (WIDTH by
HEIGHT, in COLOURS as PICTURE-GREY gives them when it has them), saved with
TABLE (its 64 steps in the order it keeps them), count unevenly over their
steps, as a second save leaves them. Its blocks are found by the first five
of those frequencies (JPEG-GRID). For each of its lowest
*REQUANTISED-FREQUENCIES* frequencies whose step is 2 or more, the blocks
are counted at each step from nothing, both signs together; each step from
the second the fuller of whose neighbours holds *REQUANTISED-COUNTED-LEAST*
blocks or more is set beside them, the logarithm of its count over theirs
\(their geometric mean, each count one more). Saved once, counts fall evenly
and that is near nothing; saved twice with other steps first, some steps
are left all but empty and others crowded, regularly. As three values: the
middle over the frequencies of each one's mean, the frequencies measured,
each (POSITION STEP MEAN EMPTY), EMPTY how many of its steps held a tenth
of their neighbours or less, and how far from its steps the picture lies
\(JPEG-GRID). NIL, and why not in words, when it cannot be measured."
  (let* ((chosen (loop for k from 1 to *requantised-frequencies*
                       for q = (elt table k)
                       when (>= q 2) collect (list (aref *zigzag* k) q)))
         (positions (coerce (mapcar #'first chosen) '(simple-array fixnum (*))))
         (steps (coerce (mapcar (lambda (c) (float (second c) 1d0)) chosen) '(simple-array double-float (*))))
         (clipped (clipped-pixels grey colours)))
    (when (null chosen)
      (return-from requantised-measure
        (values nil "its steps are all 1, as at the highest quality: a second save leaves no trace there")))
    (multiple-value-bind (ox oy off) (jpeg-grid grey clipped width height
                                                (subseq positions 0 (min 5 (length positions)))
                                                (subseq steps 0 (min 5 (length steps))))
      (cond ((null ox) (return-from requantised-measure (values nil "too few of its blocks hold no black or white")))
            ((> off 0.15d0)
             (return-from requantised-measure
               (values nil (format nil "its pixels lie off its own steps (~,2f of a step, ~
                                        0.25 being none), as when it was turned, sized or changed after it was decoded"
                                   off)))))
      (let* ((largest *requantised-largest*)
             (counts (make-array (list (length positions) (1+ largest)) :element-type 'fixnum :initial-element 0))
             (out (make-array (length positions) :element-type 'double-float))
             (blocks (* (floor (- width ox) 8) (floor (- height oy) 8)))
             (stride (max 1 (ceiling blocks *requantised-blocks-most*))) (k 0))
        (loop for y0 from oy to (- height 8) by 8
              do (job-check-stop)
                 (loop for x0 from ox to (- width 8) by 8
                       do (when (zerop (mod (incf k) stride))
                            (when (block-frequencies grey clipped width x0 y0 positions out)
                              (loop for f across out for q across steps for i from 0
                                    for c = (abs (round f q))
                                    when (<= c largest) do (incf (aref counts i c)))))))
        (let ((measured
                (loop for i below (length positions)
                      for logs = (loop for s from 2 below largest
                                       for before = (aref counts i (1- s)) for after = (aref counts i (1+ s))
                                       when (>= (max before after) *requantised-counted-least*)
                                         collect (log (/ (1+ (aref counts i s)) (sqrt (* (1+ before) (1+ after))))))
                      when (>= (length logs) 3)
                        collect (list (aref positions i) (round (aref steps i))
                                      (/ (reduce #'+ logs :key #'abs) (length logs))
                                      (count-if (lambda (l) (< l (log 0.1))) logs)))))
          (if (null measured)
              (values nil "too few of its blocks hold enough at its lower frequencies to count")
              (values (median-of (mapcar #'third measured)) measured off)))))))

(defparameter *requantised-look* 1.0
  "The measure of a JPEG's second save (REQUANTISED-MEASURE) from which it is
worth a look, never more. Pictures saved once gave at most 0.47 (real scans
and phone pictures) and 0.30 (made-up ones); saved twice, coarser first,
1.1 to 4.3; saved finer first, under 0.8, not told (doc/measures.org).")

(defun check-saved-twice (file)
  "Whether FILE, a JPEG PICTURE-FILE, was saved twice as a JPEG
\(REQUANTISED-MEASURE), worth a look at most, with the honest causes named;
and, for the record, what its tables are, libjpeg's standard ones at a
quality or a camera's or scanner's own."
  (destructuring-bind (&optional octets quality table) (picture-file-tables file)
    (declare (ignore octets))
    (when (and table (picture-file-grey file))
      (let ((standard (standard-table-quality table)) (out '()))
        (push (found :record (format nil "The JPEG's steps are ~:[a table of its own, not libjpeg's standard one ~
                                          (about quality ~d), as cameras, phones and scanners often keep~;~:*~
                                          libjpeg's standard table at quality ~d, as most programs that save ~
                                          JPEGs write~]."
                                     standard (or standard quality))
                     :rule (concatenate 'string "A JPEG keeps the steps its frequencies were rounded to; libjpeg, "
                                                "which most programs save with, scales the standard's table for "
                                                "each quality, while many devices keep tables of their own."))
              out)
        (multiple-value-bind (measure measured off)
            (requantised-measure (picture-file-grey file) (picture-file-width file) (picture-file-height file) table
                                 (picture-file-colours file))
          (cond ((null measure)
                 (push (found :record (format nil "Whether the JPEG was saved twice was not measured: ~a." measured)
                              :rule (documentation 'requantised-measure 'function))
                       out))
                ((>= measure *requantised-look*)
                 (push (found :look (format nil "The JPEG's frequencies fall unevenly over its steps (~,2f, ~,2f or ~
                                                 more being worth a look): saved as a JPEG twice, with other steps ~
                                                 the first time. A scanner or phone app that saves twice, a viewer ~
                                                 that turned it and saved it again, or an honest crop does the same."
                                            measure *requantised-look*)
                              :rule (documentation 'requantised-measure 'function)
                              :details (cons (format nil "its pixels ~,2f of a step from its own steps" off)
                                             (loop for (position step mean empty) in measured
                                                   collect (format nil "frequency ~d across, ~d down, step ~d: ~
                                                                        ~,2f, ~d step~:p all but empty"
                                                                   (mod position 8) (floor position 8) step mean
                                                                   empty)))
                              :method :picture-editor)
                       out))
                (t
                 (push (found :record (format nil "The JPEG's frequencies fall evenly over its steps (~,2f, under ~
                                                   ~,2f): no sign of a second save with other steps. A second save ~
                                                   with the same steps, or at the highest quality, leaves none."
                                              measure *requantised-look*)
                              :rule (documentation 'requantised-measure 'function))
                       out))))
        (nreverse out)))))

(defun picture-findings (pdf file progress)
  "Every sign found in the picture file FILE (a PICTURE-FILE, *PICTURE-FILE*
bound to it), examined as PDF, its one page: what it says of itself and of
how it was made, a JPEG saved twice, a JPEG's header and thumbnail beside
the device it names, and its page's checks (CHECK-SCANS,
CHECK-SCAN-PIECES, CHECK-LOCAL-TRACES, CHECK-GENERATOR-GRID,
CHECK-COPIED-PAPER, CHECK-REPEATS), the strongest first. PROGRESS is called with a number
from 0 to 1."
  (let ((last (last-revision pdf)))
    (sorted-findings
     (append (run-check "what the picture says of itself" #'check-picture-metadata file)
             (run-check "what the picture says of how it was made" #'check-picture-making file)
             (progn (funcall progress 1/10) nil)
             (run-check "the JPEG's saves" #'check-saved-twice file)
             (run-check "the JPEG's header and thumbnail" #'check-jpeg-header-file file)
             (progn (funcall progress 3/10) nil)
             (run-check "the scanned page" #'check-scans pdf last)
             (progn (funcall progress 9/10) nil)
             (run-check "the scanned page's JPEG blocks" #'check-scan-pieces pdf last)
             (run-check "the scanned page's JPEG traces" #'check-local-traces pdf last)
             (run-check "the picture's pixels as a generator draws them" #'check-generator-grid pdf last)
             (run-check "the picture's copied paper" #'check-copied-paper pdf last)
             (run-check "the picture's marks repeated" #'check-repeats pdf last))
     last)))

(defun examine-picture (job analysis octets kind)
  "What a job examining a picture file of KIND does once its OCTETS are read
and summed into ANALYSIS: the picture read (READ-PICTURE-FILE) and examined
as one scanned page (PICTURE-FINDINGS), its pixels let go after. No revision
is written: a picture file has none."
  (funcall (stage job "Reading the picture" 0.05 0.3) 0)
  (let* ((file (read-picture-file octets kind))
         (*picture-file* file)
         (pdf (read-pdf (picture-page-octets (max 1 (picture-file-width file)) (max 1 (picture-file-height file)))
                        :path (analysis-path analysis) :name (analysis-name analysis))))
    (job-check-stop)
    (setf (analysis-pdf analysis) pdf
          (analysis-picture analysis) file
          (analysis-findings analysis) (picture-findings pdf file (stage job "Looking for signs of editing" 0.3 0.95)))
    (analysis-traits analysis)
    (setf (picture-file-grey file) nil (picture-file-colours file) nil
          (analysis-shown analysis) (multiple-value-list (pieces-text (report-pieces analysis)))
          (palimpsest-job-analysis job) analysis)
    (forget-workings pdf)))

(defun call-with-picture-pixels (analysis cancel function)
  "Call FUNCTION with *PICTURE-FILE* bound to ANALYSIS's picture read again from
its file, for what asks for its pixels after they were let go, as the drawing
of its signs does; with ANALYSIS of a PDF, just call it. CANCEL is the cell
that stops a sandboxed read. An error when the file is no longer the one
examined (its SHA-256 differs)."
  (let ((picture (analysis-picture analysis)))
    (if (null picture)
        (funcall function)
        (let* ((*sandbox-cancel* cancel)
               (*picture-file* (read-picture-file (source-octets (analysis-path analysis) (analysis-sha256 analysis))
                                                  (picture-file-kind picture))))
          (funcall function)))))

(defmacro with-picture-pixels ((analysis cancel) &body body)
  "Run BODY with the pixels of ANALYSIS's picture file to hand when it is of one
\(CALL-WITH-PICTURE-PIXELS); CANCEL as there."
  `(call-with-picture-pixels ,analysis ,cancel (lambda () ,@body)))
